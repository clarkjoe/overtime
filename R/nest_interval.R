#' @title overtime_by_interval
#' @import tidyverse
#' @export
#' @description **By time interval, designed to append on numerous summary statistics as columns.
#' Then nest into a tibble with grouped variables in `data` being the identifier(s).
#' Based on `intervalType` and `intervalLength` parameters, will aggregate
#' and create columns accordingly**
#' To allow further appending, the newly created nested tibble and the original data
#' will be return in a list.
#'
#' @param data Nested tibble or non-nested tibble with the following columns:
#' * A unique identifier (usually made by a `group_by`` call)
#' * A continious date column
#' * A numeric column, one observation per date per unique identifier
#' @param intervalType Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param intervalLength Must be positive integer excluding zero
#'
#' @return List that contains [nestedTibble, tibble]
#'
#' @examples NULL
overtime_by_interval <- function(data, intervalType, intervalLength) {

  if (is_bare_list(data)) {
    tibble <- data[[1]]
    data <- data[[2]]

    groupVariables <- group_vars(data)
    if (is_empty(groupVariables)) {
      stop("overtime_by_interval HALTED: data must have at least one group_by variable")
    }

    returnTibble <- data %>%
      prepare_intervals(intervalType, intervalLength) %>%
      overtime_get() %>%
      left_join(tibble, groupVariables)

    # newTibble <- left_join(tibbleAndDataList[[1]], tibble, groupVariables)
  }
  else {
    groupVariables <- group_vars(data)
    if (is_empty(groupVariables)) {
      stop("overtime_by_interval HALTED: data must have at least one group_by variable")
    }

    returnTibble <- data %>%
      prepare_intervals(intervalType, intervalLength)
  }

  return(list(returnTibble, data))
}

######################################################################

#' @title prepare_intervals
#' @import tidyverse lubridate
#' @importFrom purrr map2
#' @importFrom Hmisc capitalize
#' @description **Designed to prepare data by pulling out left, right, and ratio intervals**
#'
#' @param data Non-nested tibble with the following columns:
#' * A unique identifier (usually made by a `group_by`` call)
#' * A continious date column
#' * A numeric column, one observation per date per unique identifier
#' @param intervalType Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param intervalLength Must be positive integer excluding zero
#'
#' @return List that contains [nestedTibble, tibble]
#'
#' @examples NULL
prepare_intervals <- function(data, intervalType, intervalLength) {

  date <- data %>%
    ungroup() %>%
    select_if(is.Date) %>%
    colnames()

  letter <- capitalize(substr(intervalType, 1, 1))

  groupVariables <- group_vars(data)
  if (is_empty(groupVariables)) {
    stop("prepare_intervals HALTED: data must have at least one group_by variable")
  }

  intervalData <- data %>%
    filter(get(date) %within% ((max(get(date)) - invoke(get(intervalType), intervalLength * 2)) %--% max(get(date))))

  allowedData <- intervalData %>%
    # Remove all accounts that don't meet ENTIRE interval requirements
    filter(min(get(date)) == max(get(date)) - invoke(get(intervalType), intervalLength * 2))

  rightData <- allowedData %>%
    filter(get(date) >= (max(get(date)) - invoke(intervalType, intervalLength))) %>%
    do_interval_work(intervalType, intervalLength, "R")

  leftData <- allowedData %>%
    filter(get(date) < (max(get(date)) - invoke(intervalType, intervalLength))) %>%
    do_interval_work(intervalType, intervalLength, "L")

  ratioData <- left_join(leftData, rightData, by = groupVariables)

  ratioData %<>%
    mutate(
      !!paste0(letter, intervalLength, "_Ratios") := map2(ratioData[[2]],
                                                   ratioData[[3]],
                                                   ~ as_tibble(.y/.x) %>%
                                                     rename_all(funs(sub('_.', "_Ratio", .))))) %>%
    select(groupVariables, contains("Ratio"))

  intervalData %<>%
    filter(min(get(date)) == max(get(date)) - invoke(intervalType, intervalLength * 2)) %>%
    do_interval_work(intervalType, intervalLength, "A")

  returnTibble <- left_join(intervalData, leftData, by = groupVariables) %>%
    left_join(., rightData, by = groupVariables) %>%
    left_join(., ratioData, by = groupVariables)

  return(list(returnTibble, data))
}

######################################################################

#' @title do_interval_work
#' @import tidyverse Hmisc multidplyr lazyeval
#' @description **Designed to create cognostics by certain interval/ratio in the past**
#'
#' @param data Non-nested tibble with the following columns:
#' * A unique identifier (usually made by a `group_by`` call)
#' * A continious date column
#' * A numeric column, one observation per date per unique identifier
#' @param type Must be one of following strings:
#' * `years`
#' * `months`
#' * `weeks`
#' * `days`
#' @param intervalLength Must be positive integer excluding zero
#' @param divide Must be one of following strings:
#' * `A`
#' * `R`
#' * `L`
#'
#' @return List that contains [nestedTibble, tibble]
#'
#' @examples NULL
do_interval_work <- function(data, type, intervalLength, divide) {

  groupVariables <- group_vars(data)
  if (is_empty(groupVariables)) {
    stop("make_nested_interval_columns_work HALTED: tibble must have at least one group_by variable")
  }

  data %<>%
    rename(count = data %>%
             ungroup() %>%
             select_if(is_bare_numeric) %>%
             colnames())

  letter <- capitalize(substr(type, 1, 1))

  data <- map(list(is.Date, is_bare_numeric), ~ data %>% select_if(.x)) %>%
   bind_cols() %>%
   select(-contains(groupVariables))

  data %>%
    ungroup() %>%
    partition_(as.lazy_dots(groupVariables)) %>%
    summarise(!!paste0(letter, intervalLength, "_", divide, "_Count") := sum(count),
              !!paste0(letter, intervalLength, "_", divide, "_Mean") := mean(count),
              !!paste0(letter, intervalLength, "_", divide, "_Median") := median(count),
              !!paste0(letter, intervalLength, "_", divide, "_SD") := sd(count),
              !!paste0(letter, intervalLength, "_", divide, "_Max") := max(count),
              !!paste0(letter, intervalLength, "_", divide, "_Min") := min(count),
              !!paste0(letter, intervalLength, "_", divide, "_CV") := (sd(count) / mean(count)),

              #######################################################################

              #!!paste0(letter, intervalLength, "_", divide, "_SLP") := (lm(count ~ as.numeric(get(date)),
              #                                                       data = .)[["coefficients"]][2]),
              !!paste0(letter, intervalLength, "_", divide, "_OOC2") := (sum(count >= (mean(count) + (2 * sd(count))))),
              !!paste0(letter, intervalLength, "_", divide, "_OOC3") := (sum(count >= (mean(count) + (3 * sd(count))))),

              #######################################################################

              !!paste0(letter, intervalLength, "_", divide, "_P") := overtime::find_SignedSequence(count, 1),
              !!paste0(letter, intervalLength, "_", divide, "_N") := overtime::find_SignedSequence(count, -1),
              !!paste0(letter, intervalLength, "_", divide, "_Z") := overtime::find_SignedSequence(count, 0),

              #######################################################################

              !!paste0(letter, intervalLength, "_", divide, "_I") := overtime::find_LadderSequence(count, "I"),
              !!paste0(letter, intervalLength, "_", divide, "_D") := overtime::find_LadderSequence(count, "D"),
              !!paste0(letter, intervalLength, "_", divide, "_IP") := overtime::find_LadderSequence(count, "IP"),
              !!paste0(letter, intervalLength, "_", divide, "_DP") := overtime::find_LadderSequence(count, "DP"),
              !!paste0(letter, intervalLength, "_", divide, "_IN") := overtime::find_LadderSequence(count, "IN"),
              !!paste0(letter, intervalLength, "_", divide, "_DN") := overtime::find_LadderSequence(count, "DN")
    ) %>%
    collect() %>%
    group_by_at(vars(groupVariables)) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, intervalLength, "_", divide, "_Cognostics") := Cogs)
}

######################################################################

#' @title overtime_unnest
#' @import tidyverse purrr magrittr
#' @export
#' @description **Designed to unnest a nested tibble, which is a list of lists**
#'
#' @param data Nested tibble or tibble with the following columns:
#' * A unique identifier (usually made by a `group_by`` call)
#' * A continious date column
#' * A numeric column, one observation per date per unique identifier
#'
#' @return Unnested tibble
#'
#' @examples NULL
overtime_unnest <- function(data) {

  data %>%
    modify_if(is_list, ~ modify_if(., is_null,
                                   ~ tibble(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))) %>%
    unnest() %>%
    select(-num_range("", 1:1000000))
}
