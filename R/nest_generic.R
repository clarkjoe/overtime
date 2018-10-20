#' @title overtime_by
#' @import tidyverse lubridate multidplyr lazyeval
#' @importFrom Hmisc capitalize
#' @export
#' @description **Designed to append on numerous summary statistics as columns
#' then nest into a tibble with grouped variables in `data` being the identifier(s).
#' Based on `type` parameter, will aggregate and create cognostics by:**
#' * Day
#' * Week
#' * Month
#' * Year
#' To allow further appending, the newly created nested tibble and the original data
#' will be return in a list.
#'
#' @param data Nested tibble or non-nested tibble with the following columns:
#' * A unique identifier (usually made by a `group_by`` call)
#' * A continious date column
#' * A numeric column, one observation per date per unique identifier
#' @param type All meaningfull specifications in English language are supported.
#' Stable arguments are:
#' * `day`
#' * `week`
#' * `month`
#' * `year`
#'
#' @return List that contains [nestedTibble, tibble]
#'
#' @examples NULL
overtime_by <- function(data, type) {

  dataIsList = FALSE
  nestedTibble <- NULL

  if (is_bare_list(data)) {
    nestedTibble <- data[[1]]
    data <- data[[2]]
    dataIsList = TRUE
  }

  date <- data %>%
    ungroup() %>%
    select_if(is.Date) %>%
    colnames()

  count <- data %>%
    ungroup() %>%
    select_if(is_bare_numeric) %>%
    colnames()

  floorDateColumn <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))

  groupVariables <- group_vars(data)
  if (is_empty(groupVariables)) {
    stop("overtime_by HALTED: data must have at least one group_by variable")
  }

  data <- map(list(is.Date, is_bare_numeric), ~ data %>% select_if(.x)) %>%
    bind_cols() %>%
    select(-contains(groupVariables))

  returnTibble <- data %>%
    group_by(floorDateColumn = floor_date(get(date), type), add = TRUE) %>%
    summarise(Count = sum(get(count))) %>%
    partition_(as.lazy_dots(groupVariables)) %>%
    summarise(!!paste0(letter, "_Count") := sum(Count),
              !!paste0(letter, "_Mean") := mean(Count),
              !!paste0(letter, "_Median") := median(Count),
              !!paste0(letter, "_SD") := sd(Count),
              !!paste0(letter, "_Max") := max(Count),
              !!paste0(letter, "_Min") := min(Count),
              !!paste0(letter, "_CV") := (sd(Count) / mean(Count)),

              #######################################################################

              # !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(floorDateColumn),
              #                                 data = .)[["coefficients"]][2]),
              !!paste0(letter, "_OOC2") := (sum(Count >= (mean(Count) + (2 * sd(Count))))),
              !!paste0(letter, "_OOC3") := (sum(Count >= (mean(Count) + (3 * sd(Count))))),

              #######################################################################

              !!paste0(letter, "_P") := overtime::find_SignedSequence(Count, 1),
              !!paste0(letter, "_N") := overtime::find_SignedSequence(Count, -1),
              !!paste0(letter, "_Z") := overtime::find_SignedSequence(Count, 0),

              #######################################################################

              !!paste0(letter, "_I") := overtime::find_LadderSequence(Count, "I"),
              !!paste0(letter, "_D") := overtime::find_LadderSequence(Count, "D"),
              !!paste0(letter, "_IP") := overtime::find_LadderSequence(Count, "IP"),
              !!paste0(letter, "_DP") := overtime::find_LadderSequence(Count, "DP"),
              !!paste0(letter, "_IN") := overtime::find_LadderSequence(Count, "IN"),
              !!paste0(letter, "_DN") := overtime::find_LadderSequence(Count, "DN")
    ) %>%
    collect() %>%
    group_by_at(vars(groupVariables)) %>%
    nest(.key = "Cogs") %>%
    rename(!!paste0(letter, "_Cognostics") := Cogs)

  if (dataIsList) {
    returnTibble %<>%
      left_join(nestedTibble, groupVariables)
  }
  return(list(returnTibble, data))
}

######################################################################

#' @title overtime_get
#' @export
#' @description **Return the first element in list that contains [nestedTibble, tibble]**
#'
#' @param data List with the following elements:
#' * Nested Tibble
#' * Tibble
#'
#' @return Unnested tibble
#'
#' @examples NULL
overtime_get <- function(data) {
  return(data[[1]])
}
