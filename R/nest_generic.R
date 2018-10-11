#' @title make_all_nested_columns
#' @import tidyverse
#' @export
#' @description **Designed to create then join four nested tibbles into one tibble
#' with grouped variables in `data` being the identifier(s) by:**
#' * Daily
#' * Weekly
#' * Monthly
#' * Yearly
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#'
#' @return nested and joined tibble
#'
#' @examples test
make_all_nested_columns <- function(data) {

  groupVariables <- group_vars(data)

  day <- make_nested_columns(data, "day")
  week <- make_nested_columns(data, "week")
  month <- make_nested_columns(data, "month")
  year <- make_nested_columns(data, "year")

  day %>%
    left_join(., week, by = groupVariables) %>%
    left_join(., month, by = groupVariables) %>%
    left_join(., year, by = groupVariables)
}

######################################################################

#' @title make_nested_columns
#' @import tidyverse lubridate multidplyr lazyeval
#' @importFrom Hmisc capitalize
#' @export
#' @description **Designed to append on numerous descriptive numeric cognostics as columns
#' then nest into a tibble with grouped variables in `data` being the identifier(s).
#' Based on `type` parameter, will aggregate and create cognostics by:**
#' * Day
#' * Week
#' * Month
#' * Year
#'
#' @param data Tibble/Data Frame with the following columns:
#' * Account Number (unique identifier)
#' * Date
#' * Count
#' @param type All meaningfull specifications in
#' English language are supported.
#' Stable arguments are:
#' * `day`
#' * `week`
#' * `month`
#' * `year`
#'
#' @return nested tibble
#'
#' @examples test
make_nested_columns <- function(data, type) {

  date <- data %>%
    ungroup() %>%
    select_if(is.Date) %>%
    colnames()

  count <- data %>%
    ungroup() %>%
    select_if(is_bare_numeric) %>%
    colnames()

  tmpColName <- capitalize(type)
  letter <- capitalize(substr(type, 1, 1))

  groupVariables <- group_vars(data)

  if (is_empty(groupVariables)) {
    stop("make_nested_columns HALTED: tibble must have at least one group_by variable")
  }

  data <- map(list(is.Date, is_bare_numeric), ~ data %>% select_if(.x)) %>%
    bind_cols() %>%
    select(-contains(groupVariables))

  data %>%
    group_by(tmpColName = floor_date(get(date), type), add = TRUE) %>%
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

              # !!paste0(letter, "_SLP") := (lm(Count ~ as.numeric(tmpColName),
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
}
