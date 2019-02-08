library(overtime)
library(tidyverse)
library(magrittr)

data <- readRDS('./data/rawData.rds')

########################

data %>%
  rename(Grouping = AccountNumber, Dock = Date, Number = Count) %>%
  overtime_by("day") %>%
  overtime_by("week") %>%
  overtime_by("month") %>%
  overtime_by("year") %>%
  overtime_by_interval('days', 1) %>%
  overtime_by_interval('days', 2) %>%
  overtime_by_interval('days', 10) %>%
  overtime_by_interval('days', 20) %>%
  overtime_get() %>%
  overtime_unnest()

########################

