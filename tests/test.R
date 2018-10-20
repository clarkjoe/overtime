library(overtime)
library(magrittr)
library(multidplyr)
library(lazyeval)
library(tidyverse)
library(lubridate)
library(purrr)

data <- readRDS('./data/rawData.rds')

########################

data %>%
  rename(Grouping = AccountNumber, Dock = Date, Number = Count) %>%
  make_all_nested_columns() %>%
  make_nested_interval_columns('days', 1) %>%
  make_nested_interval_columns('days', 2) %>%
  make_nested_interval_columns('days', 10) %>%
  make_nested_interval_columns('days', 20) %>%
  extract_nested_columns()

########################

