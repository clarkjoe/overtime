library(overtime)
library(magrittr)
library(multidplyr)
library(lazyeval)
library(tidyverse)
library(lubridate)

data <- readRDS('./data/rawData.rds')

data %>%
  rename(Grouping = AccountNumber, Dock = Date, Number = Count) %>%
  make_nested_columns("day")

