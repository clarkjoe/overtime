library(overtime)
library(magrittr)
library(multidplyr)
library(lazyeval)

data <- readRDS('./data/rawData.rds')

data %<>% group_by(AccountNumber) %>%
  rename(Greg = AccountNumber)

groupVariables <- group_vars(data)

data %>%
  partition_(as.lazy_dots(groupVariables)) %>%
  collect()

data %>%
  nest_core('day')

