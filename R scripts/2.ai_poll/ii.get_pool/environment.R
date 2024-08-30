# load utils
library(data.table)
library(dplyr)

# load utils to deal with apis
library(httr)
library(jsonlite)
library(rvest)

# sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# load X API utils
source('utils/X.api.v2_functions.R')

# load date-time utils
source('utils/date.time_functions.R')

# load project-specific query parameters 
source(file = 'R scripts/2.ai_poll/i.set_parameters/z.hyper_get_user_pool.R')