# load utils
library(data.table)
library(dplyr)

# load utils to deal with apis
library(httr)
library(jsonlite)

# use digest library to create hash function to get secure anonymous IDs 
library(digest)

# sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample <- 
  function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# function to get only files from directory (and ignore folders)
get_files_from_dir <- 
  function(dir){
    # Get directory of pool files 
    all_items <- list.files(path = dir, full.names = TRUE)
    
    # Get the information about each item
    info <- file.info(all_items)
    
    # Filter out directories and keep only files
    files <- all_items[!info$isdir]
    
    # Return files names
    return(files)
  }

# load X API utils
source('utils/X.api.v2_functions.R')

# load date-time utils
source('utils/date.time_functions.R')

# load parsing utils
source('utils/parse_functions.R')

# load openAI library
library(openai)

# load gpt utils
source('utils/gpt_functions.R')

# load general utils 
library(R.utils)

# set api key
Sys.setenv(OPENAI_API_KEY = readLines(con = 'tokens/OpenAI'))

# # # (1) SET QUOTAS FOR DEMOGRAPHIC GROUPS 
source('R scripts/2.ai_poll/i.set_parameters/c.quotas_frame.R')

# # # (2) LOAD BACKGROUND INFO FOR PAST VOTE CHOICE QUESTIONS
source('R scripts/2.ai_poll/i.set_parameters/d.geo_candidates_lookup.R')
# load context for 2022 districts
load(file = 'generated_data/2022 redistricting context/context.RData')

# # # (3) LOAD BALLOT ACCESS FOR EACH PARTY - 
# REMEMBER TO RE-RUN THE BALLOT ACCESS SCRIPT AND CHECK IT MAKES SENSE, AND UNDERLYING PAGE COULD CHANGE
load(file = 'auxiliary_data/Wikipedia/Ballot access context/ballot_access.RData')

# # # (4) DEFINE HYPER-PARAMETERS

# tweet history context window
# august 8th at 14:04 -- changes this to 20 -- 
# so that we have a larger trace for trending 
# users relative to political users.
m = 20
m.trends = m*2

# temporal inclusion criteria (see each user at most once every tau days)
tau = 7 # maximum 1ce per week - under the assumption we conduct a survey every week ? 

# initialise background infor for propmpts 
bg <- ''