# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")


POOL_TYPE <- 'LATEST'

if(POOL_TYPE == 'LATEST'){
  # # # GET USER POOL ROUTINE
  source('R scripts/2.ai_poll/ii.get_pool/get_pool.R')
}
  
# should new quota counter be started, or should we pick up from the last one ? 
START_NEW_QUOTA_COUNTER = FALSE

# # # POLL USERS ROUTINE 
source('R scripts/2.ai_poll/iii.poll_users/poll_users.R')

