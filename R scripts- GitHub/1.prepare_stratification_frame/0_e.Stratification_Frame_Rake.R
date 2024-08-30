# clean workspace
rm(list=ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen=999)
# set timeout limit (laxed)
options(timeout=10000)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")
# utils
library(data.table)
library(questionr)
library(mltools)
library(bayestestR)

# performance measures
rmse <- function(y,yhat){sqrt(mean((y - yhat)^2))}
bias <- function(yhat,y){mean((yhat-y))}
cover <- function(y_lo,y_up,y){mean(y_lo < y & y_up > y)}

# utils for parallel 
library(ohenery)
library(foreach)
library(doParallel)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# load extended frame
load(file = 'generated_data/stratification_frame/extension_model/raw_SF_extended.RData')

# area-level predictor
load(file = 'generated_data/predictor/area_level.RData')
predictor_area_level$`2020.votes.stay home` <-
  predictor_area_level$N.2020.voting.eligible.pop_predict - 
  predictor_area_level$`2020.votes.T`

predictor_area_level$`2022.votes.stay home` <-
  predictor_area_level$N.2022.voting.eligible.pop_predict - 
  predictor_area_level$`2022.votes.T`

# rename weights
names(SF)[grepl('vote2022.dist.sim',names(SF))] <- 
  paste0('N_star.',1:sum(grepl('vote2022.dist.sim',names(SF))))

# now proceed area by ara and rake 
SF$state_electoral.college <- as.factor(SF$state_electoral.college)

# preserve all the available distirbutions apart from the joints with extension
# variables
var.name.list <- 
  names(SF[,!'N'])[!grepl('cell_id|state_abbreviation|state_simple|state_electoral.college|region|LP|vote|N_',names(SF[,!'N']))]

# make sure vars are factors for raking
for(j in c(var.name.list,'vote2020','vote2022')){
  SF[[j]] <- as.factor(SF[[j]])
}

# proceed by ec constituency
for(a in levels(SF$state_electoral.college)){

  # clean garbage in memory
  gc()
  
  # info
  cat(
    paste0(
      'beginning raking of state: ',a,' ( ',
      which(levels(SF$state_electoral.college)==a),' in ',
      nlevels(SF$state_electoral.college),' )')
    )
  
  # area-level frame
  a_SF <- SF[state_electoral.college==a]

  # store total N for this area 
  total_N <- sum(a_SF$N_star.1)
  
  # prepare area marginal for census variables of interest
  a_SF_census <- a_SF[ ,lapply(.SD,sum),by = c(var.name.list),.SDcols = c('N_star.1')]
  marginals_list <- list()
  for(j in var.name.list){
    marginals_list[[j]] <- questionr::wtd.table(x =  a_SF_census[[j]],weights =  a_SF_census[['N_star.1']])
  }

  # add marginals from the 2020 vote 
  marginal_names <- c('2020.votes.D','2020.votes.G','2020.votes.L','2020.votes.OTHER','2020.votes.R','2020.votes.stay home')
  marginals_list$vote2020 <- 
    unlist( 
      predictor_area_level[state_electoral.college==a][,..marginal_names]
      )
  names(  marginals_list$vote2020) <- gsub('2020.votes.','',names(  marginals_list$vote2020))
  
  marginal_names <- c('2022.votes.D','2022.votes.G','2022.votes.L','2022.votes.OTHER','2022.votes.R','2022.votes.stay home')
  marginals_list$vote2022 <- 
    unlist( 
      predictor_area_level[state_electoral.college==a][,..marginal_names]
    )
  names(  marginals_list$vote2022) <- gsub('2022.votes.','',names(  marginals_list$vote2022))
  
  # add a tiny number to handle 0s
  marginals_list <- lapply(marginals_list,function(x){x + .Machine$double.xmin })
  
  # Detect the number of cores
  numCores <- detectCores()
  
  # Register the parallel backend
  # Leave one core free for other processes
  cl <- makeCluster(numCores - 1) 
  registerDoParallel(cl)
  
  # define to-rake columns 
  weight_columns <- grep("N_star", names(a_SF), value = TRUE)
  
  # Use foreach to parallelise
  results <- 
    foreach(
      j = weight_columns, 
      .combine = cbind, 
      .packages = c("data.table",'anesrake')
      ) %dopar% {
        
        anesrake(
          inputter = marginals_list,
          dataframe = a_SF,
          caseid = 1:nrow(a_SF),
          weightvec = a_SF[[j]],
          cap = 1000,
          maxit = 1000000,
          center.baseweights = TRUE,
          verbose = TRUE,
          type = 'nolim'
        )$weightvec
        
        }
  
  stopCluster(cl)
  
  # re-hydrate weightvec 0 this is necessary to ensure national vote is compatible 
  colnames(results) <- weight_columns
  results <- as.data.table(results)
  
  for(j in  weight_columns){
    a_SF[[j]] <- round(( results[[j]]/sum(results[[j]]) )*total_N,5)
    }

  # the final thing to add is the simple MrsP estimate -- for comparison purposes 
  # assign the same 2020 vote distribution to each cell
  
  marginals_vote.2020_MrsP <- 
    data.table(
      vote2020 = gsub('2020.votes.','',names(marginals_list$vote2020)),
      MrsP_p.vote2020 = marginals_list$vote2020/sum(marginals_list$vote2020)
      )
  a_SF <- merge(  a_SF,marginals_vote.2020_MrsP,by = c('vote2020'),all=TRUE)
  
  marginals_vote.2022_MrsP <- 
    data.table(
      vote2022 = gsub('2022.votes.','',names(marginals_list$vote2022)),
      MrsP_p.vote2022 = marginals_list$vote2022/sum(marginals_list$vote2022)
    )
  a_SF <- merge(  a_SF,marginals_vote.2022_MrsP,by = c('vote2022'),all=TRUE)
  
  #calculate N according to simple MrsP
  a_SF$N_prime <- a_SF$N*a_SF$MrsP_p.vote2020*a_SF$MrsP_p.vote2022
  a_SF <- a_SF[,!c('MrsP_p.vote2020','MrsP_p.vote2022')]

  cat(paste0('\ncompleted simple MrsP for ',a,'...\n'))
  
  cat(
    paste0(
      '\n...there are ',
      nlevels(SF$state_electoral.college) - which(levels(SF$state_electoral.college)==a),
      ' states left after this one...\n'
      ) )
  
  # save the extended strat frame for this specific state 
  save(a_SF,
       file = paste0('generated_data/stratification_frame/extension_model/state_simulations/SF_extended_',a,'.RData')
       )

  print(a_SF)
}

# generate a single efficient frame 
SF_extended <- data.table()
for(a in levels(SF$state_electoral.college)){
  load(file = paste0('generated_data/stratification_frame/extension_model/state_simulations/SF_extended_',a,'.RData'))
  temp <- a_SF
  SF_extended <- rbindlist(list(SF_extended,temp))
}

# I decided against the MAP estimate of the raked weights because the 
# map estimate itself would have to be raked ! 

# save overall extended strat frame
save(
  SF_extended,
  file = 'generated_data/stratification_frame/SF_extended.RData'
  )

