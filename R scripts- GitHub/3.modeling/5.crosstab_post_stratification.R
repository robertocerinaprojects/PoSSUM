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

# load useful package
library(data.table)
# load stan and options
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
# more bayesian utils
library(bayestestR)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# map function which works on both vectors and sims matrices
map <- function(x){ 
  if(!is.null(dim(x))){
    apply(x,2,function(x){map_estimate(x)$MAP_Estimate})
  }else{
    map_estimate(x)$MAP_Estimate
  } }

for(speculation in c('High','Moderate')){
  for(dep.var in c('vote2024','fav_harris','fav_trump','fav_walz','fav_jdvance')){

# load strat frame
load(file = 'generated_data/stratification_frame/SF_extended_scaled.pred.RData')

# load shapefile
load(file = 'generated_data/shapefile/electoral.college_shape.RData')

# load fit object
load(file = paste0('generated_data/mrp_model/',dep.var,'/speculation.',speculation,'/fit_object.RData'))

# load training data
load(file = paste0('generated_data/mrp_model/',dep.var,'/speculation.',speculation,'/training_data.RData'))

# extract simulations for model parametrs
pars_sims <- rstan::extract(fit_object, inc_warmup = FALSE)

# generate crosstab predictions object to be filled in
crosstabs <- 
  c('date',
    'region',
    'state_electoral.college',
    'gender',
    'ethnicity',
    'college_grad',
    'hh_income',
    'age_bins',
    'vote2020',
    'vote2022'
  )

crosstab.pred_list <- list()
crosstab.pred_list_simpleMrsP <- list()

for(c in crosstabs){ 
  crosstab.pred_list[[c]] <- data.table()
  crosstab.pred_list_simpleMrsP[[c]] <- data.table()
  }


# for each simulated election (we have 104 sims for the posterior of the frame, 
# so we'll take 104 sims from the posterior)
sims <- 
sample(
  1:dim(pars_sims$beta_s)[1],
  size = sum(grepl('N.star',names(SF_extended))),
  replace = F
  )

# start counter for frame sims 
s.frame <- 0
for(s in sims){
s.frame <- s.frame + 1

# make cell-level predictions for vote-choice 
mu_sims <-data.table(rep(0,dim(SF_extended)[1])) 


pred_vote_logit <- sapply(2:(train_data_list$J),function(j){
     mu_sims <- 
       pars_sims$beta_s[s,j-1] +
      (as.matrix(SF_extended[,colnames(train_data_list$Z)[train_data_list$Z_id[j,1:train_data_list$P[j]]],with = F]) %*%
         pars_sims$eta_s[s,1:train_data_list$P[j],j-1]) +
      pars_sims$lambda_s[s,match(SF_extended$state_electoral.college,shape_US$state_electoral.college),j-1] +
      pars_sims$xi_gen_s[s,as.integer(as.factor(SF_extended$gender)),j-1] +
      pars_sims$xi_eth_s[s,as.integer(as.factor(SF_extended$ethnicity)),j-1] +
      pars_sims$xi_inc_s[s,as.integer(as.factor(SF_extended$hh_income)),j-1] +
      pars_sims$xi_age_s[s,as.integer(as.factor(SF_extended$age_bins)),j-1] +
      pars_sims$xi_v20_s[s,as.integer(as.factor(SF_extended$vote2020)),j-1] ; 
    
    # return latent preference
    return(mu_sims)
} )

pred_vote_logit <- cbind(mu_sims,pred_vote_logit)

# convert to vote-choice probabilities
pred_vote <- exp(pred_vote_logit)/rowSums(exp(pred_vote_logit))

# assign names 
colnames(pred_vote) <- c(train_data_list$V_labs)

if(dep.var=='vote2024'){
  source(paste0('R scripts/3.modeling/5.',dep.var,'.R'))
}

if(grepl('fav',dep.var) | dep.var %in% c('moi') ){
  source(paste0('R scripts/3.modeling/5.fav.R'))
}

}

# save object
save(crosstab.pred_list,
     file = paste0('generated_data/predictions/',dep.var,'/speculation.',speculation,'/crosstabs_',gsub('-','',unique(SF_extended$date)),'.RData'),
     compress = T)

# save object
save(crosstab.pred_list_simpleMrsP,
     file = paste0('generated_data/predictions/',dep.var,'/speculation.',speculation,'/crosstabs_simpleMrsP_',gsub('-','',unique(SF_extended$date)),'.RData'),
     compress = T)

dev.off()
  } }

for(dep.var in c('vote2024')){
  pred.files <- dir(path = paste0('generated_data/predictions/',dep.var,'/speculation.High/'))
  dates <- gsub(".*_(\\d{8})\\.RData", "\\1",pred.files[grepl('crosstabs_2024',pred.files)])
  
  for(date in dates){
    
    load(file = paste0('~/Desktop/Mechanical Pollster/generated_data/predictions/',dep.var,'/speculation.High/crosstabs_',date,'.RData'))
    
    if(dep.var == 'vote2024'){lab.order <- c('D','R','K','G','W','L','stay home','turnout')}
    
    for(c in names(crosstab.pred_list)[!names(crosstab.pred_list) %in% c("state_electoral.college",'vote2022')]){
      
      
      if(c == 'date'){
        tmp <- as.matrix(round(100*t(apply(crosstab.pred_list[[c]][,..lab.order],2,quantile,c(0.05,0.5,0.95))),1))
        val.table <- 
          data.table(
            Population = ifelse(lab.order %in% c('turnout','stay home'),'Adults','Voters'),
            `Vote2024` = 
              ifelse(lab.order == 'D','Kamala Harris',
                     ifelse(lab.order == 'R','Donald Trump (R)',
                            ifelse(lab.order == 'K','Robert F. Kennedy Jr. (Ind.)',
                                   ifelse(lab.order == 'G','Jill Stein (G)',
                                          ifelse(lab.order == 'L','Chase Oliver (L)',
                                                 ifelse(lab.order == 'W','Cornel West (Ind.)',
                                                        ifelse(lab.order == 'stay home','Stay Home',
                                                               ifelse(lab.order == 'turnout','Turnout',
                                                                      'Other'
                                                               ) ) ) ) ) ) ) ),
            `Topline` = paste0(tmp[,'50%'],' ( ',tmp[,'5%'],' , ',tmp[,'95%'],' )')
          )
        #print(c)
      }else{
        #print(c)
        val.table <- 
          data.table(
            Population = ifelse(lab.order %in% c('turnout','stay home'),'Adults','Voters'),
            `Vote2024` =        
              ifelse(lab.order == 'D','Kamala Harris (D)',
                     ifelse(lab.order == 'R','Donald Trump (R)',
                            ifelse(lab.order == 'K','Robert F. Kennedy Jr. (Ind.)',
                                   ifelse(lab.order == 'G','Jill Stein (G)',
                                          ifelse(lab.order == 'L','Chase Oliver (L)',
                                                 ifelse(lab.order == 'W','Cornel West (Ind.)',
                                                        ifelse(lab.order == 'stay home','Stay Home',
                                                               ifelse(lab.order == 'turnout','Turnout',
                                                                      'Other'
                                                               ) ) ) ) ) )))
          )
        print.levels <- levels(as.factor(crosstab.pred_list[[c]][[1]]))
        if(c=='vote2020'){
          print.levels <- c('D','R','L','G','OTHER','stay home')
        }
        for(k in print.levels){
          tmp <- crosstab.pred_list[[c]]
          tmp <- as.matrix(round(100*t(apply(tmp[tmp[[c]]==k][,..lab.order],2,quantile,c(0.05,0.5,0.95))),1))
          estimate <- paste0(tmp[,'50%'],' ( ',tmp[,'5%'],' , ',tmp[,'95%'],' )')
          tmp <- data.table(temp = estimate)
          names(tmp)[names(tmp)=='temp'] <- 
            ifelse(c %in% c('age_bins','hh_income'),
                   gsub('25000','25k',
                        gsub('50000','50k',
                             gsub('75000','75k',
                                  gsub('100000','100k',
                                       gsub('max\\]','+\\)',
                                            gsub('min','0',
                                                 gsub(".*?\\.", "",k)
                                            ) ) ) ) ) ),
                   toTitleCase(k))
          val.table <- cbind(val.table,tmp)
        }
      }
      caption = 
        ifelse(
          c=='date','Topline estimates of 2024 Vote Choice.',
          paste0('Estimates of 2024 Vote Choice by ',
                 toTitleCase(gsub('vote2020','2020 Vote Choice',
                                  gsub('\\_',' ',
                                       paste(gsub('hh_income','Houshold Income',
                                                  gsub('age_bins','Age',
                                                       gsub('gender','Sex',
                                                            gsub('ethnicity','Race/Ethnicity',
                                                                 gsub('grad','grad.',c,'.')))))))))))
      print(kbl(val.table,format = 'latex',booktabs = T,caption = caption))
      
    } } }
