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

# load utils
library(data.table)
library(questionr)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# plotting utils
library(vioplot)
# cols
cols.light <- c(D = 'skyblue',G = 'green',K = 'violet',L = 'yellow',R = 'lightcoral',`stay home` = 'darkgrey',W = 'pink',OTHER = 'darkgrey')
cols.dark <-  c(D = 'dodgerblue',G = 'darkgreen',K = 'purple',L = 'orange',R = 'orangered',`stay home` = 'black',W = 'darkred',OTHER = 'black')

# plot function 
plot.effect_violin <- 
  function(x,var.levels,cols.light,cols.dark,reference = NULL,...){
    # select the effect of interest 
    
    x <- as.matrix(x)
    
    effects <- 
      lapply(
        1:dim(x)[2],
        function(k){
          x[,k]
        }
      )
    
    # plot violins
    vioplot(
      effects,
      names = var.levels, 
      col = cols.light,
      side = "right",
      ...
    )
    # add reference
    if(!is.null(reference)){
      abline(h = reference,lty = 2)
    }
  }

# evaluation metrics
bias <- function(obs,pred,...){ mean( pred-obs,...) }
rmse <- function(obs,pred,...){ sqrt( mean( (obs - pred)^2,...) ) }
mae <- function(obs,pred,...){ mean(abs(obs - pred),...) }
corr <- function(obs,pred,...){ cor(x = pred,y = obs,...) }
cover <- function(obs,pred_lo,pred_hi,...){ mean( obs > pred_lo & obs < pred_hi,...) }

# map function which works on both vectors and sims matrices
map <- function(x){ 
  if(!is.null(dim(x))){
    apply(x,2,function(x){map_estimate(x)$MAP_Estimate})
  }else{
    map_estimate(x)$MAP_Estimate
  } }

# I'm going to use JAGS to do some basic modeling 
library(R2jags)


# load crosstabs from polls
load(file = 'generated_data/crosstabs/crosstabs.RData')

# last minute cleaning of cross data
cross$sponsor <- ifelse(is.na(cross$sponsor),'No',cross$sponsor)
idvars <- c('pollster','sponsor','start.date','end.date','population','variable','condition')

# dates 
pred.files <- dir(path = 'generated_data/predictions/vote2024/speculation.High/')
dates <- gsub(".*_(\\d{8})\\.RData", "\\1",pred.files[grepl('crosstabs_2024',pred.files)])


for(crosstab.condition in c('population','gender','race','age','income','vote2020','region')){
for(date in dates){

  # load predicted values
  load(file = paste0('generated_data/predictions/vote2024/speculation.High/crosstabs_',date,'.RData',sep = ''))
  crosstab.pred_list_spec.High <- crosstab.pred_list
  load(file = paste0('generated_data/predictions/vote2024/speculation.Moderate/crosstabs_',date,'.RData',sep = ''))
  crosstab.pred_list_spec.Moderate <- crosstab.pred_list
  
  # get distribution of interest 
  crosstab.marginal <-
    reshape(
      cross[variable == 'vote2024' & grepl(crosstab.condition,condition)],
      direction = 'wide',
      timevar = 'response',
      idvar = idvars
      )
  
  # align time frame with PoSSUM (for this particular poll)
  keep <- 
  which(
    as.Date(crosstab.marginal$start.date,'%d/%m/%Y')<= as.Date("23/08/2024",'%d/%m/%Y') &
    as.Date(crosstab.marginal$end.date,'%d/%m/%Y')>= as.Date("15/08/2024",'%d/%m/%Y')
  )
  crosstab.marginal <- crosstab.marginal[keep]
  
  # some crosstab specific cleaning, if pollsters have multiple populations
  if(any(crosstab.marginal$pollster %in% 'SoCal Strategies' )){
    crosstab.marginal <- crosstab.marginal[-which(pollster == 'SoCal Strategies' & end.date == '18/08/2024' & population == 'R')]
  }
  if(grepl('population',crosstab.condition)){
    crosstab.marginal <- crosstab.marginal[-which(pollster == 'Bullfinch Group' & end.date == '21/08/2024' & condition == 'population_adults')]
    crosstab.marginal <- crosstab.marginal[-which(pollster == 'Big Village' & end.date == '18/08/2024' & condition %in% c('population_adults','population_registered'))]
  }
  
  # remove stay home - make this a LV posterior
  crosstab.marginal <- crosstab.marginal[,!grepl('stay home',names(crosstab.marginal)),with=F]
  
  # aggregate G W L into `OTHER`
  crosstab.marginal$N.OTHER <- apply(crosstab.marginal[,grepl('G|W|L|OTHER',names(crosstab.marginal)),with=F],1,function(x){sum(x,na.rm=T)})
  crosstab.marginal <- crosstab.marginal[,!grepl('G|W|L',names(crosstab.marginal)),with=F]
  crosstab.marginal$N.OTHER <- ifelse(crosstab.marginal$N.OTHER ==0,NA,crosstab.marginal$N.OTHER)
  crosstab.marginal <- crosstab.marginal[,lapply(.SD,function(x){as.numeric(ifelse(is.na(x),NA,x))}),by = c(idvars)] 
  
  # Create a set identifier by converting NA patterns to strings
  y.matrix <- crosstab.marginal[,grepl('N.',names(crosstab.marginal)),with=F]
  
  crosstab.marginal$set <- apply(y.matrix, 1, function(x) gsub('-NA','',paste0(gsub('N.','',names(y.matrix))[x!=''],collapse = '-') ) )
  crosstab.marginal$set <- as.factor(crosstab.marginal$set)
  crosstab.marginal$set_id <- as.integer(crosstab.marginal$set)
  crosstab.marginal$pollster <- as.factor(crosstab.marginal$pollster)
  crosstab.marginal$pollster_id <- as.integer(crosstab.marginal$pollster)
  crosstab.marginal$condition <- as.factor(crosstab.marginal$condition )
  crosstab.marginal$condition_id <- as.integer(crosstab.marginal$condition )
  
  # create id to connect to predicted values 
  crosstab.condition_id <- 
    ifelse(crosstab.condition=='population','date',
           ifelse(grepl('age',crosstab.condition),'age_bins',
                  ifelse(grepl('race',crosstab.condition),'ethnicity',
                         ifelse(grepl('gender',crosstab.condition),'gender',
                                ifelse(grepl('vote2020',crosstab.condition),'vote2020',
                                       ifelse(grepl('income',crosstab.condition),'hh_income',
                                              ifelse(grepl('region',crosstab.condition),'region',
                                                     crosstab.condition
                                              ) ) ) ) ) ) )

  # speculation = High
  if(grepl('race',crosstab.condition)){
    crosstab.pred_list_spec.High[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.High[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) %in% gsub('race_','',levels(crosstab.marginal$condition)) ]
  }
  if(grepl('gender',crosstab.condition)){
    crosstab.pred_list_spec.High[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.High[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) %in% substr(gsub('gender_','',levels(crosstab.marginal$condition)),1,1)]
  }
  if(grepl('vote2020',crosstab.condition)){
    crosstab.pred_list_spec.High[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.High[[crosstab.condition_id]][crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]] %in% gsub('vote2020_','',levels(crosstab.marginal$condition))]
  }
  if(grepl('region',crosstab.condition)){
    crosstab.pred_list_spec.High[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.High[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) %in% gsub('region_','',levels(crosstab.marginal$condition))]
  }
  # aggregate `OTHER`
  crosstab.pred_list_spec.High[[crosstab.condition_id]]$OTHER <- 
    rowSums(crosstab.pred_list_spec.High[[crosstab.condition_id ]][,c('G','L','W')])
  
  # speculation = Moderate 
  if(grepl('race',crosstab.condition)){
    crosstab.pred_list_spec.Moderate[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) %in% gsub('race_','',levels(crosstab.marginal$condition)) ]
  }
  if(grepl('gender',crosstab.condition)){
    crosstab.pred_list_spec.Moderate[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) %in% substr(gsub('gender_','',levels(crosstab.marginal$condition)),1,1)]
  }
  if(grepl('vote2020',crosstab.condition)){
    crosstab.pred_list_spec.Moderate[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]] %in% gsub('vote2020_','',levels(crosstab.marginal$condition))]
  }
  if(grepl('region',crosstab.condition)){
    crosstab.pred_list_spec.Moderate[[crosstab.condition_id]] <- 
      crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) %in% gsub('region_','',levels(crosstab.marginal$condition))]
  }
  # aggregate `OTHER`
  crosstab.pred_list_spec.Moderate[[crosstab.condition_id]]$OTHER <- 
    rowSums(crosstab.pred_list_spec.Moderate[[crosstab.condition_id ]][,c('G','L','W')])
  
  # prepare jags data
  y <- as.matrix(crosstab.marginal[,grepl('N.',names(crosstab.marginal)),with=F])
  n = rowSums(y,na.rm=T)
  N = dim(y)[1]
  
  # get separate data to estimate the independent models in the same code 
  y_set_prime_j <- list()
  for(i in 1:N){
    y_set_prime_j[[i]] <- y[i,][!is.na(y[i,])]#y[i,which_j_set[[set_id[i]]]]
  }
  names(y_set_prime_j) <- paste0('y_set_prime_j',1:N)
  
  J_prime_labs <- lapply(y_set_prime_j,function(x){gsub('N.','',names(x))})
  names(J_prime_labs) <- paste0('J_prime_labs',1:N)
  
  J_prime <- lapply(y_set_prime_j,length)
  names(J_prime) <- paste0('J_prime',1:N)
  
  m <- lapply(y_set_prime_j,sum)
  names(m) <- paste0('m',1:N)
  
  # # # For age, income, need neighbourhood matrix for linear projection
  if(grepl('age',crosstab.condition)){
    # Define the age groups
    age_groups <- list(`18-24` = 18:24, `25-34` = 25:34, `35-44` = 35:44, 
                       `45-54` = 45:54, `55-64` = 55:64, `65+` = 65:100)
    
    # Extract the age conditions directly from the data
    age_conditions <- crosstab.marginal$condition
    
    # Create binary matrix Z
    Z <- t(sapply(age_conditions, function(cond) {
      if (grepl("over", cond)) {
        lower <- as.numeric(gsub(".*?(\\d+).*", "\\1", cond)) + 1
        upper <- 100 # assuming 100 as the upper bound for age
      } else {
        range <- as.numeric(unlist(regmatches(cond, gregexpr("\\d+", cond))))
        lower <- range[1]
        upper <- range[2]
      }
      sapply(age_groups, function(g) as.numeric(any(g >= lower & g <= upper)))
    }))
    
    # Assign row and column names
    rownames(Z) <- age_conditions
    colnames(Z) <- names(age_groups)
    
  }
  if(grepl('income',crosstab.condition)){
    # Define the income groups 
    income_groups <- list(`[min, 25000)` = c(-Inf, 24999), 
                          `[25000, 50000)` = c(25000, 49999), 
                          `[50000, 75000)` = c(50000, 74999), 
                          `[75000, 100000)` = c(75000, 99999), 
                          `[100000, max]` = c(100000, Inf))
    
    # Extract the income conditions from your data
    income_conditions <- crosstab.marginal$condition
    
    # Create binary matrix Z for income conditions
    Z <- t(sapply(income_conditions, function(cond) {
      if (grepl("under", cond)) {
        upper <- as.numeric(gsub(".*?(\\d+)k.*", "\\1", cond)) * 1000 - 1
        lower <- -Inf
      } else if (grepl("between", cond)) {
        limits <- as.numeric(unlist(regmatches(cond, gregexpr("\\d+", cond))))
        lower <- limits[1] * 1000
        upper <- limits[2] * 1000 - 1
      } else if (grepl("over", cond)) {
        lower <- as.numeric(gsub(".*?(\\d+)k.*", "\\1", cond)) * 1000
        upper <- Inf
      }
      sapply(income_groups, function(g) as.numeric(any(g[1] <= upper & g[2] >= lower)))
    }))
    
    # Assign row and column names
    rownames(Z) <- income_conditions
    colnames(Z) <- names(income_groups)
  }

  # prepare unchangeable
  jags.data <-
    list(
      N = dim(crosstab.marginal)[1],
      n = n,
      J = dim(y)[2],
      J_labs = gsub('N.','',colnames(y)),
      
      pollster_id = crosstab.marginal$pollster_id,
      pollster_N = max( crosstab.marginal$pollster_id),
      pollster_labs = levels( crosstab.marginal$pollster),
      
      condition_id = crosstab.marginal$condition_id,
      condition_N = max(crosstab.marginal$condition_id),
      condition_labs = levels(crosstab.marginal$condition)
    )
  # append stuff for independent models
  jags.data <- append(jags.data,y_set_prime_j)
  jags.data <- append(jags.data,J_prime_labs)
  jags.data <- append(jags.data,J_prime)
  jags.data <- append(jags.data,m)
  
  if(grepl('age|income',crosstab.condition)){
    jags.data <- append(jags.data,list(Z = Z,A = dim(Z)[2]))
  }
  # drop undefined
  if(any(sapply(jags.data,length)==0)){
    jags.data <- jags.data[-which(sapply(jags.data,length)==0)]
  }
  
  # now prepare the JAGS code dynamically 
  {
    jags_model_code <- "model{\n\n"
    
    # odel the vote share estimates independently
    for(i in 1:N) {
      # Likelihood
      jags_model_code <- paste0(jags_model_code, "y_set_prime_j",i," ~ dmulti(lambda",i,", m",i,");\n")
      jags_model_code <- paste0(jags_model_code, "lambda",i," ~ ddirch(nu",i,");\n\n")
      jags_model_code <- paste0(jags_model_code, "for(j in 1:J_prime",i,"){\n")
      jags_model_code <- paste0(jags_model_code, "  nu",i,"[j] <- 0.5 ;\n")
      jags_model_code <- paste0(jags_model_code, "}\n")
    }
  # Close the model block
  jags_model_code <- paste0(jags_model_code, "}")
}
  
  tmpf=tempfile()
  tmps=file(tmpf,"w")
  cat(jags_model_code,file=tmps)
  close(tmps)
  
  cat(jags_model_code)
  
  params.to.store <-
    c(
      'pi',
      'mu',
      'pi_gen',
      'pi_hat',
      'alpha_star',
      'beta_star'
    )
  if(grepl('gender|race|age|vote2020',crosstab.condition)){
    params.to.store <- c(params.to.store,'eta_star')
  }
  for(i in 1:N){
    params.to.store <- c(params.to.store,paste0('lambda',i))
  }

  # fit the model
  jags.fit <-
    jags.parallel(
      data = jags.data,
      parameters.to.save = params.to.store,
      n.iter = 200000,
      model.file = tmpf,
      n.chains = 8,
      n.cluster = 8,
      n.burnin = 185000,
      n.thin = 8
    ) 
  
  # plot convergence (Rhat only)
  rhat <- jags.fit$BUGSoutput$summary[,'Rhat']
  plot(
    rhat,
    ylim = c(min(min(rhat),0.9),max(max(rhat),1.25)),
    main = 'Convergence',
    ylab = expression(hat(R))
  )
  abline(h = 1.05,lty = 1)
  
  # prepare labs for addition of possum and average in plot
  jags.data$pollster_labs <- c(jags.data$pollster_labs,'PoSSUM MrP','PoSSUM MrP - Spec. Mod.')

  # topline numbers
  if(grepl('population',crosstab.condition)){
    
    pdf(file = paste0('generated_plots/comparison_with_polls/',crosstab.condition,'_',date,'.pdf'),width = 15,height = 10)
    
    par(mfrow =c(2,2),oma = c(5,0,3,0),mar = c(12,3,3,3)) 
    
    for(j.id in jags.data$J_labs ){
      # Assign candidate name based on choice name
      main = 
        ifelse(j.id == 'D','Kamala Harris',
        ifelse(j.id == 'R','Donald Trump',
        ifelse(j.id == 'K','Robert F. Kennedy Jr.','Other'
               ) ) )
      
      # Initialise list with simple estimates 
      lambda.tmp <- vector("list", length(jags.data$pollster_labs)) 
      for (k in 1:length(jags.data$pollster_labs)) {
        
        if(k==(jags.data$pollster_N+1)){
          lambda.tmp[[k]] <- as.matrix(c(crosstab.pred_list_spec.High$date[[j.id]],rep(NA,jags.fit$BUGSoutput$n.sims-max(crosstab.pred_list_spec.High$date$sim_id))))
          names(lambda.tmp)[k] <- "PoSSUM MrP"
        }
        if(k==(jags.data$pollster_N+2)){
          lambda.tmp[[k]] <- as.matrix(c(crosstab.pred_list_spec.Moderate$date[[j.id]],rep(NA,jags.fit$BUGSoutput$n.sims-max(crosstab.pred_list_spec.Moderate$date$sim_id))))
          names(lambda.tmp)[k] <- "PoSSUM MrP - Spec. Mod."
        }
        
        # for every pollster
        for (i in 1:N) {
          # go through each poll
          if (jags.data$pollster_id[i] != k) next 
          # if the poll belongs to this pollster
          # Check if the choice is amongst those in the choice set for the poll
          if (j.id %in% jags.data[[paste0('J_prime_labs', i)]]) {
            # if so, get the simulations and bind them in a matrix to other polls fro that pollster 
            lambda.tmp[[k]] <- cbind(
              lambda.tmp[[k]],
              jags.fit$BUGSoutput$sims.list[[paste0('lambda', i)]][, which(jags.data[[paste0('J_prime_labs', i)]] == j.id)]
            )
          }
        }
        # assign a name to the pollster in the list
        names(lambda.tmp)[k] <- jags.data$pollster_labs[k]
      }
      

      # take the average per plloster and bind into a matrix - useful for ordering 
      pi_hat.tmp <- sapply(lambda.tmp,function(x){if(length(dim(x)[2])==0){rep(NA,jags.fit$BUGSoutput$n.sims)}else{rowMeans(x)}})
      
      # get map for each pollster
      pi_hat.point <- apply(pi_hat.tmp,2,function(x){if(all(is.na(x))){NA}else{map(x)}})
      
      # initialise plot
      xlim.max = dim(pi_hat.tmp)[2]
      ylim.min = min(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
      ylim.max = max(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
      grid.v.every = 1
      grid.h.every = 0.01
      ref = mean(unlist(lambda.tmp[!grepl('PoSSUM',names(lambda.tmp))]),na.rm=T)
      
      plot(
        y = seq(0,1,length.out = xlim.max),
        x = 1:(xlim.max),
        ylim = c(ylim.min,ylim.max),
        xlim= c(0.5,xlim.max + 0.5),
        pch = NA,
        xaxt = 'n',
        xlab = '',
        ylab = '',
        main = main
        )
      abline(
        v =  seq(0,xlim.max,by = grid.v.every),
        h =  seq(0,1,by = grid.h.every),
        col = adjustcolor(col = 'darkgrey',0.15)
        )
      
      # plot each poll independently on the grid
      for(k in 1:xlim.max){
        if(is.null(lambda.tmp[[k]])){next}
        # wheere should we plot it ? 
        at = which(jags.data$pollster_labs[order(pi_hat.point)]==jags.data$pollster_labs[k])
        for(h in 1:dim(lambda.tmp[[k]])[2]){
          x = lambda.tmp[[k]][,h]
          if(all(is.na(x))){next}
          col = 
            ifelse(
              grepl('PoSSUM',jags.data$pollster_labs[k]),
              adjustcolor(cols.dark[j.id],0.75),
              adjustcolor(cols.light[j.id],0.5)
            )
          plot.effect_violin(
            x = x,
            var.levels = '',
            at = at,
            cols.light = col,
            horizontal = FALSE,
            reference = ref,
            las = 3,
            cex.axis = 0.7,
            ylim = c(ylim.min,ylim.max),
            xlim= c(0.5,xlim.max + 0.5),
            add = TRUE
          )
        } }
      
      axis(side = 1,
           at = 1:(xlim.max),
           labels = gsub('PoSSUM Raw|PoSSUM Raw - Spec. Mod.|PoSSUM MrP|PoSSUM MrP - Spec. Mod','',jags.data$pollster_labs[order(pi_hat.point)]),
           las = 3)
      for(ax in c("PoSSUM Raw","PoSSUM Raw - Spec. Mod.","PoSSUM MrP","PoSSUM MrP - Spec. Mod.")){
        axis(side = 1,
             at = 1:(xlim.max),
             labels = ifelse(jags.data$pollster_labs[order(pi_hat.point)]==ax,ax, ""),
             las = 3,
             font = 2)
      }
      outer.main <-'Likely Voters - Aggregate Preferences'
      mtext(side = 3,text = outer.main,outer = TRUE)
      
      for(k in jags.data$pollster_labs){
        if(!k %in% c('PoSSUM MrP','PoSSUM MrP - Spec. Mod.') ){
          y.obs <- y[which(crosstab.marginal$pollster==k),]
          pi.obs <- y.obs
          if(!is.null(dim(y.obs)[2])){
            for(l in 1:dim(y.obs)[1]){
              pi.obs[l,] <- y.obs[l,] / sum(y.obs[l,],na.rm=T)
              }
            pi.obs <- pi.obs[,jags.data$J_labs==j.id]
            }else{
              pi.obs <- y.obs/ sum(y.obs,na.rm=T)
              pi.obs <- pi.obs[jags.data$J_labs==j.id]
              }
          points(
            x = rep(which(jags.data$pollster_labs[order(pi_hat.point)]==k),
                    sum(crosstab.marginal$pollster==k)),
            y = pi.obs,
            pch = 4,
            col = 'black'
          )
        }
      }
      }
dev.off()
} 
  
  # # # For race we have to loop through the conditions 
  if(grepl('gender|race|vote2020|region',crosstab.condition)){
    for(c in jags.data$condition_labs){
      # plot the comparison 
      pdf(file = paste0('generated_plots/comparison_with_polls/',c,'_',date,'.pdf'),width = 15,height = 10)
      par(mfrow =c(2,2),oma = c(5,0,3,0),mar = c(12,3,3,3)) 

      for(j.id in jags.data$J_labs ){
        # Assign candidate name based on choice name
        main = 
          ifelse(j.id == 'D','Kamala Harris',
                 ifelse(j.id == 'R','Donald Trump',
                        ifelse(j.id == 'K','Robert F. Kennedy Jr.','Other'
                        ) ) )
        
        # Initialise list with simple estimates 
        lambda.tmp <- vector("list", length(jags.data$pollster_labs)) 
        for (k in 1:length(jags.data$pollster_labs)) {
          
          if(k==(jags.data$pollster_N+1)){
            if(grepl('race',crosstab.condition)){
              pred.condition_spec.High <- crosstab.pred_list_spec.High[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == gsub('race_','',c)]
            }
            if(grepl('gender',crosstab.condition)){
              pred.condition_spec.High <- crosstab.pred_list_spec.High[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == substr(gsub('gender_','',c),1,1)]
            }
            if(grepl('vote2020',crosstab.condition)){
              pred.condition_spec.High <- crosstab.pred_list_spec.High[[crosstab.condition_id]][[j.id]][crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]] == gsub('vote2020_','',c)]
            }
            if(grepl('region',crosstab.condition)){
              pred.condition_spec.High <- crosstab.pred_list_spec.High[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == gsub('region_','',c)]
            }
            lambda.tmp[[k]] <- as.matrix(c(pred.condition_spec.High,rep(NA,jags.fit$BUGSoutput$n.sims-length(pred.condition_spec.High))))
            names(lambda.tmp)[k] <- "PoSSUM MrP"
          }
          if(k==(jags.data$pollster_N+2)){
            if(grepl('race',crosstab.condition)){
              pred.condition_spec.Moderate <- crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == gsub('race_','',c)]
            }
            if(grepl('gender',crosstab.condition)){
              pred.condition_spec.Moderate <- crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == substr(gsub('gender_','',c),1,1)]
            }
            if(grepl('vote2020',crosstab.condition)){
              pred.condition_spec.Moderate <- crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[j.id]][crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]] == gsub('vote2020_','',c)]
            }
            if(grepl('region',crosstab.condition)){
              pred.condition_spec.Moderate <- crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[j.id]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == gsub('region_','',c)]
            }
            lambda.tmp[[k]] <-  as.matrix(c(pred.condition_spec.Moderate,rep(NA,jags.fit$BUGSoutput$n.sims-length(pred.condition_spec.High))))
            names(lambda.tmp)[k] <- "PoSSUM MrP - Spec. Mod."
          }
          
          # for every pollster
          for (i in 1:N) {
            # go through each poll
            if (jags.data$pollster_id[i] != k | jags.data$condition_labs[jags.data$condition_id[i]]!=c) next 
            # if the poll belongs to this pollster
            # Check if the choice is amongst those in the choice set for the poll
            if (j.id %in% jags.data[[paste0('J_prime_labs', i)]]) {
              # if so, get the simulations and bind them in a matrix to other polls fro that pollster 
              lambda.tmp[[k]] <- cbind(
                lambda.tmp[[k]],
                jags.fit$BUGSoutput$sims.list[[paste0('lambda', i)]][, which(jags.data[[paste0('J_prime_labs', i)]] == j.id)]
              )
            }
          }
          # assign a name to the pollster in the list
          names(lambda.tmp)[k] <- jags.data$pollster_labs[k]
        }
        
        
        # take the average per plloster and bind into a matrix - useful for ordering 
        pi_hat.tmp <- sapply(lambda.tmp,function(x){if(length(dim(x)[2])==0){rep(NA,jags.fit$BUGSoutput$n.sims)}else{rowMeans(x)}})
        
        # get map for each pollster
        pi_hat.point <- apply(pi_hat.tmp,2,function(x){if(all(is.na(x))){NA}else{map(x)}})
        
        # initialise plot
        xlim.max = dim(pi_hat.tmp)[2]
        ylim.min = min(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
        ylim.max = max(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
        grid.v.every = 1
        grid.h.every = 0.01
        ref = mean(unlist(lambda.tmp[!grepl('PoSSUM',names(lambda.tmp))]),na.rm=T)
        
        plot(
          y = seq(0,1,length.out = xlim.max),
          x = 1:(xlim.max),
          ylim = c(ylim.min,ylim.max),
          xlim= c(0.5,xlim.max + 0.5),
          pch = NA,
          xaxt = 'n',
          xlab = '',
          ylab = '',
          main = main
        )
        abline(
          v =  seq(0,xlim.max,by = grid.v.every),
          h =  seq(0,1,by = grid.h.every),
          col = adjustcolor(col = 'darkgrey',0.15)
        )
        
        # plot each poll independently on the grid
        for(k in 1:xlim.max){
          if(is.null(lambda.tmp[[k]])){next}
          # wheere should we plot it ? 
          at = which(jags.data$pollster_labs[order(pi_hat.point)]==jags.data$pollster_labs[k])
          for(h in 1:dim(lambda.tmp[[k]])[2]){
            x = lambda.tmp[[k]][,h]
            if(all(is.na(x))){next}
            col = 
              ifelse(
                grepl('PoSSUM',jags.data$pollster_labs[k]),
                adjustcolor(cols.dark[j.id],0.75),
                adjustcolor(cols.light[j.id],0.5)
              )
            plot.effect_violin(
              x = x,
              var.levels = '',
              at = at,
              cols.light = col,
              horizontal = FALSE,
              reference = ref,
              las = 3,
              cex.axis = 0.7,
              ylim = c(ylim.min,ylim.max),
              xlim= c(0.5,xlim.max + 0.5),
              add = TRUE
            )
          } }
        
        axis(side = 1,
             at = 1:(xlim.max),
             labels = gsub('PoSSUM Raw|PoSSUM Raw - Spec. Mod.|PoSSUM MrP|PoSSUM MrP - Spec. Mod','',jags.data$pollster_labs[order(pi_hat.point)]),
             las = 3)
        for(ax in c("PoSSUM Raw","PoSSUM Raw - Spec. Mod.","PoSSUM MrP","PoSSUM MrP - Spec. Mod.")){
          axis(side = 1,
               at = 1:(xlim.max),
               labels = ifelse(jags.data$pollster_labs[order(pi_hat.point)]==ax,ax, ""),
               las = 3,
               font = 2)
        }
        if(grepl('gender',crosstab.condition)){
          outer.lab <- unique(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == substr(gsub('gender_','',c),1,1)])
          outer.main <- paste0('Likely Voters - Aggregate Preferences by Sex/Gender - ',c)
        }
        if(grepl('race',crosstab.condition)){
          outer.lab <- unique(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]][tolower(crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == gsub('race_','',c)])
          outer.main <- paste0('Likely Voters - Aggregate Preferences by Race/Ethnicity - ',c)
        }
        if(grepl('region',crosstab.condition)){
          outer.lab <- unique(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]][tolower(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == gsub('region_','',c)])
          outer.main <- paste0('Likely Voters - Aggregate Preferences by Region - ',c)
        }
        if(grepl('vote2020',crosstab.condition)){
          outer.lab <- unique(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]][crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]] == gsub('vote2020_','',c)])
          outer.main <- paste0('Likely Voters - Aggregate Preferences by 2020 Vote - ',c)
        }

        mtext(side = 3,text = outer.main,outer = TRUE)
        
        for(k in jags.data$pollster_labs){
          if(!k %in% c('PoSSUM MrP','PoSSUM MrP - Spec. Mod.') ){
            if(sum(crosstab.marginal$pollster==k & crosstab.marginal$condition==c)==0){next}
            y.obs <- y[which(crosstab.marginal$pollster==k & crosstab.marginal$condition==c),]
            pi.obs <- y.obs
            if(!is.null(dim(y.obs)[2])){
              for(l in 1:dim(y.obs)[1]){
                pi.obs[l,] <- y.obs[l,] / sum(y.obs[l,],na.rm=T)
              }
              pi.obs <- pi.obs[,jags.data$J_labs==j.id]
            }else{
              pi.obs <- y.obs/ sum(y.obs,na.rm=T)
              pi.obs <- pi.obs[jags.data$J_labs==j.id]
            }
            points(
              x = rep(which(jags.data$pollster_labs[order(pi_hat.point)]==k),
                      sum(crosstab.marginal$pollster==k  & crosstab.marginal$condition==c)),
              y = pi.obs,
              pch = 4,
              col = 'black'
            )
          }
        }
      }
      dev.off()
    } 
    }
# # # For age we have to loop through target conditions, and plot all the age values distinctively to allow for a proper comparison

if(grepl('age|income',crosstab.condition)){
  for(c in colnames(jags.data$Z)){
    # identify overlappers
    valid.c <- rownames(Z)[Z[,c]==1]
    
    # plot the comparison 
    pdf(file = paste0('generated_plots/comparison_with_polls/',crosstab.condition_id,'_',c,'_',date,'.pdf'),width = 15,height = 10)
    par(mfrow =c(2,2),oma = c(5,0,3,0),mar = c(12,3,3,3)) 
    for(j.id in jags.data$J_labs ){
      
      # Assign candidate name based on choice name
        main = 
          ifelse(j.id == 'D','Kamala Harris',
                 ifelse(j.id == 'R','Donald Trump',
                        ifelse(j.id == 'K','Robert F. Kennedy Jr.','Other'
                        ) ) )
        
        # Initialise list with simple estimates 
        lambda.tmp <- vector("list", length(jags.data$pollster_labs)) 
        for (k in 1:length(jags.data$pollster_labs)) {
          
          if(k==(jags.data$pollster_N+1)){
            pred.condition_spec.High <- crosstab.pred_list_spec.High[[crosstab.condition_id]][[j.id]][gsub(".*?\\.", "",crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == c]
            lambda.tmp[[k]] <- as.matrix(c(pred.condition_spec.High,rep(NA,jags.fit$BUGSoutput$n.sims-length(pred.condition_spec.High))))
            names(lambda.tmp)[k] <- "PoSSUM MrP"
          }
          if(k==(jags.data$pollster_N+2)){
            pred.condition_spec.Moderate <- crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[j.id]][gsub(".*?\\.", "",crosstab.pred_list_spec.Moderate[[crosstab.condition_id]][[1]]) == c]
            lambda.tmp[[k]] <-  as.matrix(c(pred.condition_spec.Moderate,rep(NA,jags.fit$BUGSoutput$n.sims-length(pred.condition_spec.High))))
            names(lambda.tmp)[k] <- "PoSSUM MrP - Spec. Mod."
          }
          
          # for every pollster
          for (i in 1:N) {
            # go through each poll
            if (jags.data$pollster_id[i] != k |  !jags.data$condition_labs[jags.data$condition_id[i]] %in% c(valid.c)) next 
            # if the poll belongs to this pollster
            # Check if the choice is amongst those in the choice set for the poll
            if (j.id %in% jags.data[[paste0('J_prime_labs', i)]]) {
              # if so, get the simulations and bind them in a matrix to other polls fro that pollster 
              lambda.tmp[[k]] <- cbind(
                lambda.tmp[[k]],
                jags.fit$BUGSoutput$sims.list[[paste0('lambda', i)]][, which(jags.data[[paste0('J_prime_labs', i)]] == j.id)]
              )
            }
          }
          # assign a name to the pollster in the list
          names(lambda.tmp)[k] <- jags.data$pollster_labs[k]
        }
        
        # take the average per plloster and bind into a matrix - useful for ordering 
        pi_hat.tmp <- sapply(lambda.tmp,function(x){if(length(dim(x)[2])==0){rep(NA,jags.fit$BUGSoutput$n.sims)}else{rowMeans(x)}})
        
        # get map for each pollster
        pi_hat.point <- apply(pi_hat.tmp,2,function(x){if(all(is.na(x))){NA}else{map(x)}})
        
        # initialise plot
        xlim.max = dim(pi_hat.tmp)[2]
        ylim.min = min(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
        ylim.max = max(c(pi_hat.tmp,unlist(lambda.tmp)),na.rm=TRUE)
        grid.v.every = 1
        grid.h.every = 0.01
        ref = mean(unlist(lambda.tmp[!grepl('PoSSUM',names(lambda.tmp))]),na.rm=T)
        
        plot(
          y = seq(0,1,length.out = xlim.max),
          x = 1:(xlim.max),
          ylim = c(ylim.min,ylim.max),
          xlim= c(0.5,xlim.max + 0.5),
          pch = NA,
          xaxt = 'n',
          xlab = '',
          ylab = '',
          main = main
        )
        abline(
          v =  seq(0,xlim.max,by = grid.v.every),
          h =  seq(0,1,by = grid.h.every),
          col = adjustcolor(col = 'darkgrey',0.15)
        )
        
        # plot each poll independently on the grid
        for(k in 1:xlim.max){
          if(is.null(lambda.tmp[[k]])){next}
          # wheere should we plot it ? 
          at = which(jags.data$pollster_labs[order(pi_hat.point)]==jags.data$pollster_labs[k])
          for(h in 1:dim(lambda.tmp[[k]])[2]){
            x = lambda.tmp[[k]][,h]
            if(all(is.na(x))){next}
            col = 
              ifelse(
                grepl('PoSSUM',jags.data$pollster_labs[k]),
                adjustcolor(cols.dark[j.id],0.75),
                adjustcolor(cols.light[j.id],0.5)
              )
            plot.effect_violin(
              x = x,
              var.levels = '',
              at = at,
              cols.light = col,
              horizontal = FALSE,
              reference = ref,
              las = 3,
              cex.axis = 0.7,
              ylim = c(ylim.min,ylim.max),
              xlim= c(0.5,xlim.max + 0.5),
              add = TRUE
            )
          } }
        
        axis(side = 1,
             at = 1:(xlim.max),
             labels = gsub('PoSSUM Raw|PoSSUM Raw - Spec. Mod.|PoSSUM MrP|PoSSUM MrP - Spec. Mod','',jags.data$pollster_labs[order(pi_hat.point)]),
             las = 3)
        for(ax in c("PoSSUM Raw","PoSSUM Raw - Spec. Mod.","PoSSUM MrP","PoSSUM MrP - Spec. Mod.")){
          axis(side = 1,
               at = 1:(xlim.max),
               labels = ifelse(jags.data$pollster_labs[order(pi_hat.point)]==ax,ax, ""),
               las = 3,
               font = 2)
        }
        outer.lab <- unique(crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]][gsub(".*?\\.", "",crosstab.pred_list_spec.High[[crosstab.condition_id]][[1]]) == c])
        if(grepl('age',crosstab.condition)){
          outer.main <- paste0('Likely Voters - Aggregate Preferences by Age - ', sub(".*\\.", "",outer.lab))
        }
        if(grepl('income',crosstab.condition)){
          outer.main <- paste0('Likely Voters - Aggregate Preferences by Household Income - ',  sub(".*\\.", "",outer.lab))
        }
        mtext(side = 3,text = outer.main,outer = TRUE)
        
        for(k in jags.data$pollster_labs){
          if(!k %in% c('PoSSUM MrP','PoSSUM MrP - Spec. Mod.') ){
            for(o in valid.c){
              if(sum(crosstab.marginal$pollster==k & crosstab.marginal$condition==o)==0){next}
              y.obs <- y[which(crosstab.marginal$pollster==k & crosstab.marginal$condition==o),]
              pi.obs <- y.obs
              if(!is.null(dim(y.obs)[2])){
                for(l in 1:dim(y.obs)[1]){
                  pi.obs[l,] <- y.obs[l,] / sum(y.obs[l,],na.rm=T)
                }
                pi.obs <- pi.obs[,jags.data$J_labs==j.id]
              }else{
                pi.obs <- y.obs/ sum(y.obs,na.rm=T)
                pi.obs <- pi.obs[jags.data$J_labs==j.id]
              }
              points(
                x = rep(which(jags.data$pollster_labs[order(pi_hat.point)]==k),sum(crosstab.marginal$pollster==k  & crosstab.marginal$condition==o)),
                y = pi.obs,
                pch = 4,
                col = 'black'
              )
              if(!grepl('PoSSUM',k)){
              text(
                x = rep(which(jags.data$pollster_labs[order(pi_hat.point)]==k),sum(crosstab.marginal$pollster==k  & crosstab.marginal$condition==o)),
                y = pi.obs,adj = 1,pos = 4,
                labels = gsub('age_','',gsub('income_','',gsub('\\.',' ',o)))
              )
              }
            }
          }
        }
        
    } 
    dev.off()
    
  }
}
      
      
} }
      
      
      
      
      
      
      