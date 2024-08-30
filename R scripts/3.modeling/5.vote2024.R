# cols
cols.light <- c(D = 'skyblue',G = 'green',K = 'violet',L = 'yellow',R = 'lightcoral',`stay home` = 'darkgrey',W = 'pink',`RD_margin` = 'darkgrey')
cols.dark <-  c(D = 'blue',G = 'darkgreen',K = 'purple',L = 'orange',R = 'red',`stay home` = 'black',W = 'darkred',`RD_margin` = 'black')

# account for ballot access
pred_vote$K <- 
  ifelse(
    SF_extended$LP_field_Robert.F..Kennedy.Jr...who.is.not.affiliated.with.any.political.party==
      max(SF_extended$LP_field_Robert.F..Kennedy.Jr...who.is.not.affiliated.with.any.political.party),
    pred_vote$K,
    0)
pred_vote$W <- 
  ifelse(
    SF_extended$LP_field_Dr..Cornel.West..who.is.not.affiliated.with.any.political.party==
      max(SF_extended$LP_field_Dr..Cornel.West..who.is.not.affiliated.with.any.political.party),
    pred_vote$W,
    0)
pred_vote$G <- 
  ifelse(
    SF_extended$LP_field_Jill.Stein..the.Green.Party.candidate==
      max(SF_extended$LP_field_Jill.Stein..the.Green.Party.candidate),
    pred_vote$G,
    0)
pred_vote$L <- 
  ifelse(
    SF_extended$LP_field_Chase.Oliver..the.Libertarian.Party.candidate==
      max(SF_extended$LP_field_Chase.Oliver..the.Libertarian.Party.candidate),
    pred_vote$L,
    0)

# re-scale after ballot access
pred_vote <- pred_vote/rowSums(pred_vote)

# calculate the vote distribution for each relevant crosstabs:
# With Smooth MrsP Frame
for(c in crosstabs){
  
  # bind with crosstab-level data
  temp <- cbind(SF_extended[,c(c,paste0('N_star.',s.frame)),with=F],pred_vote)
  names(temp)[grepl('N_star',names(temp))] <- 'N_star'
  
  # calculate stratified estimate
  crosstab.pred <- temp[,lapply(.SD,function(x){sum(x*N_star)/sum(N_star)}),by = c(c),.SDcols = c(names(pred_vote))]
  
  # net turnout 
  crosstab.pred$turnout <- 1- crosstab.pred$`stay home`
  
  # rescale
  for(v in train_data_list$V_labs[-which(train_data_list$V_labs=='stay home')]){
    crosstab.pred[[v]] <- crosstab.pred[[v]]/crosstab.pred$turnout
  }

  # margin
  crosstab.pred$RD_margin <- crosstab.pred$R-crosstab.pred$D
  
  # give id number for simulation
  crosstab.pred$sim_id <- s.frame
  
  # stack simulations for this crosstab
  crosstab.pred_list[[c]] <- 
    rbindlist(list(crosstab.pred_list[[c]],crosstab.pred),fill = TRUE)
  
  # view
  print(lapply(crosstab.pred_list,tail))
  
  # plot 
  if(s.frame>10){
    par(mfrow = c(2,4))
    
    for(j in c(train_data_list$V_labs,'RD_margin')){
      hist(
        crosstab.pred_list$date[[j]],
        main = j,
        xlab = 'vote share',
        border = NA,
        col = cols.light[match(j,names(cols.light))],
        breaks = 100,
        xlim = c(0,1)
      )
      abline(
        v = map(crosstab.pred_list$date[[j]]),
        col = cols.dark[match(j,names(cols.light))],
        lwd = 1.5
      )
      legend(
        'topleft',
        legend = paste0('map:',round(map(crosstab.pred_list$date[[j]]),4))
      )
      mtext(text = 'Smooth MrsP',outer = TRUE,side = 3)
    }
  }
  
  # monitor
  print(
    paste(
      "simulating elections... already simulated:",
      round((s.frame+1)/(length(sims)+1)*100),"% % (with smooth MrsP)"
    )
  )
  
} 


# With Marginal MrsP Frame
for(c in crosstabs){
  
  # bind with crosstab-level data
  temp <- cbind(SF_extended[,c(c,'N_prime'),with=F],pred_vote)
  
  # calculate crosstab distribution conditional on turnout
  crosstab.pred <-temp[,lapply(.SD,function(x){sum(x*N_prime)/sum(N_prime)}),by = c(c),.SDcols = c(colnames(pred_vote))]
  
  # net turnout 
  crosstab.pred$turnout <- 1- crosstab.pred$`stay home`
  
  # rescale
  for(v in train_data_list$V_labs[-which(train_data_list$V_labs=='stay home')]){
    crosstab.pred[[v]] <- crosstab.pred[[v]]/crosstab.pred$turnout
  }
  
  # margin
  crosstab.pred$RD_margin <- crosstab.pred$R-crosstab.pred$D
  
  # give id number for simulation
  crosstab.pred$sim_id <- s.frame
  
  # stack simulations for this crosstab
  crosstab.pred_list_simpleMrsP[[c]] <- 
    rbindlist(list(crosstab.pred_list_simpleMrsP[[c]],crosstab.pred),fill = TRUE)
  
  # view
  print(lapply(crosstab.pred_list_simpleMrsP,tail))
  
  # plot 
  if(s.frame>10){
    par(mfrow = c(2,4))
    
    for(j in c(train_data_list$V_labs,'RD_margin')){
      hist(
        crosstab.pred_list_simpleMrsP$date[[j]],
        main = j,
        xlab = 'vote share',
        border = NA,
        col = cols.light[match(j,names(cols.light))],
        breaks = 100,
        xlim = c(0,1)
      )
      abline(
        v = map(crosstab.pred_list_simpleMrsP$date[[j]]),
        col = cols.dark[match(j,names(cols.light))],
        lwd = 1.5
      )
      legend(
        'topleft',
        legend = paste0('map:',round(map(crosstab.pred_list_simpleMrsP$date[[j]]),4))
      )
      mtext(text = 'Marginal MrsP',outer = TRUE,side = 3)
    }
  }
  
  # monitor
  print(
    paste(
      "simulating elections... already simulated:",
      round((s.frame+1)/(length(sims)+1)*100),"%"
    )
  )
  
} 
