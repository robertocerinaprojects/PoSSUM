# calculate the vote distribution for each relevant crosstabs:

# With Smooth MrsP Frame
for(c in crosstabs){
  
  # bind with crosstab-level data
  temp <- cbind(SF_extended[,c(c,paste0('N_star.',s.frame)),with=F],pred_vote)
  names(temp)[grepl('N_star',names(temp))] <- 'N_star'
  
  # calculate stratified estimate
  crosstab.pred <- temp[,lapply(.SD,function(x){sum(x*N_star)/sum(N_star)}),by = c(c),.SDcols = c(names(pred_vote))]
  
  # margin
  crosstab.pred$NET <- crosstab.pred$Favourable- crosstab.pred$Unfavourable
  
  # give id number for simulation
  crosstab.pred$sim_id <- s.frame
  
  # stack simulations for this crosstab
  crosstab.pred_list[[c]] <- 
    rbindlist(
      list(
        crosstab.pred_list[[c]],
        crosstab.pred
      ),
      fill = TRUE
    )
  
  # view
  print(lapply(crosstab.pred_list,tail))
  
  # plot 
  if(s.frame>10){
    par(mfrow = c(1,4))
    
    for(j in c(train_data_list$V_labs,'NET')){
      hist(
        crosstab.pred_list$date[[j]],
        main = j,
        xlab = 'share in pop.',
        border = NA,
        col = ifelse(grepl('Unfavourable',j),'orange',ifelse(grepl('Favourable',j),'greenyellow','darkgrey')),
        breaks = 100,
        xlim = c(0,1)
      )
      abline(
        v = map(crosstab.pred_list$date[[j]]),
        col = ifelse(grepl('Unfavourable',j),'orangered',ifelse(grepl('Favourable',j),'limegreen','black')),
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
  
  # NET fav
  crosstab.pred$NET <- crosstab.pred$Favourable- crosstab.pred$Unfavourable
  
  # give id number for simulation
  crosstab.pred$sim_id <- s.frame
  
  # stack simulations for this crosstab
  crosstab.pred_list_simpleMrsP[[c]] <- 
    rbindlist(list(crosstab.pred_list_simpleMrsP[[c]],crosstab.pred),fill = TRUE)
  
  # view
  print(lapply(crosstab.pred_list_simpleMrsP,tail))
  
  # plot 
  if(s.frame>10){
    par(mfrow = c(1,4),oma = c(0,0,3,0))
    
    for(j in c(train_data_list$V_labs,'NET')){
      hist(
        crosstab.pred_list$date[[j]],
        main = j,
        xlab = 'share in pop.',
        border = NA,
        col = ifelse(grepl('Unfavourable',j),'orange',ifelse(grepl('Favourable',j),'greenyellow','darkgrey')),
        breaks = 100,
        xlim = c(0,1)
      )
      abline(
        v = map(crosstab.pred_list$date[[j]]),
        col = ifelse(grepl('Unfavourable',j),'orangered',ifelse(grepl('Favourable',j),'limegreen','black')),
        lwd = 1.5
      )
      legend(
        'topleft',
        legend = paste0('map:',round(map(crosstab.pred_list_simpleMrsP$date[[j]]),4))
      )
    }
    mtext(text = 'Marginal MrsP',outer = TRUE,side = 3)
  }
  
  # monitor
  print(
    paste(
      "simulating elections... already simulated:",
      round((s.frame+1)/(length(sims)+1)*100),"% (with marginal MrsP)"
    )
  )
  
} 

