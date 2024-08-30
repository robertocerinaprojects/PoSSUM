# UPDATE QUOTA COUNTER 
if(START_NEW_QUOTA_COUNTER){
  
  # get quota lab for this new user
  quota_lab <- 
    paste0(
      unlist(
        ai.survey.temp[,paste0("gpt_QUOTA_",quota_features),with=F]
      ),
      collapse = '_AND_'
    )


  
  # if there is no real-life cell that matches the user - reject the user
  if(sum(counter$quota_lab==quota_lab)==0){
    message("**USER EXCLUDED DUE TO CELL BEING TOO RARE TO BE INCLUDED IN THE SURVEY**") ;
    ai.survey.temp$EXCLUDED <- 'no_cell_match_to_quota_frame'
    # bind survey object 
    ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
    # save the current survey object corresponding to a given pool object   
    save(ai.survey,file = save.path,compress = T)   
    next
  }
  
  # if the label exists, increase its counter 
  quota_lab_id <- which(counter$quota_lab==quota_lab)
  counter$counter[quota_lab_id] <- counter$counter[quota_lab_id] + 1
  
  # inform 
  message("Successfully started new quota counter ...")
  print(counter[quota_lab_id])
  
  # save this new quota counter, identify it via the date at which the counter is started
  save(counter, file = paste0('generated_data/quota_counter/',Sys.time(),'.RData'))
  
  # set new counter token to false 
  START_NEW_QUOTA_COUNTER <- FALSE
  
}else{
  
  # get list of existing counters 
  counter.files <- get_files_from_dir(dir = 'generated_data/quota_counter')
  
  # get latest available date-time
  path.counter.latest <- 
    counter.files[
      grepl(
        extract_latest_date(counter.files,time = TRUE),
        counter.files
        )
      ]
  
  # load latest available counter
  load(file = path.counter.latest)
  
  # get quota lab for this new user
  quota_lab <- 
    paste0(
      unlist(
        ai.survey.temp[,paste0("gpt_QUOTA_",quota_features),with=F]
      ),
      collapse = '_AND_'
    )
  
  # if there is no real-life cell that matches the user, reject the user
  if(sum(counter$quota_lab==quota_lab)==0){
    message("**USER EXCLUDED DUE TO CELL BEING TOO RARE TO BE INCLUDED IN THE SURVEY**") ;
    ai.survey.temp$EXCLUDED <- 'no_cell_match_to_quota_frame'
    # bind survey object 
    ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
    # save the current survey object corresponding to a given pool object   
    save(ai.survey,file = save.path,compress = T)   
    next
  }
  
  # if the label exists, increase its counter 
  quota_lab_id <- which(counter$quota_lab==quota_lab)
  counter$counter[quota_lab_id] <- counter$counter[quota_lab_id] + 1
  
  # if the quota is full - reject the user
  if( counter$counter[quota_lab_id] > counter$survey_cap[quota_lab_id] ){
    message("**USER EXCLUDED DUE TO CELL QUOTA BEING FULL**") 
    ai.survey.temp$EXCLUDED <- 'quota_is_full'
    # bind survey object 
    ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
    # save the current survey object corresponding to a given pool object   
    save(ai.survey,file = save.path,compress = T)   
    next
  }
  
  message("USER ACCEPTED AND QUOTA UPDATED !") 
  print(counter[quota_lab_id])
  
  # check that counter is working 
  if(sum(counter$counter)<=1){
    stop("Sum of quota counters is 1 or 0.
It should be at least 2 since the quota frame is not new and the user was accepted.
Stop and check !") 
  }
  
  # save updated quota frame
  save(counter, file = path.counter.latest)
  
}