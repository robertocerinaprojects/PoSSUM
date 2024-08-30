message('STARTING ROUTINE: poll_users')



# # # PART 0: Setup

# # load environment
message('Loading environment...')
source('R scripts/2.ai_poll/iii.poll_users/envirnoment.R')

# # load relevant user pool file paths
message('Loading pool of users')

# load file paths of tweets generated from the X API query 
pool.files <- get_files_from_dir(dir = 'generated_data/tweets')

# extract file paths from the most recent date
if(POOL_TYPE == 'LATEST'){
path.tweets <- 
  pool.files[
    grepl( 
      extract_latest_date(pool.files,time = FALSE),
      pool.files
      )
  ]
}
if(POOL_TYPE == 'STORAGE'){
  path.tweets <-  pool.files
}
# define number of pool objects over which to loop through today
n.pool.objects <- length(path.tweets)
path.shuffle <- sample(1:n.pool.objects)
# temporarily save shuffle path
save(path.shuffle,file = 'generated_data/tweets/path.shuffle.RData')

# load latest path shuffle
load(file = 'generated_data/tweets/path.shuffle.RData')

# # # PART 1: Loop through user pool objects
message('STARTING: Loop through objects generated in `get_pool` routine ...')

for(j in path.shuffle){

  
  
  # # implement temporal inclusion criteria
  message('ENFORCING: Temporal inclusion criteria ...')
  source('R scripts/2.ai_poll/iii.poll_users/temporal_inclusion_criteria.R')

  
  
  # initialise object to store the `survey` to be derived from this pool object
  message('Initialising survey object for current query ...')
  ai.survey <- data.table()
  # define where you want the survey to be saves
  save.path <- 
    paste0(
      'generated_data/survey_object/',
      gsub(
        'generated_data/tweets/',
        '',
        path.tweets[j]
        )
      )
  
  
  # # # PART 2: Loop through user pool objects
  message('STARTING: Loop through survining users for this query ...')
  for(user in 1:n.inc.users){

    
    # assign test labels if there are any ongoing tests
    TEST1 <- 'no'

    if(TEST1=='no'){
      # character vector for `independent variables`
      source('R scripts/2.ai_poll/i.set_parameters/a.independent_features_object.R')
      # turn char. vector into an object which facilitates parsing features from text 
      ind.features_table <- parse_features(ind.features)
      # character vector for `dependent variables `
      source('R scripts/2.ai_poll/i.set_parameters/b.dependent_features_object.R')
      dep.features_table <- parse_features(dep.features)
    }
    
    
    
    # set extracted user timeline flag -- 
    # this will be turned on once the timeline is extracted, 
    # and we have to turn it back off every time 
    EXTRACTED_USER_TIMELINE = FALSE
    
    
    
    # initialise temporary object for this specific user
    message('Initialising current user survey ...')
    ai.survey.temp <- 
      data.table(
        EXCLUDED = 'no',
        TEST = TEST1,
        EXTRACTED_USER_TIMELINE = EXTRACTED_USER_TIMELINE
        )

    
    
    # initialise SHA-256 hash of the ID
    message('Hashing of user-id ...')
    ai.survey.temp$gpt_id <- 
      as.character(unlist(digest(pool$users$id[[user]], algo = "sha256")))
    
    
    
    # if the user has no location parameter - drop
    message('ENFORCING (A): Null-location excluson ...')
    if(is.null(pool$users$location[[user]])){ 
      message('**USER EXCLUDED DUE TO UNKNOWN GEOGRAPHIC LOCATION**')
      ai.survey.temp$EXCLUDED <- 'null_location'
      # bind survey object 
      ai.survey = rbindlist( list(ai.survey,ai.survey.temp), fill = TRUE ) 
      # save the current survey object corresponding to a given pool object   
      save(ai.survey,file = save.path, compress = T)   
      next 
    }


    
    # entity exclusion criteria 
    message('ENFORCING (B): Entity inclusion criteria ...')
    criteria.question <- 
    'Is this the account of a real-life existing Person, or of another kind of entity ? 
Respond either with "P" for Person or "O" for Other.'
    timeout <- 30 # seconds 
    temperature <- 0.7
    model <- 'gpt-4o'
    cleanup_structure <- 'loose'
    source('R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
    gen.org <- gen
    org <- gen.org$choices$message.content
    # store reasoning
    ai.survey.temp$gpt_ENTITYREASON <- reasoning
    ai.survey.temp$gpt_ENTITY <- org
    message('LLM HAS EXTRACTED THE ENTITY OF THIS USER...\n',reasoning)
    # display output
    message('This user is ',
            ifelse(
              grepl('Other',org) | org == 'O',
              'NOT A PERSON',
              'A PERSON'
            )
    )
    # if it's not a person - drop
    if(grepl('Other',org) | org == 'O'){
      message('**USER EXCLUDED DUE TO ENTITY INCLUSION CRITERIA**')
      ai.survey.temp$EXCLUDED <- 'not_a_person'
      # bind survey object 
      ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
      # save the current survey object corresponding to a given pool object   
      save(ai.survey,file = save.path,compress = T)   
      next
    }
    
    
    
    # impute geo2
    message('IMPUTING (A) - GEO2: Imputing 2nd level of geography (e.g. State) ...')
    # optimal prompt for geo2 extraction
    criteria.question <- geo2_prompt
    timeout <- 30 # seconds 
    model <- 'gpt-4o'
    temperature <- 0.7
    cleanup_structure <- 'loose'
    source('R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
    gen.geo2 <- gen
    geo2 <- gen.geo2$choices$message.content
    # store reasoning
    ai.survey.temp$gpt_GEO2REASON <- reasoning
    ai.survey.temp$gpt_GEO2 <- geo2
    message('LLM HAS EXTRACTED THE ENTITY OF THIS USER...\n',reasoning)
    # display output
    message(
      'CLEAN GEO2 - ',
      geo2,
      ' ...'
    )
    # if user does not live in geo - drop
    if(grepl('Not from', geo2)){
      message('**USER EXCLUDED DUE TO GEOGRAPHIC INCLUSION CRITERIA**')
      ai.survey.temp$EXCLUDED <- paste0('not_from_USA')
      # bind survey object 
      ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
      # save the current survey object corresponding to a given pool object   
      save(ai.survey,file = save.path,compress = T)   
      next
    }
    
    

    # quota inclusion criteria 
    message('GENERATING (D): Quotas ...')
    criteria.question <- 
      paste0(
      'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
      # randomise feature order
      paste0(
        sample(
          ind.features,
          replace = FALSE
          ),
        collapse = '\n'
        )
      )
    # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low
    retry_count <- 0
    max_retries <- 15
    success <- FALSE
    while (retry_count < max_retries & !success) {
      tryCatch({
        timeout <- 120 #seconds 
        model <- 'gpt-4o'
        temperature <- 0.7
        cleanup_structure <- 'strict'
        source('R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
        gen.quota <- gen
        # present extracted quota features 
        message('LLM HAS EXTRACTED THE FOLLOWING QUOTA-FEATURES:', reasoning)
        message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING QUOTA-FEATURES:', output)
        # store these extracted variables in the temporary AI survey object
        ai.survey.temp$gpt_QUOTA <- reasoning
        # parse the output 
        ai.survey.temp <- cbind(
          ai.survey.temp,
          parse_text_to_datatable(
            text = output,
            reference_table = ind.features_table,
            label = 'QUOTA',
            speculation = TRUE
          )
        )
        success <- TRUE
      }, error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count >= max_retries) {
          message('Error: ', e$message)
          stop("Maximum retries reached. Exiting.")
        } else {
          message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
        }
      })
    }
    # # sub-criteria: speculation in quota features 
    # message(
    #  'ENFORCING (D-a): Speculation in quota features should be lower than ',
    #  s_threshold,
    #  ' on average...'
    #  )
    # # extract speculation scores 
    # spec.cols <- grep('speculation',names(ai.survey.temp),value = TRUE)
    # s_scores <- unlist(ai.survey.temp[,..spec.cols])
    # # if average speculation is above s_threshold, skip user 
    # if(mean(s_scores,na.rm=T)>s_threshold){
    #   message('**USER EXCLUDED DUE TO HIGHLY SPECULATIVE QUOTA-FEATURES IMPUTATION**')
    #   ai.survey.temp$EXCLUDED <- 'highly_speculative_quota_features'
    #   # bind survey object 
    #   ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
    #   # save the current survey object corresponding to a given pool object   
    #   save(ai.survey,file = save.path,compress = T)   
    #   next
    # }
    # now run quota test and update quotas as necessary 
    message(
      'ENFORCING (D-b): Testing whether user is acceptable based quota-features cap ...'
    )
    # if the user has survived this far, we can implement quota
    source('R scripts/2.ai_poll/iii.poll_users/quota_inclusion_criteria.R')
    
    
    
    # get max results for specific subject pool 
    max_results <- ifelse(grepl('trends',path.tweets[j]),m.trends,m)
    # get tweets
    this.user.tweets <- 
      robust_get_user_tweets(
        user_id = pool$users$id[[user]], 
        max_results = max_results, 
        store_path = paste0('generated_data/user_tweets/',pool$users$id[[user]],'_',Sys.Date(),'.RData'),
        max_retries = 100,
        sleep = 120
      )
    if(length(length(this.user.tweets$data$text))>0){
      EXTRACTED_USER_TIMELINE = TRUE
      ai.survey.temp$EXTRACTED_USER_TIMELINE <- EXTRACTED_USER_TIMELINE
      message(
        'X API HAS RETURNED THE LAST ',length(this.user.tweets$data$text),' TWEETS FOR THIS USER ...'
      )
    }


    # impute latest past geo2 vote
    message('IMPUTING (B) GEO2 PAST VOTE: Imputing most recent past vote choice at the 2nd level of geography (e.g. State or Region) ...')
    if(geo2!='USA'){
      
      message('ATTEMPTING TO GENERATE BACKRGOUND - INFORMED PAST GEO2 ELECTION VOTE PROMPT (BGRES)...')
      
      {
        bg <-
          paste0(
            'BACKGROUND INFORMATION:
The results of the 2020 US Presidential election in state -- ',
            geo2,
            ' -- are reported below. 

',
            paste0(
              sample(
                apply(
                  geo2_lookup[
                    year == 2020 & 
                      state == tolower(geo2) & 
                      candidatevotes!="0 %"
                  ][,c('candidate','party','candidatevotes')],
                  1,
                  function(x){
                    paste0(paste0(c('candidate: ','party: ','votes: '),x),collapse = '\n')
                  }
                )
              ),
              collapse = '\n\n'),
            "

In the above, note that eleciton results are stated as % of the voting age population in the state.

INSTRUCTIONS:\n")
        # generate past geo2 vote prompt with a short prompt 
        messages.list <- prep_gpt_messages()
        messages.list[[1]]$content[[1]]$text <- 
          paste0(
            bg,
            geo2_pastvote_president_candidatesprompt
          )
        # retrying if "INSERT" is present
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 30 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            # note - this does not reset bg
            gen.geo2.pastvote_prompt <- 
              call_gpt(
                model = model,
                temperature = temperature,
                messages.list = messages.list,
                timeout = timeout
              )
            # display output
            message(
              'LLM HAS EXTRACTED GEO2 BGRES PAST VOTE PROMPT FOR THIS USER...',
              gen.geo2.pastvote_prompt$choices$message.content
            )
            
            # check for insert
            if (grepl("\\<INSERT\\_", gen.geo2.pastvote_prompt$choices$message.content)) {
              stop("Output contains <INSERT_, retrying...")
            }
            
            # Check for duplicate symbols
            lines <- strsplit(gen.geo2.pastvote_prompt$choices$message.content, "\n")[[1]]
            symbols <- sub("\\).*", "", lines)
            if (any(duplicated(symbols))) {
              stop("Output contains duplicate symbols, retrying...")
            }
            
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
        # display output
        message(
          'LLM HAS EXTRACTED GEO2 BGRES PAST VOTE PROMPT FOR THIS USER...',
          gen.geo2.pastvote_prompt$choices$message.content
        )
        
        
        
        # now extract past vote for geo 2
        message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITH BACKRGOUND - INFORMED PAST GEO2 ELECTION VOTE PROMPT (BGRES)...')
        criteria.question <- 
          paste0(
            'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
            gen.geo2.pastvote_prompt$choices$message.content
          )
        reference_table <- parse_features(strsplit(gen.geo2.pastvote_prompt$choices$message.content,'\n\n')[[1]]) # for parsing 
        # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 120 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            cleanup_structure <- 'strict'
            source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
            # present extracted quota features 
            message('LLM HAS EXTRACTED THE FOLLOWING GEO2 BGRES PAST VOTE (BGRES):', reasoning)
            message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO2 BGRES PAST VOTE (BGRES):', output)
            # store these extracted variables in the temporary AI survey object
            ai.survey.temp$gpt_GEO2PASTVOTE_BGRES <- reasoning
            # parse the output 
            ai.survey.temp <- cbind(
              ai.survey.temp,
              parse_text_to_datatable(
                text = output,
                reference_table = reference_table,
                label = 'GEO2PASTVOTE_BGRES',
                speculation = TRUE
              )
            )
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
        }
      
      message('ATTEMPTING TO GENERATE BACKRGOUND - INFORMED PAST GEO2 ELECTION VOTE PROMPT (BGNORES)...')
      
      {
        bg <-
          paste0(
            'BACKGROUND INFORMATION:
The candidates for the 2020 US Presidential election in state -- ',
            geo2,
            ' -- are reported below. 

',
            paste0(
              sample(
                apply(
                  geo2_lookup[
                    year == 2020 & 
                      state == tolower(geo2) & 
                      candidatevotes!="0 %"
                  ][,c('candidate','party')],
                  1,
                  function(x){
                    paste0(paste0(c('candidate: ','party: '),x),collapse = '\n')
                  }
                )
              ),
              collapse = '\n\n'),
            "

INSTRUCTIONS:\n")
        # generate past geo2 vote prompt with a short prompt 
        messages.list <- prep_gpt_messages()
        messages.list[[1]]$content[[1]]$text <- 
          paste0(
            bg,
            geo2_pastvote_president_candidatesprompt
          )
        # retrying if "INSERT" is present
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 30 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            # note - this does not reset bg
            gen.geo2.pastvote_prompt <- 
              call_gpt(
                model = model,
                temperature = temperature,
                messages.list = messages.list,
                timeout = timeout
              )
            # display output
            message(
              'LLM HAS EXTRACTED GEO2 BGNORES PAST VOTE PROMPT FOR THIS USER...',
              gen.geo2.pastvote_prompt$choices$message.content
            )
            
            # check for insert
            if (grepl("\\<INSERT\\_", gen.geo2.pastvote_prompt$choices$message.content)) {
              stop("Output contains <INSERT_, retrying...")
            }
            
            # Check for duplicate symbols
            lines <- strsplit(gen.geo2.pastvote_prompt$choices$message.content, "\n")[[1]]
            symbols <- sub("\\).*", "", lines)
            if (any(duplicated(symbols))) {
              stop("Output contains duplicate symbols, retrying...")
            }
            
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
        # display output
        message(
          'LLM HAS EXTRACTED GEO2 BGNORES PAST VOTE PROMPT FOR THIS USER...',
          gen.geo2.pastvote_prompt$choices$message.content
        )
        
        
        
        # now extract past vote for geo 2
        message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITH BACKRGOUND - INFORMED PAST GEO2 ELECTION VOTE PROMPT (BGNORES)...')
        criteria.question <- 
          paste0(
            'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
            gen.geo2.pastvote_prompt$choices$message.content
          )
        reference_table <- parse_features(strsplit(gen.geo2.pastvote_prompt$choices$message.content,'\n\n')[[1]]) # for parsing 
        # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 120 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            cleanup_structure <- 'strict'
            source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
            # present extracted quota features 
            message('LLM HAS EXTRACTED THE FOLLOWING GEO2 PAST VOTE (BGNORES):', reasoning)
            message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO2 PAST VOTE (BGNORES):', output)
            # store these extracted variables in the temporary AI survey object
            ai.survey.temp$gpt_GEO2PASTVOTE_BGNORES <- reasoning
            # parse the output 
            ai.survey.temp <- cbind(
              ai.survey.temp,
              parse_text_to_datatable(
                text = output,
                reference_table = reference_table,
                label = 'GEO2PASTVOTE_BGNORES',
                speculation = TRUE
              )
            )
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
      }  
        
      
      }
      
      
      
      # now extract past vote for geo 2
      message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITHOUT BACKGROUND -- NOT ENOUGH LOCATION INFO TO GET GEO2 BACKRGOUND ...')
      criteria.question <- 
        paste0(
          'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
          geo2_pastvote_president_candidatesprompt_default
        )
      reference_table <- parse_features(strsplit(geo2_pastvote_president_candidatesprompt_default,'\n\n')[[1]]) # for parsing 
      # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
      retry_count <- 0
      max_retries <- 15
      success <- FALSE
      while (retry_count < max_retries & !success) {
        tryCatch({
          timeout <- 120 # seconds 
          model <- 'gpt-4o'
          temperature <- 0.7
          cleanup_structure <- 'strict'
          source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
          # present extracted quota features 
          message('LLM HAS EXTRACTED THE FOLLOWING GEO2 PAST VOTE (IND):', reasoning)
          message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO2 PAST VOTE (IND):', output)
          # store these extracted variables in the temporary AI survey object
          ai.survey.temp$gpt_GEO2PASTVOTE_IND <- reasoning
          # parse the output 
          ai.survey.temp <- cbind(
            ai.survey.temp,
            parse_text_to_datatable(
              text = output,
              reference_table = reference_table,
              label = 'GEO2PASTVOTE_IND',
              speculation = TRUE
            )
          )
          success <- TRUE
        }, error = function(e) {
          retry_count <<- retry_count + 1
          if (retry_count >= max_retries) {
            message('Error: ', e$message)
            stop("Maximum retries reached. Exiting.")
          } else {
            message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
          }
        })
      }
      
      
    
    
    
    # impute geo3 - provided we know geo2 - and past geo3 vote
    if(geo2!='USA'){
      message('IMPUTING (C) - GEO3: Imputing 3nd level of geography (e.g. Congressional District or Parliamentary Constituency) ...')
      # recall relevant context 
      gen.geo3_context <- 
        paste0(
          sample(gen.geo3_context_list[state == geo2]$context.summary),
          collapse='\n\n'
        )
      if(length(gen.geo3_context)==0){stop("**DETECTED GEO2 OUTPUT FOR WHICH WE DON'T HAVE CONTEXT -- INVESTIGATE MANUALLY**")}
      # display output
      message('LLM HAS RECALLED THE CONTEXT FOR 3RD LEVEL OF GEOGRAPHY FOR ', geo2,':\n',gen.geo3_context)
      # store these extracted variables in the temporary AI survey object
      ai.survey.temp$gpt_GEO3_context <- gen.geo3_context
      # optimal prompt for geo3 extraction
      criteria.question <- geo3_prompt
      timeout <- 120 # seconds 
      model <- 'gpt-4o'
      temperature <- 0.7
      cleanup_structure <- 'strict'
      bg <- 
        paste0(
        'BACKGROUND INFORMATION:
The following is a brief summary of the characteristics of each congressional district in',
        geo2,":
",
        gen.geo3_context,
        "

In the above, note that eleciton results are stated as % of the voting age population in the state.

INSTRUCTIONS:
")
    source('R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
    # display output
    message(
      'LLM HAS EXTRACTED GEO3 LOCATION FOR THIS USER...',
      reasoning
    )
    # store these extracted variables in the temporary AI survey object
    ai.survey.temp$gpt_GEO3_dirty <- reasoning
    # now clean the output with an easy gpt call
    messages.list <- prep_gpt_messages()
    messages.list[[1]]$content[[1]]$text <- 
      paste(
        geo3_parse_prompt,"\n<<",output,'>>.'
      )
    gen.clean.geo3 <- 
      call_gpt(
        model = model,
        temperature = temperature,
        messages.list = messages.list,
        timeout = timeout
        )
    geo3 <- gen.clean.geo3$choices$message.content
    # display output
    message('LLM HAS CLEANED THE 3RD LEVEL OF GEOGRAPHY FOR THIS USER...\n',geo3)
    # parse and store
    gpt_GEO3_clean <- 
      ifelse(
        grepl("Don't Know",geo3),
        NA,
        trimws(sub("District: (.+?) - Speculation: (\\d+)", "\\1", geo3))
      )
    ai.survey.temp$gpt_GEO3_clean <- gpt_GEO3_clean
    gpt_speculation_GEO3_clean <- 
      ifelse(
        grepl("Don't Know",geo3),
        0,
        as.numeric(trimws(sub(".*Speculation: (\\d+)", "\\1", geo3)))
      )
    ai.survey.temp$gpt_speculation_GEO3_clean <- gpt_speculation_GEO3_clean
    
    
    
    message('IMPUTING (E) GEO3 PAST VOTE: Imputing most recent past vote choice at the 3rd level of geography (e.g. Congressional District or Parliamentary Constituency) ...')
    # create congressional district specific vote-prompt, 
    # provided we have successfully identified a plausible geo3.
    if(!is.na(gpt_GEO3_clean)){
      
      
      message('ATTEMPTING TO GENERATE BACKRGOUND - INFORMED PAST GEO3 BGRES ELECTION VOTE PROMPT...')
      
      {
      # optimal prompt for geo3 past vote choice extraction
      criteria.question <- geo3_pastvote_congress_candidatesprompt 
      bg <-
        paste0(
          'BACKGROUND INFORMATION:
The results of the 2022 election for the US House of Representative in congressional district -- ',
gpt_GEO3_clean,
' -- are reported below.

',
paste0(
  sample(
    apply(
      geo3_lookup[
        year == 2022 & 
          state == tolower(geo2) & 
          district == 
          parse_district_number(
            gsub('at large','0',gpt_GEO3_clean)
            ) & 
          candidatevotes != '0 %'
        ][,c('candidate','party','candidatevotes')],
      1,
      function(x){
        paste0(paste0(c('candidate: ','party: ','votes: '),x),collapse = '\n')
        }
      )
    ),
  collapse = '\n\n'),
"

In the above, note that eleciton results are stated as % of the voting age population in the state. 

INSTRUCTIONS:\n")
      # generate past geo3 vote prompt with a short prompt 
      messages.list <- prep_gpt_messages()
      messages.list[[1]]$content[[1]]$text <- 
        paste0(
          bg,
          geo3_pastvote_congress_candidatesprompt
        )
      # retrying if "INSERT" is present
      retry_count <- 0
      max_retries <- 15
      success <- FALSE
      while (retry_count < max_retries & !success) {
        tryCatch({
          timeout <- 30 # seconds 
          model <- 'gpt-4o'
          temperature <- 0.7
          gen.geo3.pastvote_prompt <- 
            call_gpt(
              model = model,
              temperature = temperature,
              messages.list = messages.list,
              timeout = timeout
            )
          # display output
          message(
            'LLM HAS EXTRACTED GEO3 PAST VOTE PROMPT FOR THIS USER...',
            gen.geo3.pastvote_prompt$choices$message.content
          )
          # check for instert
          if (grepl("\\<INSERT\\_", gen.geo3.pastvote_prompt$choices$message.content)) {
            stop("Output contains <INSERT_, retrying...")
          }
          
          # Check for duplicate symbols
          lines <- strsplit(gen.geo3.pastvote_prompt$choices$message.content, "\n")[[1]]
          symbols <- sub("\\).*", "", lines)
          if (any(duplicated(symbols))) {
            stop("Output contains duplicate symbols, retrying...")
          }
          
          success <- TRUE
        }, error = function(e) {
          retry_count <<- retry_count + 1
          if (retry_count >= max_retries) {
            message('Error: ', e$message)
            stop("Maximum retries reached. Exiting.")
          } else {
            message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
          }
        })
      }
      # display output
      message(
        'LLM HAS EXTRACTED GEO3 PAST VOTE PROMPT FOR THIS USER...',
        gen.geo3.pastvote_prompt$choices$message.content
      )
    
    
      
    # now extract past vote for geo 3 
    message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITH BACKRGOUND - INFORMED PAST GEO3 ELECTION VOTE PROMPT...')
    criteria.question <- 
      paste0(
        'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
        gen.geo3.pastvote_prompt$choices$message.content
        )
    reference_table <- parse_features(strsplit(gen.geo3.pastvote_prompt$choices$message.content,'\n\n')[[1]]) # for parsing 
    # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
    retry_count <- 0
    max_retries <- 15
    success <- FALSE
    while (retry_count < max_retries & !success) {
      tryCatch({
        timeout <- 120 # seconds 
        model <- 'gpt-4o'
        temperature <- 0.7
        cleanup_structure <- 'strict'
        source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
        # present extracted quota features 
        message('LLM HAS EXTRACTED THE FOLLOWING GEO3 PAST VOTE (BGRES):', reasoning)
        message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO3 PAST VOTE (BGRES):', output)
        # store these extracted variables in the temporary AI survey object
        ai.survey.temp$gpt_GEO3PASTVOTE_BGRES <- reasoning
        # parse the output 
        ai.survey.temp <- cbind(
          ai.survey.temp,
          parse_text_to_datatable(
            text = output,
            reference_table = reference_table,
            label = 'GEO3PASTVOTE_BGRES',
            speculation = TRUE
          )
        )
        success <- TRUE
      }, error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count >= max_retries) {
          message('Error: ', e$message)
          stop("Maximum retries reached. Exiting.")
        } else {
          message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
        }
      })
    }
      }
      
      message('ATTEMPTING TO GENERATE BACKRGOUND - INFORMED PAST GEO3 BGNORES ELECTION VOTE PROMPT...')
      
      {
        # optimal prompt for geo3 past vote choice extraction
        criteria.question <- geo3_pastvote_congress_candidatesprompt 
        bg <-
          paste0(
            'BACKGROUND INFORMATION:
The results of the 2022 election for the US House of Representative in congressional district -- ',
            gpt_GEO3_clean,
            ' -- are reported below.

',
            paste0(
              sample(
                apply(
                  geo3_lookup[
                    year == 2022 & 
                      state == tolower(geo2) & 
                      district == 
                      parse_district_number(
                        gsub('at large','0',gpt_GEO3_clean)
                      ) & 
                      candidatevotes != '0 %'
                  ][,c('candidate','party')],
                  1,
                  function(x){
                    paste0(paste0(c('candidate: ','party: '),x),collapse = '\n')
                  }
                )
              ),
              collapse = '\n\n'),
            "

INSTRUCTIONS:\n")
        # generate past geo3 vote prompt with a short prompt 
        messages.list <- prep_gpt_messages()
        messages.list[[1]]$content[[1]]$text <- 
          paste0(
            bg,
            geo3_pastvote_congress_candidatesprompt
          )
        # retrying if "INSERT" is present
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 30 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            gen.geo3.pastvote_prompt <- 
              call_gpt(
                model = model,
                temperature = temperature,
                messages.list = messages.list,
                timeout = timeout
              )
            # display output
            message(
              'LLM HAS EXTRACTED GEO3 PAST VOTE PROMPT FOR THIS USER...',
              gen.geo3.pastvote_prompt$choices$message.content
            )
            # check for instert
            if (grepl("\\<INSERT\\_", gen.geo3.pastvote_prompt$choices$message.content)) {
              stop("Output contains <INSERT_, retrying...")
            }
            
            # Check for duplicate symbols
            lines <- strsplit(gen.geo3.pastvote_prompt$choices$message.content, "\n")[[1]]
            symbols <- sub("\\).*", "", lines)
            if (any(duplicated(symbols))) {
              stop("Output contains duplicate symbols, retrying...")
            }
            
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
        # display output
        message(
          'LLM HAS EXTRACTED GEO3 PAST VOTE PROMPT FOR THIS USER...',
          gen.geo3.pastvote_prompt$choices$message.content
        )
        
        
        
        # now extract past vote for geo 3 
        message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITH BACKRGOUND - INFORMED PAST GEO3 BGNORES ELECTION VOTE PROMPT...')
        criteria.question <- 
          paste0(
            'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
            gen.geo3.pastvote_prompt$choices$message.content
          )
        reference_table <- parse_features(strsplit(gen.geo3.pastvote_prompt$choices$message.content,'\n\n')[[1]]) # for parsing 
        # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
        retry_count <- 0
        max_retries <- 15
        success <- FALSE
        
        while (retry_count < max_retries & !success) {
          tryCatch({
            timeout <- 120 # seconds 
            model <- 'gpt-4o'
            temperature <- 0.7
            cleanup_structure <- 'strict'
            source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
            # present extracted quota features 
            message('LLM HAS EXTRACTED THE FOLLOWING GEO3 PAST VOTE (BGNORES):', reasoning)
            message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO3 PAST VOTE (BGNORES):', output)
            # store these extracted variables in the temporary AI survey object
            ai.survey.temp$gpt_GEO3PASTVOTE_BGNORES <- reasoning
            # parse the output 
            ai.survey.temp <- cbind(
              ai.survey.temp,
              parse_text_to_datatable(
                text = output,
                reference_table = reference_table,
                label = 'GEO3PASTVOTE_BGNORES',
                speculation = TRUE
              )
            )
            success <- TRUE
          }, error = function(e) {
            retry_count <<- retry_count + 1
            if (retry_count >= max_retries) {
              message('Error: ', e$message)
              stop("Maximum retries reached. Exiting.")
            } else {
              message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
            }
          })
        }
      }
      
      
    } 
    

  
    }
    
      
      
    # now extract past vote for geo 3 
    message('ATTEMPTING TO IMPUTE PAST VOTE ESTIMATE WITHOUT BACKGROUND -- NOT ENOUGH LOCATION INFO TO GET GEO3 BACKRGOUND ...')
    criteria.question <- 
      paste0(
        'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
        geo3_pastvote_congress_candidatesprompt_default
      )
    reference_table <- parse_features(strsplit(geo3_pastvote_congress_candidatesprompt_default,'\n\n')[[1]]) # for parsing 
    # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
    retry_count <- 0
    max_retries <- 15
    success <- FALSE
    while (retry_count < max_retries & !success) {
      tryCatch({
        timeout <- 120 # seconds 
        model <- 'gpt-4o'
        temperature <- 0.7
        cleanup_structure <- 'strict'
        source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
        # present extracted quota features 
        message('LLM HAS EXTRACTED THE FOLLOWING GEO3 PAST VOTE (IND):', reasoning)
        message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO3 PAST VOTE (IND):', output)
        # store these extracted variables in the temporary AI survey object
        ai.survey.temp$gpt_GEO3PASTVOTE_IND <- reasoning
        # parse the output 
        ai.survey.temp <- cbind(
          ai.survey.temp,
          parse_text_to_datatable(
            text = output,
            reference_table = reference_table,
            label = 'GEO3PASTVOTE_IND',
            speculation = TRUE
          )
        )
        success <- TRUE
      }, error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count >= max_retries) {
          message('Error: ', e$message)
          stop("Maximum retries reached. Exiting.")
        } else {
          message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
        }
      })
    }
    
     
    
    
    

    
    
    # impute current geo2 vote
    message('IMPUTING (D) GEO2 CURRENT VOTE: Imputing current vote choice at the 2nd level of geography (e.g. State or Region) ...')
    if(geo2!='USA'){
      message('ATTEMPTING TO GENERATE BACKRGOUND - INFORMED CURRENT GEO2 ELECTION VOTE PROMPT...')
      # prep candidates for this state
      candidates <- 
        unlist(
          ballot_access[
            ballot_access$`State / electors`==
              usmap::fips_info()$abbr[match(geo2,usmap::fips_info()$full)]
          ] )
      candidates <- names(grep('Yes',candidates,value = TRUE))
      
      bg <-
        paste0(
          'BACKGROUND INFORMATION:
The candidates running in the 2024 US Presidential election in state -- ',
          geo2,
          ' -- are reported below.

',
         paste0(
           candidates,
           collapse = '\n'
         ),
          "

INSTRUCTIONS:\n")
      # generate past geo2 vote prompt with a short prompt 
      messages.list <- prep_gpt_messages()
      messages.list[[1]]$content[[1]]$text <- 
        paste0(
          bg,
          geo2_currentvote_president_candidatesprompt
        )
      # retrying if "INSERT" is present
      retry_count <- 0
      max_retries <- 15
      success <- FALSE
      while (retry_count < max_retries & !success) {
        tryCatch({
          timeout <- 30 # seconds 
          model <- 'gpt-4o'
          temperature <- 0.7
          cleanup_structure <- 'strict'
          gen.geo2.currentvote_prompt <- 
            call_gpt(
              model = model,
              temperature = temperature,
              messages.list = messages.list,
              timeout = timeout
            )
          # display output
          message(
            'LLM HAS EXTRACTED GEO2 CURRENT VOTE PROMPT FOR THIS USER...',
            gen.geo2.currentvote_prompt$choices$message.content
          )
          
          # check for insert
          if (grepl("\\<INSERT\\_", gen.geo2.currentvote_prompt$choices$message.content)) {
            stop("Output contains <INSERT_, retrying...")
          }
          
          # Check for duplicate symbols
          lines <- strsplit(gen.geo2.currentvote_prompt$choices$message.content, "\n")[[1]]
          symbols <- sub("\\).*", "", lines)
          if (any(duplicated(symbols))) {
            stop("Output contains duplicate symbols, retrying...")
          }
          success <- TRUE
        }, error = function(e) {
          retry_count <<- retry_count + 1
          if (retry_count >= max_retries) {
            message('Error: ', e$message)
            stop("Maximum retries reached. Exiting.")
          } else {
            message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
          }
        })
      }
      # display output
      message(
        'LLM HAS EXTRACTED GEO2 CURRENT VOTE PROMPT FOR THIS USER...',
        gen.geo2.currentvote_prompt$choices$message.content
      )
      
      
      
      # now extract past vote for geo 2
      message('ATTEMPTING TO IMPUTE CURRENT VOTE ESTIMATE WITH BACKRGOUND - INFORMED CURRENT GEO2 ELECTION VOTE PROMPT (NORES)...')
      criteria.question <- 
        paste0(
          'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
          gen.geo2.currentvote_prompt$choices$message.content
        )
      reference_table <- parse_features(strsplit(gen.geo2.currentvote_prompt$choices$message.content,'\n\n')[[1]]) # for parsing 
      # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
      retry_count <- 0
      max_retries <- 15
      success <- FALSE
      while (retry_count < max_retries & !success) {
        tryCatch({
          timeout <- 120 # seconds 
          model <- 'gpt-4o'
          temperature <- 0.7
          cleanup_structure <- 'strict'
          source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
          # present extracted quota features 
          message('LLM HAS EXTRACTED THE FOLLOWING GEO2 CURRENT VOTE (BGNORES):', reasoning)
          message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO2 CURRENT VOTE (BGNORES):', output)
          # store these extracted variables in the temporary AI survey object
          ai.survey.temp$gpt_GEO2CURRENTVOTE_BGNORES <- reasoning
          # parse the output 
          ai.survey.temp <- cbind(
            ai.survey.temp,
            parse_text_to_datatable(
              text = output,
              reference_table = reference_table,
              label = 'GEO2CURRENTVOTE_BGNORES',
              speculation = TRUE
            )
          )
          success <- TRUE
        }, error = function(e) {
          retry_count <<- retry_count + 1
          if (retry_count >= max_retries) {
            message('Error: ', e$message)
            stop("Maximum retries reached. Exiting.")
          } else {
            message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
          }
        })
      }
      
      
      
    }
    
      
      
      
      # now extract past vote for geo 2
      message('ATTEMPTING TO IMPUTE CURRENT VOTE ESTIMATE WITHOUT BACKGROUND -- NOT ENOUGH LOCATION INFO TO GET GEO2 BACKRGOUND ...')
      criteria.question <- 
        paste0(
          'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
          geo2_currentvote_president_candidatesprompt_default
        )
      reference_table <- parse_features(strsplit(geo2_currentvote_president_candidatesprompt_default,'\n\n')[[1]]) # for parsing 
      # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
      retry_count <- 0
      max_retries <- 15
      success <- FALSE
      while (retry_count < max_retries & !success) {
        tryCatch({
          timeout <- 120 # seconds 
          model <- 'gpt-4o'
          temperature <- 0.7
          cleanup_structure <- 'strict'
          source(file = 'R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
          # present extracted quota features 
          message('LLM HAS EXTRACTED THE FOLLOWING GEO2 CURRENT VOTE (IND):', reasoning)
          message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING GEO2 CURRENT VOTE (IND):', output)
          # store these extracted variables in the temporary AI survey object
          ai.survey.temp$gpt_GEO2CURRENTVOTE_IND <- reasoning
          # parse the output 
          ai.survey.temp <- cbind(
            ai.survey.temp,
            parse_text_to_datatable(
              text = output,
              reference_table = reference_table,
              label = 'GEO2CURRENTVOTE_IND',
              speculation = TRUE
            )
          )
          success <- TRUE
        }, error = function(e) {
          retry_count <<- retry_count + 1
          if (retry_count >= max_retries) {
            message('Error: ', e$message)
            stop("Maximum retries reached. Exiting.")
          } else {
            message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
          }
        })
      }

    

    # massive feature extraction prompt 
    message('IMPUTING (E) SOCIO-DEMOGRAPHIC + POLITICAL VARIABLES...')

        criteria.question <- 
      paste0(
        'I will show you a number of categories to which this user may belong to.
The categories are preceded by a title (e.g. "AGE:" or "SEX:" etc.) and a symbol (e.g. "A1", "A2" or "E1" etc.).
Please select, for each title, the most likely category to which this user belongs to.

In your answer present, for each title, the selected symbol.
Write out in full the category associated with the selected symbol.
The chosen symbol / category must be the most likely to accurately represent this user.
You must only select one symbol / category per title.
A title, symbol and category cannot appear more than once in your answer.

For each selected symbol / category, please note the level of Speculation involved in this selection.
Present the Speculation level for each selection on a scale from 0 (not speculative at all, every single element of the user data was useful in the selection) to 100 (fully speculative, there is no information related to this title in the user data).
Speculation levels should be a direct measure of the amount of useful information available in the user data.
Speculation levels pertain only to the information available in the user data -- namely the username, name, description, location, profile picture and tweets from this user -- and should not be affected by additional information available to you from any other source. 
To ensure consistency, use the following guidelines to determine speculation levels:

0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)

For each selected category, please explain at length what features of the data contributed to your choice and your speculation level.

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**


YOU MUST GIVE AN ANSWER FOR EVERY TITLE !


Below is the list of categories to which this user may belong to: 


',
        # randomise feature order
        paste0(
          sample(
            ind.features,
            replace = FALSE
          ),
          collapse = '\n'
        ),
        paste0(
          sample(
            dep.features,
            replace = FALSE
          ),
          collapse = '\n'
        ),
        geo2_currentvote_president_candidatesprompt_default
      )
    # setup retry routine for parsing and storage (if the parser fails this can be useful -- keep retries low)
    retry_count <- 0
    max_retries <- 15
    success <- FALSE
    while (retry_count < max_retries & !success) {
      tryCatch({
        timeout <- 120 #seconds 
        model <- 'gpt-4o'
        temperature <- 0.7
        cleanup_structure <- 'strict'
        source('R scripts/2.ai_poll/iii.poll_users/gpt_build.user.call.R')
        # present extracted quota features 
        message('LLM HAS EXTRACTED THE FOLLOWING DEMOS:', reasoning)
        message('MINI-LLM HAS CLEANED THE OUTPUT TO THE FOLLOWING DEMOS:', output)
        # store these extracted variables in the temporary AI survey object
        ai.survey.temp$gpt_DEMO <- reasoning
        # parse the output 
        ai.survey.temp <- cbind(
          ai.survey.temp,
          parse_text_to_datatable(
            text = output,
            reference_table = append(
              append(
                ind.features_table,
                dep.features_table
              ),
              parse_features(strsplit(geo2_currentvote_president_candidatesprompt_default, '\n\n')[[1]])
            ),
            label = 'DEMO',
            speculation = TRUE
          )
        )
        success <- TRUE
      }, error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count >= max_retries) {
          message('Error: ', e$message)
          stop("Maximum retries reached. Exiting.")
        } else {
          message('Error encountered, retrying... (attempt ', retry_count, ' of ', max_retries, ')')
        }
      })
      
    }

    
    
    # add noise to date to enhance privacy 
    message('(F) NOISY DATE ...')
    # add date (of last tweet)
    ai.survey.temp$date <- add_noise_to_time(max(unlist(this.user.tweets$data$created_at)))
    
    
    
    # bind this user with the previous ones
    ai.survey = rbindlist( list(ai.survey,ai.survey.temp),fill = TRUE ) 
    message('***COMPLETED FEATURE EXTRACTION FOR THIS USER -- UPDATING SURVEY OBJECT***')
    # save the current survey object corresponding to a given pool object
    save(ai.survey,file = save.path,compress = T)
    
    # print output
    print(ai.survey)
    
    
  } 
  
  
  }



message('ENDING ROUTINE: poll_users')

# # # CLEAN WORKING MEMORY AT SCRIPT END

# clean workspace
rm(list = ls())
# clean garbadge
gc()


