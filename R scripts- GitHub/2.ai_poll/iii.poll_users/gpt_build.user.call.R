# prep gpt messages 
messages.list <- prep_gpt_messages()

# prepare image
image_list <- 
  prep_gpt_image(
    image_url = pool$users$profile_image_url[[user]],
    X_size = '_normal'
  )

# prepare available tweets for prompt
if(EXTRACTED_USER_TIMELINE){
  
  prompt.tweets <- 
    paste0(
      paste0('date and time of tweet (date expressed as Y-m-d): ',
             this.user.tweets$data$created_at),';\n',
      paste0('tweet text: ',
             unlist(this.user.tweets$data$text),'.'),
      collapse = '\n\n'
    )
  
}else{
  
  prompt.tweets <-
    paste0(
      sapply(
        1:sum(unlist(pool$tweets$author_id)==pool$users$id[[user]]),
        function(x){
          paste0(
            'date and time of tweet (date expressed as Y-m-d): ',
            unlist(pool$tweets[unlist(pool$tweets$author_id)==pool$users$id[[user]],]$created_at)[x],';\n',
            'tweet text: ',
            unlist(pool$tweets[unlist(pool$tweets$author_id)==pool$users$id[[user]],]$text)[x],'.'
          )
          }
        ),
      collapse = '\n\n'
    )
  }

# prepare prompt text
messages.list[[1]]$content[[1]]$text <-
  paste0(
    bg,
    "A social media account has the following username, name, description and profile image:\n",
    paste0(
      'username:',pool$users$username[[user]],'\n',
      'name:',pool$users$name[[user]],'\n',
      'description: ',pool$users$description[[user]],'\n'
    ),".
Furthermore, they self-report their location in their bio as follows:\n",
    pool$users$location[[user]],".

Finally, they have written the following tweet(s):\n",
    prompt.tweets,
    ".

",
    criteria.question
  ) 

# append image to prompt text 
messages.list <- 
  append_gpt_content(
    gpt_messages = messages.list,
    new_content = image_list
  )

# run prompt
gen <- 
  call_gpt(
    model = model,
    temperature = temperature,
    messages.list = messages.list,
    timeout = timeout
  )

message('GPT OUTPUT:', gen$choices$message.content)


if(temperature>0){
  
  
  
  # display output
  # message('GPT OUTPUT:', gen$choices$message.content)
  
  
  # store full output for record of reasoning 
  reasoning <- gen$choices$message.content
  output <- gen$choices$message.content
  
  
  # generate cleanup prompt 
  
  if(cleanup_structure == 'loose'){
    clean.messages.list <- prep_gpt_messages()
    clean.messages.list[[1]]$content[[1]]$text <- 
      paste0(
        'You will be shown a response to a question asking an LLM to detect and infer an attribute for a given user.
This attribute could be a location, an entity, a demographic, etc.
Remove all unnecessary text, including any explanation of the inference, and retain only the chosen attribute.

Below is the long-form response:

',
        output
      ) 
  }
    
  if(cleanup_structure == 'strict'){
  clean.messages.list <- prep_gpt_messages()
  clean.messages.list[[1]]$content[[1]]$text <- 
    paste0(
      'You will be shown a long-form response to a question asking to assign a series of categories to a user. 
The categories are preceded by a title (e.g. \"AGE:\" or \"SEX:\" etc.) and a symbol (e.g. \"A1\", \"A2\" or \"E1\" etc.).
The respondent reports the level of speculation with which the assessment is made.

Your task is to return, for each title, the chosen symbol, category, and level of speculation. 
Remove all unnecessary text, including any explanation of the assessment, and retain only the title, symbol, category, and speculation level 

Preserve a strictly structured answer to ease parsing of the text.
Format your output as follows (this is just an example, I do not care about this specific title or symbol / category):

**title: AGE**
**explanation: ...**
**symbol: A1)**
**category: 18-25**
**speculation: 90**

Below is the long form response: 


',
      output
    )
  }
  
  
  # set temperature 
  temperature <- 0
  model <- 'gpt-4o-mini'
  
  
  
  # run prompt
  gen <- 
    call_gpt(
      model = model,
      messages.list = clean.messages.list,
      temperature = temperature,
      timeout = timeout
    )
  
  
  
  # gpt output 
  output <- gen$choices$message.content
  
  
  
}
  

# reset background
bg <- ''
