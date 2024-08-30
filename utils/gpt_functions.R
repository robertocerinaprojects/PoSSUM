library(tools)
library(R.utils)

call_gpt <- 
  function(
    messages.list,
    model = "gpt-4o",
    temperature = 0,
    timeout = 120,
    max_tokens = 4096
  ){
    
    gen.function <- 
      function(messages.list){
        
        message(
          "calling ", model,
          "\ntemperature: ", temperature,
          '\n',
          paste0(
            paste0(sapply(messages.list[[1]]$content, function(x){x[1]}), ':'),
            paste0(sapply(messages.list[[1]]$content, function(x){x[2]}), '\n'),
            sep = '\n'
          )
        )
        x=
        openai::create_chat_completion(
          model = model,
          temperature = temperature,
          messages = messages.list,
          max_tokens = max_tokens
        ) 
        
      }
    
    gen <- 
      tryCatch(
        { 
          withTimeout( 
            {
              gc();  
              gen.function(messages.list)
            }, 
            timeout = timeout
          ) 
        },
        error = function(e) {
          message("Error during initial call: ", e$message)
          e
        }
      )
    
    # if it starts with I'm sorry, count this as an error
    if (is.list(gen) && !is.null(gen$choices)) {
      content <- gen$choices$message.content
      if (length(content) > 0 && grepl(pattern = "I'm sorry", content)) {
        gen <- structure(list(message = "Apology detected in response"), class = "error")
      }
    }
    
    # if image fails, re-run without image
    if(inherits(gen, "error") & any(grepl('Invalid image.', unlist(gen)))){
      
      message('\nTHE IMAGE FOR THIS USER IS INVALID -- PROMPT FALLING BACK TO TEXT-ONLY...\n')
      
      messages.list[[1]]$content <- list(messages.list[[1]]$content[[1]])
      
      gen <-
        tryCatch(
          { 
            withTimeout( 
              {
                gc();
                gen.function(messages.list)
              }, 
              timeout = timeout
            ) 
          },
          error = function(e) {
            message("Error during fallback call: ", e$message)
            e
          }
        )
      
    }
    
    while( inherits(gen, "error") ){
      message('\nGPT CALL FAILED: ',toupper(gen$message),'-- TRYING AGAIN...\n')
      
      gen <- 
        tryCatch(
          { 
            withTimeout( 
              {
                gc();  
                gen.function(messages.list)
              }, 
              timeout = timeout
            ) 
          },
          error = function(e) {
            message("Error during retry: ", e$message)
            e
          }
        )
    }
    
    return(gen)
    
  }

prep_gpt_messages <- 
  function(){
    # prepare default prompt 
      list(
        list(
          "role" = "user",
          "content" = 
            list(
              list(
                "type" = "text", 
                "text" = ''
              ) ) ) )
  }

prep_gpt_image <-
  function(image_url,X_size = NULL){
    
    # change size of X profile pic
    if(!is.null(X_size)){
      message('Resized X image ...')
      image_url <- gsub(X_size,'',image_url)
    }
    
    # check if the extension is one of the supported formats
    if (tools::file_ext(image_url) %in% c("png",'jpg',"jpeg", "gif", "webp")) {
      message('Image file extension is supported. Finalising image object !')
      image_list <- 
        list('type' = "image_url","image_url" = list("url" = image_url))
    }else{
      # if extension not supported, prompt the LLM to use the other info.
      message('Image file extension is NOT supported. Including "No-image" text in prompt object.')
      image_list <-
        list(
          'type' = "text",
          'text' = 'A profile picture is not available for this user.'
        )
    }
    return(image_list)
    
  }

append_gpt_content <- 
  function(gpt_messages,new_content){
    gpt_messages[[1]]$content[[length(gpt_messages[[1]]$content)+1]] <- 
      new_content
    
    return(gpt_messages)
  }
