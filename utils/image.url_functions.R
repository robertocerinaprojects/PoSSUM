# utils 
library(imguR)
library(pdftools)
library(magick)
library(jsonlite)
library(httr)
library(plumber)

# # # First login to the API -- only have to do this once at the beginning of the session. 

get_imgur.auth <- 
function(client_id,client_secret){

  # Step 1: Get the authorization URL
  authorize_url <- "https://api.imgur.com/oauth2/authorize"
  params <- list(client_id = client_id, response_type = "code")
  authorize_link <- modify_url(authorize_url, query = params)
  
  # Print the authorization URL and ask the user to open it in the browser
  cat("Please go to the following URL and authorize the application:\n", authorize_link, "\n")
  
  # Step 2: After the user authorizes, they will be redirected to a URL with a code parameter.
  # Prompt the user to enter this code
  auth_code <- readline(prompt = "Enter the code from the URL after authorization: ")
  
  return(auth_code)
}

get_imgur.token <- 
  function(client_id,client_secret,auth_code){
    # Step 3: Exchange the authorization code for an access token
    token_url <- "https://api.imgur.com/oauth2/token"
    body <- list(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "authorization_code",
      code = auth_code
      )
    
    response <- POST(token_url, body = body, encode = "form")
    token <- content(response, as = "parsed", type = "application/json")
    
    return(token$access_token)
    
  }
    
# Function to upload an image to Imgur
upload_image <- function(image_path, access_token, verbose = FALSE) {
  imgur_upload_url <- "https://api.imgur.com/3/image" # URL for the Imgur upload endpoint
  max_retries <- 100 # Maximum number of retries
  retry_delay <- 90 # Delay between retries in seconds
  attempts <- 0 # Initialize the attempt counter
  
  repeat { # Start an infinite loop
    # Make a POST request to the Imgur API to upload the image
    response <- tryCatch({
      httr::POST(
        url = imgur_upload_url,
        httr::add_headers(Authorization = paste("Bearer", access_token)), # Add authorization header with the access token
        body = list(image = httr::upload_file(image_path)), # Attach the image file to the request
        encode = "multipart" # Specify that the request body is multipart/form-data
      )
    }, error = function(e) {
      # Catch the specific curl error
      if (grepl("HTTP/2 stream 0 was not closed cleanly: STREAM_CLOSED", e$message)) {
        attempts <<- attempts + 1 # Increment the attempt counter
        if (attempts >= max_retries) {
          # Return a non-recoverable error after maximum retries
          return(list(
            status = NULL,
            success = FALSE,
            error = "Non-recoverable error after maximum retries: HTTP/2 stream was not closed cleanly."
          ))
        }
        Sys.sleep(retry_delay) # Wait for the specified delay before retrying
        return(NULL) # Return NULL to retry the request
      } else {
        stop(e) # Rethrow other errors
      }
    })
    
    if (is.null(response)) next # Retry if response is NULL due to the caught error
    
    # Check if the response status code indicates success (2xx)
    if (response$status_code >= 200 && response$status_code < 300) {
      # Try to parse the response content as JSON
      result <- tryCatch({
        content(response, as = "parsed", type = "application/json")
      }, error = function(e) {
        return(NULL) # Return NULL if parsing fails
      })
      
      # If the result is not NULL and indicates success, return the result
      if (!is.null(result) && result$success) {
        return(result) # Exit the loop and return the successful result
      } else {
        if (verbose) {
          print(content(response, "text")) # Print the raw response text if verbose is TRUE
        }
        # Return an error list if the result indicates failure
        return(list(
          status = response$status_code,
          success = FALSE,
          error = result
        ))
      }
    } else {
      if (verbose) {
        print(content(response, "text")) # Print the raw response text if verbose is TRUE
      }
    }
    
    # Try to get the raw response content as text
    content_text <- tryCatch({
      content(response, "text")
    }, error = function(e) {
      return("") # Return an empty string if getting the content fails
    })
    
    # Check if the response contains a lexical error
    if (grepl("lexical error: invalid char in json text", content_text)) {
      attempts <- attempts + 1 # Increment the attempt counter
      if (attempts >= max_retries) {
        # Return a non-recoverable error after maximum retries
        return(list(
          status = response$status_code,
          success = FALSE,
          error = "Non-recoverable error after maximum retries."
        ))
      }
      Sys.sleep(retry_delay) # Wait for the specified delay before retrying
    } else {
      break # Exit the loop if there is no lexical error
    }
  }
  
  # Return a general failure if the loop exits without success
  return(list(
    status = response$status_code,
    success = FALSE,
    error = "Upload failed without recoverable error."
  ))
}
# robust download function
download_with_retry <- function(input.url, pdf.path, max_retries = 100) {
  retry_count <- 0
  success <- FALSE
  error_message <- NULL
  
  while (retry_count < max_retries && !success) {
    tryCatch({
      download.file(
        url = input.url, 
        destfile = pdf.path, 
        mode = "wb"
      )
      success <- TRUE
    }, error = function(e) {
      retry_count <- retry_count + 1
      error_message <- conditionMessage(e)
      if (retry_count == max_retries) {
        message("An unrecoverable error has occurred after ", max_retries, " attempts.")
        message("Error message: ", error_message)
      } else {
        message("Download failed, retrying... (attempt ", retry_count, " of ", max_retries, ")")
        message("Error message: ", error_message)
      }
    })
  }
  
  if (success) {
    return(pdf.path)
  } else {
    stop("Failed to download the file after ", max_retries, " attempts. Error message: ", error_message)
  }
}

# function which first converts a pdf to another image file, then uploads it 
pdf_to_url <- 
  function(
    input.url = NULL,
    pdf.path,
    # pdf arguments 
    page = 1, 
    dpi = 300,
    # image manipulation
    resize.factor = 1/3,
    format = 'png'
  ){
    
    # download the pdf
    if(!is.null(input.url)){
      
      message("Downloading pdf from: ",input.url)
      
      download_with_retry(input.url, pdf.path)

    }
    
    message("Rendering pdf from: ",pdf.path)
    
    # render the first (and only) page of the PDF
    pdf_page <- pdf_render_page(pdf.path, page = page, dpi = dpi)
    
    # convert the rendered page to an image object
    image <- image_read(pdf_page)
    
    message("Re-sizing image...")
    
    # resize the object to reduce storage
    info <- image_info(image)
    original_width <- info$width
    original_height <- info$height
    aspect_ratio <- original_width / original_height
    new_width <- original_width*resize.factor
    new_height <- round(new_width / aspect_ratio)
    
    image <- image_resize(image, paste0(new_width, "x", new_height))
    
    message("Saving ",format," to: ",gsub('pdf',format,pdf.path))
    
    # save the image in the new format in the same path
    image_write(
      image, 
      path = gsub('pdf',format,pdf.path), 
      format = format
    )
    
    message("Beginning attempts at image upload on Imgur...")
    
    # upload image
    wait_time <- 90
    retries <- 100
    attempt <- 0
    success <- FALSE
    
    while(attempt <= retries && !success) {
      attempt <- attempt + 1
      message("Attempt round: ", attempt)
      
      imguR_upload <- upload_image(image_path = gsub('pdf', format, pdf.path),access_token = access_token)
      success <- ifelse(length(imguR_upload$success)==1,imguR_upload$success,FALSE)
      
      if(success){
        message("Upload successful on attempt ", attempt)
      }else{
        # Handle error and retry after waiting
        message("Error on attempt ", attempt, ": ", imguR_upload$error)
        if (attempt < retries) {
          message("Waiting for ", wait_time, " seconds before retrying...")
          Sys.sleep(wait_time)
        }
      }
    }
    
    if (!success) {
      stop("Failed to upload the image after ", retries, " attempts")
    }
    
    message("\n\n\n")

    # return link object
    return(imguR_upload )
  }
