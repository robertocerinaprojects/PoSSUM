text <- "**title: IDEOLOGICALLY, THIS PERSON APPEARS TO BE:**\n**explanation: The user frequently criticizes Trump for not being conservative enough, indicating a strong preference for conservative values. This includes mentions of the Second Amendment, pro-life movement, and religious right.**\n**symbol: Ide6**\n**category: very ideologically conservative**\n**speculation: 20**\n\n**title: PAYING ATTENTION TO THE 2024 PRESIDENTIAL ELECTION:**\n**explanation: The user extensively discusses Trump's policies and their potential impact on the election, indicating a high level of engagement and awareness.**\n**symbol: Att4**\n**category: paying a lot of attention to the 2024 Presidential election in the US**\n**speculation: 10**\n\n**title: INTEREST IN POLITICS:**\n**explanation: The user is highly engaged in political discussions, frequently tweeting about political figures and issues, suggesting a high level of interest in politics.**\n**symbol: I4**\n**category: highly interested in politics**\n**speculation: 10**\n\n**title: ETHNICITY:**\n**explanation: Based on the profile image, making assumptions about ethnicity can be highly speculative and unreliable. There are no explicit or strong indicators in the provided data.**\n**symbol: E1**\n**category: white**\n**speculation: 70**\n\n**title: AGE:**\n**explanation: The user’s profile image suggests an age range that is most likely between 35 and 54, but this is speculative without additional context.**\n**symbol: A5**\n**category: 45 to 54 years old**\n**speculation: 60**\n\n**title: PAST VOTE - VOTE CHOICE 2020 US PRESIDENTIAL ELECTION VOTE:**\n**explanation: The user criticizes Trump but from a conservative standpoint, suggesting they likely voted for Trump in 2020 given their conservative values and disappointment in his subsequent actions.**\n**symbol: Vpa2**\n**category: voted for Donald Trump, the Republican Party candidate, in the 2020 election for President**\n**speculation: 40**\n\n**title: HIGHEST EDUCATIONAL QUALIFICATION:**\n**explanation: There is no direct information available regarding the user’s educational background. This assessment is highly speculative.**\n**symbol: Q1**\n**category: completed education up to and including high school**\n**speculation: 90**\n\n**title: SEX:**\n**explanation: Based on the profile image, the user appears to be male.**\n**symbol: S1**\n**category: masculine sex - male**\n**speculation: 10**\n\n**title: MARITAL STATUS:**\n**explanation: There is no direct information available regarding the user's marital status. This assessment is highly speculative.**\n**symbol: M2**\n**category: single**\n**speculation: 90**\n\n**title: PARTISAN LOYALTIES:**\n**explanation: Despite criticizing Trump, the user's focus on conservative issues suggests a strong alignment with Republican values.**\n**symbol: Pid7**\n**category: strongly identifies with Republicans**\n**speculation: 30**\n\n**title: GENERAL TRUST IN OTHER PEOPLE:**\n**explanation: The user’s language and interactions with others suggest a somewhat distrustful attitude, especially towards those with opposing views.**\n**symbol: Tru4**\n**category: some of the time trust other people**\n**speculation: 50**\n\n**title: HOUSEHOLD INCOME BRACKET:**\n**explanation: There is no information provided that directly indicates the user's income bracket. This assessment is highly speculative.**\n**symbol: H2**\n**category: between 25000 and 50000 USD per year**\n**speculation: 90**\n\n**title: PAST VOTE - TURNOUT IN THE 2020 PRESIDENTIAL ELECTION:**\n**explanation: Given the user's strong political opinions and engagement, it is highly likely they turned out to vote in the 2020 election.**\n**symbol: Tpa7**\n**category: certain this individual turned out to vote - Probability: 1 - in the 2020 election for President**\n**speculation: 10**"

# Function to turn feature character vector into useful table for parsing
parse_features <- function(features) {
  parsed_list <- list()
  
  for (feature in features) {
    # Split by new line to separate the header and the values
    lines <- unlist(strsplit(feature, "\n"))
    
    # Extract the header (first line) and remove the colon
    header <- gsub(":", "", lines[1])
    
    # Extract the codes and descriptions
    codes <- gsub("^\\s+|\\s+$", "", gsub("\\).*", "", lines[-1]))
    descriptions <- gsub("^\\s+|\\s+$", "", gsub(".*\\)\\s*", "", lines[-1]))
    
    # Remove empty entries (in case there are blank lines)
    codes <- codes[codes != ""]
    descriptions <- descriptions[descriptions != ""]
    
    # Create a data.table
    parsed_list[[header]] <- data.table::data.table(
      do.call(rbind, list(codes, descriptions))
    )
  }
  
  return(parsed_list)
}

# extract common prefix
longest_common_substring <- function(strings) {
  if (length(strings) == 0) return("")
  
  # Function to find LCS between two strings
  lcs_two_strings <- function(str1, str2) {
    m <- nchar(str1)
    n <- nchar(str2)
    result <- ""
    lengths <- matrix(0, m + 1, n + 1)
    
    longest_length <- 0
    ending_index_str1 <- 0
    
    for (i in seq_len(m)) {
      for (j in seq_len(n)) {
        if (substring(str1, i, i) == substring(str2, j, j)) {
          lengths[i + 1, j + 1] <- lengths[i, j] + 1
          if (lengths[i + 1, j + 1] > longest_length) {
            longest_length <- lengths[i + 1, j + 1]
            ending_index_str1 <- i + 1
          }
        }
      }
    }
    
    if (longest_length > 0) {
      result <- substring(str1, ending_index_str1 - longest_length, ending_index_str1 - 1)
    }
    
    return(result)
  }
  
  # Initialize with the first string
  common_substring <- strings[1]
  
  # Find the common substring with each subsequent string
  for (str in strings[-1]) {
    common_substring <- lcs_two_strings(common_substring, str)
    if (common_substring == "") break
  }
  
  return(common_substring)
}

# Function to parse text and convert to data.table
parse_text_to_datatable <- 
  function(
    text, 
    reference_table, 
    label, 
    drop.na=FALSE, 
    speculation=TRUE,
    debug = FALSE) {

  features_object <- data.table()

  # remove stars - they're not necessary
  text <- gsub('\\*','',text)
  
  # if explanation line is there, drop it ro ease parsing and reduce the chance
  # of text in the exp. being contraddictory with the label
  temp <- strsplit(text,split = '\n')
  if(sum(grepl('explanation',tolower(temp[[1]])))>0 ){
    text <- paste0(temp[[1]][!grepl('explanation',tolower(temp[[1]]))],collapse = '\n')
  }
  
  # visualise the resulting text (useful for debug)
  cat(text)

  # for each feature

  for(f in names(reference_table)){

    # check if the symbol is in the text
    symbols_choice <-c()
    for(s in unlist(reference_table[[f]][1])){ 
      
      if(grepl(s,text)){ 
        symbols_choice <- 
        c(
          symbols_choice,  
          unlist(reference_table[[f]][2])[match(s,unlist(reference_table[[f]][1]))]  
        ) } } 
    if(debug){
      cat(paste0('\nsymbols choice:',symbols_choice,'\n'))
    }
    
    # check if the value is in the text
    values_choice <-c()
    for(v in unlist(reference_table[[f]][2])){ 
      if(grepl(v,text)){ values_choice <- c(values_choice,v)}
    } 
    if(debug){
      cat(paste0('\nvalues choice:',values_choice,'\n'))
    }
    # if no match was found for either symbols or values, return a missing value
    if(length(c(values_choice,symbols_choice))==0){
      values_choice <- NA
    }
    

    if(length(unique(c(values_choice,symbols_choice)))>1){
      stop('BIG PROBLEM WITH PARSER, INCOHERENT SYMBOL-VALUE MATCH -- EXAMINE LLM OUTPUT AND TEST WHAT IS GOING WRONG')
    }
    
    # now if for whatever reasons there is uncertainty, such as multiples
    # symbols matched or multiple values, pick at random amongst these
    choice <- sample(c(values_choice,symbols_choice),size = 1)
    cat(paste0('\ninferred demo: ',choice,'...\n'))
    
    # store choice in object
    features_object$temp <- choice
    names(features_object)[names(features_object)=='temp'] <- paste0('gpt_',label,'_',f)
  }
  
  if(speculation){
  # now let's extract speculation
  feature_blocks <- strsplit(text, "\n\n")[[1]]

  for(f in names(reference_table)){
    
    if(any(grepl(f,feature_blocks))){
      block <- grep(f,feature_blocks,value = T)
    }else{
      f_star <- 
      paste0(
        longest_common_substring(unlist(reference_table[[f]][1])),
        "[0-9]{1}"
        )
      if(any(grepl(f_star,feature_blocks))){
        block <- grep(f_star,feature_blocks,value = T)
      }else{
        f_star <- 
          paste0(
            longest_common_substring(unlist(reference_table[[f]][1])),
            "[0-9]{2}"
          )
        block <- grep(f_star,feature_blocks,value = T)
      }
    }

    speculation <- as.numeric(sub(".*[sS]peculat(ion|ive):? ([0-9]+).*", "\\2", block))
    
    # if no match was found for either symbols or values, return a missing value
    if(length(speculation)==0){
      speculation <- NA
      warning('**MISSING SPECULATION VALUE FOR',f,'**')
    }
    
    # now if for whatever reasons there is uncertainty, such as multiple
    # speculation values, pick max
    speculation <- max(as.numeric(as.character(unlist(speculation))))
    
    # store speculation in object
    features_object$temp <- speculation
    names(features_object)[names(features_object)=='temp'] <- paste0('gpt_speculation_',label,'_',f)
  }
  }
  
  
  if(drop.na){
    nona.cols <- which(!is.na(unlist(features_object)))
    features_object <- features_object[,..nona.cols]
  }
  
  return(features_object)
  }








parse_district_number <- function(text) {
  # Regular expression to match state and district
  match <- regexec("([A-Za-z\\s]+)\\s(\\d+(?:th|st|nd|rd)|at large)", text)
  matches <- regmatches(text, match)[[1]]
  
  if (length(matches) > 0) {
    district <- matches[3]
    if (district == "at large") {
      return("0")
    } else {
      return(gsub("[^0-9]", "", district))
    }
  } else {
    return("Invalid format")
  }
}
