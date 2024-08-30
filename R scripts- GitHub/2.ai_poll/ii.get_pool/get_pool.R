message('STARTING ROUTINE: get_user_pool ...')

# load environment
source('R scripts/2.ai_poll/ii.get_pool/environment.R')

# search substantive terms
message('Beginning X API query based on substantive search terms ...')
for(l in qs){ 
  
  message('Calling X to search query: ',query[l])
  
  # search  
  search.json <- 
    robust_search(
      query[l],
      max_results_per_substantive_query, 
      l,
      paste('generated_data/tweets/', 'twitter_substantive_talk_', names(query)[l], '_', Sys.time(), '.RData', sep = '')
      )
  
  message('Search for query ',query[l],' complete !')
  message('Completed ',l,'/',length(qs),'.')
  
  # quick naps to ensure no weird botty behaviour gets blocked by API
  Sys.sleep(5)
}

if(USE_TRENDS){
  
  # get trends
  top_trends <- get_trends(url = trends_url)
  
  # set number of trends to sub-sample 
  n.trends <- length(top_trends)
  
  # sample up to trends at random 
  trends_sample <- sample(top_trends,size = n.trends,replace = FALSE)

  # clean trends for problematic characters
  trends_sample <- gsub(" & ",' \"and\" ',gsub(" and ",' \"and\" ',gsub('#','',gsub('\\$','',trends_sample))))

  # begin call
  message('Beginning X API query based on trending search terms ...')
  
  # search trends 
  for(i in 1:length(trends_sample)){
  
    message('Calling X to search query: ',trends_sample[i])
    
    search.json <- 
      robust_search(
        trends_sample[i],
        max_results_per_trend_query, 
        i,
        paste('generated_data/tweets/trends ',trends_sample[i],Sys.time(),'.RData',sep = '')
      )
    
    message('Search for query ',trends_sample[i],' complete !')
    message('Completed ',i,'/',length(trends_sample),'.')

    # quick naps to ensure no weird botty behaviour gets blocked by API
    Sys.sleep(5)
  } 
  }

message('ENDING ROUTINE: get_user_pool ...')

# # # CLEAN WORKING MEMORY AT SCRIPT END

# clean workspace except POOL_TYPE
rm(list = setdiff(ls(), "POOL_TYPE"))
# clean garbage
gc()