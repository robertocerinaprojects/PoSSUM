# # # IMPLEMENT TEMPORAL INCLUSION CRITERIA



# load object from relevant path
load(file = path.tweets[j])



# if no users could be collected for a given api search, move on
if(length(pool$users)<=0){
  message(
    'No valid users for query in path: ',
    path.tweets[j],
    '.
Moving to next path ...'
    )
  next
  }

# get the path of files listing the users who have already been polled 
user.files <- get_files_from_dir(dir = 'generated_data/users')

# if we have an existing record of users polled for this fieldwork
if(length(user.files)>0){
  
  message(
    'Loading previously polled users...'
  )
  # initialise an empty table to temporarily store the full list of users
  user.list <- data.table()
  # loop through the user files and stack-them
  for(i in  user.files){
    load(file = i)
    users$date <- as.Date(substr(users$date,1,10),"%Y-%m-%d")
    user.list <- rbindlist(list(user.list,users))
  }
  
  message(
    'Identifying users who have already been polled within the temporal inclusion time-frame ...'
  )
  # assign a `recently seen` label to each user
  user.list$recent <- ifelse(user.list$date>=(Sys.Date()-tau),TRUE,FALSE) 
  
  # identify users to banish given recency 
  forbidden.users <- unique(user.list$user[user.list$recent])
  
  # banish the users, if there are any users to banish
  if(length(forbidden.users)>0){
    message(
      'Banishing alreay-polled users...'
    )
    pool$users <- pool$users[!unlist(pool$users$id) %in% forbidden.users,]
  } }else{
    message(
      'There is no user-file to draw upon.
This must be the first set of users we are observing from the pool.
Every user meets the temporal inclusion criteria by default.'
    )
  }

# if no users remain eligible, move on 
if(dim(pool$users)[1]<=0){
  message(
    'No valid users from path: ',
    path.tweets[j],
    ', remains eligible for polling after temporal inclusion criteria.
Moving to next path...'
  )
  next
  }  

# for each user extract latest observation date 
last_observed_user <- 
sapply(
  unlist(pool$users$id),
  function(id){
    max(
      unlist(pool$tweets[pool$tweets$author_id==id,]$created_at)
    )
  })

# store unique users that are to be processed from this jth pool-object
users <- 
  data.table(
    user = names(last_observed_user),
    date = last_observed_user
      )

# store number of surviving users
n.inc.users <- dim(pool$users)[1]

# save this table to keep count of users that we are actually going to poll, 
# so we can exclude them in later surveys via temporal exclusion criteria
message(
  'Updating database of polled users ...'
)
save(
  users,
  file = paste0(
    'generated_data/users/',
    gsub('generated_data/tweets/','',path.tweets[j])
    ),
  compress = TRUE
  )
message(
  'Database of polled users updated !'
)