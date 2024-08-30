# use trends button
USE_TRENDS <- TRUE

# define url from where to parse the latest trends from 
trends_url <- "https://trends24.in/united-states/"

# max results per trend
max_results_per_trend_query <- 40

# define terms for search query on X api
#query <-
#  "(Biden OR POTUS OR JoeBiden OR MAGA OR Trump OR realDonaldTrump OR Robert Kennedy OR RFK OR RobertKennedyJr OR RFKJr OR KennedyShanahan24 OR Kennedy24 OR Cornel West OR Dr. West OR CornelWest OR ChaseForLiberty OR Jill Stein OR DrJillStein) -from:JoeBiden -from:realDonaldTrump -from:RobertKennedyJr -from:CornelWest -from:ChaseForLiberty -from:DrJillStein"

query <-
  "(Kamala OR VP OR KamalaHarris OR MAGA OR Trump OR realDonaldTrump OR Robert Kennedy OR RFK OR RobertKennedyJr OR RFKJr OR KennedyShanahan24 OR Kennedy24 OR Cornel West OR Dr. West OR CornelWest OR ChaseForLiberty OR Jill Stein OR DrJillStein) -from:VP -from:KamalaHarris -from:realDonaldTrump -from:RobertKennedyJr -from:CornelWest -from:ChaseForLiberty -from:DrJillStein"


# define the number of queries we wish to interrogate on any given sampling effort
n.query <- length(query)

# subsample the ids of the queries if they are too many -- 
# this is project specific and needs to be tuned
qs <- 1:n.query 

# define max results per substantive query
max_results_per_substantive_query <- 2000


