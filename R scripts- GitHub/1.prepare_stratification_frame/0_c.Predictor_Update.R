# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")

# utils
library(data.table)
library(readxl)
library(questionr)
library(zoo)

# to give ordinal suffix to district names 
ordinal_suffix_vectorized <- function(numbers) {
  # Extract last two digits
  last_digits <- numbers %% 10
  second_last_digits <- (numbers %% 100) %/% 10
  
  # Default suffix is "th"
  suffixes <- rep("th", length(numbers))
  
  # Assign "st", "nd", "rd" based on conditions
  suffixes[second_last_digits != 1 & last_digits == 1] <- "st"
  suffixes[second_last_digits != 1 & last_digits == 2] <- "nd"
  suffixes[second_last_digits != 1 & last_digits == 3] <- "rd"
  
  # Combine numbers and suffixes
  ordinals <- paste0(numbers, suffixes)
  
  return(ordinals)
}

# # # # This code will gather data and generate a state-level predictor. 
# The geographic level is actually the electoral-college level, meaning 
# Nebraska and Maine are actually represented by their constituent 
# districts. 

# # # 
# # # (1) 2020 ELECTION RESULTS
# # #

# (1-a) 
# MIT Election Data and Science Lab, 2017, 
# "U.S. President 1976–2020", 
# https://doi.org/10.7910/DVN/42MVDX, 
# Harvard Dataverse, V7
vote2020.state <- 
  fread(file = 'auxiliary_data/MIT Election Data and Science Lab/U.s President State-Level Returns 1976-2020/1976-2020-president.csv')

# we are interested in 2020 
vote2020.state <- vote2020.state[year == 2020]

# relevant candidates only
vote2020.state$candidate_simple <- 
  ifelse(
    vote2020.state$candidate %in% 
      c(
        "TRUMP, DONALD J.",
        "BIDEN, JOSEPH R. JR",
        "JORGENSEN, JO",
        "HAWKINS, HOWIE",
        "LA RIVA, GLORIA ESTELLA" # keep this in for Conrel West Predictions
      ),
    vote2020.state$candidate,'OTHER'
  )

# change vote names
names(vote2020.state)[names(vote2020.state)=='candidatevotes'] <- 'votes'

# aggregate at the state level 
vote2020.state <- 
  vote2020.state[,
                 lapply(.SD,sum),
                 by = c('candidate_simple','state'),
                 .SDcols = c('votes')]
# indicate 2020 vote 
names(vote2020.state) <- gsub('votes','2020.votes',names(vote2020.state) )

# change candidate name to party 
vote2020.state$party <- 
  ifelse(vote2020.state$candidate_simple=='TRUMP, DONALD J.','R',
         ifelse(vote2020.state$candidate_simple=='BIDEN, JOSEPH R. JR','D',
                ifelse(vote2020.state$candidate_simple=='HAWKINS, HOWIE','G',
                       ifelse(vote2020.state$candidate_simple=='JORGENSEN, JO','L',
                              ifelse(vote2020.state$candidate_simple=='LA RIVA, GLORIA ESTELLA','PSL',
                                     'OTHER') ) ) ) )

# drop candidate column
vote2020.state <- 
  vote2020.state[,!'candidate_simple']

# wide format to improve compatibility with merging 
vote2020.state <- 
  reshape(
    vote2020.state,
    direction = 'wide',
    timevar = 'party',
    idvar = c('state')
    )

# turn missing values for vote variables into 0s 
# (the candidate did not run there)
party.cols <- names(vote2020.state)[grepl('2020.votes',names(vote2020.state))]
for(i in party.cols){
  if(any(is.na(vote2020.state[[i]]))){
    vote2020.state[[i]] <- 
      ifelse(
        is.na(vote2020.state[[i]]),
        0,vote2020.state[[i]]
        )
  }
}

# (1-b) Use precinct level returns to get data on Nebraska and Maine districts
# MIT Election Data and Science Lab, 2022, 
# "U.S. President Precinct-Level Returns 2020", 
# https://doi.org/10.7910/DVN/JXPREB,
# Harvard Dataverse, V4
vote2020.precinct <- 
  fread(file = 'auxiliary_data/MIT Election Data and Science Lab/U.S. President Precinct-Level Returns 2020/PRESIDENT_precinct_general.csv')

# we are interested in 2020 
vote2020.precinct[year=='2020']

# relevant candidates
vote2020.precinct$candidate_simple <- 
  ifelse(
    vote2020.precinct$candidate %in% 
      c(
        "DONALD J TRUMP",
        "JOSEPH R BIDEN",
        "JO JORGENSEN",
        "HOWIE HAWKINS",
        "GLORIA LA RIVA" # keep this in for Conrel West Predictions
      ),
    vote2020.precinct$candidate,'OTHER'
  )

# apply elecotoral college logic to the districts
vote2020.precinct$district[
  !vote2020.precinct$state %in% c('MAINE','NEBRASKA')
  ] = 'STATEWIDE'

# focus on `total` mode - absentee ballots etc. are not of primary interest. 
vote2020.precinct <- vote2020.precinct[mode=='TOTAL']

# ignore the `statistical adjustments` - they can lead to weird negative votes
# if simply summed...
vote2020.precinct <- 
  vote2020.precinct[jurisdiction_name!='{STATISTICAL ADJUSTMENTS}']

# negative votes indivate missing values - for now set them to 0
vote2020.precinct$votes[vote2020.precinct$votes<0] <- 0

# sum over the states 
vote2020.precinct.state <- 
  vote2020.precinct[,
                    lapply(.SD,sum),
                    by = c('candidate_simple','state','district'),
                    .SDcols = c('votes')]

# change nomenclature to match previous state-level dataset
names(vote2020.precinct.state) <- gsub('votes','2020.votes',names(vote2020.precinct.state) )

# change candidate names to parties
vote2020.precinct.state$party <- 
  ifelse(vote2020.precinct.state$candidate_simple=='DONALD J TRUMP','R',
         ifelse(vote2020.precinct.state$candidate_simple=='JOSEPH R BIDEN','D',
                ifelse(vote2020.precinct.state$candidate_simple=='HOWIE HAWKINS','G',
                       ifelse(vote2020.precinct.state$candidate_simple=='JO JORGENSEN','L',
                              ifelse(vote2020.precinct.state$candidate_simple=='GLORIA LA RIVA','PSL',
                                     'OTHER') ) ) ) )

# drop candidate names 
vote2020.precinct.state <- vote2020.precinct.state[,!'candidate_simple']

# wide format to improve compatibility with merging 
vote2020.precinct.state <- 
  reshape(
    vote2020.precinct.state,
    direction = 'wide',
    timevar = 'party',
    idvar = c('state','district')
    )

# turn missing values for vote variables into 0s 
# (the candidate did not run there)
party.cols <- names(vote2020.precinct.state)[
  grepl('2020.votes',names(vote2020.precinct.state))
  ]
for(i in party.cols){
  if(any(is.na(vote2020.precinct.state[[i]]))){
    vote2020.precinct.state[[i]] <- 
      ifelse(
        is.na(vote2020.precinct.state[[i]]),
        0,
        vote2020.precinct.state[[i]]
        )
  }
}

# Get Nebraska and Maine only
vote2020.precinct.state <- 
  vote2020.precinct.state[state %in% c('NEBRASKA','MAINE')]

# rename the districts to match the nomenclature used also in the SD
vote2020.precinct.state$state <- 
  paste(
    gsub(
      "\\b([a-z])", 
      "\\U\\1", 
      tolower(
        vote2020.precinct.state$state
        ), 
      perl=TRUE
      ),
    ordinal_suffix_vectorized(
      as.numeric(
        substr(vote2020.precinct.state$district,2,3)
        )
      )
  )

# drop old nomenclature
vote2020.precinct.state <- 
  vote2020.precinct.state[,!'district']

# now swap these values for Nebraska and Maine into the state-level dataset

# drop Nebraska and Maine as states
vote2020.state <- vote2020.state[!state %in% c('MAINE','NEBRASKA')]

# change naming convention to match 
vote2020.state$state <- tolower(vote2020.state$state)  # Convert the entire string to lowercase
vote2020.state$state <- gsub("\\b([a-z])", "\\U\\1", vote2020.state$state, perl=TRUE)  

# append Nebraska and Maine districts to state-level data
vote2020.state <- rbindlist(list(vote2020.state,vote2020.precinct.state),use.names=TRUE)

# rename geo id to electoral college undrstanding
names(vote2020.state)[names(vote2020.state)=="state"] <- 'state_electoral.college'

# rename DC to match general nomenclature
vote2020.state$state_electoral.college[
  vote2020.state$state_electoral.college=='District Of Columbia'
  ] <- 'District of Columbia'

# calculate total number of votes 
vote2020.state$`2020.votes.T` <- rowSums(vote2020.state[,!'state_electoral.college'])

tmp.running <- vote2020.state[,lapply(.SD,function(x){ifelse(x==0,0,1)}),by = c('state_electoral.college')]
running <- c(TRUE,as.vector(unlist(tmp.running[,!'state_electoral.college'][,lapply(.SD,function(x){any(var(x)>0)})])))
tmp.running <- tmp.running[,..running]
names(tmp.running) <- gsub('2020.votes','2020.run',names(tmp.running))
vote2020.state <- merge(vote2020.state,tmp.running,by = c('state_electoral.college'))

# (1-c) GET 2022 CONGRESSIONAL DISTRCIT ELECTION RESULTS AT THE ELECTORAL COLLEGE LEVEL
# MIT Election Data and Science Lab, 2017, 
# "U.S. House 1976–2022", 
# https://doi.org/10.7910/DVN/42MVDX, 
# Harvard Dataverse, V7
vote2022.state <- 
  fread(file = 'auxiliary_data/MIT Election Data and Science Lab/U.S House District-Level Returns 1976-2022/1976-2022-house.csv')

vote2022.state <- vote2022.state[year == 2022]

vote2022.state[state == 'NORTH DAKOTA' & party == 'INDEPENDENT']$party <- 'DEMOCRAT'

vote2022.state$party <- 
  ifelse(vote2022.state$party == 'DEMOCRAT', 'D',
         ifelse(vote2022.state$party == 'REPUBLICAN','R',
                ifelse(vote2022.state$party == 'LIBERTARIAN','L',
                       ifelse(vote2022.state$party == 'GREEN','G',
                              'OTHER') ) ) )

# fix maine ranked choice voting in district 2
# https://ballotpedia.org/Maine%27s_2nd_Congressional_District_election,_2022
vote2022.state[grepl('MAINE',state) & district == 2]$totalvotes <- 316382
vote2022.state[grepl('MAINE',state) & district == 2 & candidate == 'BRUCE POLIQUIN']$candidatevotes <- 141260
vote2022.state[grepl('MAINE',state) & district == 2 & candidate == 'JARED F GOLDEN']$candidatevotes <- 153074
vote2022.state[grepl('MAINE',state) & district == 2 & candidate == 'TIFFANY BOND']$candidatevotes <- 21655
vote2022.state[grepl('MAINE',state) & district == 2 & candidate == 'WRITEIN']$candidatevotes <- 393
vote2022.state <- vote2022.state[-which(candidate == 'EXHAUSTED BALLOT' & state == 'MAINE')]

vote2022.state$state_electoral.college <- 
  gsub(
    "\\b([a-z])", 
    "\\U\\1", 
    tolower(
      vote2022.state$state
    ), 
    perl=TRUE
  )

vote2022.state$state_electoral.college <-  
  ifelse(
    vote2022.state$state %in% c("NEBRASKA",'MAINE'),
  paste(
    vote2022.state$state_electoral.college,
    ordinal_suffix_vectorized(
      as.numeric(
        vote2022.state$district
      ) ) ),
  vote2022.state$state_electoral.college)


vote2022.state <- 
  vote2022.state[,lapply(.SD,sum,na.rm=T),
                 by = c('state_electoral.college','party'),
                 .SDcols = c('candidatevotes')]

vote2022.state <- 
  rbindlist(
    list(
      vote2022.state,
      data.table(
        state_electoral.college = 'District of Columbia',
        party = c('R','D','G','L','OTHER'),
        candidatevotes = c(11699,174212,9867,4003,1521))
      ) )

names(vote2022.state)[names(vote2022.state)=='candidatevotes'] <- '2022.votes'
vote2022.state <- 
  reshape(
    vote2022.state,
    direction = 'wide',
    idvar = 'state_electoral.college',
    timevar = 'party'
    )

vote2022.state [grepl('Maine',state_electoral.college)]

for(j in c('R','D','L','G','OTHER')){
  vote2022.state[[paste0('2022.votes.',j)]] <- 
    ifelse(is.na(vote2022.state[[paste0('2022.votes.',j)]]),0,
           vote2022.state[[paste0('2022.votes.',j)]])
}

# calculate total number of votes 
vote2022.state$`2022.votes.T` <- rowSums(vote2022.state[,!'state_electoral.college'])

tmp.running <- vote2022.state[,lapply(.SD,function(x){ifelse(x==0,0,1)}),by = c('state_electoral.college')]
running <- c(TRUE,as.vector(unlist(tmp.running[,!'state_electoral.college'][,lapply(.SD,function(x){any(var(x)>0)})])))
tmp.running <- tmp.running[,..running]
names(tmp.running) <- gsub('2022.votes','2022.run',names(tmp.running))
vote2022.state <- merge(vote2022.state,tmp.running,by = c('state_electoral.college'))

# # # 
# # # (2) 2020 POPULATION SIZE 
# # #

# get total population from daily kos congressional district data 
pop.cd <- 
  fread(input = 'auxiliary_data/Daily Kos/Daily Kos Elections 2010-2020 population change for various jurisdictions - Congressional Districts.csv',header = TRUE,skip = 1)

# make 2020 values into numbers 
pop.cd[['2020']] <- as.numeric(gsub(',','',pop.cd [['2020']]))

# drop everything else 
pop.cd <- pop.cd[,c('District','2020')]

# add DC (even if it is not a congressional district)
pop.cd <- rbindlist(list(pop.cd,list(District = 'DC-AL',`2020` = 712186))) # google trends

# change districts into electoral college ID 
pop.cd$state_abbreviation <- substr(pop.cd$District,1,2)

pop.cd$state_simple <- 
  usmap::fips_info()$full[match(pop.cd$state_abbreviation,usmap::fips_info()$abbr)]

pop.cd$state_electoral.college <- 
  ifelse(
    pop.cd$state_simple %in% c('Nebraska','Maine'),
    paste(
      pop.cd$state_simple,
      ordinal_suffix_vectorized(as.numeric(substr(pop.cd$District,4,5)))
    ),
    pop.cd$state_simple
  )

# change name to provide useful identifier of the variable 
names(pop.cd)[names(pop.cd)=='2020'] <- 'N.pop'

# assign full state name 
pop.cd$state_simple <- 
usmap::fips_info()$full[
  match(
    substr(pop.cd$District,1,2),
    usmap::fips_info()$abbr
  )]

# assign full cd name 
pop.cd$congressional.district_simple <- 
  paste(
    usmap::fips_info()$full[
      match(
        substr(pop.cd$District,1,2),
        usmap::fips_info()$abbr
      )],
    ifelse(
      grepl('AL',substr(pop.cd$District,4,5)),
      'at-large',
      ordinal_suffix_vectorized(as.numeric(substr(pop.cd$District,4,5)))
    )
  )

# drop old naming 
pop.cd <- pop.cd[,!'District']

# # # 
# # # (3) STRATIFICATION FRAME MARGINALS 
# # #

# load stratification frame 
load(file = 'generated_data/stratification_frame/SF.RData')

# define variables of interest
var.names <- 
  names(SF)[
    names(SF) %in% c(
      'region',
      "gender",
      "ethnicity",
      "age_bins",
      "marital_status",
      "college_grad",
      "hh_income",
      "internet"
    )]

# for the electoral college geographies
SF.margins_electoral.college <- list()
for(i in var.names){
  # calculate relevant weighted counts 
  wtd.counts <- 
    questionr::wtd.table(
      x = SF[['state_electoral.college']],
      y = SF[[i]],
      weights = SF$N
    )
  # calculate relative frequency
  if(i!='region'){
    wtd.prop <- round(100*wtd.counts/rowSums(wtd.counts),2)
  }else{
    wtd.prop <- round(wtd.counts/rowSums(wtd.counts),2)
  }
  # make into data table with refined names
  wtd.prop <- as.data.table(wtd.prop)
  names(wtd.prop) <- c('state_electoral.college',i,paste0('pct.',i,'.'))
  # reshape to wide format
  wtd.prop <- 
    reshape(
      wtd.prop,
      idvar = 'state_electoral.college',
      timevar = i,
      direction = 'wide'
      )
  names(wtd.prop) <- make.names(names(wtd.prop))
  # collate together with other marginals
  SF.margins_electoral.college <- 
    append(SF.margins_electoral.college,list(wtd.prop))
}
# rename 
names(SF.margins_electoral.college) <- var.names

# get also number of `voting age citizens` from the microdata sample 
# this will be used as the denominator for the turnout variable 
#SF.margins_electoral.college$voting.age.citizens.micro <-
#  as.data.table(
#    questionr::wtd.table(
#      x = SF[['state_electoral.college']],
#      weights = SF$N
#      ) )
# names(SF.margins_electoral.college$voting.age.citizens.micro) <- 
#   c('state_electoral.college','N.voting.age.citizens')

# # # 
# # # (4) VOTING ELIGIBLE POPULATION
# # #
 
VEP.2020 <- 
  fread(
    file = 'auxiliary_data/US Election Project/2020 November General Election - Turnout Rates.csv',
    header = T
    )
VEP.2020 <- VEP.2020[!(VEP.2020$State %in% c('','United States') | VEP.2020$Denominators==''),c('State','Denominators')]
VEP.2020$Denominators <- as.numeric(gsub(',','',VEP.2020$Denominators))
VEP.2020$State <- gsub('\\*','',VEP.2020$State)
names(VEP.2020) <- c('state_simple','N.2020.voting.eligible.pop')

VEP.2022 <- 
  fread(
    file = 'auxiliary_data/US Election Project/2022 November General Election - Turnout Rates.csv',
    header = T
  )
VEP.2022 <- VEP.2022[!(VEP.2022$State %in% c('','United States') | VEP.2022$Denominators==''),c('State','Denominators')]
VEP.2022$Denominators <- as.numeric(gsub(',','',VEP.2022$Denominators))
VEP.2022$State <- gsub('\\*','',VEP.2022$State)
names(VEP.2022) <- c('state_simple','N.2022.voting.eligible.pop')

# NOTE: marginals for specific congressional districts are a little noisy - 
# cor. on white ethnicity with Daily Kos measures is, for example, 0.91.
# Noise can be attributable to noisy assignment procedure. 
# Fortunately, CD level data is not directly of interest here. 

# # # 
# # # (5) ELECTORAL COLLEGE LEVEL RELIGION DATA
# # #

# Data from Daily Kos 
religion.cd <- 
  fread(file = 'auxiliary_data/Daily Kos/Religion by CD 2017.csv',
        header = TRUE)

# focus on main groups + mormons
religion.cd <- 
  religion.cd [
    !grepl('reflects',religion.cd$District),
    c('District',
      'Evangelical %',
      'Black Protestant %',
      'Mainline %',
      'Orthodox %',
      'Catholic %',
      'All Other %',
      'Unclaimed %',
      'LDS %')]

# rename to match nomenclature
names(religion.cd) <- 
  c(
    'congressional.district_simple',
    paste0(
      'pct.religion.',
      tolower(make.names(gsub(' %','',names(religion.cd)[-1])))
      )
  )

# apply electoral college names 
religion.cd$congressional.district_simple <-
  paste(
    usmap::fips_info()$full[
      match(
        substr(religion.cd$congressional.district_simple,1,2),
        usmap::fips_info()$abbr
      )],
    ifelse(
      grepl('AL',substr(religion.cd$congressional.district_simple,4,5)),
      'at-large',
      ordinal_suffix_vectorized(
        as.numeric(substr(religion.cd$congressional.district_simple,4,5))
        )
    )
  )

# make numeric 
for(i in names(religion.cd)[-1]){
  religion.cd[[i]] <- as.numeric(gsub('%','',religion.cd[[i]]))
}

# # # 
# # # (6) ELECTORAL COLLEGE LEVEL RURALITY DATA
# # #

# Data from Daily Kos 
rural.cd <- 
  fread(
    file = 'auxiliary_data/Daily Kos/Daily Kos Elections urban-suburban-rural population by congressional district - Total pop..csv',
    header = TRUE
    )

# drop state names 
rural.cd <- rural.cd[
  !rural.cd$District %in% 
    c('U.S.A.',usmap::fips_info()$full[
      usmap::fips_info()$full!='District Of Columbia'
      ])]

# focus on subset of columns
rural.cd <- rural.cd [,c("District","Urban %","Suburban %","Rural %")]

# assign electoral college geographic id
rural.cd$congressional.district_simple <- 
  paste(
    usmap::fips_info()$full[
      match(
        substr(rural.cd$District,1,2),
        usmap::fips_info()$abbr
      )],
    ifelse(
      grepl('AL',substr(rural.cd$District,4,5)),
      'at-large',
      ordinal_suffix_vectorized(as.numeric(substr(rural.cd$District,4,5)))
    )
  )

# drop old id
rural.cd <- rural.cd[,!'District']

# rename to adjust nomenclature
names(rural.cd)[names(rural.cd)!='congressional.district_simple'] <- 
  paste0(
    'pct.rurality.',
    make.names(
      gsub(
        ' %','',
        names(rural.cd)[names(rural.cd)!='congressional.district_simple']
        )
      ) )
names(rural.cd) <- tolower(names(rural.cd))

# # # 
# # # (7) AGGREGATE FOR STATIC 2020 / 2022 AREA-LEVEL PREDICTOR
# # #

cd.dt = 
  merge(
    pop.cd,
    merge(
      rural.cd,
      religion.cd,
      by = 'congressional.district_simple',
      all=T
      ),
    by = 'congressional.district_simple',
    all=T
  )

# aggregate at the electoral college level 
cd.dt[,N.pop.state := sum(N.pop),by = c('state_electoral.college','state_abbreviation','state_simple')]
ec.dt <- 
  cd.dt[,lapply(.SD,function(x){sum(x * N.pop) / sum(N.pop)}), 
        by = c('state_electoral.college','state_abbreviation','state_simple'),
        .SDcols = names(cd.dt)[grepl('pct|N.pop.state',names(cd.dt))]]

# merge in electoral college level past voting data 
ec.dt <- merge(ec.dt,vote2020.state,by = c('state_electoral.college'),all=TRUE)
ec.dt <- merge(ec.dt,vote2022.state,by = c('state_electoral.college'),all=TRUE)

# merge with SF marginals
SF.margins.dt <- data.table()
for(i in names(SF.margins_electoral.college)){
  if(i ==  names(SF.margins_electoral.college)[1]){
    SF.margins.dt <- SF.margins_electoral.college[[i]]
  }else{
    SF.margins.dt <- 
      merge(
        SF.margins.dt,
        SF.margins_electoral.college[[i]],
        by = c('state_electoral.college'),
        all=TRUE
        )
  } 
}
# merge 
ec.dt = merge(SF.margins.dt,ec.dt,by = c('state_electoral.college'),all=TRUE)

# merge in VEP at state level 
ec.dt <- merge(ec.dt,VEP.2020,by = c('state_simple'),all=TRUE)
ec.dt$N.2020.voting.eligible.pop_state <- 
  ec.dt$N.2020.voting.eligible.pop
ec.dt[ec.dt$state_simple %in% c('Nebraska','Maine')]$N.2020.voting.eligible.pop <- NA
ec.dt <- merge(ec.dt,VEP.2022,by = c('state_simple'),all=TRUE)
ec.dt$N.2022.voting.eligible.pop_state <- 
  ec.dt$N.2022.voting.eligible.pop
ec.dt[ec.dt$state_simple %in% c('Nebraska','Maine')]$N.2022.voting.eligible.pop <- NA

# train a quick model to impute 
# N.voting.eligible.pop for Nebraska and Maine districts

# regression tree seems useful here given the low-n and potential for
# massive overfitting to actually be somewhat useful here, given the 
# underlying regularity of the process .

library(rpart)
library(rpart.plot)

# tends to fail for the largest states because it has never seen states as large
# as those. 
# Maybe it could fail for our very small districts in a similar way - 
# Wyoming Vermont DC North Dakota and Alaska are around the same size as 
# our districts, so that should limit the damage. 

# # # 2020 Presidential Election VEP
tree_model.2020 <- rpart(N.2020.voting.eligible.pop ~ 
                      pct.rurality.rural + 
                      N.pop.state + 
                      `2020.votes.T` + 
                      `2020.votes.D` + 
                      `2020.votes.R` + 
                      `2020.votes.L` + 
                      `2020.votes.OTHER` + 
                      `2020.votes.PSL` + 
                      `2020.votes.G` +
                      pct.college_grad..Does.not.have.a.College.Degree + 
                      pct.gender..M + # institutionalised people tend to be young, minorities, poor, uneducated 
                      pct.ethnicity..Black + 
                      pct.ethnicity..Hispanic + 
                      I(pct.age_bins..18.24 + pct.age_bins..25.34) + 
                      pct.hh_income...min..25000. + 
                      pct.region..Midwest + 
                      pct.region..Northeast + 
                      pct.region..South + 
                      pct.region..West,
                    data = ec.dt,
                    method="poisson",
                    cp=0.0000001, 
                    minsplit=1, 
                    minbucket=1,
                    maxdepth=30
                    ) 

# encourage massive overfitting here - make maximum use of the ~50 obs we have

# generate complete variable
ec.dt$N.2020.voting.eligible.pop_predict <- 
  ifelse(ec.dt$state_simple %in% c('Nebraska','Maine'),
         predict(tree_model.2020, newdata = ec.dt),
         ec.dt$N.2020.voting.eligible.pop)

# ensure sum is consistent 
ec.dt$N.2020.voting.eligible.pop_predict[
  ec.dt$state_simple=='Nebraska'
] <- 
unique(
  ec.dt$N.2020.voting.eligible.pop_state[
    ec.dt$state_simple=='Nebraska'
    ])*
ec.dt$N.2020.voting.eligible.pop_predict[
  ec.dt$state_simple=='Nebraska'
]/sum(ec.dt$N.2020.voting.eligible.pop_predict[
  ec.dt$state_simple=='Nebraska'
])

ec.dt$N.2020.voting.eligible.pop_predict[
  ec.dt$state_simple=='Maine'
] <- 
  unique(
    ec.dt$N.2020.voting.eligible.pop_state[
      ec.dt$state_simple=='Maine'
    ])*
  ec.dt$N.2020.voting.eligible.pop_predict[
    ec.dt$state_simple=='Maine'
  ]/sum(ec.dt$N.2020.voting.eligible.pop_predict[
    ec.dt$state_simple=='Maine'
  ])

# now calculate % votes in 2020 
tmp <- 
  100*
  ec.dt [,grepl('votes',names(ec.dt )) & !grepl('\\.T',names(ec.dt )) & grepl('2020',names(ec.dt )),with=F]/
  rowSums(
    ec.dt [,grepl('votes',names(ec.dt )) & !grepl('\\.T',names(ec.dt )) & grepl('2020',names(ec.dt )),with=F]
    )
names(tmp) <- paste0('pct.',names(tmp))
tmp$pct.2020.votes.T <-  
  100*ec.dt$`2020.votes.T`/ 
  ec.dt$N.2020.voting.eligible.pop_predict
tmp$pct.2020.votes.stay.home <- 100-tmp$pct.2020.votes.T 

# append to the rest of the dataset
ec.dt <- cbind(ec.dt,tmp)
ec.dt <- ec.dt[,!'N.2020.voting.eligible.pop'] # we don't need this - we have the imp. version

# # # 2022 Congressional Election VEP

tree_model.2022 <- rpart(N.2022.voting.eligible.pop ~ 
                           pct.rurality.rural + 
                           N.pop.state + 
                           `2022.votes.T` + 
                           `2022.votes.D` + 
                           `2022.votes.R` + 
                           `2022.votes.L` + 
                           `2022.votes.G` + 
                           `2022.votes.OTHER` + 
                           pct.college_grad..Does.not.have.a.College.Degree + 
                           pct.gender..M + 
                           pct.ethnicity..Black + 
                           pct.ethnicity..Hispanic + 
                           I(pct.age_bins..18.24 + pct.age_bins..25.34) + 
                           pct.hh_income...min..25000. + 
                           pct.region..Midwest + 
                           pct.region..Northeast + 
                           pct.region..South + 
                           pct.region..West,
                         data = ec.dt,
                         method="poisson",
                         cp=0.0000001, 
                         minsplit=1, 
                         minbucket=1,
                         maxdepth=30
                         ) 

# generate complete variable
ec.dt$N.2022.voting.eligible.pop_predict <- 
  ifelse(ec.dt$state_simple %in% c('Nebraska','Maine'),
         predict(tree_model.2022, newdata = ec.dt),
         ec.dt$N.2022.voting.eligible.pop)

# ensure sum is consistent 
ec.dt$N.2022.voting.eligible.pop_predict[
  ec.dt$state_simple=='Nebraska'
] <- 
  unique(
    ec.dt$N.2022.voting.eligible.pop_state[
      ec.dt$state_simple=='Nebraska'
    ])*
  ec.dt$N.2022.voting.eligible.pop_predict[
    ec.dt$state_simple=='Nebraska'
  ]/sum(ec.dt$N.2022.voting.eligible.pop_predict[
    ec.dt$state_simple=='Nebraska'
  ])

ec.dt$N.2022.voting.eligible.pop_predict[
  ec.dt$state_simple=='Maine'
] <- 
  unique(
    ec.dt$N.2022.voting.eligible.pop_state[
      ec.dt$state_simple=='Maine'
    ])*
  ec.dt$N.2022.voting.eligible.pop_predict[
    ec.dt$state_simple=='Maine'
  ]/sum(ec.dt$N.2022.voting.eligible.pop_predict[
    ec.dt$state_simple=='Maine'
  ])

# now calculate % votes in 2022 
tmp <- 
  100*
  ec.dt [,grepl('votes',names(ec.dt )) & !grepl('\\.T',names(ec.dt )) & grepl('2022',names(ec.dt )),with=F]/
  rowSums(
    ec.dt [,grepl('votes',names(ec.dt )) & !grepl('\\.T',names(ec.dt )) & grepl('2022',names(ec.dt )),with=F]
  )
names(tmp) <- paste0('pct.',names(tmp))
tmp$pct.2022.votes.T <-  
  100*ec.dt$`2022.votes.T`/ 
  ec.dt$N.2022.voting.eligible.pop_predict
tmp$pct.2022.votes.stay.home <- 100-tmp$pct.2022.votes.T 

# append to the rest of the dataset
ec.dt <- cbind(ec.dt,tmp)
ec.dt <- ec.dt[,!'N.2022.voting.eligible.pop'] # we don't need this - we have the imp. version

# rename object
predictor_area_level <- ec.dt

# make a couple of additions 
predictor_area_level$pct.religion.evangelical.and.lds <- 
  predictor_area_level$pct.religion.evangelical + 
  predictor_area_level$pct.religion.lds

# save this static area dataset - useful for MrsP
save(predictor_area_level,file = 'generated_data/predictor/area_level.RData')

# # #
# # # Now we build a state-time level predictor
# # #

# (1) gather favourability / presidential approval ratings
# From 538: https://projects.fivethirtyeight.com/polls/favorability
favorability.avg <- fread(input = 'https://projects.fivethirtyeight.com/polls/data/favorability_averages.csv')
# candidates of interest: 
favorability.avg <- favorability.avg[politician %in% c('Donald Trump','Joe Biden','Robert F. Kennedy','Nikki Haley','Ron DeSantis') & subpopulation != 'r']
# focus on point estimates for now 
favorability.avg.wide <- reshape(favorability.avg[,!'subpopulation'],direction = 'wide',idvar = c('politician','date'),timevar = 'answer')
# get net rating 
favorability.avg.wide$pct.estimate_net.favourable <- favorability.avg.wide$pct_estimate.Favorable - favorability.avg.wide$pct_estimate.Unfavorable
net.favorability.avg.wide <- favorability.avg.wide[,c('politician','date','pct.estimate_net.favourable')]
net.favorability.avg.wide <- reshape(net.favorability.avg.wide,idvar = c('date'),timevar = 'politician',direction = 'wide')

# for Joe Biden (incumbent president)
#net.favorability.avg.wide <- net.favorability.avg.wide[politician !='Joe Biden']
approval.avg <- fread(input = 'https://projects.fivethirtyeight.com/biden-approval-data/approval_topline.csv')
approval.avg$pct.estimate_net.approval  <- approval.avg$approve_estimate-approval.avg$disapprove_estimate
approval.avg <- approval.avg[,c('politician','end_date','pct.estimate_net.approval')]
names(approval.avg)[names(approval.avg)=='end_date'] <- 'date'
approval.avg$politician = 'Incumbent President'
approval.avg.wide <- reshape(approval.avg,idvar = c('date'),timevar = 'politician',direction = 'wide')
# merge in 
net.favorability.avg.wide <- merge(approval.avg.wide,net.favorability.avg.wide ,by = 'date',all=TRUE)
# order by date
net.favorability.avg.wide <- net.favorability.avg.wide[order(net.favorability.avg.wide$date)]
# fill missing values with last observation carried forward 
net.favorability.avg.wide[, 
                          names(net.favorability.avg.wide)[-1] := 
                            lapply(.SD, function(col){ 
                              zoo::na.locf(col, na.rm = FALSE)
                            }), 
                          .SDcols = -"date"]

# Columns to apply LOCF on
locf_cols <- names(net.favorability.avg.wide)[!names(net.favorability.avg.wide) %in% c('date')]
# Apply LOCF by each state for the selected columns
net.favorability.avg.wide[, (locf_cols) := lapply(.SD, na.locf, na.rm = FALSE),.SDcols = locf_cols]

# focus on time period starting October 1st 2023
net.favorability.avg.wide$date <- as.Date(net.favorability.avg.wide$date,'%Y-%m-%d')
net.favorability.avg.wide <- 
  net.favorability.avg.wide[net.favorability.avg.wide$date>=as.Date('1/10/2023',"%d/%m/%Y")]

# (2) gather real-time economic data 
# Real-Time Economic Data

retry_download <- function(url, retries = 100) {
  for (i in 1:retries) {
    tryCatch(
      {
        econ_tracker_url <- url
        # Assuming you are using the download.file function to download the file
        download.file(econ_tracker_url, destfile = "auxiliary_data/Opportunity Insights Economic Tracker Data/EconomicTracker-main.zip")
        print("Download successful")
        break  # Exit the loop if the download is successful
      },
      error = function(e) {
        message(paste("Attempt", i, "failed:", e$message))
        if (i < retries) {
          Sys.sleep(10)  # Wait for 10 seconds before retrying
        } else {
          message("Max retries reached. Exiting.")
        }
      }
    )
  }
}

# Call the function with the URL
retry_download("https://github.com/OpportunityInsights/EconomicTracker/archive/main.zip")

# from this dataset, the national econ data is correlated around 0.88 with 538 composite economic index - this is a good proxy
unzip(
  zipfile = "auxiliary_data/Opportunity Insights Economic Tracker Data/EconomicTracker-main.zip",
  exdir = 'auxiliary_data/Opportunity Insights Economic Tracker Data',
  overwrite = TRUE
  )
# credit card data
econ <- fread(file = 'auxiliary_data/Opportunity Insights Economic Tracker Data/EconomicTracker-main/data/Affinity - State - Daily.csv')
econ$date <- as.Date(paste(econ$year,econ$month,econ $day,sep='/'),'%Y/%m/%d')
econ <- econ[freq == 'w'][,c('date','statefips','spend_all')]                     
econ$spend_all <- as.numeric(econ$spend_all)
state.names <- usmap::fips_info()
names(state.names) <- c('state.abbreviation','state.fips','state_simple')
state.names $state.fips <- as.numeric(as.character(unlist(state.names $state.fips )))
econ <- merge(econ,state.names,by.x = 'statefips',by.y = 'state.fips',all.x = TRUE)
econ <- econ[,!c('statefips','state.abbreviation')]

# load and merge state similarity index
state.sim <- 
  fread(file = 'auxiliary_data/Daily Kos/Daily Kos Elections State Similarity Index - Similarity.csv')
econ <- merge(econ,state.sim,by.x = c('state_simple'),by.y = c('Geography'),all.x = T)
names(econ) <- make.names(names(econ))

# district of Columbia is missing unfortuantely - use miceRanger to impute 
m = 30
econ.impute <- 
  miceRanger::miceRanger(
    data = econ,
    m = m,maxiter = 100,
    verbose = TRUE)
# get average across imputations 
imp <- 
  lapply(1:m,
         function(x){
           miceRanger::completeData(
             miceObj = econ.impute,
             datasets = x,
             verbose = TRUE)[[1]]
         })
j <- 'spend_all'
  imp.summary <- 
    sapply(
      1:m,
      function(x){
        imp[[x]]$spend_all[which(as.data.table(econ.impute$naWhere)[[j]])]
      }
    )
  # plug in imputation for DC
  econ[[j]][which(as.data.table(econ.impute$naWhere)[[j]])] <- 
    apply(imp.summary,1,median)

  # make series daily: 
  temp <- 
    as.data.table(
      expand.grid(
        date = seq(min(c(econ$date)),Sys.Date(),by = 1),
        state_simple = unique(econ$state_simple)
      ) )
  econ <- 
    merge(
      econ[,c('state_simple','date','spend_all')] ,
      temp,by = c('state_simple','date'),
      all = TRUE)
  
# focus on time period starting October 1st 2023
econ <- 
  econ [econ $date>=as.Date('1/10/2023',"%d/%m/%Y")]

# (3) pull together the area-time predictor
net.favorability.avg.wide <- 
  merge(
    net.favorability.avg.wide,
    expand.grid(
      date = unique(net.favorability.avg.wide$date),
      state_simple = unique(econ$state_simple)
    ),by = c('date'),all=TRUE )
predictor_area.date_level <- 
  merge(
    econ,
    net.favorability.avg.wide,
    by = c('date','state_simple'),
    all=TRUE
    )

# fill missing values with last observation carried forward 

# Columns to apply LOCF on
locf_cols <- names(predictor_area.date_level)[!names(predictor_area.date_level) %in% c('state_simple','date')]
# Apply LOCF by each state for the selected columns
predictor_area.date_level[, (locf_cols) := lapply(.SD, na.locf, na.rm = FALSE), by = .(state_simple), .SDcols = locf_cols]

# save this dynamic area dataset
save(predictor_area.date_level,file = 'generated_data/predictor/area.date_level.RData')




