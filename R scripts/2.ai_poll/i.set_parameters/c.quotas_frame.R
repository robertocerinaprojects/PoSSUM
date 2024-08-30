library(bayestestR)

# define key quota variables 
quota_features <- c('SEX','AGE','ETHNICITY','HOUSEHOLD INCOME BRACKET','PAST VOTE - VOTE CHOICE IN THE 2020 PRESIDENTIAL ELECTION')

# define how many people we wish to calibrate a survey for
n <- 1500

# load stratification frame 
load(file = 'generated_data/stratification_frame/SF_extended.RData')

# re-name the levels to match the independent features being extracted - 
# this is had-hoc and requires project-specific inputs 

# clean SEX variable 
levels(SF_extended$gender)[grepl('M',levels(SF_extended$gender))] <- 'masculine sex - male'
levels(SF_extended$gender)[grepl('F',levels(SF_extended$gender))] <- 'feminine sex - female'
names(SF_extended)[grepl('gender',names(SF_extended))] <- 'SEX'

# clean AGE variable
levels(SF_extended$age_bins)[grepl('18-24',levels(SF_extended$age_bins))] <- '18 to 24 years old'
levels(SF_extended$age_bins)[grepl('25-34',levels(SF_extended$age_bins))] <- '25 to 34 years old'
levels(SF_extended$age_bins)[grepl('35-44',levels(SF_extended$age_bins))] <- '35 to 44 years old'
levels(SF_extended$age_bins)[grepl('45-54',levels(SF_extended$age_bins))] <- '45 to 54 years old'
levels(SF_extended$age_bins)[grepl('55-64',levels(SF_extended$age_bins))] <- '55 to 64 years old'
levels(SF_extended$age_bins)[grepl('65+',levels(SF_extended$age_bins))] <- '65 or older'
names(SF_extended)[grepl('age_bins',names(SF_extended))] <- 'AGE'

levels(SF_extended$AGE) <- c(levels(SF_extended$AGE),'under 18 years old')

# clean ETHNICITY variable

levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='Asian'] <- 'asian - individuals with origins in any of the original peoples of central or east asia, southeast asia, or south asia, including, for example, chinese, asian indian, filipino, vietnamese, korean, and japanese.'
levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='Black'] <- 'black or african american - individuals with origins in any of the black racial groups of africa, including, for example, african american, jamaican, haitian, nigerian, ethiopian, and somali.'
levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='Hispanic'] <- 'hispanic or latino - includes individuals of mexican, puerto rican, salvadoran, cuban, dominican, guatemalan, and other central or south american or spanish culture or origin.'
levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='Mixed-race'] <- 'multiracial - individuals who identify explicitly as belonging to more than one of the racial and ethnic groups above, such as biracial individuals with one white and one black parent, or those with a combination of asian and hispanic heritage, etc. mixed-race individuals often face unique social experiences, such as celebrating diverse cultural holidays, speaking multiple languages, and bridging different cultural perspectives within their families and communities.'
levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='White'] <- 'white - individuals with origins in any of the original peoples of europe, including, for example, english, german, irish, italian, polish, and scottish -- as well as arab or middle-eastern with origins in any of the original peoples of the middle east or north africa, including, for example, lebanese, iranian, egyptian, syrian, iraqi, and israeli.'
levels(SF_extended$ethnicity)[levels(SF_extended$ethnicity)=='Native'] <- 'american indian or alaskan native or native hawaiian or pacific islander - individuals with origins in any of the original peoples of north, central, and south america, including, for example, navajo nation, blackfeet tribe of the blackfeet the indian reservation of montana, native village of barrow inupiat traditional government, nome eskimo community, aztec, and maya -- as well as individuals with origins in any of the original peoples of hawaii, guam, samoa, or other pacific islands, including, for example, native hawaiian, samoan, chamorro, tongan, fijian, and marshallese.'

SF_extended$ethnicity <- as.factor(as.character(unlist(SF_extended$ethnicity)))

names(SF_extended)[grepl('ethnicity',names(SF_extended))] <- 'ETHNICITY'

# clean HIGHEST EDUCATIONAL QUALIFICATION variable
#levels(SF_extended$college_grad)[levels(SF_extended$college_grad)=='Does not have a College Degree'] <- 'completed education up to and including high school - high school diploma, vocational training, associate degree'
#levels(SF_extended$college_grad)[levels(SF_extended$college_grad)=='Has a College Degree'] <- "completed education at the college or university level - bachelor's degree, master's degree, doctorate"

#names(SF_extended)[grepl('college_grad',names(SF_extended))] <- 'HIGHEST EDUCATIONAL QUALIFICATION'

# clean HOUSEHOLD INCOME BRACKET variable
levels(SF_extended$hh_income)[levels(SF_extended$hh_income)=='1.[min, 25000)'] <- 'up to 25000 USD per year'
levels(SF_extended$hh_income)[levels(SF_extended$hh_income)=='2.[25000, 50000)'] <- 'between 25000 and 50000 USD per year'
levels(SF_extended$hh_income)[levels(SF_extended$hh_income)=='3.[50000, 75000)'] <- 'between 50000 and 75000 USD per year'
levels(SF_extended$hh_income)[levels(SF_extended$hh_income)=='4.[75000, 100000)'] <- 'between 75000 and 100000 USD per year'
levels(SF_extended$hh_income)[levels(SF_extended$hh_income)=='5.[100000, max]'] <- 'more than 100000 USD per year'

names(SF_extended)[grepl('hh_income',names(SF_extended))] <- 'HOUSEHOLD INCOME BRACKET'


# clean PAST VOTE 2020 variable
SF_extended$vote2020 <- as.factor(SF_extended$vote2020)
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='D'] <- 'voted for Joe Biden, the Democratic Party candidate, in the 2020 election for President in their state'
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='R'] <- 'voted for Donald Trump, the Republican Party candidate, in the 2020 election for President in their state'
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='OTHER'] <- 'voted for a candidate other than the Republican, Democrat, Libertarian, or Green Party candidates, in the 2020 election for President in their state'
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='L'] <- 'voted for Jo Jorgensen, the Libertarian Party candidate, in the 2020 election for President in their state'
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='G'] <- 'voted for Howie Hawkins, the Green Party candidate, in the 2020 election for President in their state'
levels(SF_extended$vote2020)[levels(SF_extended$vote2020)=='stay home'] <- 'did not vote in the 2020 election for President in their state'

names(SF_extended)[grepl('vote2020',names(SF_extended))] <- 'PAST VOTE - VOTE CHOICE IN THE 2020 PRESIDENTIAL ELECTION'

# get the distribution of the new cells 

# first use the map estimate for each 
SF_extended <- 
  SF_extended[,lapply(.SD,sum),
              by = c(quota_features),
              .SDcols = names(SF_extended)[grepl('N_star',names(SF_extended))]
              ]

# get expected counts in n-sized survey
SF_extended$survey_cap <- 
    apply(
      SF_extended[,names(SF_extended)[grepl('N_star',names(SF_extended))],with=F]/
        sum(SF_extended$N_star.1),
      1,
      function(x){
        map_estimate(x)$MAP_Estimate
        } 
      )

# sample at random to have a little variability in the sampleon every round, 
# so that the larger categories are not always favoured over time 
SF_extended$survey_cap <- 
  rmultinom(n = 1,size = n,prob = SF_extended$survey_cap)


# drop the original weights for clarity
SF_extended <- SF_extended[,!grepl('N_star',names(SF_extended)),with=F]

# drop cells with cap at 0
SF_extended <- SF_extended[survey_cap>0]

# initialise the quota labs and counters
SF_extended$quota_lab <- 
  apply(SF_extended[,quota_features,with=F],1,function(x){paste0(x,collapse = '_AND_')})  
SF_extended$counter <- 0

# rename the frame
counter <- SF_extended
