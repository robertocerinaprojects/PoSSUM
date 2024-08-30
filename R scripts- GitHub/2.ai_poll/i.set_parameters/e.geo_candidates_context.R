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

# load utils
library(data.table)
library(dplyr)

# load utils to deal with apis
library(httr)
library(jsonlite)

# sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# function for gpt calls
source(file = 'utils/gpt_functions.R')

# set OpenAI API key 
Sys.setenv(OPENAI_API_KEY = readLines(con = 'tokens/OpenAI'))

# get other functions related to images and uploading those on the cloud 
source(file = 'utils/image.url_functions.R')

# get authorisation code for imgur api 
source(file = 'tokens/imgur_auth.R')

# get features
source(file = 'R scripts/2.ai_poll/i.set_parameters/a.independent_features_object.R')
source(file = 'R scripts/2.ai_poll/i.set_parameters/b.dependent_features_object.R')

# get geo lookp
source(file = 'R scripts/2.ai_poll/i.set_parameters/d.geo_candidates_lookup.R')


# racial composition of the districts
kos_race <- fread(file = "auxiliary_data/Redistricting 2022 Context/Daily Kos/House and Senate 5-Year American Community Survey Racial Demographics by Citizenship Status - 118th House—2022.csv")
tmp <- unlist(kos_race[1])
tmp[tmp=='']<- NA
kos_race[1] <- as.data.table(t(zoo::na.locf(tmp)))
kos_race <- kos_race[,grepl('District|Code|Total Population by Race',kos_race[1]),with=F]
names(kos_race) <- unlist(kos_race[2])
names(kos_race)[grepl('District|Code',kos_race[1])] <- c('district','code')
kos_race <- kos_race[-c(1:3)]
kos_race <- cbind(extract_district_info(kos_race$district),kos_race[,!'district'])
# rural / suburban / urban breakdown 
kos_rural <- fread(file = "auxiliary_data/Redistricting 2022 Context/Daily Kos/Daily Kos Elections urban-suburban-rural population by congressional district (118th congress) - Total Pop..csv")
kos_rural <- kos_rural[,grepl('%|CD',names(kos_rural)),with=F]
kos_rural[kos_rural$CD=='TX-20']$CD[2] <- 'TX-21'
for(i in grep('%|CD',names(kos_rural),value = T)){ kos_rural[[i]] <- gsub('%','',kos_rural[[i]])}
names(kos_rural) <-  gsub(' %','',names(kos_rural))
kos <- merge(kos_race,kos_rural,by.x = 'code',by.y = 'CD',all.x = TRUE)
# education and income breakdown
kos_edu <- fread(file = "auxiliary_data/Redistricting 2022 Context/Daily Kos/Bachelor's degrees or greater for ages 25+ & median household income by congressional district (2018-2022) - 2022 - 118th House.csv")
kos <- merge(kos,kos_edu[,c('District',"Bachelor's","Median Income")],by.x = 'code',by.y = 'District',all.x = TRUE)
# overall descriprion
kos_description <- fread(file = "auxiliary_data/Redistricting 2022 Context/Daily Kos/Daily Kos Elections congressional district geographic descriptions & largest places (118th Congress) - Descriptive name.csv")
kos <- merge(kos,kos_description[,c('CD',"Geographic Description")],by.x = 'code',by.y = 'CD',all.x = TRUE)
# major places
kos_places <- fread(file = "auxiliary_data/Redistricting 2022 Context/Daily Kos/Daily Kos Elections congressional district geographic descriptions & largest places (118th Congress) - By places.csv")
kos_places$`Major Places` <- 
  paste(
    'The most populated place in the district is',
    kos_places $`Largest place`,
    'accounting for',
    kos_places [[grep('% of CD',names(kos_places))[1]]],
    '% of the population.',
    ifelse(!is.na(kos_places [[grep('% of CD',names(kos_places))[2]]]),
           paste(
           'The second most populated place in the district is',
           kos_places $`Second-largest place`,
           'accounting for',
           kos_places [[grep('% of CD',names(kos_places))[2]]],
           '% of the population.'),
           NA),
    ifelse(!is.na(kos_places [[grep('% of CD',names(kos_places))[3]]]),
           paste(
             'The third most populated place in the district is',
             kos_places $`Third-largest place`,
             'accounting for',
             kos_places [[grep('% of CD',names(kos_places))[3]]],
             '% of the population.'),
           NA)
           ) 
kos <- merge(kos,kos_places[,c('CD',"Major Places")],by.x = 'code',by.y = 'CD',all.x = TRUE)
kos <- kos[,!'code']
names(kos)[!grepl('state|district|Median Income|Geographic Description|Major Places|Urban|Rural|Bachelor|Suburban',names(kos))] <- 
paste0('% Pop. who identifies as ',
       names(kos)[!grepl('state|district|Median Income|Geographic Description|Major Places|Urban|Rural|Bachelor|Suburban',names(kos))] 
       )  
names(kos)[grepl('Bachelor',names(kos))] <- "% Pop. who achieved a Bachelor's degree"
names(kos)[grepl('Urban|Suburban|Rural',names(kos))] <- paste("% Pop. who lives in",names(kos)[grepl('Urban|Suburban|Rural',names(kos))],'areas')


# now turn this into a text prompt 
geo3_lookup_demos <- kos[,c('state','district')]
geo3_lookup_demos$prompt <- NA
for(i in 1:dim(kos)[1]){
  geo3_lookup_demos$prompt[i] <- 
    paste0(
      paste(
        names(kos),
        kos[i],
        sep = ': '
      ),
      collapse = '\n'
    )
}
geo3_lookup_demos$state_po <- usmap::fips_info()[,'abbr'][match(geo3_lookup_demos$state,usmap::fips_info()[,'full'])]
geo3_lookup_demos$state_fips <- as.numeric(usmap::fips_info()[,'fips'])[match(geo3_lookup_demos$state,usmap::fips_info()[,'full'])]
geo3_lookup_demos$state <- tolower(geo3_lookup_demos$state)

# this is for a short context to be passed aggregated at the state level and passed on to assign a district to a user.
prompt.boundaries <- 
  function(geo2,geo3){
    paste0(
    'Consider the following Congressional District:  State - ',geo2,', District - ',geo3,'.
I attach an image of the map of this district after the 2022 redistircting took place. 
Describe in detail the geographical boundaries of the district AFTER REDISTRICTING, including the counties within its boundaries.'
    ) }
  

prompt.text <- 
  function(geo2,geo3,geo3_lookup,geo3_lookup_demos,boundaries_text){
    paste0(
      'BACKGROUND: 

Consider the following Congressional District:  State - ',geo2,', District - ',geo3,'.

Its geographical boundaries are as follows:',boundaries_text,'


The results of the election for the congressional seat in this district for 2022 (after redistricting) are reported below:\n\n',
      paste0(
        apply(
          geo3_lookup[
            year == 2022 & 
              state_po == geo2 & 
              district == geo3
          ][,c('candidate','party','candidatevotes')],
          1,
          function(x){
            paste0(
              c('candidate: ','party: ','votes: '),
              x,
              collapse = '\n')
          }
        ),
        collapse = '\n'
      ),
      '.
      
If a party is not mentioned in the results, it means they did not field a candidate for this election.
      
Furthermore, the congressional district has the following socio-demographics  (after redistricting):\n\n',
geo3_lookup_demos[state_po == geo2 & district == geo3]$prompt,'.

Summarise all this information in 150 words or less.'
)
  }

gen.geo3_context_list <- data.table()

# loop across geo2
for(geo2 in unique(geo3_lookup$state_po)[31:52]){
  # for each district within geo2 
  for(geo3 in unique(geo3_lookup[state_po==geo2]$district)){
    # prepare default prompt 
    messages.list <- 
      list(
        list(
          "role" = "user",
          "content" = 
            list(
              list(
                "type" = "text", 
                "text" = 'describe the image'
              ) ) ) )
    
    if(geo2=='USA'){ next }
    
    # for each year 
    #for(yr in sort(unique(geo3_lookup$year))){
    yr <- 2022
      # if district does not exist, skip
      if(
        sum(
          geo3_lookup$year == yr & 
          geo3_lookup$state_po == geo2 & 
          geo3_lookup$district == geo3
          )==0){next}
 
      # prep the text prompt 
      messages.list[[1]]$content[[1]]$text <- 
        prompt.boundaries(geo2 = geo2, geo3 = geo3 )
      
      # create congress id - useful to identify relevant images later 
      congress.id <- 
        ifelse(
          yr==2020,
          117,
          ifelse(
            yr==2022,
            118,
            NA
          )
        )
      
      if(geo2=='DE'){
        congress.id <- 116
      }
      
      # get the image url
      geo3_content_object <- 
        list(
          temp = 
            pdf_to_url(
              input.url = 
                paste0(
                  "https://www2.census.gov/geo/maps/cong_dist/cd",
                  congress.id,
                  "/cd_based/ST",
                  sprintf(
                    "%02d",
                    unique(
                      geo3_lookup[
                        year == yr & 
                          state_po == geo2 & 
                          district == geo3
                      ]$state_fips
                    )
                  ),
                  "/CD",
                  congress.id,
                  "_",
                  geo2,
                  paste0(
                    sprintf(
                      "%02d", 
                      geo3
                    )
                  ),
                  ".pdf"
                ),
              pdf.path = 
                paste0(
                  'auxiliary_data/Redistricting 2022 Context/Census/CD',congress.id,'_',
                  geo2,
                  paste0(
                    sprintf(
                      "%02d", 
                      geo3
                    )
                  ),
                  '.pdf'
                )
            )
        )
      names(geo3_content_object)[names(geo3_content_object)=='temp'] <- yr
      
      # prep the image object
      image_list <- 
        list(
          'type' = 'image_url',
          'image_url' = list('url' = geo3_content_object[[paste0(yr)]]$data$link)
        )
      
      # update messages list 
      messages.list[[1]]$content[[length(messages.list[[1]]$content)+1]] <- 
        image_list
    #}
    
  # run prompt
  gen.geo3_boundaries <- 
    call_gpt(
      messages.list = messages.list,
      temperature = 0.7,
      timeout = 120
    )
  
  # show 
  cat(gen.geo3_boundaries$choices$message.content)
  
  # summary
  messages.list <- 
    list(
      list(
        "role" = "user",
        "content" = 
          list(
            list(
              "type" = "text", 
              "text" =  
                prompt.text(
                  geo2 = geo2, 
                  geo3 = geo3,
                  geo3_lookup = geo3_lookup,
                  geo3_lookup_demos = geo3_lookup_demos,
                  boundaries_text =  gen.geo3_boundaries$choices$message.content
                )
            ) ) ) )
  
  
  # run prompt
  gen.geo3_context.summary <- 
    call_gpt(
      messages.list = messages.list,
      temperature = 0.7,
      timeout = 120
    )
  
  # show 
  cat(gen.geo3_context.summary$choices$message.content)
  
  # append 
  gen.geo3_context_list <- 
    rbindlist(
      list(
        gen.geo3_context_list,
        data.table(
          state = usmap::fips_info()[match(geo2,usmap::fips_info()$abbr),'full'],
          district = geo3,
          boundaries =gen.geo3_boundaries$choices$message.content,
          context.summary = gen.geo3_context.summary$choices$message.content ))
    )
  
  # store district context database    
  save(
    gen.geo3_context_list,
    file = paste0('generated_data/2022 redistricting context/context.RData')
  )
  
  
  } 
  
}

