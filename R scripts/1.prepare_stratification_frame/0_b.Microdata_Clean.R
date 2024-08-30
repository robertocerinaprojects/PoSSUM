# clean workspace
rm(list=ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen=999)
# set timeout limit (laxed)
options(timeout=10000)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")
# utils
library(data.table)

# load microdata
load(file = 'generated_data/stratification_frame/SF_microdata.RData')

# drop non-citizens
microdata <- microdata[microdata$CIT!="Not a citizen of the U.S."]
microdata <- microdata [,!'CIT']

# clean state
microdata$state_simple <- sub("\\/.*", "", microdata$ST)
microdata$state_abbreviation <- sub("^.*/", "", microdata$ST)
 
# # # congressional district

# some district are not allocated (*) - apply one at random from the state 
starred_cds <- 
  unique(microdata$CD[grepl('\\*',microdata$CD)])

# for each star cd
for(k in 1:length(starred_cds)){

# identify these individuals
test.star <- 
  substr(microdata$CD,1,2)==substr(starred_cds,1,2)[k] & grepl('\\*',microdata$CD)

# what's the puma for individuals with the star ? 
star.pumas <- unique(microdata$PUMA20[test.star])
star.state <- unique(microdata$ST[test.star])

# for each puma 
for(i in 1:length(star.pumas)){
  
# what cds are typically associated with each puma ? 
possible_cds <- 
  lapply(
    star.pumas[i],
    function(x){
      unique(
        microdata$CD[
          microdata$PUMA20==x & 
            microdata$ST==star.state &
            !grepl('\\*',microdata$CD)]
        )
      })

# assign one amongst these cds 
microdata$CD[microdata$PUMA20==star.pumas[i] & test.star ] <- 
  sample(
    possible_cds[[1]],
    size = sum(microdata$PUMA20==star.pumas[i] & test.star),
    replace = TRUE
    )
} }

# rename district
microdata$congressional.district_code <- microdata$CD
microdata$congressional.district_simple <- microdata$CD

# rename districts 
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

microdata$congressional.district_simple[
  substr(microdata$congressional.district_simple,3,4)=='00'
  ] <- paste(microdata$state_simple[
    substr(microdata$congressional.district_simple,3,4)=='00'
  ],'at-large')

microdata$congressional.district_simple[
  !grepl('at-large',microdata$congressional.district_simple)
  ] <- paste(microdata$state_simple[
    !grepl('at-large',microdata$congressional.district_simple)
  ],
  ordinal_suffix_vectorized(
    as.numeric(substr(microdata$congressional.district_simple,3,4)[
    !grepl('at-large',microdata$congressional.district_simple)
  ]))
  )

# there is a weird `Indiana 21st` district - drop these guys

# in the electoral college Nebraska and Maine districts have to be treated as states
microdata$state_electoral.college <- 
  ifelse(
    microdata$state_simple=='Nebraska'|microdata$state_simple=='Maine',
    microdata$congressional.district_simple,
    microdata$state_simple
  )

# clean region
microdata$region <- microdata$REGION

# clean gender
microdata$gender <- substr(microdata$SEX,1,1)

# clean ethnicity
microdata$ethnicity <-
    ifelse(microdata$HISP!= 'Not Spanish/Hispanic/Latino','Hispanic',
        ifelse(microdata$RAC1P=='White alone','White',
            ifelse(microdata$RAC1P=='Black or African American alone','Black',
                ifelse(microdata$RAC1P=="Asian alone",'Asian',
                       ifelse(microdata$RAC1P=="Two or More Races",'Mixed-race',
                              ifelse(microdata$RAC1P %in% c('Alaska Native alone',
                                                            'American Indian alone',
                                                            'American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races',
                                                            'Native Hawaiian and Other Pacific Islander alone'),
                                     'Native',
                                     NA
            ) ) ) ) ) )


# clean age
microdata$age_bins <-
as.character(unlist(
    cut(
      microdata$AGEP,
      c(-1,17,24,34,44,54,64,max(microdata$AGEP)),
      c("0-17","18-24","25-34","35-44","45-54","55-64","65+")
    ) ) )
# drop under-age people
microdata <- microdata[microdata$age_bins!='0-17']

# not going to use marital status - doesn't seem overwhelmingly relevant to
# vote choice + measurememt problems for surveys & Twitter

# clean marital status
microdata$marital_status <-
    ifelse(microdata$MAR == 'Married','Married','Not Married')

# clean education
microdata$college_grad <-
    ifelse(
        microdata$SCHL %in%
        c(
        "Bachelor's degree",
        "Doctorate degree",
        "Master's degree",
        "Professional degree beyond a bachelor's degree"
        ),
        "Has a College Degree",
        "Does not have a College Degree"
        )

# clean household income
microdata$hh_income <-
    cut(
        microdata$HINCP,
        c(min(microdata$HINCP)-1, 24999,49999,74999,99999,max(microdata$HINCP)),
        c("[min, 25000)","[25000, 50000)","[50000, 75000)","[75000, 100000)","[100000, max]")
    )

# clean internet access
microdata$internet <- 
  ifelse(
    microdata$ACCESSINET=="No access to the Internet at this house, apartment, or mobile home",
    'Has no access to the internet',
    'Has access to the internet'
    )

# drop incomplete 
microdata <- microdata[complete.cases(microdata)]

# create stratification frame
vars <-
    c(
        'congressional.district_simple',
        'congressional.district_code',
        'state_abbreviation',
        'state_simple',
        'state_electoral.college',
        'region',
        'gender',
        'ethnicity',
        'age_bins',
        'marital_status',
        'college_grad',
        'hh_income',
        'internet'
    )
microdata$N <- 1

SF <- microdata[, lapply(.SD,sum),by = c(vars),.SDcols = c('N')]

# save clean stratification frame
save(SF,file = 'generated_data/stratification_frame/SF.RData',compress = TRUE)
