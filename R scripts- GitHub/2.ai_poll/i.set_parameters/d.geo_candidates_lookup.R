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

# extract state and district from daily kos district nomenclature
extract_district_info <- function(districts) {
  # Extract the state names
  state_names <- sub("'s?\\s+.*$", "", districts)
  
  # Extract the district numbers, replacing "At-Large" with 0
  district_numbers <- ifelse(grepl("At-Large", districts), 0, 
                             as.integer(gsub("[^0-9]", "", districts)))
  
  # Create a data.table with the state and district information
  dt <- data.table(state = state_names, district = district_numbers)
  
  return(dt)
}


# # # PREPARE BACKGROUND INFO ON GEO3 RACES

geo3_lookup <- 
  fread(file = 'auxiliary_data/MIT Election Data and Science Lab/U.S House District-Level Returns 1976-2022/1976-2022-house.csv')

# focus on 2020 and 2022
geo3_lookup <- geo3_lookup[year %in% c(2020,2022)]

# exclude write ins 
geo3_lookup <- geo3_lookup[writein!=TRUE]

# fix Maine 2nd for 2022
geo3_lookup[grepl('MAINE',state) & district == 2 & year == 2022]$totalvotes <- 316382
geo3_lookup[grepl('MAINE',state) & district == 2 & candidate == 'BRUCE POLIQUIN'& year == 2022]$candidatevotes <- 141260
geo3_lookup[grepl('MAINE',state) & district == 2 & candidate == 'JARED F GOLDEN'& year == 2022]$candidatevotes <- 153074
geo3_lookup[grepl('MAINE',state) & district == 2 & candidate == 'TIFFANY BOND'& year == 2022]$candidatevotes <- 21655
geo3_lookup[grepl('MAINE',state) & district == 2 & candidate == 'WRITEIN'& year == 2022]$candidatevotes <- 393
geo3_lookup <- geo3_lookup[-which(candidate == 'EXHAUSTED BALLOT' & state == 'MAINE' & year == 2022)]

# add district of columbia 
geo3_lookup <- 
rbindlist(
  list(
    geo3_lookup,
    data.table(
      state = 'District of Columbia',
      state_po = 'DC',
      state_fips = 11,
      district = 0,
      year = 2020,
      candidate = c('Eleanor Holmes Norton','Nelson Rimensnyder','Barbara Washington Franklin','Omari Musa','Natale Lino Stracuzzi',
                    'David Krucoff','Amir Lowery','John Cheeks'),
      party = c('DEMOCRAT','LIBERTARIAN','INDEPENDENT','SOCIALIST WORKERS','DC STATEHOOD - GREEN PARTY',
                'INDEPENDENT','INDEPENDENT','INDEPENDENT'),
      candidatevotes = c(281831,9678,7628,6702,5553,5017,5001,2914)),
    data.table(
      state = 'District of Columbia',
      state_po = 'DC',
      state_fips = 11,
      district = 0,
      year = 2022,
      candidate = c('Eleanor Holmes Norton','Nelson Rimensnyder','Natale Stracuzzi','Bruce Major'),
      party = c('DEMOCRAT','REPUBLICAN','DC STATEHOOD - GREEN PARTY','LIBERTARIAN'),
      candidatevotes = c(174238,11701,9867,4003))
    ),
  fill = T
  )

geo3_lookup$state <- tolower(geo3_lookup$state)
geo3_lookup$party <- ifelse(grepl('GREEN',geo3_lookup$party),'GREEN',geo3_lookup$party)

# calculate national results 
nat.cols <- c('party','year')
geo3_lookup.nat <- geo3_lookup
geo3_lookup.nat$party <- 
  ifelse(geo3_lookup.nat$party %in% c('DEMOCRAT','REPUBLICAN','LIBERTARIAN','GREEN'),
         geo3_lookup.nat$party,
         'OTHER MINOR PARTIES')

geo3_lookup.nat <- geo3_lookup.nat[,lapply(.SD,sum),by = c(nat.cols),.SDcols = c('candidatevotes')]
geo3_lookup.nat$state = 'USA'

geo3_lookup <- rbindlist(list(geo3_lookup,geo3_lookup.nat),fill = T)
geo3_lookup$state <- tolower(geo3_lookup$state)
geo3_lookup$state_po <- ifelse(geo3_lookup$state=='usa','USA',geo3_lookup$state_po)
geo3_lookup$district <- ifelse(geo3_lookup$state=='usa',0,geo3_lookup$district)


# for geo3, we will need to augment the MIT results with `non voters`
# use the data from the daily kos to get the total number of voting 
# age citizens 

geo3_vap.2022 <- 
  fread(file = 'auxiliary_data/Daily Kos/House and Senate 5-Year American Community Survey Racial Demographics by Citizenship Status - 118th House—2022.csv')
geo3_vap.2022 <- geo3_vap.2022[,grepl('District|Citizen Voting Age Population \\(Hispanics/Latinos include any race\\)',unlist(geo3_vap.2022[1])),with=F]
names(geo3_vap.2022) <- c('district','voting.age.pop')
geo3_vap.2022 <- geo3_vap.2022[-c(1:2)]
geo3_vap.2022 <- 
  cbind(
    extract_district_info(geo3_vap.2022 $district ),
    voting.age.pop = 
      as.numeric(gsub(',','',geo3_vap.2022$voting.age.pop))
  )
# https://www.federalregister.gov/documents/2023/03/31/2023-06717/estimates-of-the-voting-age-population-for-2022
geo3_vap.2022 <- rbindlist(list(geo3_vap.2022,data.table(state = 'District of Columbia',district = 0,voting.age.pop = 547328)))
geo3_vap.2022$year = 2022
geo3_vap.2022$state_po <- usmap::fips_info()[,'abbr'][match(geo3_vap.2022$state,usmap::fips_info()[,'full'])]
geo3_vap.2022$state_fips <- as.numeric(usmap::fips_info()[,'fips'])[match(geo3_vap.2022$state,usmap::fips_info()[,'full'])]
geo3_vap.2022$state <- tolower(geo3_vap.2022$state)
geo3_vap.2022[state == 'nationwide']$ state <- 'usa'
geo3_vap.2022[state == 'usa']$ district <- 0
geo3_vap.2022[state == 'usa']$state_po <- 'USA'
geo3_vap.2022[state == 'usa']$state_fips <- NA

  
geo3_vap.2020 <- 
  fread(file = 'auxiliary_data/Daily Kos/House and Senate 5-Year American Community Survey Racial Demographics by Citizenship Status - 118th House—2020.csv')
geo3_vap.2020 <- geo3_vap.2020[,grepl('District|Citizen Voting Age Population \\(Hispanics/Latinos include any race\\)',unlist(geo3_vap.2020[1])),with=F]
names(geo3_vap.2020) <- c('district','voting.age.pop')
geo3_vap.2020 <- geo3_vap.2020[-c(1:2)]
geo3_vap.2020 <- 
  cbind(
    extract_district_info(geo3_vap.2020 $district ),
    voting.age.pop = 
      as.numeric(gsub(',','',geo3_vap.2020$voting.age.pop))
  )
# https://www.census.gov/library/stories/state-by-state/district-of-columbia-population-change-between-census-decade.html
geo3_vap.2020 <- rbindlist(list(geo3_vap.2020,data.table(state = 'District of Columbia',district = 0,voting.age.pop = 575161)))
geo3_vap.2020$state_po <- usmap::fips_info()[,'abbr'][match(geo3_vap.2020$state,usmap::fips_info()[,'full'])]
geo3_vap.2020$state_fips <- as.numeric(usmap::fips_info()[,'fips'])[match(geo3_vap.2020$state,usmap::fips_info()[,'full'])]
geo3_vap.2020$year = 2020
geo3_vap.2020$state <- tolower(geo3_vap.2020$state)
geo3_vap.2020[state == 'nationwide']$ state <- 'usa'
geo3_vap.2020[state == 'usa']$district <- 0
geo3_vap.2020[state == 'usa']$state_po <- 'USA'
geo3_vap.2020[state == 'usa']$state_fips <- NA


# bind 2020 and 2022
geo3_vap <- rbindlist(list(geo3_vap.2020,geo3_vap.2022),use.names = TRUE)


geo3_lookup_did.not.vote <- 
  geo3_lookup[,lapply(.SD,sum),
              by = c('state','district','year'),
              .SDcols = c('candidatevotes')]
geo3_lookup_did.not.vote <- 
  merge(
    geo3_lookup_did.not.vote,
    geo3_vap,
    by = c('state','district','year'),
    all=T
  )
geo3_lookup_did.not.vote <- 
  geo3_lookup_did.not.vote[complete.cases(geo3_lookup_did.not.vote)]
geo3_lookup_did.not.vote$party = 'ABSTAINED / DID NOT VOTE / STAYED HOME'
geo3_lookup_did.not.vote$candidatevotes <- 
  geo3_lookup_did.not.vote$voting.age.pop-geo3_lookup_did.not.vote$candidatevotes
  
geo3_lookup <- 
  rbindlist(
    list(
      geo3_lookup,
      geo3_lookup_did.not.vote[,!c('voting.age.pop')]
    ),
    fill=T
  )
geo3_lookup$candidate[is.na(geo3_lookup$candidate)] <- ''


# make votes into percentages
geo3_lookup[,candidatevotes:=round(100*candidatevotes/sum(candidatevotes),2),by = c('state','state_po','state_fips','district','year')]
geo3_lookup$candidatevotes <- paste0(geo3_lookup$candidatevotes,' %')

# # # GEO 2 LOOKUP
vote2020.state <- 
  fread(file = 'auxiliary_data/MIT Election Data and Science Lab/U.s President State-Level Returns 1976-2020/1976-2020-president.csv')

# we are interested in 2020 
vote2020.state <- vote2020.state[year == 2020]

# change party name 
names(vote2020.state)[names(vote2020.state)=='party_detailed'] <- 'party'

# we are interested in 2020 
vote2020.state <- vote2020.state[year=='2020']

#
vote2020.state$state <- tolower(vote2020.state$state)  # Convert the entire string to lowercase
vote2020.state$state <- gsub("\\b([a-z])", "\\U\\1", vote2020.state$state, perl=TRUE)  

# rename DC to match general nomenclature
vote2020.state$state[vote2020.state$state=='District Of Columbia'] <- 'District of Columbia'

# sum over the states 
geo2_lookup <- 
  vote2020.state[,
                    lapply(.SD,sum),
                    by = c('candidate','party','state','state_po','state_fips','year'),
                    .SDcols = c('candidatevotes')]

# calculate national results 
geo2_lookup.nat <- geo2_lookup
geo2_lookup.nat$candidate <- 
  ifelse(geo2_lookup.nat$candidate %in% c('BIDEN, JOSEPH R. JR','TRUMP, DONALD J.','JORGENSEN, JO','HAWKINS, HOWIE'),
         geo2_lookup.nat$candidate,
         'OTHER MINOR CANDIDATES')
geo2_lookup.nat$party <- ifelse(geo2_lookup.nat$candidate=='OTHER MINOR CANDIDATES','OTHER MINOR PARTIES',geo2_lookup.nat$party)
geo2_lookup.nat$party <- ifelse(geo2_lookup.nat$candidate=='HAWKINS, HOWIE','GREEN',geo2_lookup.nat$party)
geo2_lookup.nat$party <- ifelse(geo2_lookup.nat$candidate=='JORGENSEN, JO','LIBERTARIAN',geo2_lookup.nat$party)
geo2_lookup.nat$party <- ifelse(geo2_lookup.nat$party %in% c('DC STATEHOOD GREEN','GREEN-RAINBOW','PACIFIC GREEN'),'GREEN',geo2_lookup.nat$party)

geo2_lookup.nat <- geo2_lookup.nat[,lapply(.SD,sum),by = c('candidate','party','year'),.SDcols = c('candidatevotes')]
geo2_lookup.nat$state = 'usa'
geo2_lookup.nat$state_po = 'USA'
geo2_lookup.nat$state_fips = NA

geo2_lookup <- rbindlist(list(geo2_lookup,geo2_lookup.nat),fill = T)
geo2_lookup$state <- tolower(geo2_lookup$state)


# now add to this the voting age population for the state
geo2_vap <- geo3_vap[,lapply(.SD,sum),by = c('state','state_po','state_fips','year'),.SDcols = c('voting.age.pop')]
geo2_vap <- geo2_vap[year == 2020]

geo2_lookup_did.not.vote <- 
  geo2_lookup[,lapply(.SD,sum),
              by = c('state','year'),
              .SDcols = c('candidatevotes')]

geo2_lookup_did.not.vote <- 
  merge(
    geo2_lookup_did.not.vote,
    geo2_vap,
    by = c('state','year'),
    all=T
  )
geo2_lookup_did.not.vote <- 
  geo2_lookup_did.not.vote[complete.cases(geo2_lookup_did.not.vote)]
geo2_lookup_did.not.vote$party = 'VOTING AGE PEOPLE WHO ABSTAINED / DID NOT VOTE / STAYED HOME'
geo2_lookup_did.not.vote$candidatevotes <- 
  geo2_lookup_did.not.vote$voting.age.pop-geo2_lookup_did.not.vote$candidatevotes

geo2_lookup <- 
  rbindlist(
    list(
      geo2_lookup,
      geo2_lookup_did.not.vote[,!c('voting.age.pop')]
    ),
    fill=T
  )

geo2_lookup$candidate[is.na(geo2_lookup$candidate)] <- ''

# make votes into percentages
geo2_lookup[,candidatevotes:=round(100*candidatevotes/sum(candidatevotes),2),by = c('state','state_po','state_fips','year')]
geo2_lookup$candidatevotes <- paste0(geo2_lookup$candidatevotes,' %')