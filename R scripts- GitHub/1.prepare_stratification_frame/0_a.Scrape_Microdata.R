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
library(dplyr)
library(tidycensus)
library(parallel)
library(pbapply)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# load crosswalk to go from PUMA to CD
# first transform puma 10 to puma 20
puma_10.20 <- 
  read.delim(
    "https://www2.census.gov/geo/docs/maps-data/data/rel2020/puma520/tab20_puma520_puma510_natl.txt", 
    header = TRUE, 
    sep = "|", 
    stringsAsFactors = FALSE,
    colClasses = c(OID_PUMA5_20="character",GEOID_PUMA5_20="character",OID_PUMA5_10="character",GEOID_PUMA5_10="character"),
    )
puma_10.20 <- puma_10.20[complete.cases(puma_10.20),]
puma_10.20$STATEFP <- as.numeric(substr(puma_10.20$GEOID_PUMA5_20,1,2))
puma_10.20$PUMA5CE <- as.numeric(substr(puma_10.20$GEOID_PUMA5_10,3,nchar(puma_10.20$GEOID_PUMA5_10)))
puma_10.20$PUMA5CE.20 <- as.numeric(substr(puma_10.20$GEOID_PUMA5_20,3,nchar(puma_10.20$GEOID_PUMA5_20)))

# assign census tract 
# 2020 Census Tract to 2020 PUMA Relationship File
puma_tract <- 
  read.table(
    file = 'https://www2.census.gov/geo/docs/maps-data/data/rel2020/2020_Census_Tract_to_2020_PUMA.txt', 
    header = TRUE, 
    sep = ",", 
    stringsAsFactors = FALSE
  )
# Convert the columns to character format
puma_tract$STATEFP <- as.character(puma_tract$STATEFP)
puma_tract$COUNTYFP <- as.character(puma_tract$COUNTYFP)
puma_tract$TRACTCE <- as.character(puma_tract$TRACTCE)

# Use sprintf to format with leading zeros
puma_tract$TRACT <- 
  sprintf(
    "%02s%03s%06s",
    puma_tract$STATEFP,
    puma_tract$COUNTYFP,
    puma_tract$TRACTCE
  )

# load tract to zip crosswalk
library(openxlsx)
tract_zip <- 
  openxlsx::read.xlsx(
    xlsxFile = 'auxiliary_data/HUD-USPS ZIP Crosswalk Files/TRACT_ZIP_062023.xlsx'
  )
names(tract_zip)[grepl('RATIO',names(tract_zip))] <- paste0('TRACT_ZIP_',names(tract_zip)[grepl('RATIO',names(tract_zip))])
# TRACT: This represents the Census Tract code. Census tracts are small, relatively stable geographic areas that usually have a population between 1,200 and 8,000. They are located within counties and are used by local agencies and organizations for planning and analysis.
# ZIP: This is the ZIP code. ZIP codes are postal codes used by the United States Postal Service (USPS) to efficiently deliver mail.
# USPS_ZIP_PREF_CITY: This is the preferred city name for the ZIP code as designated by the USPS.
# USPS_ZIP_PREF_STATE: This is the state abbreviation for the preferred city of the ZIP code.
# RES_RATIO: This represents the residential ratio. It's the proportion of residential addresses in the census tract that fall within the ZIP code. A ratio of 1 would mean that all residential addresses in the tract are within the ZIP code, while a ratio of 0.5 would mean half of them are.
# BUS_RATIO: This is the business ratio. Similar to the RES_RATIO, it represents the proportion of business addresses in the census tract that fall within the ZIP code.
# OTH_RATIO: This stands for "other ratio". It represents the proportion of other types of addresses (not residential or business) in the census tract that are within the ZIP code.
# TOT_RATIO: This is the total ratio, which represents the overall proportion of all addresses (residential, business, and other) in the census tract that fall within the ZIP code.
puma_zip <- merge(puma_tract,tract_zip,by = 'TRACT')

# load zip to congressional district crosswalk
zip_cd <- 
  openxlsx::read.xlsx(
    xlsxFile = 'auxiliary_data/HUD-USPS ZIP Crosswalk Files/ZIP_CD_062023.xlsx'
  )
names(zip_cd)[grepl('RATIO',names(zip_cd))] <- paste0('ZIP_CD_',names(zip_cd)[grepl('RATIO',names(zip_cd))])
zip_cd$ZIP_CD_TOT_RATIO = as.numeric(zip_cd$ZIP_CD_TOT_RATIO)

# merge to create puma cd crosswalk
puma_cd <- merge(puma_zip,zip_cd ,by = c('ZIP','USPS_ZIP_PREF_CITY','USPS_ZIP_PREF_STATE'))

puma_cd[puma_cd $PUMA5CE=='2000' & puma_cd$USPS_ZIP_PREF_STATE=='AL',]

zip_cd[zip_cd$ZIP==36855,]

# # # EXTRACT ACS MICRODATA FROM CENSUS API
# load census API key 
census_api_key("...")

# get variables available for latest acs
pums_variables <- as.data.table(pums_variables)

# select identifiers for relevant variables
vars <-
  c("TYPEHUGQ",
    "CIT",
    "PUMA",
    "ST",
    "REGION",
    "SEX",
    "RAC1P",
    "HISP",
    "AGEP",
    "MAR",
    "HINCP",
    "SCHL",
    "ACCESSINET"
  )

# use most current 5-years aggregated acs values at the time of estimation 
most_current_vars_test <-
  (
    pums_variables$year == 2021 &
    pums_variables$survey == "acs5" &
    pums_variables$var_code %in% vars
  )

# download data
microdata <- data.table()

for(s in usmap::fips_info()$abbr[-c(1:(which(usmap::fips_info()$abbr=='CO')-1))]) {

  max_retries <- 50
  retry_count <- 0
  success <- FALSE
  
  while (!success && retry_count < max_retries) {
    tryCatch({
      microdata_temp <- get_pums(
        variables = vars,
        survey = unique(pums_variables$survey[most_current_vars_test]),
        year = as.numeric(unique(pums_variables$year[most_current_vars_test])),
        state = s
      )
      success <- TRUE
    }, error = function(e) {
      retry_count <- retry_count + 1
      Sys.sleep(5)  # Wait for 5 minutes before retrying
      cat("Error encountered:", e$message, "\nRetrying...\n")
    })
  }
  
  if (!success) {
    cat("Failed to retrieve data after", max_retries, "attempts.\n")
  }
  

microdata_temp  <- as.data.table(microdata_temp)[,..vars]

# # # recode variables in the same format as they will be in the survey
names.microdata <- names(microdata_temp)
for(i in names.microdata){

  if(i == 'PUMA'){
    # remove leading 0s
    microdata_temp$PUMA <- sub("^0+", "",microdata_temp$PUMA)
    
    # initialise columns 
    microdata_temp$PUMA20 <- NA
    microdata_temp$TRACT <- NA
    microdata_temp$ZIP <- NA
    microdata_temp$CD <- NA
    
    # Pre-compute the fips_info
    fips_info_data <- usmap::fips_info()
    
    # Create a function to get the USPS_ZIP_PREF_STATE for a given ST
    get_state_abbr <- function(st_value) {
      fips_info_data$abbr[match(st_value, fips_info_data$fips)]
    }
    
    # function to allocate micro geographies
    assign_row <- function(k) {
  
      current_state_abbr <- get_state_abbr(microdata_temp$ST[k])
      
      # (0) allocate PUMA 20 to a PUMA 10  
      subset_data.PUMA <- 
        puma_10.20[
          puma_10.20$PUMA5CE == microdata_temp$PUMA[k] & 
          puma_10.20$STATEFP == as.numeric(microdata_temp$ST[k]),
        ]
      subset_data.PUMA <- unique(subset_data.PUMA)
      
      PUMA20 <-sample(subset_data.PUMA$PUMA5CE.20, size = 1)

      # (1) allocate PUMA 20 to a census tract 
      subset_data.TRACT <- puma_cd[puma_cd$PUMA5CE == PUMA20 & 
                               puma_cd$USPS_ZIP_PREF_STATE == current_state_abbr, 
                             c('PUMA5CE', 'TRACT')]
      subset_data.TRACT <- unique(subset_data.TRACT)
      
      # sometimes tracts have nobody living in it 
      nobody_lives_here <- TRUE
      while(nobody_lives_here){
      TRACT <-sample(subset_data.TRACT$TRACT, size = 1)
      
      # (2) allocate census tract to zip code
      subset_data.ZIP <- puma_cd[puma_cd$TRACT == TRACT & 
                                 puma_cd$USPS_ZIP_PREF_STATE == current_state_abbr, 
                                 c('TRACT', 'ZIP', 'TRACT_ZIP_RES_RATIO')]
      subset_data.ZIP <- unique(subset_data.ZIP)
      nobody_lives_here <- ifelse(sum(subset_data.ZIP$TRACT_ZIP_RES_RATIO)<=0,TRUE,FALSE)
      }
      ZIP <-sample(subset_data.ZIP$ZIP, size = 1,prob = subset_data.ZIP$TRACT_ZIP_RES_RATIO)
      
      # (3) allocate zip code to congressional district
      subset_data.CD <- puma_cd[puma_cd$ZIP == ZIP & 
                             puma_cd$USPS_ZIP_PREF_STATE == current_state_abbr, 
                             c('ZIP', 'CD', 'ZIP_CD_RES_RATIO')]
      subset_data.CD <- unique(subset_data.CD)
      
      # extremely rarely a PUMA on the border may have individuals belonging to a congressional district in another state
      subset_data.CD <- subset_data.CD[substr(subset_data.CD$CD,1,2)==microdata_temp$ST[k],]
      
      CD <-sample(subset_data.CD$CD, size = 1,prob = subset_data.CD$ZIP_CD_RES_RATIO)
      
      # return
      return(list(CD = CD, ZIP = ZIP, TRACT = TRACT, PUMA20 = PUMA20))
      
    }
    
    # Determine the number of cores
    num_cores <- detectCores() - 1
    
    # Create a cluster object
    cl <- makeCluster(num_cores)
    clusterEvalQ(cl, library(data.table))
    
    # Export all the objects that your loop will need to each of the cluster nodes
    clusterExport(cl, list("microdata_temp", "puma_10.20", "puma_cd", "assign_row","sample","get_state_abbr","fips_info_data"))
    
    # Use 'parLapply' to apply the 'assign_row' function to each row of your data
    results <- pblapply(1:nrow(microdata_temp), assign_row, cl = cl)
    
    # Stop the cluster once we are done
    stopCluster(cl)

    # Now, 'results' is a list of return values from 'process_row'. You would
    # then combine these back into your 'microdata_temp' data frame.
    for(l in c("PUMA20", "TRACT", "ZIP", "CD")){
        microdata_temp[[l]] <- sapply(seq_along(results),function(k){unlist(results[[k]])[l]})
    }
}else{
  
  # once CD has been allocated , translate the other variables 
  var_test <- pums_variables$var_code == i & most_current_vars_test

  if(unique(pums_variables$data_type[var_test]) == "num"){next}
  
  microdata_temp[[i]] <-
    pums_variables$val_label[var_test][
      match(
        microdata_temp[[i]],
        pums_variables$val_max[var_test]
      )]
  }
}

print( microdata_temp)
print(paste0('completed cleaning of microdata for state: ',s))

# bind
microdata <- rbindlist(list(microdata,microdata_temp))

# save microdata
save(microdata,file = 'generated_data/stratification_frame/SF_microdata.RData',compress = TRUE)

# completed state s
print(
  paste(
  'completed state',s,'.',
  round(100*which( usmap::fips_info()$abbr==s)/length(usmap::fips_info()$abbr),2),
  '% completed...'
  )
)

}
