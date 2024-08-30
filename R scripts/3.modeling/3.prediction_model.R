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
# ensure warnings are not converted into errors
options(warn = 1)
# set work directory
setwd(dir = "~/Desktop/Mechanical Pollster/")

# Update latest predictor
# source('R scripts/1.prepare_stratification_frame/0_c.Predictor_Update.R')

# clean workspace
rm(list = ls())
# clean garbadge
gc()

# construct shapefile
source('R scripts/3.modeling/0.prepare_shape.file.R')

# clean workspace
rm(list = ls())
# clean garbadge
gc()

# load useful package
library(data.table)
library(mltools)

# useful plotting utils
# plotting utils
library(vioplot)
# plot function 
plot.effect_violin <- 
  function(x,var.levels,cols.light,cols.dark,reference = NULL,...){
    # select the effect of interest 
    
    x <- as.matrix(x)
    
    effects <- 
      lapply(
        1:dim(x)[2],
        function(k){
          x[,k]
        }
      )
    
    # plot violins
    vioplot(
      effects,
      names = var.levels, 
      col = cols.light,
      side = "right",
      ...
    )
    # add reference
    if(!is.null(reference)){
      abline(h = reference,lty = 2)
    }
  }

# load stan and options
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# majority class function for aggregating categorical variables 
majority_class <- function(x) {
  # Remove missing values
  non_missing <- x[!is.na(x)]
  
  # Check if all values are missing
  if (length(non_missing) == 0) {
    return(NA)
  }
  
  # Calculate the frequency of each class
  class_freq <- table(non_missing)
  
  # Find the class(es) with the maximum frequency
  max_classes <- names(class_freq)[class_freq == max(class_freq)]
  
  # If there is a tie, pick one class at random
  if (length(max_classes) > 1) {
    return(sample(max_classes, 1))
  }
  
  # Otherwise, return the majority class
  return(max_classes)
}

# load shapefile 
load(file = 'generated_data/shapefile/electoral.college_shape.RData')

# get neighborhood objects - these will be useful later for the fitting of the model
nb <- spdep::poly2nb(shape_US, row.names=shape_US$state_electoral.college, queen=TRUE)

# get adjacency matrix
C <- shape2mat(shape_US, style = "B")

# prep icar data
icar.data <- geostan::prep_icar_data(C)

# load linear predictor
load(file = "generated_data/predictor/area_level.RData")
load(file = "generated_data/predictor/area.date_level.RData")

# add competitiveness in 2020
predictor_area_level$`pct.2020.RD_margin` <- 
  predictor_area_level$pct.2020.votes.R - 
  predictor_area_level$pct.2020.votes.D
predictor_area_level$`abs.pct.2020.RD_margin` <- 
  abs(predictor_area_level$`pct.2020.RD_margin`)
# and competitiveness in 2022
predictor_area_level$`pct.2022.RD_margin` <- 
  predictor_area_level$pct.2022.votes.R - 
  predictor_area_level$pct.2022.votes.D
predictor_area_level$`abs.pct.2022.RD_margin` <- 
  abs(predictor_area_level$`pct.2022.RD_margin`)


# aggregate predictors 
OMNI_pred <- 
  merge(
    predictor_area_level,
    predictor_area.date_level,
    by = c('state_simple'),
    all=TRUE,
    allow.cartesian = TRUE
    )


# # add fielding candidate indicator (this would not have been in the building of the predictor prior)
load(file = 'auxiliary_data/Wikipedia/Ballot access context/ballot_access.RData')
# Ensure unique column names in ballot_access
names(ballot_access) <- make.names(names(ballot_access), unique = TRUE)

# Convert Electoral.votes to numeric
ballot_access$Electoral.votes <- as.numeric(as.character(ballot_access$Electoral.votes))

# Convert Yes/No columns to numeric (1/0)
cols_to_convert <- !grepl('elector', colnames(ballot_access), ignore.case = TRUE)
ballot_access[, (colnames(ballot_access)[cols_to_convert]) := 
                lapply(.SD, function(x) as.numeric(factor(x, levels = c("No", "Yes"))) - 1),
              .SDcols = cols_to_convert]

# Rename columns containing 'candidate' or 'party' to start with 'field_'
candidate_party_cols <- grepl('candidate|party', names(ballot_access))
names(ballot_access)[candidate_party_cols] <- paste0('field_', names(ballot_access)[candidate_party_cols])

# if all 1s or all 0s, drop
var.0.test <- apply(ballot_access,2,function(x){var(x)==0})
if(any(var.0.test)){
  ballot_access <- ballot_access[,-which(var.0.test),with=F]
}

# Merge OMNI_pred with ballot_access by state_abbreviation and State...electors
OMNI_pred <- merge(OMNI_pred, ballot_access, by.x = 'state_abbreviation', by.y = 'State...electors', all.x = TRUE)

# Calculate the days to election (dte) by computing the difference between the current date and the date in OMNI_pred
OMNI_pred$dte <- 
  floor(
    as.numeric(
      difftime(
        Sys.Date(), 
        as.Date(OMNI_pred$date, "%Y-%m-%d"), 
        units = 'days')
      )
    )

# Select the columns to scale (excluding date, dte, state_simple, state_electoral.college, and state_abbreviation)
cols_to_scale <- 
  setdiff(
    names(OMNI_pred), 
    c("date", "dte", "state_simple", "state_electoral.college", "state_abbreviation")
    )

# Scale the selected columns 
scaled_temp <- 
  OMNI_pred[, 
            lapply(.SD, 
                   function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
                   ), 
            .SDcols = cols_to_scale]

# Rename the scaled columns by adding the 'LP_' prefix
names(scaled_temp) <- paste0('LP_', names(scaled_temp))

# Combine the scaled columns with the original unscaled columns in OMNI_pred
OMNI_pred_scale <- 
  cbind(
    OMNI_pred[, c("date", "dte", "state_electoral.college", "state_simple", "state_abbreviation")], 
    scaled_temp
    )

# # # Load Stratification frame 
load(file = "generated_data/stratification_frame/SF_extended.RData")

# drop previous LP (used for extending frame, will be added back in later along
# with more robust predictor) 
SF_extended <- SF_extended[,!grepl('LP_',names(SF_extended)),with=F]

# # # drop interaction at this stage -- if necessary we will re-build them below 
SF_extended <- SF_extended[,!grepl('AND',names(SF_extended)),with=F]

# merge linear predictor with AI_poll data
load(file = "generated_data/survey_object/survey_object.clean.RData")

# identify `latest` date for which to make a prediction
SF_extended$date <- min(c(max(OMNI_pred_scale$date),max(survey_object.clean$date)))

# # # augment frame with context
SF_extended <-
  merge(
    SF_extended,
    OMNI_pred_scale[date<=unique(SF_extended$date)],
    all.x = TRUE,
    by = c('state_electoral.college','date','state_abbreviation','state_simple')
  )

# latest data points depend on the last predictor update
survey_object.clean <- survey_object.clean[date<=unique(SF_extended$date)]

# clean favourability measures
survey_object.clean$fav_harris <- as.factor(survey_object.clean$fav_harris)
levels(survey_object.clean$fav_harris)[
  levels(survey_object.clean$fav_harris) %in%
    c('1.Very unfavourable','2.Somewhat unfavourable')
] <- 'Unfavourable'
levels(survey_object.clean$fav_harris)[
  levels(survey_object.clean$fav_harris) %in%
    c('3.Somewhat favourable','4.Very favourable')
] <- 'Favourable'

survey_object.clean$fav_trump <- as.factor(survey_object.clean$fav_trump)
levels(survey_object.clean$fav_trump)[
  levels(survey_object.clean$fav_trump) %in%
    c('1.Very unfavourable','2.Somewhat unfavourable')
] <- 'Unfavourable'
levels(survey_object.clean$fav_trump)[
  levels(survey_object.clean$fav_trump) %in%
    c('3.Somewhat favourable','4.Very favourable')
] <- 'Favourable'

survey_object.clean$fav_jdvance <- as.factor(survey_object.clean$fav_jdvance)
levels(survey_object.clean$fav_jdvance)[
  levels(survey_object.clean$fav_jdvance) %in%
    c('1.Very unfavourable','2.Somewhat unfavourable')
] <- 'Unfavourable'
levels(survey_object.clean$fav_jdvance)[
  levels(survey_object.clean$fav_jdvance) %in%
    c('3.Somewhat favourable','4.Very favourable')
] <- 'Favourable'

survey_object.clean$fav_walz <- as.factor(survey_object.clean$fav_walz)
levels(survey_object.clean$fav_walz)[
  levels(survey_object.clean$fav_walz) %in%
    c('1.Very unfavourable','2.Somewhat unfavourable')
] <- 'Unfavourable'
levels(survey_object.clean$fav_walz)[
  levels(survey_object.clean$fav_walz) %in%
    c('3.Somewhat favourable','4.Very favourable')
] <- 'Favourable'

survey_object.clean$fav_kennedy <- as.factor(survey_object.clean$fav_kennedy)
levels(survey_object.clean$fav_kennedy)[
  levels(survey_object.clean$fav_kennedy) %in%
    c('1.Very unfavourable','2.Somewhat unfavourable')
] <- 'Unfavourable'
levels(survey_object.clean$fav_kennedy)[
  levels(survey_object.clean$fav_kennedy) %in%
    c('3.Somewhat favourable','4.Very favourable')
] <- 'Favourable'


# identify variables of interest 
model.vars <- c("gender",
                "ethnicity",
                "age_bins",
                "college_grad",
                "hh_income",
                'vote2020'
                )
dep.vars <- c('vote2024',
              'moi',
              'fav_harris',
              'fav_trump',
              'fav_jdvance',
              'fav_walz',
              'fav_kennedy')

# get speculation values of these 
spec.vars <- 
  unlist(
  sapply(c(model.vars,dep.vars),function(x){
    names(survey_object.clean)[
      grepl('speculation',names(survey_object.clean)) & 
              grepl(x,names(survey_object.clean))]
  }) 
  )

# let's try to aggregate these 3 estimates of 2020 vote 
survey_object.clean$vote2020 <- 
  apply(
    survey_object.clean[,c('vote2020.quota','vote2020.demo','vote2020.bgres','vote2020.bgnores','vote2020.ind')],
    1,
    majority_class
  )
survey_object.clean$speculation...vote2020 <- 
  apply(
    survey_object.clean[,grepl('speculation...vote2020',names(survey_object.clean)),with=F],
    1,
    function(x){
      mean(as.numeric(as.character(unlist(x))),na.rm=T)
    }
  )

survey_object.clean$vote2022 <- 
  apply(
    survey_object.clean[,c('vote2022.quota','vote2022.demo','vote2022.bgres','vote2022.bgnores','vote2022.ind')],
    1,
    majority_class
  )
survey_object.clean$speculation...vote2022 <- 
  apply(
    survey_object.clean[,grepl('speculation...vote2022',names(survey_object.clean)),with=F],
    1,
    function(x){
      mean(as.numeric(as.character(unlist(x))),na.rm=T)
    }
  )

survey_object.clean$vote2024 <- 
  apply(
    survey_object.clean[,c('vote2024.demo','vote2024.bgnores','vote2024.ind')],
    1,
    majority_class
  )
survey_object.clean$speculation...vote2024 <- 
  apply(
    survey_object.clean[,grepl('speculation...vote2024',names(survey_object.clean)),with=F],
    1,
    function(x){
      mean(as.numeric(as.character(unlist(x))),na.rm=T)
      }
    )

# plot speculation levels 
spec.vars  <- c(spec.vars[!grepl('vote|kennedy|moi|grad|fav',spec.vars)],'speculation...vote2020','speculation...vote2024')

pdf(file = 'generated_plots/speculation/speculation_overview.pdf',width = 10,height = 7.5)
# Custom function to plot separate violins for each category within each column
plot.effect_violin_by_category <- 
  function(
    spec.data, 
    cat.data, 
    spec.vars, 
    cat.vars, 
    cols.light, 
    reference = NULL, 
    shade_color = "lightblue", 
    shade_alpha = 0.2, 
    ylab.alt,
    ...){
  
    par(mfrow = c(3,2),oma = c(1,1,1,0),mar = c(5,4,4.5,1))
    
  # Loop through each variable
  for (i in seq_along(spec.vars)) {
    
    # Extract the current speculation and categorical data
    current_spec <- as.numeric(as.character(unlist(spec.data[[spec.vars[i]]])))
    current_cat <- cat.data[[cat.vars[i]]]
    
    # Find unique categories
    unique_cats <- unique(current_cat[!is.na(current_cat)])
    cats.order <- order(unique_cats)
    
    # Prepare a list to store data for each category
    effects <- lapply(unique_cats[cats.order], function(cat) current_spec[current_cat == cat])
  
    
    # plot grid 
    plot(
      y = 0.5:(length(unique_cats )+0.5),
      x = 0.5:(length(unique_cats )+0.5),
      ylim = c(0,100),
      xlim= c(0.5,length(unique_cats)+0.5),
      pch = NA,
      xaxt = 'n',
      yaxt = 'n',
      xlab = '',
      ylab = '',
      bty = 'n'
    )
    abline(
      v =  seq(0,length(unique_cats ),by = 1),
      h =  seq(0,100,by = 10),
      col = adjustcolor(col = 'darkgrey',0.15)
    )
    par(new = TRUE)
    # Plot violins
    vioplot(
      effects,
      names = unique_cats[cats.order],
      col = cols.light,
      side = "right",
      main = gsub('speculation...', '', gsub('gender','sex',spec.vars[i])), # Title for each plot
      ylab = '',
      ylim = c(0,100),
      xlim= c(0.5,length(unique_cats)+0.5),
      ...
    )
    # alternative ylab to avoid overlap
    axis(
      side = 2, 
      at = 50,
      tick = FALSE,
      line = FALSE,
      labels = ylab.alt ,
      las = 3,
      outer = NA,padj = -2.5
    )
    
    # Add light shading for the area above 80
    ylims <- par("usr")[3:4]
    rect(
      xleft = par("usr")[1],
      xright = par("usr")[2],
      ybottom = 80,
      ytop = ylims[2],
      col = adjustcolor(shade_color, shade_alpha),
      border = NA
    )
    
    
    # Add reference line if provided
    if (!is.null(reference)) {
      abline(h = reference, lty = 2)
    }
    
    # Add text annotations showing the percentage of values > 80 for each category
    text(
      y = 90,
      x = 1:length(unique_cats),
      labels = round(sapply(effects, function(x) 100 * mean(x > 80, na.rm = TRUE)), 2),
      srt = 45,
      adj = 1, pos = 2
    )
  }
}

# Assuming you have the spec.data and cat.data data frames ready
plot.effect_violin_by_category(
  spec.data = survey_object.clean[, ..spec.vars],
  cat.data = survey_object.clean[, gsub('speculation...', '', spec.vars),with=F],
  spec.vars = spec.vars,
  cat.vars = gsub('speculation...', '', spec.vars),
  cols.light = adjustcolor('darkgrey', 0.8),
  reference = 80,
  las = 3,
  ylab.alt = 'S',
  shade_color = "lightcoral",
  shade_alpha = 0.1
)
dev.off()

# add low speculation mode 
# 0-20 (Low speculation): The user data provides clear and direct information relevant to the title. (e.g., explicit mention in the profile or tweets)
# 21-40 (Moderate-low speculation): The user data provides indirect but strong indicators relevant to the title. (e.g., context from multiple sources within the profile or tweets)
# 41-60 (Moderate speculation): The user data provides some hints or partial information relevant to the title. (e.g., inferred from user interests or indirect references)
# 61-80 (Moderate-high speculation): The user data provides limited and weak indicators relevant to the title. (e.g., very subtle hints or minimal context)
# 81-100 (High speculation): The user data provides no or almost no information relevant to the title. (e.g., assumptions based on very general information)
SPECULATION_FLAG = FALSE
EXTEND_FRAME_FLAG = FALSE

for(speculation in c('High','Moderate')){
  
  if(speculation == 'Moderate' & SPECULATION_FLAG == FALSE){
    for(j in spec.vars){
      survey_object.clean[[gsub('speculation...','',j)]] <- 
        as.factor(
          ifelse(
            as.numeric(as.character(unlist(survey_object.clean[[j]])))>80,
            NA,
            as.character(unlist(survey_object.clean[[gsub('speculation...','',j)]]))
          )
        )
    }
    SPECULATION_FLAG <- TRUE
  }

for(dep.var in c('vote2024','moi','fav_harris','fav_trump','fav_walz','fav_jdvance')){

# filter users to analyse
survey_object.clean.dep <- survey_object.clean[,c('date','state_electoral.college','state_simple',model.vars,dep.var),with=F]

# drop missing values
survey_object.clean.dep <- survey_object.clean.dep[complete.cases(survey_object.clean.dep)]

# # # augment with context
survey_object.clean.dep <-
  merge(
    survey_object.clean.dep,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_electoral.college','state_simple','date')
  )


# get number of observations
N <- dim(survey_object.clean.dep)[1]

# vote choice
V <- as.factor(survey_object.clean.dep[[dep.var]])

if(dep.var== 'vote2024'){
# define linear predictors for each choice
LP_labs <- 
  list(
    D = c('LP_pct.2020.votes.D'),
    R = c('LP_pct.2020.votes.R','LP_abs.pct.2020.RD_margin'),
    K = c('LP_field_Robert.F..Kennedy.Jr...who.is.not.affiliated.with.any.political.party'),
    G = c('LP_pct.2020.votes.G','LP_field_Jill.Stein..the.Green.Party.candidate'),
    L = c('LP_pct.2020.votes.L','LP_field_Chase.Oliver..the.Libertarian.Party.candidate'),
    W = c('LP_field_Dr..Cornel.West..who.is.not.affiliated.with.any.political.party'),
    `stay home` = c('LP_pct.2020.votes.stay.home')
  )
LP_labs <- LP_labs[levels(V)]

P <- sapply(LP_labs,length)
Z <- survey_object.clean.dep[,unique(unlist(LP_labs)),with=F]
Z <- as.data.table(apply(Z ,2,function(x){ifelse(is.na(x),0,x)}))
# to give it to stan , it mast be balanced in dimensions 
pad_vector <- function(vec, x, value = 99) {
  # Check if the vector length is less than x
  if (length(vec) < x) {
    # Calculate how many 99s need to be added
    padding <- rep(value, x - length(vec))
    # Append valuess to the end of the vector
    vec <- c(vec, padding)
  }
  return(vec)
}
Z_id <- t(sapply(LP_labs,function(x){ pad_vector(which(names(Z) %in% x),x = max(P)) }))
}

if(dep.var == 'moi'){
  # vote choice
  V <- 
    as.factor(
      ifelse(
        survey_object.clean.dep$moi==majority_class(survey_object.clean.dep$moi),
        paste0('1.',majority_class(survey_object.clean.dep$moi)),
        as.character(unlist(survey_object.clean.dep$moi))
      )
    )
  
  gen.moi.pred <-
    c('LP_pct.2020.votes.D','LP_abs.pct.2020.RD_margin','LP_pct.2020.votes.T',
      'LP_pct.rurality.rural',
      'LP_pct.religion.evangelical.and.lds',
      'LP_pct.college_grad..Has.a.College.Degree',
      "LP_pct.region..Northeast","LP_pct.region..South","LP_pct.region..West",
      "LP_pct.ethnicity..Asian","LP_pct.ethnicity..Black","LP_pct.ethnicity..Hispanic"
    )
  
  # define linear predictors for each choice
  LP_labs <- list()
  for(i in levels(V)){ LP_labs[[i]] <- gen.moi.pred}
  LP_labs <- LP_labs[levels(V)]
  
  
  P <- sapply(LP_labs,length)
  Z <- survey_object.clean.dep[,unique(unlist(LP_labs)),with=F]
  Z <- as.data.table(apply(Z ,2,function(x){ifelse(is.na(x),0,x)}))
  # to give it to stan , it mast be balanced in dimensions 
  Z_id <- t(sapply(LP_labs,function(x){ pad_vector(which(names(Z) %in% x),x = max(P)) }))
}

if(grepl('fav',dep.var)){
  # vote choice
  V <- as.factor(survey_object.clean.dep[[dep.var]])
  
  gen.pred <-
    c('LP_pct.2020.votes.D','LP_abs.pct.2020.RD_margin','LP_pct.2020.votes.T',
      'LP_pct.rurality.rural',
      'LP_pct.religion.evangelical.and.lds',
      'LP_pct.college_grad..Has.a.College.Degree',
      "LP_pct.region..Northeast","LP_pct.region..South","LP_pct.region..West",
      "LP_pct.ethnicity..Asian","LP_pct.ethnicity..Black","LP_pct.ethnicity..Hispanic"
    )
  
  # define linear predictors for each choice
  LP_labs <- list()
  for(i in levels(V)){ LP_labs[[i]] <- gen.pred}
  LP_labs <- LP_labs[levels(V)]
  
  
  P <- sapply(LP_labs,length)
  Z <- survey_object.clean.dep[,unique(unlist(LP_labs)),with=F]
  Z <- as.data.table(apply(Z ,2,function(x){ifelse(is.na(x),0,x)}))
  # to give it to stan , it mast be balanced in dimensions 
  Z_id <- t(sapply(LP_labs,function(x){ pad_vector(which(names(Z) %in% x),x = max(P)) }))
  
}


# area identifier
area_id <- match(survey_object.clean.dep$state_electoral.college,shape_US$state_electoral.college)
area_id <- ifelse(is.na(area_id),length(shape_US$state_electoral.college)+1,area_id)

# # # days-to-election id
# dte_id <- survey_object.clean.dep$dte + 1
# dte_N <- max(dte_id)
  
# function to build random effects
get_id <- function(dt,SF,var.name,model.var.name = NA){
  
  if( is.na(model.var.name) ){
    model.var.name <- var.name
  }
  
  id <-
    match(
      dt[[var.name]],
      levels(
        as.factor(
          SF[[var.name]]
        )
      )
    )
  
  
  N <-
    nlevels(
      as.factor(
        SF[[var.name]]
      )
    )
  
  object <-
    list(
      id = id,
      N = N
    )
  
  names(object) <- paste(model.var.name,names(object),sep="_")
  
  return(object)
}

# these have to be in the same order
var.name.list <-
  c(
    'gender',
    'ethnicity',
    'college_grad',
    'age_bins',
    'hh_income',
    'vote2020'
  )

# make all possible 2-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
    
    if(EXTEND_FRAME_FLAG==FALSE){
      temp <- paste(SF_extended[[v1]],'_AND_',SF_extended[[v2]],sep = '')
      SF_extended <- cbind(SF_extended,temp)
      names(SF_extended)[which(names(SF_extended)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
    }
    
    temp <- paste(survey_object.clean.dep[[v1]],'_AND_',survey_object.clean.dep[[v2]],sep = '')
    survey_object.clean.dep <- cbind(survey_object.clean.dep,temp)
    names(survey_object.clean.dep)[which(names(survey_object.clean.dep)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
    
  } }

# make all possible 3-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
    for(v3 in var.name.list[-c(1:which(var.name.list==v2))]){
      
      if(EXTEND_FRAME_FLAG==FALSE){
        temp <- paste(SF_extended[[v1]],'_AND_',SF_extended[[v2]],'_AND_',SF_extended[[v3]],sep = '')
        SF_extended <- cbind(SF_extended,temp)
        names(SF_extended)[which(names(SF_extended)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
      }
      
      temp <- paste(survey_object.clean.dep[[v1]],'_AND_',survey_object.clean.dep[[v2]],'_AND_',survey_object.clean.dep[[v3]],sep = '')
      survey_object.clean.dep <- cbind(survey_object.clean.dep,temp)
      names(survey_object.clean.dep)[which(names(survey_object.clean.dep)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
      
    } } }

var.name.list <- c(var.name.list,names(SF_extended)[grepl('AND',names(SF_extended))])


# store augmented SF 
if( EXTEND_FRAME_FLAG == FALSE){
  save(
    SF_extended,
    file = 'generated_data/stratification_frame/SF_extended_scaled.pred.RData',
    compress = TRUE
  )
  
  EXTEND_FRAME_FLAG <- TRUE
}

model.var.name.list <-
  c(
    'gen',
    'eth',
    'edu',
    'age',
    'inc',
    'v20'
  )

int_name_list <- c()
for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    
    int_name_list <- c(int_name_list,paste(v1,'_',v2,sep=''))
  }}

for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    for(v3 in model.var.name.list[-c(1:which(model.var.name.list==v2))]){
      
      int_name_list <- c(int_name_list,paste(v1,'_',v2,'_',v3,sep=''))
    }}}

model.var.name.list <- c(model.var.name.list,int_name_list)

id_list  <- list()

for(i in 1:length(model.var.name.list)) {

temp <-
    get_id(
      dt = survey_object.clean.dep,
      SF = SF_extended,
      var.name = var.name.list[i],
      model.var.name = model.var.name.list[i]
    )
  
  id_list <- append(id_list, temp)
}



# # # (A) SET UP STAN DATA LIST

train_data_list =
  list(

    yes_area = which(area_id %in% 1:dim(shape_US)[1]),
    yes_area_N = sum(area_id %in% 1:dim(shape_US)[1]),
    no_area = which(!area_id %in% 1:dim(shape_US)[1]),
    no_area_N = sum(!area_id %in% 1:dim(shape_US)[1]),
    
    # dependent var
    N = N,
    V = as.integer(V),
    J = nlevels(V),
    V_labs = levels(V),
    
    # fixed effects
    Z = Z,
    P = P,
    P_sum = dim(Z)[2],
    P_max = max(P),
    Z_id = Z_id,
    
    # time varying effects
    # dte_N = dte_N,
    # dte_id = dte_id,
    
    # area varying effects
    area_id = area_id,
    area_N =  dim(shape_US)[1],
    
    kappa = icar.data$k,
    group_size = icar.data$group_size,
    group_idx = icar.data$group_idx,
    N_edges = icar.data$n_edges,
    node1 = icar.data$node1,
    node2 = icar.data$node2,
    comp_id = icar.data$comp_id,
    inv_sqrt_scaling_factor = icar.data$inv_sqrt_scale_factor
  )

  # add individual-level random effects
  train_data_list <- append(train_data_list , id_list)

    
  # RUN STAN MODEL
  
  fit_object <- list()
  NUTS_time <- Sys.time()
  
  fit_object <-
    stan(
      file = "stan_models/model_ai.survey_structured.stan",
      data = train_data_list,
      iter = 1500,
      warmup = 1250,
      refresh = 1,
      thin = 4,
      cores = 8,
      chains = 8,
      control = list(max_treedepth = 15,adapt_delta = 0.8),
      verbose = TRUE
    )

  NUTS_time <- Sys.time() - NUTS_time

  # clean trash
  gc()
  
  # save object
  save(fit_object,
       file = paste0('generated_data/mrp_model/',dep.var,'/speculation.',speculation,'/fit_object.RData'),
       compress = T)
  
  # save time taken to fit
  save(NUTS_time,
       file = paste0('generated_data/mrp_model/',dep.var,'/speculation.',speculation,'/NUTS_time.RData'),
       compress = T)
  
  # save training list
  save(train_data_list,
       file = paste0('generated_data/mrp_model/',dep.var,'/speculation.',speculation,'/training_data.RData'),
       compress = T)
  
  } }
  

