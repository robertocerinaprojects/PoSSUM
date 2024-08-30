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
library(readxl)
library(questionr)
library(mltools)
library(bayestestR)

# performance measures
rmse <- function(y,yhat){sqrt(mean((y - yhat)^2))}
bias <- function(yhat,y){mean((yhat-y))}
cover <- function(y_lo,y_up,y){mean(y_lo < y & y_up > y)}

# utils for parallel 
library(ohenery)
library(foreach)
library(doParallel)

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

# load spatial utils
library(sf)
library(sp)
library(rgdal)

library(INLA)
library(geostan)
library(ape)

#' compute scaling factor for adjacency matrix, accounting for differences in spatial connectivity #'
#' @param C connectivity matrix
#'
#' @details
#'
#' Requires the following packages:
#'
#' library(Matrix)
#' library(INLA);
#' library(spdep)
#' library(igraph)
#'
#' @source
#'
#' Morris, Mitzi (2017). Spatial Models in Stan: Intrinsic Auto-Regressive Models for Areal Data. <ht #'
scale_c <- function(C) {
  geometric_mean <- function(x) exp(mean(log(x)))
  N = dim(C)[1]
  Q =  Diagonal(N, rowSums(C)) - C
  Q_pert = Q + Diagonal(N) * max(diag(Q)) * sqrt(.Machine$double.eps)
  Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,N),e=0))
  scaling_factor <- geometric_mean(Matrix::diag(Q_inv))
  return(scaling_factor)
}

# Function to plot nodes and edges (if you want, over a map from a shapefile)
plot_network <- function(shape, 
                         nb_object,
                         plot.over.map = T,
                         map.border.col = adjustcolor('grey',0.5), 
                         map.plygon.col = adjustcolor('lightcoral',0.10),
                         map.border.lwd = 0.1,
                         node.cex=0.01,
                         edge.lwd=0.25,
                         node.pch=0,
                         edge.lty=1,
                         xlim = NA,
                         ylim = NA,
                         edge.col = adjustcolor('blue',0.25)){
  
  if(any(is.na(xlim))){
    
    n.poly <- length(shape[['geometry']])
    
    x_range <- 
      range(
        sapply(X = 1:n.poly,
               FUN = 
                 function(x){
                   range(
                     st_coordinates(
                       shape[['geometry']][[x]]
                     )[, "X"] 
                   )
                 }
        ) )
    
    xlim <- x_range 
  }
  
  if(any(is.na(ylim))){
    
    n.poly <- length(shape[['geometry']])
    
    y_range <-   
      range(
        sapply(X = 1:n.poly,
               FUN = 
                 function(x){
                   range(
                     st_coordinates(
                       shape[['geometry']][[x]]
                     )[, "Y"] 
                   )
                 }
        ) )
    
    ylim <- y_range
    
  }
  
  if(plot.over.map){
    connectedplot = 
      plot(st_geometry(shape),
           border = map.border.col,
           lwd = map.border.lwd,
           col = map.plygon.col,
           xlim = xlim ,
           ylim = ylim
      )
  }
  
  
  
  
  connectedplot = 
    plot(nb_object,
         st_coordinates(st_centroid(shape)),
         add = plot.over.map,
         col = edge.col,
         cex = node.cex,
         lwd = edge.lwd,
         pch = node.pch,
         lty = edge.lty,
         xlim = xlim ,
         ylim = ylim)
}

# # # SETUP SPATIAL STRUCTURE

# Load maps by state for the congressional districts 
base_url <- "https://www2.census.gov/geo/tiger/TIGER_RD18/LAYER/CD/"

# Define the folder where you want to save the downloaded files
dest_folder <- "auxiliary_data/US Census Bureau 2022 CD 118 Shape/"

# Create a vector with the folder names (or codes)
area_folders <- usmap::fips_info()$fips # Add all area codes here

# Loop over the folder names, download and merge the shapefiles
shape_list <- list()
for (area in area_folders) {

  suffix <- ''
  if(area == '08'){suffix <- '_whole_block'}

    
  # Construct the download URL
  file_url <- paste0(base_url, "tl_rd22_", area, "_cd118",suffix,".zip")
  
  # Construct the destination file path
  dest_file <- paste0(dest_folder, area, "_cd118.zip")
  
  # Download the file
  download.file(file_url, dest_file, mode="wb")
  
  # Unzip the file
  unzip(dest_file, exdir = dest_folder)

  # Load the shapefile - assuming the shapefile has the same name structure
  shape_name <- paste0(dest_folder, "tl_rd22_", area, "_cd118.shp")
  shape_list[[area]] <- sf::read_sf(dsn = shape_name)
  
}

# merge into national
shape_US <- shape_list[[1]]
for(area in area_folders[-1]){
  shape_US <- rbind(shape_US,shape_list[[area]])
}

# add state names 
shape_US$state_simple <- usmap::fips_info()$full[match(shape_US$STATEFP20, usmap::fips_info()$fips)]

# Ensure the geometry column is correctly identified
shape_US <- st_set_geometry(shape_US, "geometry")

# Remove Z and M dimensions from geometries
shape_US <- st_zm(shape_US)

# Function to clean geometries and extract only POLYGON or MULTIPOLYGON
clean_geometry <- function(geom) {
  valid_geom <- st_make_valid(geom)
  # Extract POLYGON or MULTIPOLYGON if it turns into GEOMETRYCOLLECTION
  if("GEOMETRYCOLLECTION" %in% class(valid_geom)) {
    return(st_collection_extract(valid_geom, "POLYGON|MULTIPOLYGON"))
  }
  return(valid_geom)
}

# Clean geometries and preserve the structure as an sf object
shape_US <- st_sf(data.table(shape_US), geometry = lapply(st_geometry(shape_US), clean_geometry))

# Split the data for Nebraska and Maine
#states_to_keep <- shape_US[shape_US$state_simple %in% c("Nebraska", "Maine"), ]

# Split the rest of the states
#rest_of_states <- shape_US[!shape_US$state_simple %in% c("Nebraska", "Maine"), ]
rest_of_states <- shape_US

# Aggregate geometries by state, preserving state names
aggregated_states_list <- lapply(split(rest_of_states, rest_of_states$state_simple), function(x) {
  # Union cleaned geometries
  unioned_geom <- st_union(st_sfc(x$geometry))
  # Create a simple feature collection with the correct CRS
  st_sf(state = unique(x$state), geometry = st_sfc(unioned_geom, crs = st_crs(x)))
})

# Combine the list into a single sf object
aggregated_states_sf <- do.call(rbind, aggregated_states_list)
#names(aggregated_states_sf)[names(aggregated_states_sf)=='state'] <- 'state_electoral.college'
names(aggregated_states_sf)[names(aggregated_states_sf)=='state'] <- 'state_simple'
  
# Combine Nebraska and Maine with the aggregated states

#states_to_keep$state_electoral.college <- 
#  paste(states_to_keep$state,
#        ifelse(as.numeric(states_to_keep$CD118FP)==1,'1st',
#               ifelse(as.numeric(states_to_keep$CD118FP)==2,'2nd',
#                      ifelse(as.numeric(states_to_keep$CD118FP)==3,'3rd',
#                             NA))))

# shape_US <- rbind(states_to_keep[c('state_electoral.college','geometry')], aggregated_states_sf)
shape_US <- aggregated_states_sf
rownames(shape_US) <- shape_US$state_simple

save(shape_US,file = 'generated_data/shapefile/states_shape.RData',compress = TRUE)

# load shapefile
load(file = 'generated_data/shapefile/states_shape.RData')

# get neighborhood objects - these will be useful later for the fitting of the model
nb <- spdep::poly2nb(shape_US, row.names=shape_US$state_simple, queen=TRUE)

# get adjacency matrix
C <- shape2mat(shape_US, style = "B")

# prep icar data
icar.data <- geostan::prep_icar_data(C)

# plot US map
pdf(file = 'generated_plots/maps/states.map_network.structure.pdf',width = 5,height = 5)
plot_network(
  shape = shape_US,
  nb_object = nb,
  map.border.lwd = 0.1,
  map.border.col = 'black',
  edge.col = 'blue',
  node.pch = 16,
  node.cex = 0.1,
  xlim = c(-170,-70)
)
dev.off()

# load linenar predictor - we will use the actual area-level election results 
# as area-level predictors - and nothing else
load(file = "generated_data/predictor/area_level.RData")

# we need to get the average vote share for nebraska, 
# as we need to aggregate over state_simple for ANES data
# Note: The average is good enough for predicting past-vote in Nebraska, 
# but when we rake we will rake back to the CDs. 

# # # 2020 linear pred 

SDcols <- c('pct.2020.votes.D','pct.2020.votes.R',
            'pct.2020.votes.L','pct.2020.votes.G',
            'pct.2020.votes.stay.home','pct.2020.votes.OTHER')
by = c('state_simple','state_abbreviation')

pred <- 
predictor_area_level[,
                     lapply(.SD,mean),
                     by=by,
                     .SDcols =SDcols]

# scale predictors for
temp <-pred[,
            lapply(.SD,function(x){ (x - mean(x))/sd(x) } ),
            .SDcols =
              c(names(
                pred[,!c(
                  "state_simple",
                  "state_abbreviation"
                ) ] ) ) ]

names(temp ) <- paste('LP_',names(temp),sep = '')

pred_scale.20 <-
  cbind(
    pred[,c("state_simple","state_abbreviation")],
    temp
  )


# # # 2022 linear pred 

SDcols <- c('pct.2022.votes.D','pct.2022.votes.R',
            'pct.2022.votes.L','pct.2022.votes.G',
            'pct.2022.votes.stay.home','pct.2022.votes.OTHER')
by = c('state_simple','state_abbreviation')

pred <- 
  predictor_area_level[,
                       lapply(.SD,mean),
                       by=by,
                       .SDcols =SDcols]

# scale predictors 
temp <-pred[,
            lapply(.SD,function(x){ (x - mean(x))/sd(x) } ),
            .SDcols =
              c(names(
                pred[,!c(
                  "state_simple",
                  "state_abbreviation"
                ) ] ) ) ]

names(temp ) <- paste('LP_',names(temp),sep = '')

pred_scale.22 <-
  cbind(
    pred[,c("state_simple","state_abbreviation")],
    temp
  )

pred_scale <- 
  merge(pred_scale.20,pred_scale.22,by = c('state_simple','state_abbreviation'))

pred_scale_int <- pred_scale
ps <- 
  gsub('LP_pct.2022.votes.','',
       names(pred_scale.22[,!grepl('state_',names(pred_scale.22)),with=F])
       )
for(j in ps){
  for(i in ps){
    tmp <- 
      data.table(
        product = 
          pred_scale[[paste0('LP_pct.2020.votes.',j)]]*
          pred_scale[[paste0('LP_pct.2022.votes.',i)]]
      )
    names(tmp)[names(tmp)=='product'] <- paste0('LP_pct.2020.2022.votes.',j,'_AND_',i)
    pred_scale_int <- cbind(pred_scale_int,tmp)
} }

# # # merge linear predictor with stratification frame

load(file = 'generated_data/stratification_frame/SF.RData')

# change levels of income to be ordered in factor form
SF$hh_income <-
  as.factor(as.character(unlist(SF$hh_income)))

levels(SF$hh_income)[
  which(levels(SF$hh_income)=="[min, 25000)")
] = '1.[min, 25000)'
levels(SF$hh_income)[
  which(levels(SF$hh_income)=="[25000, 50000)")
] = '2.[25000, 50000)'
levels(SF$hh_income)[
  which(levels(SF$hh_income)=="[50000, 75000)")
] = '3.[50000, 75000)'
levels(SF$hh_income)[
  which(levels(SF$hh_income)=="[75000, 100000)")
] = '4.[75000, 100000)'
levels(SF$hh_income)[
  which(levels(SF$hh_income)=="[100000, max]")
] = '5.[100000, max]'

SF$hh_income <-
  as.factor(as.character(unlist(SF$hh_income)))

SF$age_bins <- 
  as.factor(as.character(unlist(SF$age_bins)))

levels(SF$age_bins)[
  which(levels(SF$age_bins)=="18-24")
] = '1.18-24'
levels(SF$age_bins)[
  which(levels(SF$age_bins)=="25-34")
] = '2.25-34'
levels(SF$age_bins)[
  which(levels(SF$age_bins)=="35-44")
] = '3.35-44'
levels(SF$age_bins)[
  which(levels(SF$age_bins)=="45-54")
] = '4.45-54'
levels(SF$age_bins)[
  which(levels(SF$age_bins)=="55-64")
] = '5.55-64'
levels(SF$age_bins)[
  which(levels(SF$age_bins)=="65+")
] = '6.65+'

SF$age_bins <-
  as.factor(as.character(unlist(SF$age_bins)))

# focus on the variables we are going to actually rake by so we reduce dataset size
vars <- c("state_abbreviation","state_simple","state_electoral.college","region",
         "gender","ethnicity","age_bins",
         "college_grad","hh_income")
SF <- SF[,lapply(.SD,sum),by = c(vars),.SDcols = c('N')]

# merge linear predictor with AI_poll data
survey_object.clean <- 
  fread(file = "generated_data/stratification_frame/CCES_2022_clean.csv",stringsAsFactors = T)
# ensure empty spots are actually missing values 
for(j in names(survey_object.clean)){
  if(class(survey_object.clean[[j]])=='factor'){
  levels(survey_object.clean[[j]]) <- ifelse(levels(survey_object.clean[[j]])=="",NA,levels(survey_object.clean[[j]]))
  } }

# # # augment with area level
SF <-
  merge(
    SF,
    pred_scale_int,
    all.x = TRUE,
    by = c('state_abbreviation','state_simple')
  )

# only work with complete cases
survey_object.clean <- survey_object.clean [complete.cases(survey_object.clean )]

survey_object.clean <-
  merge(
    survey_object.clean,
    pred_scale_int,
    all.x = TRUE,
    by = c('state_abbreviation')
  )

# # # 
# # # 
# # # Stan model for 2020 vote
# # # 
# # # 
V <- as.factor(survey_object.clean$vote2020)

tmp <- one_hot(survey_object.clean,cols = c('vote2020','vote2022'),dropCols = TRUE)
survey_object.clean_multinom <-
  tmp[,lapply(.SD,
              function(x){ 
                sum(x*weight_post)
                }),
      by = c(names(tmp)[
        !names(tmp) %in% 
          c(names(tmp)[grepl('vote2020|vote2022',names(tmp))],
            'weight_post','weight','internet','user_id','party_code','marital_status')]),
      .SDcols = names(tmp)[grepl('vote2020|vote2022',names(tmp))]]

V_star <- as.matrix(survey_object.clean_multinom [,grepl('vote2020',names(survey_object.clean_multinom )),with=F])

N <- dim(survey_object.clean_multinom)[1]

Z <- survey_object.clean_multinom[,grepl('LP_',names(survey_object.clean_multinom)),with=F]
Z <- Z[,paste0('LP_pct.2020.votes.',gsub(' ','.',levels(V))),with=F]

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
    'state_simple'
  )

# make all possible 2-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
  temp <- paste(SF[[v1]],'_AND_',SF[[v2]],sep = '')
  SF <- cbind(SF,temp)
  names(SF)[which(names(SF)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
  
  temp <- paste(survey_object.clean_multinom[[v1]],'_AND_',survey_object.clean_multinom[[v2]],sep = '')
  survey_object.clean_multinom <- cbind(survey_object.clean_multinom,temp)
  names(survey_object.clean_multinom)[which(names(survey_object.clean_multinom)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
  
} }

# make all possible 3-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
    for(v3 in var.name.list[-c(1:which(var.name.list==v2))]){
      
    temp <- paste(SF[[v1]],'_AND_',SF[[v2]],'_AND_',SF[[v3]],sep = '')
    SF <- cbind(SF,temp)
    names(SF)[which(names(SF)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
    
    temp <- paste(survey_object.clean_multinom[[v1]],'_AND_',survey_object.clean_multinom[[v2]],'_AND_',survey_object.clean_multinom[[v3]],sep = '')
    survey_object.clean_multinom <- cbind(survey_object.clean_multinom,temp)
    names(survey_object.clean_multinom)[which(names(survey_object.clean_multinom)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
    
    } } }

var.name.list <- c(var.name.list,names(SF)[grepl('AND',names(SF)) & !grepl('LP',names(SF))])

model.var.name.list <-
  c(
    'gend',
    'ethn',
    'educ',
    'age_',
    'inco',
    'stat'
  )

int_name_list <- c()
for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    
    int_name_list <- c(int_name_list,paste(v1,'_AND_',v2,sep=''))
  }}

for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    for(v3 in model.var.name.list[-c(1:which(model.var.name.list==v2))]){
      
    int_name_list <- c(int_name_list,paste(v1,'_AND_',v2,'_AND_',v3,sep=''))
  }}}

model.var.name.list <- c(model.var.name.list,int_name_list)

id_list <- list()

for(i in 1:length(var.name.list)) {

temp <-
    get_id(
      dt = survey_object.clean_multinom,
      SF = SF,
      var.name = var.name.list[i],
      model.var.name = model.var.name.list[i]
    )
  
  id_list <- append(id_list, temp)
}

# # # (A) SET UP STAN DATA LIST
train_data_list =
  list(
    
    # dependent var
    N = N,
    V_star = V_star,
    J = nlevels(V),
    V_labs = levels(V),

    # fixed effects
    Z = Z
  )

# add individual-level random effects
train_data_list <- append(train_data_list , id_list)

# # # Add relevant covariates to the model 
skeleton <- readLines(con = 'stan_models/model_ai.survey_SmoothMrsP_skeleton.stan')

# add data
skeleton <- 
  gsub(
    '// DATA RANDOM EFFECTS',
    paste0(
      '// DATA RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  int ',j,'_id[N];\n',
              '  int<lower = 1> ',j,'_N;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add params
skeleton <- 
  gsub(
    '// PARAMS RANDOM EFFECTS',
    paste0(
      '// PARAMS RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  matrix[',j,'_N,J] xi_',j,';\n',
              '  real<lower=0> xi_',j,'_sd;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add transformed parameters
skeleton <- 
  gsub(
    '// TRANSFORMED PARAMS EFFECTS',
    paste0(
      '// TRANSFORMED PARAMS EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  matrix[',j,'_N,J] xi_',j,'_s = xi_',j,'*xi_',j,'_sd;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add linear predictor
skeleton <- 
  gsub(
    '// LINEAR PREDICTOR RANDOM EFFECTS',
    paste0(
      '// LINEAR PREDICTOR RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '      xi_',j,'_s[',j,'_id,j] +\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add priors
skeleton <- 
  gsub(
    '// PRIORS RANDOM EFFECTS',
    paste0(
      '// PRIORS RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  to_vector(xi_',j,') ~ std_normal();\n',
              '  xi_',j,'_sd ~ std_normal();\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# save edited skeleton
write.table(
  x = paste0(skeleton,collapse = '\n'),
  file = 'stan_models/model_ai.survey_SmoothMrsP.stan',
  quote = FALSE,sep = "",row.names = FALSE,eol = '',col.names = '')

# # # RUN STAN MODEL
NUTS_time_list <- c()
NUTS_time <- Sys.time()

fit_object <-
  stan(
    file = "stan_models/model_ai.survey_SmoothMrsP.stan",
    data = train_data_list,
    iter = 1000,
    warmup = 950,
    refresh = 1,
    thin = 4,
    cores = 8,
    chains = 8,
    control = list(max_treedepth = 15,adapt_delta = 0.8),
    verbose = TRUE
  )

NUTS_time_temp <- Sys.time() - NUTS_time
NUTS_time_list <- c(NUTS_time_list,NUTS_time_temp)

# clean trash
gc()

# save object
save(fit_object,
     file = 'generated_data/stratification_frame/extension_model/fit_object.vote2020.RData',
     compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/fit_object.vote2020.RData' )
# save time taken to fit
save(NUTS_time_list,
     file = 'generated_data/stratification_frame/extension_model/NUTS_time.vote2020.RData',
    compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/NUTS_time.vote2020.RData' )
# save training list
save(train_data_list,
     file = 'generated_data/stratification_frame/extension_model/training_data.vote2020.RData',
     compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/training_data.vote2020.RData' )


# # # Get predicted posterior draws for each cell
pars_hat <- rstan::extract(fit_object, inc_warmup = FALSE)

# calculate latent-scale predicitons
xi_id <- 
  names(pars_hat)[
    grepl('xi',names(pars_hat)) &
      substr(names(pars_hat),
             nchar(names(pars_hat))-1,
             nchar(names(pars_hat)))=='_s']
x_id <- substr(xi_id,4,nchar(xi_id)-2)

SF_model_name.map <- 
  data.table(
    var.name.list = var.name.list,
    model.var.name.list = model.var.name.list
  )

S = dim(pars_hat$beta_s)[1]

# identify cells with id
SF$cell_id <- 1:dim(SF)[1]

for(s in 1:S){
  mu_star <- 
    sapply(1:train_data_list$J,
           function(j){
             xi_pred <- 
               sapply(
                 x_id,
                 function(k){
                   pars_hat[[paste0('xi_',k,'_s')]][s,,j][
                     as.integer(
                       as.factor(
                         SF[[ SF_model_name.map$var.name.list[match(k,SF_model_name.map$model.var.name.list)] ]]
                       ) ) ]
                 } )
             
             mu_s_hat <- 
               pars_hat$beta_s[s,j] + 
               SF[,colnames(train_data_list$Z),with=F][[j]]*pars_hat$eta_s[s,j] +
               rowSums(  xi_pred )
             
             return(mu_s_hat)
           } )
  
  colnames(mu_star) <- train_data_list$V_labs
  
  # sum to one
  pi_temp <- as.data.table(t(apply(mu_star,1,smax)))
  
  pi_star <- 
    data.table(
      cell_id = unique(SF$cell_id),
      N = SF$N,
      pi_temp
    )
  
  pi_star <- 
    melt(
      pi_star,
      id.vars = c('cell_id','N'),
      value.name = paste0('vote2020.dist.sim',s),
      variable.name = 'vote2020')
  
  pi_star[[paste0('vote2020.dist.sim',s)]] <- pi_star$N*pi_star[[paste0('vote2020.dist.sim',s)]]
  pi_star <- pi_star[,!'N']
  
  if(s==1){
    pi_star_list <- pi_star
  }
  if(s>1){
    pi_star_list <- merge(pi_star_list,pi_star,by = c('cell_id','vote2020'))

  }
  
  print(paste('simulated elections:',round(100*s/S,1),'%'))
  
}

SF <- merge(SF,pi_star_list,by = c('cell_id'),all = TRUE)
SF <- SF[,!grepl("_AND_",names(SF)),with=F]

# # # 
# # # 
# # # Stan model for 2022 vote
# # # 
# # # 
V <- as.factor(as.character(unlist(survey_object.clean$vote2022)))

tmp <- one_hot(survey_object.clean,cols = c('vote2022'),dropCols = TRUE)
survey_object.clean_multinom <-
  tmp[,lapply(.SD,
              function(x){ 
                sum(x*weight_post)
              }),
      by = c(names(tmp)[
        !names(tmp) %in% 
          c(names(tmp)[grepl('vote2022',names(tmp))],
            'weight_post','weight','internet','user_id','party_code','marital_status')]),
      .SDcols = names(tmp)[grepl('vote2022',names(tmp))]]

V_star <- as.matrix(survey_object.clean_multinom [,grepl('vote2022',names(survey_object.clean_multinom )),with=F])

N <- dim(survey_object.clean_multinom)[1]

Z <- survey_object.clean_multinom[,grepl('LP_',names(survey_object.clean_multinom)),with=F]
Z <- Z[,paste0('LP_pct.2022.votes.',gsub(' ','.',levels(V))),with=F]

# these have to be in the same order
var.name.list <-
  c(
    'gender',
    'ethnicity',
    'college_grad',
    'age_bins',
    'hh_income',
    'state_simple',
    'vote2020'
  )

# make all possible 2-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
    temp <- paste(SF[[v1]],'_AND_',SF[[v2]],sep = '')
    SF <- cbind(SF,temp)
    names(SF)[which(names(SF)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
    
    temp <- paste(survey_object.clean_multinom[[v1]],'_AND_',survey_object.clean_multinom[[v2]],sep = '')
    survey_object.clean_multinom <- cbind(survey_object.clean_multinom,temp)
    names(survey_object.clean_multinom)[which(names(survey_object.clean_multinom)=='temp')] <- paste(v1,'_AND_',v2,sep = '')
    
  } }

# make all possible 3-way interactions 
for(v1 in var.name.list[-length(var.name.list)]){
  for(v2 in var.name.list[-c(1:which(var.name.list==v1))]){
    for(v3 in var.name.list[-c(1:which(var.name.list==v2))]){
      
      temp <- paste(SF[[v1]],'_AND_',SF[[v2]],'_AND_',SF[[v3]],sep = '')
      SF <- cbind(SF,temp)
      names(SF)[which(names(SF)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
      
      temp <- paste(survey_object.clean_multinom[[v1]],'_AND_',survey_object.clean_multinom[[v2]],'_AND_',survey_object.clean_multinom[[v3]],sep = '')
      survey_object.clean_multinom <- cbind(survey_object.clean_multinom,temp)
      names(survey_object.clean_multinom)[which(names(survey_object.clean_multinom)=='temp')] <- paste(v1,'_AND_',v2,'_AND_',v3,sep = '')
      
    } } }

var.name.list <- c(var.name.list,names(SF)[grepl('AND',names(SF)) & !grepl('LP',names(SF))])

model.var.name.list <-
  c(
    'gend',
    'ethn',
    'educ',
    'age_',
    'inco',
    'stat',
    'vote'
  )

int_name_list <- c()
for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    
    int_name_list <- c(int_name_list,paste(v1,'_AND_',v2,sep=''))
  }}

for(v1 in model.var.name.list[-length(model.var.name.list)]){
  for(v2 in model.var.name.list[-c(1:which(model.var.name.list==v1))]){
    for(v3 in model.var.name.list[-c(1:which(model.var.name.list==v2))]){
      
      int_name_list <- c(int_name_list,paste(v1,'_AND_',v2,'_AND_',v3,sep=''))
    }}}

model.var.name.list <- c(model.var.name.list,int_name_list)

id_list <- list()

for(i in 1:length(var.name.list)) {
  
  temp <-
    get_id(
      dt = survey_object.clean_multinom,
      SF = SF,
      var.name = var.name.list[i],
      model.var.name = model.var.name.list[i]
    )
  
  id_list <- append(id_list, temp)
}


# # # (A) SET UP STAN DATA LIST
train_data_list =
  list(
    
    # dependent var
    N = N,
    V_star = V_star,
    J = nlevels(V),
    V_labs = levels(V),
    
    # fixed effects
    Z = Z
  )

# add individual-level random effects
train_data_list <- append(train_data_list , id_list)

# # # Add relevant covariates to the model 
skeleton <- readLines(con = 'stan_models/model_ai.survey_SmoothMrsP_skeleton.stan')

# add data
skeleton <- 
  gsub(
    '// DATA RANDOM EFFECTS',
    paste0(
      '// DATA RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  int ',j,'_id[N];\n',
              '  int<lower = 1> ',j,'_N;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add params
skeleton <- 
  gsub(
    '// PARAMS RANDOM EFFECTS',
    paste0(
      '// PARAMS RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  matrix[',j,'_N,J] xi_',j,';\n',
              '  real<lower=0> xi_',j,'_sd;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add transformed parameters
skeleton <- 
  gsub(
    '// TRANSFORMED PARAMS EFFECTS',
    paste0(
      '// TRANSFORMED PARAMS EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  matrix[',j,'_N,J] xi_',j,'_s = xi_',j,'*xi_',j,'_sd;\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add linear predictor
skeleton <- 
  gsub(
    '// LINEAR PREDICTOR RANDOM EFFECTS',
    paste0(
      '// LINEAR PREDICTOR RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '      xi_',j,'_s[',j,'_id,j] +\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# add priors
skeleton <- 
  gsub(
    '// PRIORS RANDOM EFFECTS',
    paste0(
      '// PRIORS RANDOM EFFECTS\n',
      paste0(
        sapply(
          model.var.name.list,
          function(j){
            paste0(
              '  to_vector(xi_',j,') ~ std_normal();\n',
              '  xi_',j,'_sd ~ std_normal();\n\n'
            ) } ),
        collapse = '' ) 
    ),
    skeleton
  )

# save edited skeleton
write.table(
  x = paste0(skeleton,collapse = '\n'),
  file = 'stan_models/model_ai.survey_SmoothMrsP.stan',
  quote = FALSE,sep = "",row.names = FALSE,eol = '',col.names = '')

# # # RUN STAN MODEL
NUTS_time_list <- c()
NUTS_time <- Sys.time()

fit_object <-
  stan(
    file = "stan_models/model_ai.survey_SmoothMrsP.stan",
    data = train_data_list,
    iter = 1000,
    warmup = 950,
    refresh = 1,
    thin = 4,
    cores = 8,
    chains = 8,
    control = list(max_treedepth = 15,adapt_delta = 0.8),
    verbose = TRUE
  )

NUTS_time_temp <- Sys.time() - NUTS_time
NUTS_time_list <- c(NUTS_time_list,NUTS_time_temp)

# clean trash
gc()

# save object
save(fit_object,
     file = 'generated_data/stratification_frame/extension_model/fit_object.vote2022.RData',
     compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/fit_object.vote2022.RData' )
# save time taken to fit
save(NUTS_time_list,
     file = 'generated_data/stratification_frame/extension_model/NUTS_time.vote2022.RData',
     compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/NUTS_time.vote2022.RData' )
# save training list
 save(train_data_list,
     file = 'generated_data/stratification_frame/extension_model/training_data.vote2022.RData',
     compress = T)
# load( file = 'generated_data/stratification_frame/extension_model/training_data.vote2022.RData' )

# # # Get predicted posterior draws for each cell
pars_hat <- rstan::extract(fit_object, inc_warmup = FALSE)

# calculate latent-scale predicitons
xi_id <- 
  names(pars_hat)[
    grepl('xi',names(pars_hat)) &
      substr(names(pars_hat),
             nchar(names(pars_hat))-1,
             nchar(names(pars_hat)))=='_s']
x_id <- substr(xi_id,4,nchar(xi_id)-2)

SF_model_name.map <- 
  data.table(
    var.name.list = var.name.list,
    model.var.name.list = model.var.name.list
  )

S = dim(pars_hat$beta_s)[1]

# replace cells_id to account for expanded vote2020 var
SF$cell_id <- 1:dim(SF)[1]

for(s in 1:S){
  mu_star <- 
    sapply(1:train_data_list$J,
           function(j){
             xi_pred <- 
               sapply(
                 x_id,
                 function(k){
                   pars_hat[[paste0('xi_',k,'_s')]][s,,j][
                     as.integer(
                       as.factor(
                         SF[[ SF_model_name.map$var.name.list[match(k,SF_model_name.map$model.var.name.list)] ]]
                       ) ) ]
                 } )
             
             mu_s_hat <- 
               pars_hat$beta_s[s,j] + 
               SF[,colnames(train_data_list$Z),with=F][[j]]*pars_hat$eta_s[s,j] +
               rowSums(  xi_pred )
             
             return(mu_s_hat)
           } )
  
  colnames(mu_star) <- train_data_list$V_labs
  
  # sum to one
  pi_temp <- as.data.table(t(apply(mu_star,1,smax)))
  
  pi_star <- 
    data.table(
      cell_id = SF$cell_id,
      N = SF[[paste0('vote2020.dist.sim',s)]],
      pi_temp
    )
  
  pi_star <- 
    melt(
      pi_star,
      id.vars = c('cell_id','N'),
      value.name = paste0('vote2022.dist.sim',s),
      variable.name = 'vote2022')
  
  pi_star[[paste0('vote2022.dist.sim',s)]] <- pi_star$N*pi_star[[paste0('vote2022.dist.sim',s)]]
  pi_star <- pi_star[,!'N']
  
  if(s==1){
    pi_star_list <- pi_star
  }
  if(s>1){
    pi_star_list <- merge(pi_star_list,pi_star,by = c('cell_id','vote2022'))
    
  }
  
  print(paste('simulated elections:',round(100*s/S,1),'%'))
  
}

SF <- merge(SF,pi_star_list,by = c('cell_id'),all = TRUE)

save(SF,file = 'generated_data/stratification_frame/extension_model/raw_SF_extended.RData',compress = TRUE)

# # # Let's check performance of un-raked predicted counts to get a sense 
# of how reasonable the predictions from the model are

# set vars
yr <- '2022'
vote_var <- paste0('vote',yr)
sim.vars <- names(SF)[grepl('vote2022.dist.sim',names(SF))]

# get counts for voters at the area level 
counts <- 
  SF[,lapply(.SD,sum)
     ,by = c('state_electoral.college',vote_var),
     .SDcols = c(sim.vars)]

# handle stay home -- the observed are net of stay home for the parties 
counts.home <- counts
counts.home[,state_totals := sum(vote2022.dist.sim1),by = c('state_electoral.college')]
# handle the rest 
counts.party <- counts[counts[[vote_var]]!='stay home']
counts.party[,state_totals := sum(vote2022.dist.sim1),by = c('state_electoral.college')]

# get percentages at the are level
for(s in sim.vars){
  counts.home[[s]] <- 100*counts.home[[s]]/counts.home$state_totals
  counts.party[[s]] <- 100*counts.party[[s]]/counts.party$state_totals
}

# get obesrved percentages
obs <- 
  melt(
    predictor_area_level[,
                         c(paste0('pct.',yr,'.votes.D'),
                           paste0('pct.',yr,'.votes.R'),
                           paste0('pct.',yr,'.votes.L'),
                           paste0('pct.',yr,'.votes.OTHER'),
                           paste0('pct.',yr,'.votes.G'),
                           paste0('pct.',yr,'.votes.stay.home'),
                           'state_electoral.college'),
                         with = F],
    id.vars = 'state_electoral.college',
    variable.name = paste0('vote',yr),
    value.name = 'observed'
    )
# clean outcome var
obs[[vote_var]] <- 
  gsub('stay.home','stay home',gsub(paste0('pct.',yr,'.votes.'),'',obs[[vote_var]]))

# merge with predictions
obs.home <- 
  merge(
    counts.home[counts[[vote_var]]=='stay home'],
    obs,
    by = c('state_electoral.college',vote_var),
    all.x = TRUE)

obs.party <- 
  merge(counts.party,
        obs,
        by = c('state_electoral.college',vote_var),
        all.x = TRUE)

pvo <- rbindlist(list(obs.party,obs.home))

pvo$MAP <- apply(pvo[,grepl('vote2022.dist.',names(pvo)),with=F],1,function(x){bayestestR::map_estimate(x)$MAP_Estimate})
pvo$lo <- apply(pvo[,grepl('vote2022.dist.',names(pvo)),with=F],1,function(x){quantile(x,0.05)})
pvo$hi <- apply(pvo[,grepl('vote2022.dist.',names(pvo)),with=F],1,function(x){quantile(x,0.95)})

par(mfrow = c(2,3))
for(j in unique(pvo[[vote_var]])){
y <- pvo[pvo[[vote_var]]==j]$observed
x <- pvo[pvo[[vote_var]]==j]$MAP
x.lo <- pvo[pvo[[vote_var]]==j]$lo
x.hi <- pvo[pvo[[vote_var]]==j]$hi

plot(x = x, y = y, 
     ylim = c(0,100),xlim = c(0,100),
     main = j)
segments(x0 = x.lo,y0 = y,x1 = x.hi,y1 = y)
abline(0,1)
legend(
  'topleft',
  legend = c(
    paste0('bias:',round(bias(y = y,yhat = x),3)),
    paste0('rmse:',round(rmse(y = y,yhat = x),3)),
    paste0('cor:',round(cor(y = y,x = x),3)),
    paste0('cover:',round(cover(y_lo = x.lo,y_up = x.hi, y = y),3))
  )
)
}

