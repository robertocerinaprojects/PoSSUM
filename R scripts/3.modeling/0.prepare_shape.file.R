# load useful package
library(data.table)

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
#' Morris, Mitzi (2017). Spatial Models in Stan: Intrinsic Auto-Regressive Models for Areal Data.'
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

# # # SETUP SPATIAL STRUCTURE OF STATES

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
states_to_keep <- shape_US[shape_US$state_simple %in% c("Nebraska", "Maine"), ]

# Split the rest of the states
rest_of_states <- shape_US[!shape_US$state_simple %in% c("Nebraska", "Maine"), ]

# Aggregate geometries by state, preserving state names
aggregated_states_list <- lapply(split(rest_of_states, rest_of_states$state_simple), function(x) {
  # Union cleaned geometries
  unioned_geom <- st_union(st_sfc(x$geometry))
  # Create a simple feature collection with the correct CRS
  st_sf(state = unique(x$state), geometry = st_sfc(unioned_geom, crs = st_crs(x)))
})

# Combine the list into a single sf object
aggregated_states_sf <- do.call(rbind, aggregated_states_list)
names(aggregated_states_sf)[names(aggregated_states_sf)=='state'] <- 'state_electoral.college'

# Combine Nebraska and Maine with the aggregated states
states_to_keep$state_electoral.college <- 
  paste(states_to_keep$state,
        ifelse(as.numeric(states_to_keep$CD118FP)==1,'1st',
               ifelse(as.numeric(states_to_keep$CD118FP)==2,'2nd',
                      ifelse(as.numeric(states_to_keep$CD118FP)==3,'3rd',
                             NA))))

shape_US <- rbind(states_to_keep[c('state_electoral.college','geometry')], aggregated_states_sf)
rownames(shape_US) <- shape_US$state_electoral.college

save(shape_US,file = 'generated_data/shapefile/electoral.college_shape.RData',compress = TRUE)

# get neighborhood objects - these will be useful later for the fitting of the model
nb <- spdep::poly2nb(shape_US, row.names=shape_US$state_electoral.college, queen=TRUE)

# get adjacency matrix
C <- shape2mat(shape_US, style = "B")

# prep icar data
icar.data <- geostan::prep_icar_data(C)

# plot US map
pdf(file = 'generated_plots/maps/network.structure.pdf',width = 5,height = 5)
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