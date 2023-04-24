# Pre-processing of various data for the main script
# Copyright (C) 2020-2021  Dan Dediu
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

##
## Load the language data ####
##

d_colors <- read.table("./input_files/databases/data_colors.csv", header=TRUE, sep=";", quote='"', stringsAsFactors=FALSE); # comma-separated double-quoted CVS file

# Alternative view of the globe: all longitudes < 0 are "flipped" to positive values (for new databases: other and clics)
d_colors$longitude_180 <- d_colors$longitude 
d_colors$longitude     <- ifelse(d_colors$longitude <= 0, d_colors$longitude + 360, d_colors$longitude);

# Alternative view of the globe: all longitudes > 180 are "flipped" to negative values (for scirep paper)
d_colors$longitude_180 <- ifelse(d_colors$longitude > 180, d_colors$longitude -360, d_colors$longitude );


# Extract the geographic coordinates:
# For entries that have precisely the same geographic coordinates as other entries (if any), add 0.01 degrees (on the order of hundreds of meters/the scale of towns or villages):
#same_coords <- duplicated(d_colors[, c("latitude", "longitude_180")]); 
#d_colors$latitude[ same_coords ] <- d_colors$latitude[ same_coords ] + 0.01; 
#d_colors$longitude_180[ same_coords ] <- d_colors$longitude_180[ same_coords ] + 0.01; 

# Assemble together the location of the languages and of their families:
#d_coords <- d_colors[,c("glottocode_family", "latitude_family", "longitude_family", "longitude_180_family")];
#names(d_coords) <- c("glottocode", "latitude", "longitude", "longitude_180");
#d_coords <- rbind(d_colors[,c("glottocode", "latitude", "longitude", "longitude_180")], d_coords);
#d_coords <- unique(d_coords);

d_coords <- d_colors[,c("glottocode", "latitude", "longitude", "longitude_180")];
#d_coords <- unique(d_coords)

# starting from here, all parts will use the file d_coords *except* for the climate (see below) that needs all files to be merged together

##
## Climate data from WorldClim ####
##

# As we compute here PC, we need to merge all files.
# please indicate here the reference of the 4 databases (other, clics, scirep, family)
d_clics <- read.table("./input_files/databases/clics/data_culture.csv", header=TRUE, sep=";", quote='"', stringsAsFactors=FALSE); # comma-separated double-quoted CVS file
d_others <- read.table("./input_files/databases/other/data_culture.csv", header=TRUE, sep=";", quote='"', stringsAsFactors=FALSE); # comma-separated double-quoted CVS file
d_scirep <- read.table("./input_files/databases/scirep_paper/data_colors.csv", header=TRUE, sep=";", quote='"', stringsAsFactors=FALSE); # comma-separated double-quoted CVS file
d_family <- read.table("./input_files/databases/family/data_families.csv", header=TRUE, sep=";", quote='"', stringsAsFactors=FALSE); # comma-separated double-quoted CVS file

d_clics <- d_clics[,c("glottocode", "latitude", "longitude")]
d_clics$database <- "clics"
d_others <- d_others[,c("glottocode", "latitude", "longitude")]
d_others$database <- "other"

d_scirep <- d_scirep[,c("glottocode", "latitude", "longitude")]
d_scirep$longitude <- ifelse(d_scirep$longitude > 180, d_scirep$longitude - 360, d_scirep$longitude)
d_scirep$database <- "scirep"

d_family <- d_family[,c("glottocode", "latitude", "longitude")]
d_family$database <- "family"


d_coords <- rbind(d_clics, d_others, d_scirep, d_family)

# General climate data from the World Clim Database...
if( !file.exists("./input_files/data_climate.tsv") ) {
    
  # Variables used are:
  wolrdclim.bio.vars <- read.table(text="
  VAR   = DESCRIPTION
  BIO1  = Annual Mean Temperature
  BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
  BIO3  = Isothermality (BIO2/BIO7) (* 100)
  BIO4  = Temperature Seasonality (standard deviation *100)
  BIO5  = Max Temperature of Warmest Month
  BIO6  = Min Temperature of Coldest Month
  BIO7  = Temperature Annual Range (BIO5-BIO6)
  BIO8  = Mean Temperature of Wettest Quarter
  BIO9  = Mean Temperature of Driest Quarter
  BIO10 = Mean Temperature of Warmest Quarter
  BIO11 = Mean Temperature of Coldest Quarter
  BIO12 = Annual Precipitation
  BIO13 = Precipitation of Wettest Month
  BIO14 = Precipitation of Driest Month
  BIO15 = Precipitation Seasonality (Coefficient of Variation)
  BIO16 = Precipitation of Wettest Quarter
  BIO17 = Precipitation of Driest Quarter
  BIO18 = Precipitation of Warmest Quarter
  BIO19 = Precipitation of Coldest Quarter
                                   ", 
                                   header=TRUE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE, quote="");
  
  extract.ecology.for.coords <- function(coords # a data.frame or matrix with two columns, the 1st being the latitude, and the 2nd the longitude
  )
  {
    if( is.null(coords) || (!inherits(coords,"data.frame") && !inherits(coords,"matrix")) || nrow(coords) == 0 || ncol(coords) != 2 )
    {
      warning("Unrecognized 'coords' parameter for extracting ecolgical variables.\n")
      return (NULL);
    }
    
    # Map geographic coordinates to raster cell locations:
    
    # Load the ecological data:
    library(rgdal);
    library(raster);
    
    # Load the various data (this is downloaded only 1st time and chached locally):
    bio.raster <- getData('worldclim', var='bio', res=5);
    
    # Extract the data for the give coordinates:
    bio.data <- extract(bio.raster, coords[,c(2,1)]); # don't forget that latitude is y and longitude is x!
    
    # return the info:
    return (bio.data);
  }

  # extract climate data
  clim_data = as.matrix(d_coords[,c("latitude", "longitude")]) # we feed it first latitude, the conversion is done above
  clim_data.clim <- extract.ecology.for.coords(clim_data)
  clim_data.clim <- cbind(d_coords, clim_data.clim);
  climate_data <- as.data.frame(clim_data.clim)

  # PCA:
  PCs <- prcomp( ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + 
                    bio16 + bio17 + bio18 + bio19, data=climate_data, scale=TRUE, center=TRUE);
  climate_data <- base::merge(cbind("row"=as.numeric(rownames(climate_data)), climate_data), 
                                cbind("row"=as.numeric(rownames(PCs$x)), PCs$x[,1:3]), by="row", all.x=TRUE)
  
  climate_data$longitude_180 <- climate_data$longitude
  climate_data$longitude <- ifelse(climate_data$longitude < 0, climate_data$longitude + 360, climate_data$longitude)
  
  # Plots for checking:
  if( TRUE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=PC1)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=PC2)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=climate_data, aes(x = longitude, y = latitude, color=PC3)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }
  
  # split and change format
  colnames(climate_data)[colnames(climate_data) == 'PC1'] <- 'clim_PC1'
  colnames(climate_data)[colnames(climate_data) == 'PC2'] <- 'clim_PC2'
  colnames(climate_data)[colnames(climate_data) == 'PC3'] <- 'clim_PC3'

  clim_data_clics <- climate_data[climate_data$database=="clics",]
  clim_data_clics <- subset(clim_data_clics, select=-c(row,database))
  clim_data_other <- climate_data[climate_data$database=="other",]
  clim_data_other <- subset(clim_data_other, select=-c(row,database))
  clim_data_scirep <- climate_data[climate_data$database=="scirep",]
  clim_data_scirep <- subset(clim_data_scirep, select=-c(row,database))
  clim_data_family <- climate_data[climate_data$database=="family",]
  clim_data_family <- subset(clim_data_family, select=-c(row,database))
  
  # Save it:
  write.table(clim_data_clics, "./input_files/databases/clics/data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE); # comma-separated double-quoted CVS file
  write.table(clim_data_other, "./input_files/databases/other/data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE); # comma-separated double-quoted CVS file
  write.table(clim_data_scirep, "./input_files/databases/scirep_paper/data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE); # comma-separated double-quoted CVS file
  write.table(clim_data_family, "./input_files/databases/family/data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE); # comma-separated double-quoted CVS file

  # Clean up the downloaded files:
  unlink("./input_files/wc5/*.bil", recursive=TRUE);
  unlink("./input_files/wc5/*.hdr", recursive=TRUE);

}

# 
# # General climate data from the World Clim Database...
# if( !file.exists("./input_files/data_climate.tsv") )
# {
#   # This is based on the code in:
#   # Bentz, C., Dediu, D., Verkerk, A., & JÃ¤ger, G. (2018). The evolution of language families is shaped by the environment beyond neutral drift. Nature Human Behaviour, 2(11), 816. https://doi.org/10.1038/s41562-018-0457-6
#   # Download data from the World Clim Database and compute the first two PCs
#   
#   # Variables used are:
#   wolrdclim.bio.vars <- read.table(text="
# VAR   = DESCRIPTION
# BIO1  = Annual Mean Temperature
# BIO2  = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3  = Isothermality (BIO2/BIO7) (* 100)
# BIO4  = Temperature Seasonality (standard deviation *100)
# BIO5  = Max Temperature of Warmest Month
# BIO6  = Min Temperature of Coldest Month
# BIO7  = Temperature Annual Range (BIO5-BIO6)
# BIO8  = Mean Temperature of Wettest Quarter
# BIO9  = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter
#                                  ", 
#                                    header=TRUE, sep="=", strip.white=TRUE, stringsAsFactors=FALSE, quote="");
#   
#   library(rgdal);
#   library(raster);
#   
#   # Load the various data (this is downloaded only 1st time and cached locally):
#   bio_raster <- getData('worldclim', var='bio', res=5, download=TRUE, path="./input_files/");
#   
#   # Extract the data for the give coordinates:
#   bio_data <- extract(bio_raster, as.matrix(d_coords[,c("longitude_180","latitude")])); # don't forget that latitude is y and longitude is x!
#   bio_data <- cbind(d_coords[,c("glottocode", "latitude", "longitude")], as.data.frame(bio_data));
#   rownames(bio_data) <- bio_data$glottocode;
# 
#   # PCA:
#   library(fpc);
#   library(factoextra);
#   PCs <- prcomp( ~ bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19, 
#                  data=as.data.frame(bio_data), scale=TRUE, center=TRUE);
#   summary(PCs); # PC1 (49.7%), PC2 (24.7%), PC3 (8.6%)
#   climate_data <- data.frame("glottocode"=rownames(PCs$x), PCs$x[,1:3]);
#   names(climate_data) <- c("glottocode", paste0("clim_PC",1:(ncol(climate_data)-1)));
#   climate_data <- merge(climate_data, d_coords[,c("glottocode", "latitude", "longitude")], by="glottocode", all.x=TRUE, all.y=FALSE);
# 
#   # Plots for checking:
#   if( FALSE )
#   {
#     library(ggplot2);
#     library(ggrepel);
#     mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC1)) +
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC2)) +
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=climate_data, aes(x = longitude, y = latitude, color=clim_PC3)) +
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#   }
#   
#   # Save it:
#   write.table(climate_data, file="./input_files/data_climate.tsv", quote=FALSE, sep="\t", row.names=FALSE);
#   
#   # Clean up the downloaded files:
#   unlink("./input_files/wc5/*.bil", recursive=TRUE);
#   unlink("./input_files/wc5/*.hdr", recursive=TRUE);
# }


##
## Humidity data from the NOAA ####
##

if( !file.exists("./input_files/data_humidity.tsv") )
{
  library(ncdf4);
  library(raster);
  library(lubridate);
  library(dplyr);
  
  # Download the specific humidity data from the NOAA:
  # http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/datafiles.html
  if( !file.exists("./input_files/noaa-humidity.nc") )
  {
    if( download.file("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/data.nc", 
                      destfile="./input_files/noaa-humidity.nc") != 0 )
    {
      stop("Cannot download specific retrive humidity data from the NOAA (http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Diagnostic/.above_ground/.qa/datafiles.html)!\n");
    }
  }
  
  # Info about the netCDF file:
  ncdf4::nc_open("./input_files/noaa-humidity.nc");
  
  # Load the NOAA humidity netCDF file as a brick:
  noaa_humidity <- raster::brick("./input_files/noaa-humidity.nc");
  
  # Summarize humidity at a given location:
  humidity_summary <- function(latitude, longitude)
  {
    # Extract the whole time series for the location:
    humidity_ts <- extract(noaa_humidity, matrix(c(ifelse(longitude < 0, 360+longitude, longitude), # noaa_humidity logitudes are between 0 and 360
                                                   latitude),ncol=2));
    humidity_ts <- data.frame("humidity"=t(humidity_ts)[,1],
                              "date"=as.Date("1960-01-01", format="%Y-%m-%d") + 
                                months(round(vapply(substring(colnames(humidity_ts),2), 
                                                    function(s) ifelse(substring(s,1,1)==".", -0.5-as.numeric(substring(s,2)), as.numeric(as.character(s))), 
                                                    numeric(1))))); 
    
    # Compute the yearly means, medians, sds and IQRs:
    by_year <- humidity_ts %>%
      group_by(lubridate::year(humidity_ts$date)) %>%
      summarise("annual_mean"=mean(humidity, na.rm=TRUE),
                "annual_median"=median(humidity, na.rm=TRUE),
                "annual_sd"=sd(humidity, na.rm=TRUE),
                "annual_IQR"=IQR(humidity, na.rm=TRUE));
    
    # Compute and return the overall mean, median, sd and IQR:
    return (data.frame("overall_mean"=mean(humidity_ts$humidity, na.rm=TRUE),
                       "overall_median"=median(humidity_ts$humidity, na.rm=TRUE),
                       "overall_sd"=sd(humidity_ts$humidity, na.rm=TRUE),
                       "overall_IQR"=IQR(humidity_ts$humidity, na.rm=TRUE),
                       "mean_annual_mean"=mean(by_year$annual_mean, na.rm=TRUE),
                       "mean_annual_median"=mean(by_year$annual_median, na.rm=TRUE),
                       "mean_annual_sd"=mean(by_year$annual_sd, na.rm=TRUE),
                       "mean_annual_IQR"=mean(by_year$annual_IQR, na.rm=TRUE)));
    
  }
  humidity_data <- do.call(rbind, lapply(1:nrow(d_coords), function(i) humidity_summary(d_coords$latitude[i], d_coords$longitude_180[i])));
  
  humidity_data <- cbind(d_coords[,c("glottocode", "latitude", "longitude")], humidity_data);
  names(humidity_data) <- c("glottocode", "latitude", "longitude", 
                            "humidity_overall_mean", "humidity_overall_median", "humidity_overall_sd", "humidity_overall_IQR", 
                            "humidity_mean_annual_mean", "humidity_mean_annual_median", "humidity_mean_annual_sd", "humidity_mean_annual_IQR");
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=humidity_data, aes(x = longitude, y = latitude, color=humidity_mean_annual_median)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=humidity_data, aes(x = longitude, y = latitude, color=humidity_mean_annual_IQR)) +
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }

  # Save the data:
  write.table(humidity_data, file="./input_files/data_humidity.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}


##
## Distance to water ####
##

# Adapted from the code in Bentz, C., Dediu, D., Verkerk, A., & JÃ¤ger, G. (2018). The evolution of language families is shaped by the environment beyond neutral drift. Nature Human Behaviour, 2(11), 816. https://doi.org/10.1038/s41562-018-0457-6
if( !file.exists("./input_files/data_dist2water.tsv") )
{
  library(raster);
  library(rgdal);
  library(geosphere);
  library(parallel);
  
  # Unfortunately, the primary data from OpenStreetMap (http://openstreetmapdata.com/), the "Reduced waterbodies as raster masks" (http://openstreetmapdata.com/data/water-reduced-raster)
  # seems to no longer be available for download separately as of July 2020, but we include here, for reproducibility, the original data as downloaded in March 2018 by Dan Dediu
  # in the folder ./input_files/openstreet/ :

  extract.dist2water.for.coords <- function(coords, # a data.frame or matrix with two columns, the 1st being the longitude, and the 2nd the latitude
                                            type=c("ocean", "lakes", "river")[1],
                                            zoom=(0:6)[3], # the zoom level -- the higher the better the precision but more computationally expensive
                                            no.cores=1 # number of cores for mclapply
                                           )
  {
    cat("Processing ",type," ...\n");
    
    if( !(type %in% c("ocean", "lakes", "river")) )
    {
      warning('type must be "ocean", "lakes" or "river"!\n');
      return (NULL);
    }
    
    if( !(zoom %in% 0:6) )
    {
      warning('zoom must be 0 to 6!\n');
      return (NULL);
    }
    
    cat("Extracting the required water type at the required zoom...\n");
    unzip(paste0("./input_files/openstreet/", type, "-raster-reduced-3857.zip"), 
          files=paste0(type, "-raster-reduced-3857/", type, "_raster_z", zoom, ".tif"),
          overwrite=TRUE, exdir="./input_files");
    
    cat("Loading the OSM data...\n");
    os.data <- raster(paste0("./input_files/", type, "-raster-reduced-3857/", type, "_raster_z", zoom, ".tif")); # load the original OopenStreetMaps data
    #crs(os.data);
    
    cat("Converting it to the WGS84 projection...\n");
    os.data.proj <- projectRaster(os.data, crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"), method="ngb", over=TRUE); # convert it to the WGS84 projection
    
    cat("Extracting the points that are marked as being water...\n");
    os.data.rp <- rasterToPoints(os.data.proj, fun=function(x){x>0}, spatial=TRUE); # extract the points that are marked as water
    rm(os.data, os.data.proj); gc(); # free up memory
    unlink(paste0("./input_files/", type, "-raster-reduced-3857/"), recursive=TRUE, force=TRUE); # remove the unpacked files...
  
    cat("Computing the minimum distance to water for each of the given points\n");
    d <- unlist(mclapply(1:nrow(coords), 
                         function(i) min(distGeo(coords[i,], os.data.rp), na.rm=TRUE), mc.cores=no.cores)); # compute the minimum distance from every point to water
    
    return (d/1000); # return the minimum distances in Km
  }

  # Compute the distances:
  m <- as.matrix(d_coords[,c("longitude_180","latitude")]); colnames(m) <- c("longitude","latitude"); rownames(m) <- d_coords$glottocode;
  tmp <- extract.dist2water.for.coords(coords=m, type="ocean", zoom=2); # oceans are big, lower zoom should not be a problem
  d_colors_water <- cbind(d_coords[,c("glottocode", "longitude", "latitude")], "dist2ocean"=tmp);
  tmp <- extract.dist2water.for.coords(coords=m, type="lakes", zoom=4); # but for lakes and rivers, higher zoom should capture smaller ones
  d_colors_water <- cbind(d_colors_water, "dist2lakes"=tmp);
  tmp <- extract.dist2water.for.coords(coords=m, type="river", zoom=4);
  d_colors_water <- cbind(d_colors_water, "dist2rivers"=tmp);
  d_colors_water <- cbind(d_colors_water, "dist2water"=pmin(d_colors_water$dist2ocean, d_colors_water$dist2lakes, d_colors_water$dist2rivers, na.rm=TRUE));
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2ocean)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2ocean,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2lakes)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2lakes,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2rivers)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2rivers,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_colors_water, aes(x = longitude, y = latitude, color=dist2water)) +
      geom_label_repel(data=d_colors_water, aes(x = longitude, y = latitude, label=round(dist2water,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }
  
  # Remove temporary files:
  unlink("./input_files/lakes-raster-reduced-3857", recursive=TRUE);
  unlink("./input_files/river-raster-reduced-3857", recursive=TRUE);
  unlink("./input_files/ocean-raster-reduced-3857", recursive=TRUE);
  
  # Save the data:
  write.table(d_colors_water, file="./input_files/data_dist2water.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}

##
## UV incidence - new ####
##

# Extract & parse the UVB data from TOMS Nimbus-7 UV-B Erythemal Local Noon Irradiance Monthly 
# Local noon erythemal UV irradiance values (monthly averaged), in mW/m²
# Two dataset covering different time range are used: from 1978-11-01 to  1993-05-01 (link 1, see below) and from  1996-08-01 to  2003-09-01 (link 2, see below)
# It covers a total of 22 years, with a break between 1993 and 1996.
# link1: https://disc.gsfc.nasa.gov/datasets/TOMSN7L3mery_008/summary?keywords=erythemal%20uv
# link2: https://disc.gsfc.nasa.gov/datasets/TOMSEPL3mery_008/summary?keywords=erythemal%20uv
# Both links were downloaded and named respectively filelist_1978_to_1993.txt and filelist_19996_to_2003.txt

if( !file.exists("./input_files/data_UV_incidence.tsv") ){
  
  library(httr)
  library(stringi)
  
  if( !file.exists("./input_files/nasa_uv/data_summary/av_year.csv") ){
    
    dir.create("./input_files/nasa_uv/raw_data")
    dir.create("./input_files/nasa_uv/data_csv")
    dir.create("./input_files/nasa_uv/data_summary")
    dir.create("./input_files/nasa_uv/data_summary/monthly")


    ### STEP 1 - download from NASA website
    
    ## need username and password for NASA website
    # If you don't have an account you cannot access the data!
    usrname <- "" # enter your username here
    pass <- "" # enter your password here
    
    ## read the files with the list of links
    file_list_raw <- read.table("./input_files/nasa_uv/filelist_1978_to_1993.txt")
    file_list_raw2 <- read.table("./input_files/nasa_uv/filelist_1996_to_2003.txt")
    
    ## download the data: with the first link file
    for(i in 1:(nrow(file_list_raw))){
      url = as.character(file_list_raw[i,])
      name_raw_file = substr(url, 100, 144)
      x = httr::GET(url, authenticate(usrname, pass, type = "basic"), verbose(info = TRUE, ssl = TRUE))
      bin = content(x, "raw")
      writeBin(bin, paste("./input_files/nasa_uv/raw_data/", name_raw_file, sep="")) 
      print(paste("File", i, "out of", nrow(file_list_raw)))
    }
    
    ## download the data: with the second link file
    for(i in 1:(nrow(file_list_raw2))){
      url = as.character(file_list_raw2[i,])
      name_raw_file = substr(url, 103, 144)
      x = httr::GET(url, authenticate(usrname, pass, type = "basic"), verbose(info = TRUE, ssl = TRUE))
      bin = content(x, "raw")
      writeBin(bin, paste("./input_files/nasa_uv/raw_data/", name_raw_file, sep="")) 
      print(paste("File", i, "out of", nrow(file_list_raw2)))
      
    }
    
    ## the name of files are written with the format yearmonth (197812.txt is the 12th month, December, of the year 1978)
    
    
    ### STEP 2 - convert these files to csv 
    
    files <- list.files( "./input_files/nasa_uv/raw_data", full.names = TRUE) 
    
    for (num_files in c(1:length(files))){
      
      # read file
      con=file(files[num_files],open="r")
      res <- readLines(con)
      res <- res[4:length(res)]
      print(paste("File", num_files, "out of", length(files)))
      
      # initialize df
      df <- data.frame()
      seq_to_check <- seq(0, length(res), 12)
      
      # read data for each latitude and add to dataframe
      for (mylat in seq_to_check[-c(length(seq_to_check))]){
        newlat = res[(1+mylat):(12+mylat)]
        for (row in 1:length(newlat)){ newlat[row] <- stri_sub(newlat[row], from = 2, to = -1) }
        newlat = paste(newlat, collapse = '')
        newlat2 <- substr(newlat, 1, (nchar(newlat) - 15))
        vecnum2 <- gsub("(.{3})", "\\1,", newlat2)
        vecnum3 <- strsplit(vecnum2, ",")
        df <- rbind(df, t(data.frame(vecnum3)))
      }
      
      # write csv
      rownames(df) <- seq(-89.5, 89.5,  by=1)
      colnames(df) <- seq(-179.375, 179.375,  by=1.25)
      write.csv(df, paste("./input_files/nasa_uv/data_csv/", stri_sub(files[num_files], -10, -4), "csv", sep=""), quote = FALSE)
      
    }
    
    
    ### Step 3 - average per month and for all months
    
    files_csv <- list.files("./input_files/nasa_uv/data_csv", full.names = TRUE) 
    year_list <- c()
    month_list <- c()
    list_allmat <- list()
    
    # read file and add to big list, in order to make some computations
    for (el in 1:length(files_csv)){
      mymat <- as.matrix(read.csv(files_csv[el], row.names=1))
      mymat[mymat==999] <- 0 # this is when there is no sun at all
      list_allmat <- append(list_allmat, list(mymat))
      year_list <- c(year_list, stri_sub(files_csv[el], -10, -7))
      month_list <- c(month_list, stri_sub(files_csv[el], -6, -5))
    }
    
    # convert to numeric
    month_list <- as.numeric(month_list)
    year_list <- as.numeric(year_list)
    
    # list of longitudes and latitudes
    longitudes <- seq(-179.375, 179.375, 1.25)
    latitudes <- seq(-89.5, 89.5, 1)
    
    # initialize the list of aggregated months
    list_av_month <- list()
    
    # aggregate for each month
    for (mon in c(1:12)){
      
      subset_list  = list_allmat[which(month_list %in% mon)]
      Y <- array()
      Y <- do.call(cbind, subset_list)
      Y <- array(Y, dim=c(dim(subset_list[[1]]), length(subset_list)))
      meann <- apply(Y, c(1, 2), mean, na.rm=TRUE)
      colnames(meann) <- longitudes
      rownames(meann) <- latitudes
      meann <- list(meann)
      list_av_month <- append(list_av_month, meann)
      write.csv(meann, paste("./input_files/nasa_uv/data_summary/monthly/av_month", as.character(mon),".csv", sep=""), quote = FALSE)
      
    }
    
    # aggregate accross all month (better than to aggregate with all data, because some months have more data than other)
    Y <- array()
    Y <- do.call(cbind, list_av_month)
    Y <- array(Y, dim=c(dim(list_av_month[[1]]), length(list_av_month)))
    meann <- data.frame(apply(Y, c(1, 2), mean, na.rm=TRUE))
    colnames(meann) <- longitudes
    rownames(meann) <- latitudes
    write.csv(meann, "./input_files/nasa_uv/data_summary/av_year.csv", quote = FALSE)
    
    # compute std accross all month 
    Y <- array()
    Y <- do.call(cbind, list_av_month)
    Y <- array(Y, dim=c(dim(list_av_month[[1]]), length(list_av_month)))
    stdd <- data.frame(apply(Y, c(1, 2), sd, na.rm=TRUE))
    colnames(stdd) <- longitudes
    rownames(stdd) <- latitudes
    write.csv(stdd, "./input_files/nasa_uv/data_summary/std_year.csv", quote = FALSE)

  }  
  
  # if these files already exist, then just check the value of each long/lat population
  files_csv <- list.files("./input_files/nasa_uv/data_summary/monthly", full.names = TRUE) 
  #files_csv <- list.files("./input_files/nasa_uv/1998/", full.names = TRUE) 
  list_av_month <- list()
  month_list <- c()
  
  # read file from each month
  for (el in 1:length(files_csv)){
    mymat <- as.matrix(read.csv(files_csv[el], row.names=1))
    list_av_month <- append(list_av_month, list(mymat))
    month_list <- c(month_list, stri_sub(files_csv[el], -6, -5))
  }
  month_list2 <- as.numeric(gsub("[^0-9]", "", month_list))
  
  # read aggregated data per year
  df_uvb <- read.csv("./input_files/nasa_uv/data_summary/av_year.csv", header=T, row.names=1)
  df_uvb_sd <- read.csv("./input_files/nasa_uv/data_summary/std_year.csv", header=T, row.names=1)
 
  # read aggregated data per year
  #df_uvb <- read.csv("./input_files/nasa_uv/1998/av_year.csv", header=T, row.names=1)
  #df_uvb_sd <- read.csv("./input_files/nasa_uv/1998/std_year.csv", header=T, row.names=1)
  
  # initialize variables
  uv_incidence <- d_coords
  uv_incidence$UV_mean <- 999; uv_incidence$UV_sd <- 999; 
  uv_incidence$UV_jan <- 999; uv_incidence$UV_fev <- 999; uv_incidence$UV_mar <- 999; uv_incidence$UV_apr <- 999; 
  uv_incidence$UV_may <- 999; uv_incidence$UV_jun <- 999; uv_incidence$UV_jul <- 999; uv_incidence$UV_aug <- 999; 
  uv_incidence$UV_sep <- 999; uv_incidence$UV_oct <- 999; uv_incidence$UV_nov <- 999; uv_incidence$UV_dec <- 999
  
  # list of longitudes and latitudes
  longitudes <- seq(-179.375, 179.375, 1.25)
  latitudes <- seq(-89.5, 89.5, 1)
  
  for (pop in c(1:nrow(uv_incidence))){
    print(paste("Population", pop, "out of", nrow(uv_incidence), "populations"))
    # find closest latitude and longitude
    indexlat <- which.min(abs(uv_incidence[pop,"latitude"] - latitudes))
    indexlong <- which.min(abs(uv_incidence[pop,"longitude_180"] - longitudes))
    # fill UV mean and UV sd
    uv_incidence[pop,5] <- as.numeric(df_uvb[indexlat, indexlong])
    uv_incidence[pop,6] <- as.numeric(df_uvb_sd[indexlat, indexlong])
    # fill UV mean for each month
    for (month in c(1:12)){
      uv_incidence[pop,(month+6)] <- data.frame(list_av_month[[month]])[indexlat, indexlong]
    }
  }
  
  # Save the data:
  write.table(uv_incidence, file="./input_files/data_UV_incidence.tsv", quote=FALSE, sep="\t", row.names=FALSE)
  
  # Plot for checking
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=uv_incidence, aes(x = longitude, y = latitude, color=UV_mean)) +
      geom_label_repel(data=uv_incidence, aes(x = longitude, y = latitude, label=round(UV_mean,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=uv_incidence, aes(x = longitude, y = latitude, color=UV_sd)) +
      geom_label_repel(data=uv_incidence, aes(x = longitude, y = latitude, label=round(UV_sd,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
    
  }
}


# ##
# ## Previous UV incidence ####
# ##
# 
# # Extract & parse the UVB data from NASA Goddard Space Flight Center's "New UV Irradiance and Exposure Data Product"
# # TOMS (Total Ozone Mapping Spectrometer): http://toms.gsfc.nasa.gov/ery_uv/
# # available in raw format at: http://toms.gsfc.nasa.gov/ery_uv/new_uv/
# #
# # Original file format is described in http://toms.gsfc.nasa.gov/ery_uv/new_uv/1README.UV
# #
# # These are different wavelengths (305, 310, 320, and 380 nm) for UV as explain on the page http://toms.gsfc.nasa.gov/ery_uv/:
# # "[...] Ultraviolet radiation is at shorter wavelengths than the visible spectrum (400 to 700 nm) and is divided into three 
# # components: UV-A (315 to 400 nm), UV-B (280 to 315 nm) and UV-C (less than 280 nm). The shorter wavelengths that comprise
# # UV-B are the most dangerous portion of UV radiation that can reach ground level [...]"
# #
# # Unfortunately, as of July 2020, this data is not longer available online (seems to have been repackaged as per
# # https://disc.gsfc.nasa.gov/datacollection/TOMSEPL3mery_008.html), but we provide the originally processed data for reproducibility
# # in the toms_nasa_uv folder.
# if( !file.exists("./input_files/data_UV_incidence.tsv") )
# {
#   library(dplyr);
#   
#   # Load the data for the year 1998:
#   d_uv_1998 <- rbind(read.table(xzfile("./input_files/toms_nasa_uv/UV-for-year-1998-freq-305.csv.xz"), header=TRUE, sep="\t"),
#                      read.table(xzfile("./input_files/toms_nasa_uv/UV-for-year-1998-freq-310.csv.xz"), header=TRUE, sep="\t"),
#                      read.table(xzfile("./input_files/toms_nasa_uv/UV-for-year-1998-freq-325.csv.xz"), header=TRUE, sep="\t"),
#                      read.table(xzfile("./input_files/toms_nasa_uv/UV-for-year-1998-freq-380.csv.xz"), header=TRUE, sep="\t"));
#   
#   # For a given location, obtain the summaries:
#   get_uv_data_for_location <- function(longitude, latitude)
#   {
#     # Find the closest datapoint to the location:
#     longitudes <- as.numeric(substring(names(d_uv_1998)[6:ncol(d_uv_1998)], 5)) - 0.5;
#     longitude_hit <- longitudes[ which.min(abs(longitudes - longitude)) ] + 0.5;
#     
#     latitudes <- sort(unique(as.numeric(d_uv_1998$latitude)));
#     latitude_hit <- latitudes[ which.min(abs(latitudes - latitude)) ];
#     
#     # Extract the data:
#     d <- d_uv_1998[ d_uv_1998$latitude == latitude_hit, c("year", "month", "day", "uv", "latitude", paste0("Deg.",longitude_hit)) ];
#     names(d)[ncol(d)] <- "incidence";
#     if( nrow(d) == 0 ) stop(paste0("Cannot find UV data for location (",longitude,", ",latitude,")."));
#     
#     # And return summaries:
#     return (data.frame(# for each frequency band
#                        "UV_305_mean"=mean(d$incidence[ d$uv == 305 ], na.rm=TRUE),
#                        "UV_305_sd"  =sd(d$incidence[   d$uv == 305 ], na.rm=TRUE),
#                        "UV_310_mean"=mean(d$incidence[ d$uv == 310 ], na.rm=TRUE),
#                        "UV_310_sd"  =sd(d$incidence[   d$uv == 310 ], na.rm=TRUE),
#                        "UV_325_mean"=mean(d$incidence[ d$uv == 325 ], na.rm=TRUE),
#                        "UV_325_sd"  =sd(d$incidence[   d$uv == 325 ], na.rm=TRUE),
#                        "UV_380_mean"=mean(d$incidence[ d$uv == 380 ], na.rm=TRUE),
#                        "UV_380_sd"  =sd(d$incidence[   d$uv == 380 ], na.rm=TRUE),
#                        # for UV-A
#                        "UV_A_mean"  =mean(d$incidence[ d$uv >= 315 & d$uv < 400 ], na.rm=TRUE),
#                        "UV_A_sd"    =sd(d$incidence[   d$uv >= 315 & d$uv < 400 ], na.rm=TRUE),
#                        # for UV-B
#                        "UV_B_mean"  =mean(d$incidence[ d$uv >= 280 & d$uv < 315 ], na.rm=TRUE),
#                        "UV_B_sd"    =sd(d$incidence[   d$uv >= 280 & d$uv < 315 ], na.rm=TRUE),
#                        # for all UVs
#                        "UV_mean"    =mean(d$incidence, na.rm=TRUE),
#                        "UV_sd"      =sd(d$incidence, na.rm=TRUE)));
#   }
#   
#   # Get this data for all the populations:
#   d_uv <- do.call(rbind, 
#                   lapply(1:nrow(d_coords), function(i) cbind(d_coords[i, c("glottocode", "longitude", "latitude")], 
#                                                              get_uv_data_for_location(d_coords$longitude[i], d_coords$latitude[i]))));
#   
#   # Plots for checking:
#   if( FALSE )
#   {
#     library(ggplot2);
#     library(ggrepel);
#     mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_305_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_305_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_305_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_305_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_310_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_310_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_310_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_310_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_325_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_325_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_325_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_325_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_380_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_380_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_380_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_380_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_A_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_A_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_A_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_A_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_B_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_B_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_B_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_B_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
# 
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_mean)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_mean,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#     
#     ggplot() + theme_bw() +
#       theme_bw() +
#       geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
#       geom_point(data=d_uv, aes(x = longitude, y = latitude, color=UV_sd)) +
#       geom_label_repel(data=d_uv, aes(x = longitude, y = latitude, label=round(UV_sd,0)), alpha=0.5, fill="lightyellow") + 
#       theme(legend.position = c(0.75, 0.5), 
#             legend.justification = c(1, 1), 
#             legend.title = element_text(size = 9), 
#             legend.text = element_text(size = 10)) +
#       NULL;
#   }
#   
#   # Save the data:
#   write.table(d_uv, file="./input_files/data_UV_incidence.tsv", quote=FALSE, sep="\t", row.names=FALSE);
# }

##
## UV-incidence from Worldclim ####
##

if( !file.exists("./input_files/data_UV_incidence_worldclim.tsv") )
{
  library(raster);
  
  UV_jan <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_01.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_fev <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_02.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_mar <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_03.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_apr <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_04.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_may <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_05.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_jun <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_06.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_jul <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_07.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_aug <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_08.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_sep <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_09.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_oct <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_10.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_nov <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_11.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  UV_dec <- extract(raster('./input_files/worldclim_uv/wc2.1_2.5m_srad_12.tif' ), d_coords[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  
  uv.data <- cbind(d_coords, UV_jan, UV_fev, UV_mar, UV_apr, UV_may, UV_jun, UV_jul, UV_aug, UV_sep, UV_oct, UV_nov, UV_dec);
  
  uv.data %>% 
    mutate(UV_sd = apply(.[(5:16)],1,sd)) -> uv.data
  uv.data %>% 
    mutate(UV_mean = apply(.[(5:16)],1,mean)) -> uv.data
  
  # Save the data:
  write.table(uv.data, file="./input_files/data_UV_incidence_worldclim.tsv", quote=FALSE, sep="\t", row.names=FALSE);
  
}

##
## Altitude ####
##

if( !file.exists("./input_files/data_elevation.tsv") )
{
  
  # Use the elevatr package to get the Mapzen elevation data, currently (July 2020) still available on https://registry.opendata.aws/terrain-tiles/
  # see https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#get_raster_elevation_data
  library(raster);
  library(elevatr);

  # Get the elevation data:
  elevation_tiles <- list();
  elevation <- rep(NA, nrow(d_coords));
  for( i in 1:nrow(d_coords) )
  {
    cat(paste0("Obtaining elevation for ",d_coords$glottocode[i]," (",i," of ",nrow(d_coords),"):\n"));
    # Try several levels of zoom, until one succeeds:
    for( z in 1:14) 
    {
      cat(paste0("... trying zoom level ",z,"...\n"));
      e <- NULL;
      try(e <- get_elev_raster(d_coords[i,c("longitude_180", "latitude"), drop=FALSE], 
                               prj="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", z=z, expand=ifelse(z < 5, 10, ifelse(z < 10, 2, 1))), # adapt expansion to the zoom level
          silent=TRUE);
      if( !is.null(e) && inherits(e, "RasterLayer") )
      {
        # Succeeded:
        elevation_tiles[[i]] <- list("z"=z, "raster"=e);
        elevation[i] <- extract(e, d_coords[i,c("longitude_180", "latitude"), drop=FALSE], method="simple");
        #plot(e, main=d_coords$glottocode[i]); points(d_coords$longitude_180[i], d_coords$latitude[i]);
        break;
      }
    }
    if( is.null(e) ){ warning("Error obtaining elevation info..."); elevation_tiles[[i]] <- list(); elevation[i] <- NA; }
  }
  names(elevation_tiles) <- names(elevation) <- d_coords$glottocode;
  d_elevation <- cbind(d_coords[,c("glottocode", "longitude_180", "latitude")], "elevation"=round(elevation,1));
  
  
  ### PROBLEM: some elevation are way below 0
  # After a manual check, I realize that the one above 0 are correct, and only there is a problem with some glottocode
  # Thus, for these glottocode, I use another database extracted from Worldclim website
  # This database gives correct elevation for the problematic data
  
  # the problematic data
  torecheck <- d_elevation[d_elevation$elevation<0,c("glottocode", "latitude", "longitude_180")]
  
  # check with other database for problematic data
  imported_raster=raster('./input_files/wc2.1_2.5m_elev.tif' )
  bio.data <- extract(imported_raster, torecheck[,c("longitude_180","latitude")]); # don't forget that latitude is y and longitude is x!
  torecheck <- cbind(torecheck, bio.data);
  colnames(torecheck) <- c("glottocode","latitude","longitude_180", "elevation")
  
  # change format
  d_elevation2 <- d_elevation[!(d_elevation$glottocode %in% c(torecheck$glottocode) & d_elevation$latitude %in% c(torecheck$latitude)),]
  d_elevation2 <- rbind(d_elevation2[, c("glottocode", "latitude", "longitude_180", "elevation")],
                       torecheck[,c("glottocode","latitude","longitude_180", "elevation")])
  d_elevation2$longitude <- ifelse(d_elevation2$longitude < 0, d_elevation2$longitude + 360, d_elevation2$longitude)
  d_elevation2 <- d_elevation2[,c("glottocode", "longitude", "longitude_180", "latitude", "elevation")]
  
  # Plots for checking:
  if( FALSE )
  {
    library(ggplot2);
    library(ggrepel);
    mapWorld <- map_data("world", wrap=c(-20,340), ylim=c(-70,100));
    
    ggplot() + theme_bw() +
      theme_bw() +
      geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group) ,fill = "grey") +
      geom_point(data=d_elevation2, aes(x = longitude, y = latitude, color=elevation)) +
      geom_label_repel(data=d_elevation2, aes(x = longitude, y = latitude, label=round(elevation,0)), alpha=0.5, fill="lightyellow") + 
      theme(legend.position = c(0.75, 0.5), 
            legend.justification = c(1, 1), 
            legend.title = element_text(size = 9), 
            legend.text = element_text(size = 10)) +
      NULL;
  }  
  
  # Save the tiles for later use as an xz-compressed tar archive:
  dir.create("./input_files/elevation_Mapzen_tiles", showWarnings=FALSE);
  for(i in seq_along(elevation_tiles) )
  {
    cat(paste0("Saving tile for ",names(elevation_tiles)[i]," (",i," of ",length(elevation_tiles),"):\n"));
    writeRaster(floor(elevation_tiles[[i]]$raster), # the floating-point precision is not useful and wastes disk space
                paste0("./input_files/elevation_Mapzen_tiles/raster_for_",names(elevation_tiles)[i],"_z_",elevation_tiles[[i]]$z,".tif"), 
                format="GTiff", options=c("COMPRESS=LZW", "PREDICTOR=2"), overwrite=TRUE); # compress it as per https://kokoalberti.com/articles/geotiff-compression-optimization-guide/
  }
  tar("./input_files/elevation_Mapzen_tiles.txz", "./input_files/elevation_Mapzen_tiles/", compression="xz", compression_level=9);
  unlink("./input_files/elevation_Mapzen_tiles/", recursive=TRUE);

  # Save the data:
  write.table(d_elevation2, file="./input_files/data_elevation.tsv", quote=FALSE, sep="\t", row.names=FALSE);
}

# 
# ##
# ## Genetic distances ####
# ##
# 
# if( !file.exists("./input_files/data_genetics.tsv") )
# {
#   # Load the genetic distances with ultrametric imputation:
#   d_gen_ult <- read.table("./input_files/distmat_gen_ult.csv", header=TRUE, sep=","); rownames(d_gen_ult) <- d_gen_ult[,1]; d_gen_ult <- d_gen_ult[,-1]; d_gen_ult <- as.matrix(d_gen_ult);
# 
#   # Let's pick the first 10 MDS dimensions:
#   k <- 10; x_ult <- cmdscale(d_gen_ult, k=k, eig=TRUE, add=TRUE);
#   
#   d_genetics <- merge(d_colors[,c("glottocode"), drop=FALSE], data.frame("glottocode"=rownames(x_ult$points), x_ult$points), by="glottocode", all.x=TRUE, all.y=FALSE);
#   names(d_genetics)[ (ncol(d_genetics)-k+1):ncol(d_genetics) ] <- paste0("gen_D",1:k);
# 
#   # Save the data:
#   write.table(d_genetics, file="./input_files/data_genetics.tsv", quote=FALSE, sep="\t", row.names=FALSE);
# }




