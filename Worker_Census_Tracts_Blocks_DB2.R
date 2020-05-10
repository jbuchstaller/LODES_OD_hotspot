library(plyr)
library(dplyr)
library(sp)
library(R.utils)
library(rgdal)
library(RSQLite)
library(data.table)

options("scipen" = 99)

#reference: http://www.alex-singleton.com/GDS_UA_2017/Mapping_Flows
unzip("OD_Block.sl3.zip")
sqlitePath_OD <- "OD_Block.sl3"
table_OD <- "OD_Block"

loadData_OD <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath_OD)
  # Construct the fetching query
  query_OD_data <- sprintf("SELECT w_geocode, h_geocode, S000, h_geocode_trct, w_geocode_trct, h_lon, h_lat, w_lon, w_lat FROM %s", table_OD )
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query_OD_data)
  dbDisconnect(db)
  data
}

#h_lon, h_lat, w_lon, w_lat
LODES_JT00_2015_S100_tri <- loadData_OD()

LODES_JT00_2015_S100_tri$w_geocode <- as.numeric(LODES_JT00_2015_S100_tri$w_geocode)
LODES_JT00_2015_S100_tri$h_geocode <- as.numeric(LODES_JT00_2015_S100_tri$h_geocode)
LODES_JT00_2015_S100_tri$S000 <- as.integer(LODES_JT00_2015_S100_tri$S000)
LODES_JT00_2015_S100_tri$w_geocode_trct <- as.numeric(LODES_JT00_2015_S100_tri$w_geocode_trct)
LODES_JT00_2015_S100_tri$h_geocode_trct <- as.numeric(LODES_JT00_2015_S100_tri$h_geocode_trct)
LODES_JT00_2015_S100_tri$h_lon <- as.numeric(LODES_JT00_2015_S100_tri$h_lon)
LODES_JT00_2015_S100_tri$h_lat <- as.numeric(LODES_JT00_2015_S100_tri$h_lat)
LODES_JT00_2015_S100_tri$w_lon <- as.numeric(LODES_JT00_2015_S100_tri$w_lon)
LODES_JT00_2015_S100_tri$w_lat <- as.numeric(LODES_JT00_2015_S100_tri$w_lat)

#LODES_JT00_2015_S100_tri <- mutate_all(LODES_JT00_2015_S100_tri, function(x) as.numeric(x))
#agreggate, not needed for blocks, as already aggregates, but needed for tracts
#OD_Tract <- aggregate(data= LODES_JT00_2015_S100_tri,S000 ~ w_geocode_trct + h_geocode_trct, sum)

LODES_JT00_2015_S100_tri <- data.table(LODES_JT00_2015_S100_tri)
OD_Tract <- LODES_JT00_2015_S100_tri[, sum(S000), by=list(w_geocode_trct,h_geocode_trct)]
colnames(OD_Tract) = c("w_geocode_trct","h_geocode_trct", "S000")


#merge onto tract centroids, downloadad from TIGER
#https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2018&layergroup=Blocks+%282010%29
#unzip("tl_2018_26_tract.zip", exdir="tl_2018_26_tract")
MI_SP_trct <- readOGR(dsn="tl_2018_26_tract", layer = "tl_2018_26_tract")
MI_SP_trct <- spTransform(MI_SP_trct, CRS("+init=epsg:4326"))

OD_Tract <- group_by(OD_Tract, w_geocode_trct)
#sum over w_geocodes
OD_Tract <- mutate(OD_Tract, sumS000byw_geocode_trct = sum(S000))

OD_Tract <- unique(select(OD_Tract, w_geocode_trct, sumS000byw_geocode_trct))

########merge to MI_trct_shapefile
mi_censustracts_LODES_OD_w_trct <- sp::merge(MI_SP_trct, OD_Tract,
                                             by.x = "GEOID",
                                             by.y = "w_geocode_trct",
                                             sort = FALSE)

mi_censustracts_LODES_OD_w_trct <-subset(mi_censustracts_LODES_OD_w_trct, !is.na(sumS000byw_geocode_trct))

