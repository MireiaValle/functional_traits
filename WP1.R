
#-LOADING Required Libraries ------------------------------------------------------------------ #
#libraries for spatial analysis
require(foreign)
require(sp)
library(raster)       #Main raster library with nearly all functions used in this analysis
library(rgdal)        #Spatial library - most functions used from rgdal are for vectors (shapefiles)
require(maptools)

#for data management
library (tibble)
library (tidyverse) 
library(dplyr)        #a data wrangling library
library (data.table)
library (reshape)

#specific libraries for getting functional traits
#FishLife
#devtools::install_github("DanOvando/FishLife", ref= "query-fishlife")
library(FishLife)
#Fishbase
#install.packages("rfishbase", 
#                 repos = c("http://packages.ropensci.org", "http://cran.rstudio.com"), 
#                 type="source")
library (rfishbase)

#WORKING DIRECTORY
setwd("/home/valle/WP1")

################################################################################################
## LOADING AQUAMAPS DATA 
###############################################################################################

#Loading species list 
species <- read.csv('Aquamaps/csv/speciesoccursum_speccode.csv',sep=',', header = TRUE)
head (species)
summary (species)


#habitat suitability
hs <- read.csv('Aquamaps/csv/hcaf_species_native.csv',sep=',', header = TRUE)
head (hs)
summary (hs)
colnames (hs)
colnames (hs)<- c("SPECIESID", "CsquareCode", "probability", "FAOAreaYN" ,  "BoundBoxYN" )
colnames (hs)


#xyz (location + variables)
xyz <- read.csv('Aquamaps/csv/hcaf_v6.csv',sep='\t', header = TRUE)
head (xyz)
colnames (xyz)

#-PREPARING OUR DATA MERGING AQUAMAPS+FISHLIFE+FISHBASE with HS AND XYZ-----------------------------------------------------------------------#

#cuando tengamos lista la tabla con todos los datos de los functional traits aplicaremos el siguiente código a esa tabla
#read species life traits table (species_lt), the table we got from FishLife
species_lt <- read.csv('species_lt.csv')
as.tibble (species_lt)
head (species_lt)
aquamaps_keyed <- data.table(species_lt, key = "SPECIESID") 
head (aquamaps_keyed)
class(aquamaps_keyed)

#important columns in this table are: SPECIESID and CsquareCode
head (hs)
hs_keyed <- data.table(hs,  key = "SPECIESID")
head (hs_keyed)
class(hs_keyed)


#new table joining both tables by SpeciesID field. 
species_hs <- hs_keyed[aquamaps_keyed] %>%
  setkey(NULL)
head (species_hs)
colnames (species_hs)

#code for replacing CSquareCode with LOICZID

head(xyz)
colnames (xyz)

#xyz data frame contains information on the location of each cell and the value of diferente variables on that cell


if(!'LOICZID' %in% names(species_hs)) {
  ### replace CsquareCode with LOICZID... use keyed data.table for speed
  library(data.table)
  csq_loiczid <- (xyz) %>%
    data.table(key = "CsquareCode")
  species_hs_dt <- species_hs %>%
    data.table(key = "CsquareCode")
  
  species_hs_dt1 <- csq_loiczid[species_hs_dt]
  
  species_hs1 <-  species_hs_dt1 %>%
    as.data.frame()
}

species_hs <- species_hs1 %>%
  select(SPECIESID, LOICZID)

message(sprintf('Writing Aquamaps species per cell file to: \n  %s', species_hs_file))
write_csv(species_hs, species_hs_file)

}

#------------------------------------------------------------------------------------- #
# Importing raster with LOICID value into the RasterLayer format  #
#------------------------------------------------------------------------------------- #

basemap<-raster("loiczid.tif")        
plot (basemap)
basemap


