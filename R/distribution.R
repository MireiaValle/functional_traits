#R VERSION
version

#-LOADING Required Libraries ------------------------------------------------------------------ #
#libraries for spatial analysis
require(foreign)
require(sp)
library(raster)       #Main raster library with nearly all functions used in this analysis
library(rgdal)        #Spatial library - most functions used from rgdal are for vectors (shapefiles)
require(maptools)

#for data management
#library (tibble)
library (tidyverse) 
#library(dplyr)        #a data wrangling library
library (data.table)
library (reshape)
library (dbplyr)

#See WORKING DIRECTORY
getwd()

################################################################################################
## LOADING DATA OF SPECIES WITH LIFE TRAITS INFORMATION GOTTEN FROM FISHLIFE AND FISHBASE
###############################################################################################

species_lt <- read.csv('/home/valle/github_annex/species_lt.csv')
as.tibble (species_lt)
colnames (species_lt)
species_lt_subset <- subset(species_lt, select=c("SPECIESID", "Genus", "Species"))
rm (species_lt)
species_lt_keyed <- data.table(species_lt_subset, key = "SPECIESID") 
rm (species_lt_subset)

################################################################################################
## LOADING AQUAMAPS DATA 
###############################################################################################

#hs:table with information about the probability of occurrence of species 
hs <- read.csv('/home/valle/github_annex/Aquamaps/csv/hcaf_species_native.csv',sep=',', header = TRUE)
head (hs)
colnames (hs)
colnames (hs)<- c("SPECIESID", "CsquareCode", "probability", "FAOAreaYN" ,  "BoundBoxYN" )
colnames (hs)
hs_subset <- subset(hs, select=c("SPECIESID", "CsquareCode"))
rm (hs)

#important columns in this table are: SPECIESID and CsquareCode
hs_keyed <- data.table(hs_subset,  key = "SPECIESID")
head (hs_keyed)
class(hs_keyed)
rm(hs_subset)

#new table joining both tables by SpeciesID field. 
species_hs <- hs_keyed[species_lt_keyed] %>%
  setkey(NULL)
head (species_hs)
colnames (species_hs)
rm(species_lt_keyed)
rm (hs_keyed)

#code for replacing CSquareCode with LOICZID ------------------------------------------------------

#xyz (location + variables)
#xyz data frame contains information on the location of each cell and the value of diferente variables on that cell

xyz <- read.csv('/home/valle/github_annex/Aquamaps/csv/hcaf_v6.csv',sep='\t', header = TRUE)
head (xyz)
colnames (xyz)
xyz_subset <- subset(xyz, select=c("CsquareCode", "LOICZID"))
rm (xyz)
colnames (xyz_subset)


#subset data we want to keep SPECIESID and CsquareCode

head (species_hs)
species_hs_keyed <- data.table(species_hs, key = "CsquareCode") 
rm(species_hs)
class (species_hs_keyed)

head (xyz_subset)
csq_loiczid <- data.table (xyz_subset, key="CsquareCode")
rm (xyz_subset)
class (csq_loiczid)

#new table joining both tables by SpeciesID field. 
species_loiczid <- csq_loiczid[species_hs_keyed]
head (species_loiczid)
species_loiczid <- subset(species_loiczid, select=c("LOICZID", "SPECIESID"))
head (species_loiczid)

#save the results
write.csv (species_loiczid, file = "species_loiczid")
#read table
species_loiczid <- read.table ("github_annex/species_loiczid", head=TRUE, sep=',')
head (species_loiczid)
unique (species_loiczid$SPECIESID) %>% length()#12368

table(is.na (species_loiczid))

#------------------------------------------------------------------------------------- #
# Creating a raster with LOICID value #
#------------------------------------------------------------------------------------- #

basemap <- raster (ext= extent (c(-180,180, -90, 90)), res=0.5)
values (basemap) <- 1:length (basemap)
basemap

#class       : RasterLayer 
#dimensions  : 360, 720, 259200  (nrow, ncol, ncell)
#resolution  : 0.5, 0.5  (x, y)
#extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
#data source : in memory
#names       : layer 
#values      : 1, 259200  (min, max)

plot (basemap)
writeRaster(basemap, "basemap", format = "GTiff")

#mapping salintity 
head (xyz)
class(xyz)
unique (xyz$LOICZID) %>% length()
#to check the unique values on the map
unique (values(basemap)) %>% length()

# we first create a data.frame with the two columns we want to use for mapping
text <- xyz %>% select(LOICZID, SalinityMin)
salinity <- subs (basemap, text, by= "LOICZID", which= "SalinityMin")


#mapping the distribution of the all species------------------------------------------------------------------------

all_spp <-  species_loiczid %>%
  group_by (LOICZID)%>%
  summarize(n_spp = n())
head (all_spp)
unique (all_spp$LOICZID) %>% length()#166842
unique (all_spp$n_spp) %>% length()#3704

allspecies_map <- subs (basemap, all_spp, by= "LOICZID", which= "n_spp")
plot (allspecies_map)


#Mapping the distribution of those species under 25 cm (Loo<=25)----------------------------------------------------
under25cm <-subset (species_lt, species_lt$Loo<=25)
summary (under25cm$Loo)
colnames (under25cm)
under25cm <- under25cm [c("SPECIESID", "Genus", "Species" )]

#read table
species_loiczid <- read.table ("github_annex/species_loiczid", head=TRUE, sep=',')
head (species_loiczid)

#key species_loiczid

species_loiczid_keyed <- data.table(species_loiczid, key = "SPECIESID") 
rm(species_loiczid)
class (species_loiczid_keyed)

#key under25cm
under25cm_keyed <- data.table (under25cm, key="SPECIESID")
rm (under25cm)
class (under25cm_keyed)

#new table joining both tables by SpeciesID field. 
under25cm_xy <- species_loiczid_keyed[under25cm_keyed]
head (under25cm_xy)

#look at the unique values, it should be the same as number of observation in under25cm data frame:4049 
unique (under25cm_xy$SPECIESID) %>% length()#4049

unique (under25cm_xy$LOICZID) %>% length()#162502

#mapping the distribution of the species group

under25_grouped <-  under25cm_xy %>%
                    group_by (LOICZID)%>%
                    summarize(n_spp = n())

head (under25_grouped)  


under25map <- subs (basemap, under25_grouped, by= "LOICZID", which= "n_spp")
plot (under25map)
writeRaster(under25map, "under25map", format = "GTiff")

#--COMPARING-------------------------------------------------------------------------------------------------------------

proportion <- (under25map*100)/allspecies_map
plot (proportion)
writeRaster(proportion, "proportion_under25", format = "GTiff")
par(mfrow=c(1,3))
plot (under25map,
      main="Distribution of under 25 cm group")
plot (allspecies_map,
      main="Distribution of all species",
      axes=FALSE)
plot (proportion,
      main="Proportion of under 25 cm fish compared to all fish",
      axes=FALSE)

####################################################################################################################
#Mapping the distribution of those species betwee 25 cm and 50 cm (Loo>25<50)----------------------------------------------------
####################################################################################################################
summary (species_lt)

btw25and50cm <-subset (species_lt, species_lt$Loo>25 & species_lt$Loo<50)
colnames (btw25and50cm)
summary (btw25and50cm$Loo)
btw25and50cm <- btw25and50cm [c("SPECIESID", "Genus", "Species" )]
head (btw25and50cm)

#read table
species_loiczid <- read.table ("github_annex/species_loiczid", head=TRUE, sep=',')
head (species_loiczid)

#key species_loiczid

species_loiczid_keyed <- data.table(species_loiczid, key = "SPECIESID") 
rm(species_loiczid)
class (species_loiczid_keyed)
head (species_loiczid_keyed)

#key btw25and50cm
btw25and50cm_keyed <- data.table (btw25and50cm, key="SPECIESID")
rm (btw25and50cm)
class (btw25and50cm_keyed)
head (btw25and50cm_keyed)

#new table joining both tables by SpeciesID field. 
btw25and50cm_xy <- species_loiczid_keyed[btw25and50cm_keyed]
head (btw25and50cm_xy)

#look at the unique values, it should be the same as number of observation in under25cm data frame:4049 
unique (btw25and50cm_xy$SPECIESID) %>% length()#5392

unique (btw25and50cm_xy$LOICZID) %>% length()#166185

#mapping the distribution of the species group

btw25and50cm_grouped <-  btw25and50cm_xy %>%
  group_by (LOICZID)%>%
  summarize(n_spp = n())

head (under25_grouped)  


btw25and50cm_map <- subs (basemap, btw25and50cm_grouped, by= "LOICZID", which= "n_spp")
plot (btw25and50cm_map)
writeRaster(btw25and50cm_map, "btw25and50cm_map", format = "GTiff")
#---------------------------------------------------------------------------------------------------------------
proportion25_50 <- (btw25and50cm_map*100)/allspecies_map
plot (proportion25_50)
writeRaster(proportion25_50, "proportion25_50", format = "GTiff")
####################################################################################################################
#Mapping the distribution of those species between 50 cm and 100 cm (Loo>50<100)----------------------------------------------------
####################################################################################################################
btw50and100cm <-subset (species_lt, species_lt$Loo>50 & species_lt$Loo<100)
colnames (btw50and100cm)
summary (btw50and100cm$Loo)
btw50and100cm <- btw50and100cm [c("SPECIESID", "Genus", "Species" )]
head (btw50and100cm)

#read table
species_loiczid <- read.table ("github_annex/species_loiczid", head=TRUE, sep=',')
head (species_loiczid)

#key species_loiczid

species_loiczid_keyed <- data.table(species_loiczid, key = "SPECIESID") 
rm(species_loiczid)
class (species_loiczid_keyed)
head (species_loiczid_keyed)

#key btw25and50cm
btw50and100cm_keyed <- data.table (btw50and100cm, key="SPECIESID")
rm (btw50and100cm)
class (btw50and100cm_keyed)
head (btw50and100cm_keyed)

#new table joining both tables by SpeciesID field. 
btw50and100cm_xy <- species_loiczid_keyed[btw50and100cm_keyed]
head (btw50and100cm_xy)

#look at the unique values, it should be the same as number of observation in under25cm data frame:4049 
unique (btw50and100cm_xy$SPECIESID) %>% length()#1927

unique (btw50and100cm_xy$LOICZID) %>% length()#1163544

#mapping the distribution of the species group

btw50and100cm_grouped <-  btw50and100cm_xy %>%
  group_by (LOICZID)%>%
  summarize(n_spp = n())

head (btw50and100cm_grouped)  


btw50and100cm_map <- subs (basemap, btw50and100cm_grouped, by= "LOICZID", which= "n_spp")
plot (btw50and100cm_map)
writeRaster(btw50and100cm_map, "btw50and100cm_map", format = "GTiff")
#---------------------------------------------------------------------------------------------------------------
proportion50_100 <- (btw50and100cm_map*100)/allspecies_map
plot (proportion50_100)
writeRaster(proportion50_100, "proportion50_100", format = "GTiff")
####################################################################################################################
#Mapping the distribution of those species above 100 cm (Loo>50<100)----------------------------------------------------
####################################################################################################################
above100cm <-subset (species_lt, species_lt$Loo>100)
colnames (above100cm)
summary (above100cm$Loo)
above100cm <- above100cm [c("SPECIESID", "Genus", "Species" )]
head (above100cm)

#read table
species_loiczid <- read.table ("github_annex/species_loiczid", head=TRUE, sep=',')
head (species_loiczid)

#key species_loiczid

species_loiczid_keyed <- data.table(species_loiczid, key = "SPECIESID") 
rm(species_loiczid)
class (species_loiczid_keyed)
head (species_loiczid_keyed)

#key btw25and50cm
above100cm_keyed <- data.table (above100cm, key="SPECIESID")
rm (above100cm)
class (above100cm_keyed)
head (above100cm_keyed)

#new table joining both tables by SpeciesID field. 
above100cm_xy <- species_loiczid_keyed[above100cm_keyed]
head (above100cm_xy)

#look at the unique values, it should be the same as number of observation in under25cm data frame:4049 
unique (above100cm_xy$SPECIESID) %>% length()#1000

unique (above100cm_xy$LOICZID) %>% length()#147607

#mapping the distribution of the species group

above100cm_grouped <-  above100cm_xy %>%
  group_by (LOICZID)%>%
  summarize(n_spp = n())

head (above100cm_grouped)  


above100cm_map <- subs (basemap, above100cm_grouped, by= "LOICZID", which= "n_spp")
plot (above100cm_map)
writeRaster(above100cm_map, "above100cm_map", format = "GTiff")

proportion100 <- (above100cm_map*100)/allspecies_map
plot (proportion100)
#---------------------------------------------------------------------------------------------------------------
writeRaster(proportion100, "proportion100", format = "GTiff")

###MAPPING THE PROPORTION MAPS##############
par(mfrow=c(2,2))
plot (proportion,
      main="Proportion of species under 25 cm length")
plot (proportion25_50,
      main="Proportion of species between 25 and 50 cm length",
      axes=FALSE)
plot (proportion50_100,
      main="Proportion of species between 50 and 100 cm length",
      axes=FALSE)
plot (proportion100,
      main="Proportion of species above 100 cm length",
      axes=FALSE)
