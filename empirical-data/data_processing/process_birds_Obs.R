library(tidyverse)
library(sf)
library(tmap)

setwd("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets/Obs/birds")

### read in bird dataset ###################################

unzip("0022466-210914110416597.zip", list=TRUE) 

data <- readr::read_delim(unzip("0022466-210914110416597.zip", 
                                "occurrence.txt"),
                          delim="\t")

### subset the data #########################################

dat <- data %>%
  select(species, year,month,day,eventDate,class,
         decimalLongitude, decimalLatitude, countryCode,
         hasGeospatialIssues, recordedBy,recordedByID) %>%
  filter(class=="Aves") %>%
  filter(year>=1990) %>%
  filter(month %in% c(4,5,6)) %>%
  filter(countryCode=="DE") %>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(hasGeospatialIssues==FALSE)

nrow(data)
nrow(dat)

### summarise recorders ##############################################

#get first year for each observer
recorderData <- dat %>%
  dplyr::group_by(recordedBy) %>%
  dplyr::summarise(firstYear = min(year)) %>%
  dplyr::group_by(firstYear) %>%
  dplyr::summarise(firstyearObs = length(unique(recordedBy)))

#total observers each year
annualRecorders <- dat %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(totalObs = length(unique(recordedBy)))

annualRecorders$firstyearObs <- recorderData$firstyearObs[match(annualRecorders$year,
                                                                recorderData$firstYear)]
annualRecorders$firstyearObs[is.na(annualRecorders$firstyearObs)] <- 0

#save as data frame
saveRDS(annualRecorders,file="C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets/Obs/recorders_revision_Obs_birds.rds")

# #add to full data set
# recorderData <- dat %>%
#   dplyr::group_by(recordedBy) %>%
#   dplyr::summarise(firstYear = min(year))
# 
# dat$firstYear <-  recorderData$firstYear[match(dat$recordedBy,
#                                          recorderData$recordedBy)] 
# 
# dat$newObs <- ifelse(dat$firstYear==dat$year,1,0)

### get mtbq shapefile ##################################################

mtbqs <- st_read("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/MTB_Q Informations/MTBQ_shapefile/MTBQ_25833.shp")
mtbqs$Q <- NA
mtbqs$Q[mtbqs$Quadrant=="NW"] <- 1
mtbqs$Q[mtbqs$Quadrant=="NO"] <- 2
mtbqs$Q[mtbqs$Quadrant=="SW"] <- 3
mtbqs$Q[mtbqs$Quadrant=="SO"] <- 4
mtbqs$MTB_Q <- paste0(mtbqs$Value,mtbqs$Q)


### map species obs to mtbqs #############################################

coords <- st_as_sf(dat, coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
coords <- st_transform(coords, crs=st_crs(mtbqs))

#tm_shape(mtbqs)+
#  tm_polygons()+
#  tm_shape(coords)+
#  tm_dots(col="red")

temp <- st_join(coords,mtbqs,left=TRUE)

### summarise mtbqs #####################################################

#get first year for each mtbq
recorderData <- temp %>%
  as_tibble() %>%
  dplyr::group_by(MTB_Q) %>%
  dplyr::summarise(firstYear = min(year)) %>%
  dplyr::group_by(firstYear) %>%
  dplyr::summarise(firstyearMTBQs = length(unique(MTB_Q)))

#total observers each year
annualRecorders <- temp %>%
  as_tibble() %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(totalMTBQs = length(unique(MTB_Q)))

annualRecorders$firstyearMTBQs <- recorderData$firstyearMTBQs[match(annualRecorders$year,
                                                                    recorderData$firstYear)]
annualRecorders$firstyearMTBQs[is.na(annualRecorders$firstyearMTBQs)] <- 0

#save as data frame
saveRDS(annualRecorders,file="C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets/Obs/mtbqs_revision_Obs_birds.rds")

### slice ##############################################################

surveys <- temp %>%
  group_by(year,MTB_Q) %>%
  summarise(nuRecs = length(species), 
            nuSpecies = n_distinct(species))

expandGrid <- expand_grid(MTB_Q = sort(unique(mtbqs$MTB_Q)),
                          year = 1990:2019)

surveys <- left_join(expandGrid,surveys)
surveys$nuRecs[is.na(surveys$nuRecs)] <- 0
surveys$nuSpecies[is.na(surveys$nuSpecies)] <- 0
surveys$Visited <- ifelse(surveys$nuRecs>0,1,0)
tapply(surveys$Visited,surveys$year,mean)
names(surveys)[which(names(surveys)=="year")] <- "Year"

### get land use data ##################################################

environData <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/environ-data/esacci_MTBQ.rds")
environData$Q <- NA
environData$Q[environData$Quadrant=="NW"] <- 1
environData$Q[environData$Quadrant=="NO"] <- 2
environData$Q[environData$Quadrant=="SW"] <- 3
environData$Q[environData$Quadrant=="SO"] <- 4
environData$MTB_Q <- paste0(environData$Value,environData$Q)

surveys <- inner_join(surveys,environData,by=c("Year","MTB_Q"))
floor_decade    = function(value){ return(value - value %% 10) }
surveys$Decade <- floor_decade(surveys$Year)
surveys$DecadeF <- factor(surveys$Decade)
levels(surveys$DecadeF) <- c("1990s","2000s","2010s")

#### save file ##########################################################

saveRDS(surveys,file="C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets/observation.org/surveys_revision_Obs_birds.rds")
