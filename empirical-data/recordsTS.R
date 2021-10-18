library(tidyverse)
library(sf)
library(ggthemes)
library(cowplot)

setwd("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets")

### functions ####

#for each folder
#for each taxa
#we want to plot the number of records per year

getRecordTS <- function(myfolder,mytaxa){
  
  mydata <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  summaryData <- mydata %>%
                  as_tibble() %>%
                  group_by(Year) %>%
                  summarise(nuVisits = sum(Visited)) %>%
                  filter(Year> 1991 & Year<2019)
  
  return(summaryData)
  
}


plotTS <- function(myfolder,mytaxa){
 
  summaryData <- getRecordTS(myfolder,mytaxa)
  
  ggplot(summaryData, aes(x=Year, y=nuVisits)) +
    geom_point() +
    geom_line() +
    theme_few() +
    scale_y_log10() + 
    scale_x_continuous(breaks = seq(1990,2020,by=5)) +
    ylab("Number of visited grids")

}

#### application

#plants
p1 <- plotTS(myfolder = "Obs", mytaxa="plants")
p2 <- plotTS(myfolder = "NG", mytaxa="plants")
p3 <- plotTS(myfolder = "iNat", mytaxa="plants")
p4 <- plotTS(myfolder = "GBIF", mytaxa="plants")
plants <- plot_grid(p1,p2,p4, nrow=1)

#amphibians/reptiles
p1 <- plotTS(myfolder = "Obs", mytaxa="amphibians")
p2 <- plotTS(myfolder = "NG", mytaxa="amphibians")
p3 <- plotTS(myfolder = "iNat", mytaxa="amphibians")
p4 <- plotTS(myfolder = "GBIF", mytaxa="amphibians")
amphis <- plot_grid(p1,p2,p4, nrow=1)

#birds
p1 <- plotTS(myfolder = "Obs", mytaxa="birds")
p2 <- plotTS(myfolder = "NG", mytaxa="birds")
p3 <- plotTS(myfolder = "iNat", mytaxa="birds")
p4 <- plotTS(myfolder = "GBIF", mytaxa="birds")
birds <- plot_grid(p1,p2,p4, nrow=1)

#butterflies
p1 <- plotTS(myfolder = "Obs", mytaxa="butterflies")
p2 <- plotTS(myfolder = "NG", mytaxa="butterflies")
p3 <- plotTS(myfolder = "iNat", mytaxa="butterflies")
p4 <- plotTS(myfolder = "GBIF", mytaxa="butterflies")
butts <- plot_grid(p1,p2,p4, nrow=1)

#based on the above, iNat only has data from recent years so lets not use that one
plot_grid(
  plants, amphis, birds, butts,
  nrow=4)

