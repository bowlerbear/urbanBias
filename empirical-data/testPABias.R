library(sf)
library(tidyverse)
library(ggthemes)
library(broom)
library(cowplot)
library(mgcv)

### environ datasets ####

#localPC
load("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/mtbqsDF.RData")

#or rstudio server
#load("/data/dbowler/Odonata/data/mtbqsDF.RData")

### functions #####

source("empirical-data/main_functions.R")

### PA data ######

protectedArea <- readRDS("empirical-data/protectedarea_MTBQ.rds")

### set wd ####

#local PC
setwd("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets")

#setwd("/data/dbowler/urbanBias")


### plot pa bias ####

#plants
p1 <- plotPABias(myfolder = "NG",mytaxa ="plants")
p2 <- plotPABias(myfolder = "Obs",mytaxa ="plants")
p3 <- plotPABias(myfolder = "GBIF",mytaxa ="plants")
plants <- plot_grid(p1,p3,nrow=1)
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotPABias(myfolder = "NG",mytaxa ="birds")
p2 <- plotPABias(myfolder = "Obs",mytaxa ="birds")
p3 <- plotPABias(myfolder = "GBIF",mytaxa ="birds")
birds <- plot_grid(p1,p3,nrow=1)
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotPABias(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotPABias(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotPABias(myfolder = "GBIF",mytaxa ="butterflies")
butts <- plot_grid(p1,p3,nrow=1)
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotPABias(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotPABias(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotPABias(myfolder = "GBIF",mytaxa ="amphibians")
amphis <- plot_grid(p1,p3,nrow=1)
amphisSI <- plot_grid(p1,p2,p3,nrow=1)

#main text

plot_grid(amphis,butts,birds,plants,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.9,0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95,0.95), hjust = c(-0.35,-0.35,-0.5,-0.5))

ggsave("realworldBias_PA_revision.png",width=9.3,height=9.5)

#SI

plot_grid(amphisSI,buttsSI,birdsSI,plantsSI,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.85,0.85,0.85,0.85),
          vjust = c(0.95,0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.5,-0.5))

ggsave("realworldBias_PA_SI_revision.png",width=13.5,height=9)


#interaction

#plants
p1 <- testVisitBias(myfolder = "NG",mytaxa ="plants",type="PA")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="plants",type="PA")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="plants",type="PA")
plants <- bind_rows(p1,p2,p3)

#birds
p1 <- testVisitBias(myfolder = "NG",mytaxa ="birds",type="PA")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="birds",type="PA")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="birds",type="PA")
birds <- bind_rows(p1,p2,p3)

#butterflies
p1 <- testVisitBias(myfolder = "NG",mytaxa ="butterflies",type="PA")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="butterflies",type="PA")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="butterflies",type="PA")
butts <- bind_rows(p1,p2,p3)

#amphibians
p1 <- testVisitBias(myfolder = "NG",mytaxa ="amphibians",type="PA")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="amphibians",type="PA")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="amphibians",type="PA")
amphis <- bind_rows(p1,p2,p3)

#combine all and format into a table
allDF <- bind_rows(plants,birds,butts,amphis) %>%
  filter((term=="PA_area" & model=="PA")|
           (term=="I(Year - 1991):PA_area") & model=="year*PA") %>%
  group_by(dataset,taxa) %>%
  select(c(term, estimate,std.error,p.value)) %>%
  pivot_wider(names_from = term, values_from=c(estimate,std.error,p.value)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write.csv(allDF,file="PA_tests.csv",row.names=FALSE)
