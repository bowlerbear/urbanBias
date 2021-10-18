library(sf)
library(tidyverse)
library(ggthemes)
library(broom)
library(plyr)
library(cowplot)
library(mgcv)

### environ datasets ####

#mtbq
load("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/mtbqsDF.RData")

### functions #####

source("empirical-data/main_functions.R")

### plot urban bias ####

setwd("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets")

#plants
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="plants")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="plants")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="plants")
plants <- plot_grid(p1,p3,nrow=1)
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="birds")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="birds")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="birds")
birds <- plot_grid(p1,p3,nrow=1)
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="butterflies")
butts <- plot_grid(p1,p3,nrow=1)
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="amphibians")
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
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

ggsave("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Spatial_bias/urbanBias/urbanBias/plots/realworldBias_urban_revision.png",width=9.3,height=11)

### plot urban cover difference ####

#plants
p1 <- plotUrbanCover3(myfolder = "NG",mytaxa ="plants")
p2 <- plotUrbanCover3(myfolder = "Obs",mytaxa ="plants")
p3 <- plotUrbanCover3(myfolder = "GBIF",mytaxa ="plants")
plants <- plot_grid(p1,p3,nrow=1)
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotUrbanCover3(myfolder = "NG",mytaxa ="birds")
p2 <- plotUrbanCover3(myfolder = "Obs",mytaxa ="birds")
p3 <- plotUrbanCover3(myfolder = "GBIF",mytaxa ="birds")
birds <- plot_grid(p1,p3,nrow=1)
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotUrbanCover3(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotUrbanCover3(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotUrbanCover3(myfolder = "GBIF",mytaxa ="butterflies")
butts <- plot_grid(p1,p3,nrow=1)
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotUrbanCover3(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotUrbanCover3(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotUrbanCover3(myfolder = "GBIF",mytaxa ="amphibians")
amphis <- plot_grid(p1,p3,nrow=1)
amphisSI <- plot_grid(p1,p2,p3,nrow=1)

plot_grid(amphis,butts,birds,plants,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

plot_grid(amphisSI,buttsSI,birdsSI,plantsSI,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

### test bias #####

testUrbanBias(myfolder = "NG",mytaxa ="butterflies")
