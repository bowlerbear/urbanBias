#explore models
library(tidyverse)
library(ggthemes)
library(cowplot)

modelFiles <- list.files("model-outputs")

### annual urban #######

modelFiles_subset <- modelFiles %>% 
  str_subset("annualBiasSpace_") %>% 
  str_subset("PA_area", negate=TRUE)

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][2]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels$Year <- as.numeric(allModels$Year)
allModels$taxa[which(allModels$taxa=="b")] <- "birds"

#quick look

ggplot(subset(allModels,model=="glm"))+
  geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(dataset~taxa,scales="free")

ggplot(subset(allModels,model=="spaMM"))+
  geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(dataset~taxa,scales="free")

### plotting ####

plotUrbanBias <- function(myfolder,mytaxa){

  modelCoefs <- allModels %>%
                  filter(model=="spaMM") %>%
                  filter(dataset==myfolder) %>%
                  filter(taxa==mytaxa) %>%
                  filter(Year>2001)
  
  ggplot(modelCoefs)+
    geom_crossbar(aes(x = Year, y = estimate, 
                      ymax = upper, ymin = lower, fill=Year))+
    geom_hline(yintercept = 0, colour="red", linetype="dashed")+
    theme_few()+ylab("Effect of urban cover")+
    scale_fill_viridis_c("Year")+
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
}

#plants
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="plants")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="plants")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="plants")
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="birds")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="birds")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="birds")
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="butterflies")
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotUrbanBias(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotUrbanBias(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotUrbanBias(myfolder = "GBIF",mytaxa ="amphibians")
amphisSI <- plot_grid(p1,p2,p3,nrow=1)

#main text

plot_grid(amphisSI,buttsSI,birdsSI,plantsSI,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.9,0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95,0.95), 
          hjust = c(-0.35,-0.35,-0.5,-0.5))

ggsave("plots/annual_urban_spaMM.png",width=8,height=9)

### urban interaction ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("interactionBiasSpace_") %>% 
  str_subset("PAarea", negate=TRUE)

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][2]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels

allModels <- allModels %>%
              mutate(across(where(is.numeric), round, 3))

write.csv(allModels,"plots/urban_interaction.csv",row.names = FALSE)

### urban main ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("mainBiasSpace") %>%
  str_subset("PAarea", negate=TRUE)

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][2]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels

allModels <- allModels %>%
  mutate(across(where(is.numeric), round, 3))


write.csv(allModels,"plots/urban_main.csv",row.names = FALSE)

### annual PA area ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("annualBiasSpace_") %>% 
  str_subset("PA_area")

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][4]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][5]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels$Year <- as.numeric(allModels$Year)
allModels$taxa[which(allModels$taxa=="b")] <- "birds"

plotBias <- function(myfolder,mytaxa){
  
  modelCoefs <- allModels %>%
    filter(model=="spaMM") %>%
    filter(dataset==myfolder) %>%
    filter(taxa==mytaxa) %>%
    filter(Year>2001)
  
  ggplot(modelCoefs)+
    geom_crossbar(aes(x = Year, y = estimate, 
                      ymax = upper, ymin = lower, fill=Year))+
    geom_hline(yintercept = 0, colour="red", linetype="dashed")+
    theme_few()+ylab("Effect of protected area")+
    scale_fill_viridis_c("Year")+
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
}

#plants
p1 <- plotBias(myfolder = "NG",mytaxa ="plants")
p2 <- plotBias(myfolder = "Obs",mytaxa ="plants")
p3 <- plotBias(myfolder = "GBIF",mytaxa ="plants")
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotBias(myfolder = "NG",mytaxa ="birds")
p2 <- plotBias(myfolder = "Obs",mytaxa ="birds")
p3 <- plotBias(myfolder = "GBIF",mytaxa ="birds")
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotBias(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotBias(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotBias(myfolder = "GBIF",mytaxa ="butterflies")
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotBias(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotBias(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotBias(myfolder = "GBIF",mytaxa ="amphibians")
amphisSI <- plot_grid(p1,p2,p3,nrow=1)

#main text

plot_grid(amphisSI,buttsSI,birdsSI,plantsSI,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds",
                   "D - Plants"),
          align = "v",
          scale = c(0.9,0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95,0.95), 
          hjust = c(-0.35,-0.35,-0.5,-0.5))

ggsave("plots/annual_PAarea_spaMM.png",width=8,height=9)

### PA area interaction ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("interactionBiasSpace_PAarea") 

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][4]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels

allModels <- allModels %>%
  mutate(across(where(is.numeric), round, 3))


write.csv(allModels,"plots/paArea_interaction.csv",row.names = FALSE)

### PA main ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("mainBiasSpace_PAarea") 

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][4]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels

allModels <- allModels %>%
  mutate(across(where(is.numeric), round, 3))


write.csv(allModels,"plots/paArea_main.csv",row.names = FALSE)


### test prop years ####

modelFiles <- list.files("model-outputs")

modelFiles_subset <- modelFiles %>% 
  str_subset("testpropYears") 

allModels <- lapply(modelFiles_subset, function(x){
  temp <- readRDS(paste("model-outputs",x,sep="/"))
  temp$Files <- x
  temp$dataset <- strsplit(temp$Files,"_")[[1]][2]
  temp$taxa <- strsplit(temp$Files,"_")[[1]][3]
  temp$taxa <- gsub(".rds","",temp$taxa)
  return(temp)
})
allModels <- do.call(rbind,allModels)
allModels

write.csv(allModels,file="plots/testPropyears.csv")

### end ####

