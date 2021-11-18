#explore models
library(tidyverse)

modelFiles <- list.files("model-outputs")

### annual urban #######

modelFiles_subset <- modelFiles %>% str_subset("annualBiasSpace_") 

#glm plots
library(ggplot2)

ggplot(subset(allModels,model=="glm"))+
  geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(dataset~taxa,scales="free")

ggplot(subset(allModels,model=="spaMM"))+
  geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(dataset~taxa,scales="free")

#run again for for longer and more iterations
ggplot(subset(allModels,model=="spaMM" & Year >2010))+
  geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
  geom_hline(yintercept=0,linetype="dashed")+
  facet_wrap(dataset~taxa,scales="free")


### urban interaction ####

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



