library(sf)
library(tidyverse)
library(ggthemes)
library(broom)
library(cowplot)
library(mgcv)

### functions #####

source("empirical-data/main_functions.R")

### environ datasets ####

#localPC
load("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/mtbqsDF.RData")

#or rstudio server
#load("/data/dbowler/Odonata/data/mtbqsDF.RData")

### set wd ####

#local PC
setwd("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_other_data/urbanBias_datasets")

#setwd("/data/dbowler/urbanBias")

### plot time series ####

p1 <- plotTS(myfolder = "NG",mytaxa ="plants")
p2 <- plotTS(myfolder = "Obs",mytaxa ="plants")
p3 <- plotTS(myfolder = "GBIF",mytaxa ="plants")
plantsSI <- plot_grid(p1,p2,p3,nrow=1)

#birds
p1 <- plotTS(myfolder = "NG",mytaxa ="birds")
p2 <- plotTS(myfolder = "Obs",mytaxa ="birds")
p3 <- plotTS(myfolder = "GBIF",mytaxa ="birds")
birdsSI <- plot_grid(p1,p2,p3,nrow=1)

#butterflies
p1 <- plotTS(myfolder = "NG",mytaxa ="butterflies")
p2 <- plotTS(myfolder = "Obs",mytaxa ="butterflies")
p3 <- plotTS(myfolder = "GBIF",mytaxa ="butterflies")
buttsSI <- plot_grid(p1,p2,p3,nrow=1)

#amphibians
p1 <- plotTS(myfolder = "NG",mytaxa ="amphibians")
p2 <- plotTS(myfolder = "Obs",mytaxa ="amphibians")
p3 <- plotTS(myfolder = "GBIF",mytaxa ="amphibians")
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

ggsave("realworldBias_TS_revision.png",width=9.3,height=9.5)

###plot urban bias ####

plotUrbanBias <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  #put numbers in 2 -year periods
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$Year2 <- samplingIntensity$Year
  samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
  samplingIntensity_Group <- samplingIntensity %>%
    dplyr::group_by(MTB_Q,Year2) %>%
    dplyr::summarise(Visited = max(Visited),
                     urban = median(urban))
  
  #left plot
  buttL <- ggplot(samplingIntensity_Group,
                  aes(x=urban*100,y=Visited,group=Year2))+
    stat_smooth(aes(color=Year2),
                method = "glm", 
                method.args = list(family = "binomial"),
                size=1.5,
                se=FALSE)+
    theme_few()+
    ylim(0,1)+
    theme(legend.position="none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_color_viridis_c("Year")+
    xlab("Urban cover (%)")+ylab("Visit probability")
  
  #right plot
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using glm
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    if(sum(tempData$Visited)>10){
      glm1 <- glm(Visited ~ urban, family= binomial, data = tempData)
      glmConfint <- confint(glm1)
      data.frame(Year = y, 
                 estimate=summary(glm1)$coefficients[2,1],
                 lower=glmConfint[2,1],
                 upper=glmConfint[2,2])}
    
    else{
      data.frame(Year = y, 
                 estimate=NA,
                 lower=NA,
                 upper=NA)
    }
    
  })
  
  
  buttR <- ggplot(modelCoefs)+
    geom_crossbar(aes(x = Year, y = estimate, 
                      ymax = upper, ymin = lower, fill=Year))+
    geom_hline(yintercept = 0, colour="red", linetype="dashed")+
    theme_few()+ylab("Effect of urban cover")+
    scale_fill_viridis_c("Year")+
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
  
  
  cowplot::plot_grid(buttL,buttR,nrow=1)
  
  
}


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
          scale = c(0.9,0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95,0.95), hjust = c(-0.35,-0.35,-0.5,-0.5))

#ggsave("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Spatial_bias/urbanBias/urbanBias/plots/realworldBias_urban_revision.png",width=9.3,height=11)
ggsave("realworldBias_urban_revision.png",width=9.3,height=9.5)

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

#ggsave("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Spatial_bias/urbanBias/urbanBias/plots/realworldBias_urban_SI_revision.png",width=9.3,height=11)
ggsave("realworldBias_urban_SI_revision.png",width=13,height=9)

### test visit bias #####

##### annual effects ####

out <- getAnnualBias(myfolder = "GBIF",mytaxa ="plants")
out2 <- getAnnualBiasSpace(myfolder = "GBIF",mytaxa ="plants")
saveRDS(out2,file="annualBiasSpace_GBIF_plants_2000.rds")

#compare glm vs spaMM
out2 <- readRDS("annualBiasSpace_GBIF_plants_2000.rds")

ggplot(subset(out2,Year<2018))+
  geom_pointrange(aes(x=Year,y=estimate,ymin=lower,ymax=upper,colour=model))+
  theme_few()

out2_pivot <- out2 %>%
              group_by(Year) %>%
              select(model,estimate,lower,upper) %>%
              pivot_wider(.,names_from = model, values_from = c(estimate,lower,upper))

ggplot(data = subset(out2_pivot,Year<2018),
       aes(x = estimate_glm, y = estimate_spaMM)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower_spaMM, ymax = upper_spaMM)) + 
  geom_errorbarh(aes(xmin = lower_glm, xmax = upper_glm))+
  theme_few()

#run bias models
out2 <- getAnnualBiasSpace(myfolder = "GBIF",mytaxa ="plants")
saveRDS(out2,file="annualBiasSpace_GBIF_plants_5000.rds")

out2 <- getAnnualBiasSpace(myfolder = "GBIF",mytaxa ="plants")
saveRDS(out2,file="annualBiasSpace_GBIF_plants_5000.rds")

out2 <- getAnnualBiasSpace(myfolder = "GBIF",mytaxa ="plants")
saveRDS(out2,file="annualBiasSpace_GBIF_plants_5000.rds")

out2 <- getAnnualBiasSpace(myfolder = "GBIF",mytaxa ="plants")
saveRDS(out2,file="annualBiasSpace_GBIF_plants_5000.rds")

##### year interaction #####

#plants
p1 <- testVisitBias(myfolder = "NG",mytaxa ="plants")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="plants")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="plants")
plants <- bind_rows(p1,p2,p3)

#birds
p1 <- testVisitBias(myfolder = "NG",mytaxa ="birds")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="birds")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="birds")
birds <- bind_rows(p1,p2,p3)

#butterflies
p1 <- testVisitBias(myfolder = "NG",mytaxa ="butterflies")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="butterflies")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="butterflies")
butts <- bind_rows(p1,p2,p3)

#amphibians
p1 <- testVisitBias(myfolder = "NG",mytaxa ="amphibians")
p2 <- testVisitBias(myfolder = "Obs",mytaxa ="amphibians")
p3 <- testVisitBias(myfolder = "GBIF",mytaxa ="amphibians")
amphis <- bind_rows(p1,p2,p3)

#combine all and format into a table
allDF <- bind_rows(plants,birds,butts,amphis) %>%
          filter((term=="urban" & model=="urban")|
                   (term=="I(Year - 1991):urban") & model=="year*urban") %>%
  group_by(dataset,taxa) %>%
  select(c(term, estimate,std.error,p.value)) %>%
  pivot_wider(names_from = term, values_from=c(estimate,std.error,p.value)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write.csv(allDF,file="urban_tests.csv",row.names=FALSE)

### understanding the bias ####

#how else to test the bias

#increased bias through time - focus on 2005 onwards

# compare in 5-year periods (over last 15 years)
# urban cover of newly visited sites
# urban cover of sites visited by new recorders
# urban change in sites that are sampled in all time periods vs only old samples vs new #samples

#### urban effect ####

urbanchangeEffect(myfolder = "GBIF", mytaxa = "amphibians")
urbanchangeEffect(myfolder = "GBIF", mytaxa = "plants")
urbanchangeEffect(myfolder = "GBIF", mytaxa = "birds")
urbanchangeEffect(myfolder = "GBIF", mytaxa = "butterflies")
urbanchangeEffect(myfolder = "NG", mytaxa = "amphibians")
urbanchangeEffect(myfolder = "NG", mytaxa = "plants")
urbanchangeEffect(myfolder = "NG", mytaxa = "birds")
urbanchangeEffect(myfolder = "NG", mytaxa = "butterflies")
urbanchangeEffect(myfolder = "Obs", mytaxa = "amphibians")
urbanchangeEffect(myfolder = "Obs", mytaxa = "plants")
urbanchangeEffect(myfolder = "Obs", mytaxa = "birds")
urbanchangeEffect(myfolder = "Obs", mytaxa = "butterflies")


#### annual ####
plotAllTS(myfolder = "GBIF", mytaxa = "amphibians")
plotAllTS(myfolder = "GBIF", mytaxa = "plants")
plotAllTS(myfolder = "GBIF", mytaxa = "birds")
plotAllTS(myfolder = "GBIF", mytaxa = "butterflies")

#naturgucker
plotAllNew(myfolder = "NG", mytaxa = "amphibians")
amphis <- testAllNew(myfolder = "NG", mytaxa = "amphibians")
plotAllNew(myfolder = "NG", mytaxa = "plants")
plants <- testAllNew(myfolder = "NG", mytaxa = "plants")
plotAllNew(myfolder = "NG", mytaxa = "birds")
birds <- testAllNew(myfolder = "NG", mytaxa = "birds")
plotAllNew(myfolder = "NG", mytaxa = "butterflies")
butts <- testAllNew(myfolder = "NG", mytaxa = "butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

NG <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()


#GBIF
plotAllNew(myfolder = "GBIF", mytaxa = "amphibians")
amphis <- testAllNew(myfolder = "GBIF", mytaxa = "amphibians")
plotAllNew(myfolder = "GBIF", mytaxa = "plants")
plants <- testAllNew(myfolder = "GBIF", mytaxa = "plants")
plotAllNew(myfolder = "GBIF", mytaxa = "birds")
birds <- testAllNew(myfolder = "GBIF", mytaxa = "birds")
plotAllNew(myfolder = "GBIF", mytaxa = "butterflies")
butts <- testAllNew(myfolder = "GBIF", mytaxa = "butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

GBIF <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()


#Obs 
plotAllNew(myfolder = "Obs", mytaxa = "amphibians")
amphis <- testAllNew(myfolder = "Obs", mytaxa = "amphibians")
plotAllNew(myfolder = "Obs", mytaxa = "plants")
plants <- testAllNew(myfolder = "Obs", mytaxa = "plants")
plotAllNew(myfolder = "Obs", mytaxa = "birds")
birds <- testAllNew(myfolder = "Obs", mytaxa = "birds")
plotAllNew(myfolder = "Obs", mytaxa = "butterflies")
butts <- testAllNew(myfolder = "Obs", mytaxa = "butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

Obs <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()

cowplot::plot_grid(NG,GBIF,Obs,
                   ncol=1,
                   labels=c("A - Naturgucker",
                            "B - GBIF",
                            "C - Observation.org"),
                   align = "v",
                   scale = c(0.85,0.85,0.85),
                   vjust = c(0.95,0.95,0.95), 
                   hjust = c(-0.5,-0.8,-0.4))

#ggsave("realworldBias_urban_causes_revision.png",width=13,height=9)
ggsave("realworldBias_urban_causes_revision_2000on.png",width=13,height=9)

#### annual change ####

#naturgucker
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
amphis <- testAnnualDifferences(AnnualDifferences_DF) %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
plants<- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
birds <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
butts <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

NG <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()


#GBIF
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
amphis <- testAnnualDifferences(AnnualDifferences_DF) %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
plants<- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
birds <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
butts <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

GBIF <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()

#Obs
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
amphis <- testAnnualDifferences(AnnualDifferences_DF) %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
plants<- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
birds <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="change")
butts <- testAnnualDifferences(AnnualDifferences_DF)%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

Obs <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()

cowplot::plot_grid(NG,GBIF,Obs,
                   ncol=1,
                   labels=c("A - Naturgucker",
                            "B - GBIF",
                            "C - Observation.org"),
                   align = "v",
                   scale = c(0.85,0.85,0.85),
                   vjust = c(0.95,0.95,0.95), 
                   hjust = c(-0.5,-0.8,-0.4))

#ggsave("realworldBias_urban_changecauses_revision.png",width=13,height=9)
ggsave("realworldBias_urban_changecauses_revision_2000on.png",width=13,height=9)

#### differences ####

AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
amphis <- testAnnualDifferences(AnnualDifferences_DF, type="diff") %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
plants<- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
birds <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "NG", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
butts <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

NG <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()


#GBIF
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
amphis <- testAnnualDifferences(AnnualDifferences_DF, type="diff") %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
plants<- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
birds <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "GBIF", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
butts <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

GBIF <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()

#Obs
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "amphibians")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
amphis <- testAnnualDifferences(AnnualDifferences_DF, type="diff") %>% add_column(taxa="amphis")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "plants")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
plants<- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="plants")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "birds")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
birds <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="birds")
AnnualDifferences_DF <- getAnnualDifferences(myfolder = "Obs", mytaxa = "butterflies")
plotAnnualDifferences(AnnualDifferences_DF, type="diff")
butts <- testAnnualDifferences(AnnualDifferences_DF, type="diff")%>% add_column(taxa="butterflies")
allDF <- bind_rows(amphis, plants, birds, butts)
allDF$type <- factor(allDF$type, levels = c("total recorders","prop new recorders",
                                            "total sites", "prop new sites",
                                            "urban cover"))

Obs <- ggplot(allDF)+
  geom_col(aes(x=taxa,y=cor))+
  facet_wrap(~type,nrow=1)+
  coord_flip()+
  xlab("taxa group") + ylab("correlation with annual bias") +
  geom_hline(yintercept=0,linetype="dashed")+
  theme_few()

cowplot::plot_grid(NG,GBIF,Obs,
                   ncol=1,
                   labels=c("A - Naturgucker",
                            "B - GBIF",
                            "C - Observation.org"),
                   align = "v",
                   scale = c(0.85,0.85,0.85),
                   vjust = c(0.95,0.95,0.95), 
                   hjust = c(-0.5,-0.8,-0.4))

ggsave("realworldBias_urban_diffcauses_revision.png",width=13,height=9)

### prop yearly samples ####

p1 <- testpropYears(myfolder = "NG",mytaxa ="plants")
p2 <- testpropYears(myfolder = "Obs",mytaxa ="plants")
p3 <- testpropYears(myfolder = "GBIF",mytaxa ="plants")
plants <- bind_rows(p1,p2,p3)

#birds
p1 <- testpropYears(myfolder = "NG",mytaxa ="birds")
p2 <- testpropYears(myfolder = "Obs",mytaxa ="birds")
p3 <- testpropYears(myfolder = "GBIF",mytaxa ="birds")
birds <- bind_rows(p1,p2,p3)

#butterflies
p1 <- testpropYears(myfolder = "NG",mytaxa ="butterflies")
p2 <- testpropYears(myfolder = "Obs",mytaxa ="butterflies")
p3 <- testpropYears(myfolder = "GBIF",mytaxa ="butterflies")
butts <- bind_rows(p1,p2,p3)

#amphibians
p1 <- testpropYears(myfolder = "NG",mytaxa ="amphibians")
p2 <- testpropYears(myfolder = "Obs",mytaxa ="amphibians")
p3 <- testpropYears(myfolder = "GBIF",mytaxa ="amphibians")
amphis <- bind_rows(p1,p2,p3)

allDF <- bind_rows(plants,birds,butts,amphis) %>%
          filter(term!="(Intercept)")

### end ####
