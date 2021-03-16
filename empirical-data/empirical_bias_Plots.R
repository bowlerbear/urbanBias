library(sf)
library(tidyverse)
library(ggthemes)
library(broom)
library(plyr)

#### amphibians ######
samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Amphibians/amphiAnalysis/HPC_inputs/samplingIntensity.rds")
st_geometry(samplingIntensity) <- NULL

#put numbers in 2 -year periods
samplingIntensity$Year2 <- samplingIntensity$Year
samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
samplingIntensity_Group <- samplingIntensity %>%
  dplyr::group_by(MTB_Q,Year2) %>%
  dplyr::summarise(Visited = max(Visited),
                   urban = median(urban))

#left
amphisL <- 
  ggplot(samplingIntensity_Group,
       aes(x=urban*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  theme(legend.position = c(0.1,0.75))+
  theme(legend.key.height = unit(0.275, 'cm'),
        legend.key.width = unit(0.15, 'cm'))+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")


#right
#separate models
myYears <- 1992:2018
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = subset(samplingIntensity,Year == y))
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

amphisR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))


#### butterflies ####

samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_data/Butterflies/surveys_butterflies.rds")
#filtered to human observations

#put numbers in 2 -year periods
samplingIntensity$Year2 <- samplingIntensity$Year
samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
samplingIntensity_Group <- samplingIntensity %>%
  dplyr::group_by(MTB_Q,Year2) %>%
  dplyr::summarise(Visited = max(Visited),
            urban = median(urban))

#left
buttL <- ggplot(samplingIntensity_Group,
       aes(x=urban*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  theme(legend.position="none")+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")

#right
myYears <- sort(unique(samplingIntensity$Year))
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = subset(samplingIntensity,Year == y))
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

buttR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))

#### birds ####

samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_data/Naturgucker/surveys_birds.rds")

#put numbers in 2 -year periods
samplingIntensity$Year2 <- samplingIntensity$Year
samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
samplingIntensity_Group <- samplingIntensity %>%
  dplyr::group_by(MTB_Q,Year2) %>%
  dplyr::summarise(Visited = max(Visited),
            urban = median(urban))

#left
birdsL <- ggplot(samplingIntensity_Group,
       aes(x=urban*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  theme(legend.position = "none")+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")

#right
# year as a factor
samplingIntensity$YearF <- factor(samplingIntensity$Year)

#one model
model1 <- glm(Visited ~ -1 + YearF * urban, family= binomial, data = samplingIntensity)
summary(model1)

modelCoefs <- model1 %>%
  tidy() %>%
  mutate(lower = estimate - 2*std.error, 
         upper = estimate + 2*std.error) %>%
  mutate(Year = as.numeric(str_remove_all(term,"[^0-9.-]"))) %>%
  filter(str_detect(term, ':'))

#separate models
myYears <- sort(unique(samplingIntensity$Year))
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = subset(samplingIntensity,Year == y))
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

birdsR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
    
#### combine ####

library(cowplot)
amphis <- plot_grid(amphisL,amphisR,nrow=1)
butt <- plot_grid(buttL,buttR,nrow=1)
birds <- plot_grid(birdsL,birdsR,nrow=1)

plot_grid(amphis,butt,birds,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds"),
          align = "v",
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

ggsave("C:/Users/db40fysa/Dropbox/CS spatial pattern/MS/urbanBias/realworldBias.png",width=9.3,height=9)

#### end ####