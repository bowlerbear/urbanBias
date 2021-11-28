library(sf)
library(tidyverse)
library(ggthemes)
library(broom)
library(plyr)
library(cowplot)
library(mgcv)

#add on x and y
load("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/mtbqsDF.RData")

### URBAN #####

#### amphibians ######
samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Amphibians/amphiAnalysis/HPC_inputs/samplingIntensity.rds")
st_geometry(samplingIntensity) <- NULL
nrow(samplingIntensity)#17523

#put numbers in 2 -year periods
samplingIntensity$MTB_Q <- gsub("_","",samplingIntensity$MTB_Q)
samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
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
  ylim(0,1)+
  theme(legend.position = c(0.15,0.75), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  theme(legend.key.height = unit(0.275, 'cm'),
        legend.key.width = unit(0.15, 'cm'))+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")


#right

#one model
model1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity)
summary(model1)

model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity)
summary(model1)

model1 <- gam(Visited ~ urban + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)

model1 <- gam(Visited ~ I(Year-1991) * urban + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)

#separate models
myYears <- 1992:2018

#as glm
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ urban +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})


amphisR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))


amphisUrban <- plot_grid(amphisL,amphisR)

#### butterflies ####

samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_data/Butterflies/surveys_butterflies.rds")
#filtered to human observations

#put numbers in 2 -year periods
samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
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
  ylim(0,1)+
  theme(legend.position="none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")

#right
myYears <- sort(unique(samplingIntensity$Year))

#one model
model1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity)
summary(model1)

model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity)
summary(model1)

model1 <- gam(Visited ~ urban + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)

model1 <- gam(Visited ~ I(Year-1991) * urban + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)

#as glm
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ urban +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})


buttR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))

#### birds ####

samplingIntensity <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/GBIF_data/Naturgucker/surveys_birds2.rds")

#put numbers in 2 -year periods
samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
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
  ylim(0,1)+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_color_viridis_c("Year")+
  xlab("Urban cover (%)")+ylab("Visit probability")

#right
model1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity)
summary(model1)#positive

model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity)
summary(model1)#positive

model1 <- gam(Visited ~ urban +s(x,y), family= binomial, data = samplingIntensity)
summary(model1)#positive

model1 <- gam(Visited ~ I(Year-1991) * urban +s(x,y), family= binomial, data = samplingIntensity)
summary(model1)#positive

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
  
  glm1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ urban +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})

birdsR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of urban cover")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
    
#### combine ####

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

ggsave("plots/realworldBias_urban.png",width=9.3,height=9)

### PROTECTED AREA ####

protectedArea <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/environ-data/protectedarea_MTBQ.rds")

### amphibians ##############

#run code above to get samplingIntensity and samplingIntensity_Group

#add on protected area data
samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))
samplingIntensity_Group <- inner_join(samplingIntensity_Group,protectedArea,by=c("MTB_Q"))

#left
amphisL <- 
  ggplot(samplingIntensity_Group,
         aes(x=PA_area*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  ylim(0,1)+
  theme(legend.position = c(0.1,0.75),
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  theme(legend.key.height = unit(0.275, 'cm'),
        legend.key.width = unit(0.15, 'cm'))+
  scale_color_viridis_c("Year")+
  xlab("Protected area cover (%)")+ylab("Visit probability")


#right
#one model
model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
summary(model1)#sig neg

model1 <- glm(Visited ~ I(Year-1991) * PA_area, family= binomial, data = samplingIntensity)
summary(model1)#sig neg

model1 <- glm(Visited ~PA_area, family= binomial, data = samplingIntensity)
summary(model1)#sig neg

model1 <- gam(Visited ~ I(Year-1991) * PA_area +s(x,y), family= binomial, data = samplingIntensity)
summary(model1)#sig neg

#separate models
myYears <- 1992:2018
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ PA_area +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})

amphisR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of protected area")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))

amphis <- amphisPA <- plot_grid(amphisL,amphisR,nrow=1)

### butterflies #############

#run code above to get samplingIntensity and samplingIntensity_Group

#add on protected area data
samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))
samplingIntensity_Group <- inner_join(samplingIntensity_Group,protectedArea,by=c("MTB_Q"))

#left
buttL <- ggplot(samplingIntensity_Group,
                aes(x=PA_area*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  ylim(0,1)+
  theme(legend.position="none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_color_viridis_c("Year")+
  xlab("Protected area cover (%)")+ylab("Visit probability")


#one model
model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
summary(model1)#no effect

model1 <- glm(Visited ~ I(Year-1991) * PA_area, family= binomial, data = samplingIntensity)
summary(model1)#no effect

model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
summary(model1)

model1 <- gam(Visited ~ I(Year-1992) * PA_area + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)

#right
myYears <- sort(unique(samplingIntensity$Year))
modelCoefs <- ldply(myYears,function(y){
  
  glm1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ PA_area +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})

buttR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of protected area")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))

butt <- plot_grid(buttL,buttR,nrow=1)

### birds ####################

#run code above to get samplingIntensity and samplingIntensity_Group

#add on protected area data
samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))
samplingIntensity_Group <- inner_join(samplingIntensity_Group,protectedArea,by=c("MTB_Q"))

#left
birdsL <- ggplot(samplingIntensity_Group,
                 aes(x=PA_area*100,y=Visited,group=Year2))+
  stat_smooth(aes(color=Year2),
              method = "glm", 
              method.args = list(family = "binomial"),
              size=1.5,
              se=FALSE)+
  theme_few()+
  ylim(0,1)+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_color_viridis_c("Year")+
  xlab("Protected area cover (%)")+ylab("Visit probability")

#right

#one model
model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
summary(model1)#negative

model1 <- glm(Visited ~ I(Year-1991) * PA_area, family= binomial, data = samplingIntensity)
summary(model1)#negative

model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
summary(model1)#negative

model1 <- gam(Visited ~ I(Year-1991) * PA_area + s(x,y), family= binomial, data = samplingIntensity)
summary(model1)#negative

# year as a factor
samplingIntensity$YearF <- factor(samplingIntensity$Year)

#one model
model1 <- glm(Visited ~ -1 + YearF * PA_area, family= binomial, data = samplingIntensity)
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
  
  glm1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity[samplingIntensity$Year == y,])
  glmConfint <- confint(glm1)
  data.frame(Year = y, 
             estimate=summary(glm1)$coefficients[2,1],
             lower=glmConfint[2,1],
             upper=glmConfint[2,2])
})

#as gam
modelCoefs <- ldply(myYears,function(y){
  gam1 <- gam(Visited ~ PA_area +s(x,y), family= binomial, 
              data = samplingIntensity[samplingIntensity$Year == y,])
  beta <- coef(gam1)
  Vb <- vcov(gam1, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  data.frame(Year = y, 
             estimate = beta[2],
             lower=beta[2] - 1 * (1.96 * se[2]),
             upper=beta[2] + 1* (1.96 * se[2]))
})

birdsR <- ggplot(modelCoefs)+
  geom_crossbar(aes(x = Year, y = estimate, 
                    ymax = upper, ymin = lower, fill=Year))+
  geom_hline(yintercept = 0, colour="red", linetype="dashed")+
  theme_few()+ylab("Effect of protected area")+
  scale_fill_viridis_c("Year")+
  theme(legend.position = "none",
        axis.title = element_text(size = 15),
        axis.text = element_text(size=12))+
  scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))

birds <- plot_grid(birdsL,birdsR,nrow=1)


### combine #############

plot_grid(amphis,butt,birds,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds"),
          align = "v",
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

ggsave("plots/realworldBias_protectedarea.png",width=9.3,height=9)

### LAND USE CHANGE #####

environData <- readRDS("C:/Users/db40fysa/Nextcloud/sMon/sMon-Analyses/Odonata_Git/sMon-insects/environ-data/esacci_MTBQ.rds")
environData$Q <- NA
environData$Q[environData$Quadrant=="NW"] <- 1
environData$Q[environData$Quadrant=="NO"] <- 2
environData$Q[environData$Quadrant=="SW"] <- 3
environData$Q[environData$Quadrant=="SO"] <- 4
environData$MTB_Q <- paste0(environData$Value,environData$Q)

#get max and min of each land cover for each grid
environChange <- environData %>%
                        dplyr::group_by(MTB_Q) %>%
                        dplyr::summarise(urbanChange = max(urban) - min(urban),
                                        waterChange = max(water) - min(water),
                                        treeChange = max(tree) - min(tree),
                                        grassChange = max(grass) - min(grass),
                                        cropChange = max(crop) - min(crop)) %>%
                        tidyr::pivot_longer(!MTB_Q,names_to="landCover",values_to="change") %>%
                        dplyr::group_by(MTB_Q) 

urbanChange <- environChange %>%
                        dplyr::filter(landCover=="urbanChange") %>%
                        dplyr::rename(urbanChange = "change")

cropChange <- environChange %>%
                        dplyr::filter(landCover=="cropChange") %>%
                        dplyr::rename(cropChange = "change")
                        
environChange <- environChange %>%
                          dplyr::filter(change==max(change)) %>%
                          dplyr::filter(!duplicated(change)) %>%
                          left_join(.,urbanChange,by=c("MTB_Q")) %>%
                          left_join(.,cropChange,by=c("MTB_Q"))

hist(environChange$change)
summary(environChange$change)
table(environChange$landCover.x)

#cropChange         5000
#grassChange         272
#treeChange         2395
#urbanChange        4278
#waterChange          79

qplot(urbanChange,cropChange,data=environChange)

### amphibians ##############

#run code above to get samplingIntensity and samplingIntensity_Group

#get number of years in which a MTBQ was visited
samplingSummary <- samplingIntensity %>%
                      dplyr::group_by(MTB_Q,x,y) %>%
                      dplyr::summarise(nuYears = sum(Visited),
                                nuVisits = sum(nuVisits)) %>%
                      ungroup() %>%
                      left_join(.,environChange,by=c("MTB_Q")) %>%
                      mutate(totalYears = length(unique(samplingIntensity$Year)))

#does the number of years of sampling depend on environ change
#crop change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ cropChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#urban change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ urbanChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ change, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

amphiModel <- glm1

#as gam
library(mgcv)
gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ change + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ urbanChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ cropChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ log(change+1) + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

ggplot(subset(samplingSummary,change<0.2),
       aes(x=change,y=nuYears/totalYears))+
  geom_point(size=4,alpha=0.4)+
  stat_smooth(method="glm")+
  theme_classic()

#with spaMM
library(spaMM)
spamm1 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ change + 
                  Matern(1|x+y), 
            family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
            data = samplingSummary)

spamm2 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ 1 + 
                  Matern(1|x+y), 
                family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
                data = samplingSummary)


1-pchisq(2*(logLik(spamm1)-logLik(spamm2)),df=1)

### butterflies #############

#run code above to get samplingIntensity and samplingIntensity_Group

#get number of years in which a MTBQ was visited
samplingSummary <- samplingIntensity %>%
  dplyr::group_by(MTB_Q,x,y) %>%
  dplyr::summarise(nuYears = sum(Visited)) %>%
  ungroup() %>%
  left_join(.,environChange,by=c("MTB_Q")) %>%
  mutate(totalYears = length(unique(samplingIntensity$Year)))


#does the number of years of sampling depend on environ change
#crop change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ cropChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#urban change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ urbanChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ change, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

buttModel <- glm1

#as gam
library(mgcv)
gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ change + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ urbanChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ cropChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

#with spaMM
library(spaMM)
spamm1 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ change + 
                  Matern(1|x+y), 
                family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
                data = samplingSummary)

spamm2 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ 1 + 
                  Matern(1|x+y), 
                family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
                data = samplingSummary)


1-pchisq(2*(logLik(spamm1)-logLik(spamm2)),df=1)

### birds ####################

#run code above to get samplingIntensity and samplingIntensity_Group

#get number of years in which a MTBQ was visited
samplingSummary <- samplingIntensity %>%
  dplyr::group_by(MTB_Q,x,y) %>%
  dplyr::summarise(nuYears = sum(Visited)) %>%
  ungroup() %>%
  left_join(.,environChange,by=c("MTB_Q")) %>%
  mutate(totalYears = length(unique(samplingIntensity$Year)))

#does the number of years of sampling depend on environ change
#crop change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ cropChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#urban change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ urbanChange, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

predict(glm1,newdata=data.frame(urbanChange=0),type="response")
predict(glm1,newdata=data.frame(urbanChange=max(samplingSummary$urbanChange)),
        type="response")

#change
glm1 <- glm(cbind(nuYears,totalYears-nuYears) ~ change, 
            family ="binomial", 
            data = samplingSummary)
summary(glm1)#positive effect

#predict number of years with samples
predict(glm1,newdata=data.frame(change=0),type="response")*length(myYears)
predict(glm1,newdata=data.frame(change=max(samplingSummary$change)),type="response")*length(myYears)

birdModel <- glm1

#as gam
library(mgcv)
gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ change + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ urbanChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect

gam1 <- gam(cbind(nuYears,totalYears-nuYears) ~ cropChange + s(x,y), 
            family ="binomial", 
            data = samplingSummary)
summary(gam1)#positive effect




#with spaMM
library(spaMM)
spamm1 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ change + 
                  Matern(1|x+y), 
                family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
                data = samplingSummary)

spamm2 <- HLCor(cbind(nuYears,totalYears-nuYears) ~ 1 + 
                  Matern(1|x+y), 
                family = binomial(), ranPars=list(nu=0.5,rho=1/0.7),
                data = samplingSummary)


1-pchisq(2*(logLik(spamm1)-logLik(spamm2)),df=1)


### plotting ############

#for each model -get estimate and coef

getCoefs <- function(model){
  estimate <- summary(model)$coefficients[2,1]
  se <- summary(model)$coefficients[2,2]
  lowerCI <- confint(model)[2,1]
  upperCI <- confint(model)[2,2]
  data.frame(estimate,se,lowerCI,upperCI)
}

allCoefs <- rbind(getCoefs(birdModel),getCoefs(buttModel),getCoefs(amphiModel))
allCoefs$Taxa <- c("Bird","Butterfly","Amphibian")

ggplot(allCoefs)+
  geom_crossbar(aes(x = Taxa, y = estimate, ymin = lowerCI, ymax = upperCI))+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  theme_bw()+
  ylab("Effect of environ change on annual visitation probability")+
  xlab("Dataset")+
  coord_flip()

### combine #############

plot_grid(amphis,butt,birds,
          ncol=1,
          labels=c("A - Amphibians",
                   "B - Butterflies",
                   "C - Birds"),
          align = "v",
          scale = c(0.9,0.9,0.9),
          vjust = c(0.95,0.95,0.95), hjust = c(-0.5,-0.5,-0.75))

ggsave("plots/realworldBias_environChange.png",width=9.3,height=9)



### just amphibian data #####

plot_grid(amphisUrban,
          amphisPA,
          labels=c("a)","b)"),
          ncol=1)
ggsave("plots/amphis_SI_Landesamtdata.png",width=8.5,height=5.7)

### end ####