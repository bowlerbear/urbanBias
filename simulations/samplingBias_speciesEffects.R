library(plyr)
library(ggplot2)
library(cowplot)
library(ggthemes)

source('simulations/spatialBias_functions.R')

#### Species negatively associated with urban change ####

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=-3)
  next_df <- extendData(df,beta1=-3,change="no_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q1 <- plotObsProp_P(outputNC,mytitle="No urban change")
g1 <- plotObsChange(outputNC)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=-3)
  next_df <- extendData(df,beta1=-3,change="uniform_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp_P(outputUC,mytitle="Uniform urban change")
g2 <- plotObsChange(outputUC)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=-3)
  next_df <- extendData(df,beta1=-3,change="clustered_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q3 <- plotObsProp_P(outputCC,mytitle="Clustered urban change")
g3 <- plotObsChange(outputCC)


plot1 <- plot_grid(q1,q2,q3,nrow=1)
plot2 <- plot_grid(g1,g2,g3,nrow=1)

plot_grid(plot1,plot2,ncol=1,align='v',
          labels=c("A","B"),
          vjust=1)

ggsave("plots/SOM/Obs_change_negativeSpecies.png",width=10.5,height=6)


#combine all outputs
outputNC$Envion_change <- "no change"
outputUC$Envion_change <- "uniform change"
outputCC$Envion_change <- "clustered change"
outputNegative <- rbind(outputNC,outputUC,outputCC) 
outputNegative$Species <- "Negative"

### Species positively associated with urban cover #####

### observed change ###################################

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=3)
  next_df <- extendData(df,beta1=3,change="no_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q1 <- plotObsProp_P(outputNC,mytitle="No urban change")
g1 <- plotObsChange(outputNC)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=3)
  next_df <- extendData(df,beta1=3,change="uniform_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp_P(outputUC,mytitle="Uniform urban change")
g2 <- plotObsChange(outputUC)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=3)
  next_df <- extendData(df,beta1=3,change="clustered_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q3 <- plotObsProp_P(outputCC,mytitle="Clustered urban change")
g3 <- plotObsChange(outputCC)


plot1 <- plot_grid(q1,q2,q3,nrow=1)
plot2 <- plot_grid(g1,g2,g3,nrow=1)

plot_grid(plot1,plot2,ncol=1,align='v',
          labels=c("A","B"),
          vjust=1)

ggsave("plots/SOM/Obs_change_positiveSpecies.png",width=10.5,height=6)

#combine all outputs
outputNC$Envion_change <- "no change"
outputUC$Envion_change <- "uniform change"
outputCC$Envion_change <- "clustered change"
outputPositive <- rbind(outputNC,outputUC,outputCC) 
outputPositive$Species <- "Positive"

### Species neutrally associated with urban cover #####

### observed change ###################################

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=0)
  next_df <- extendData(df,beta1=0,change="no_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q1 <- plotObsProp_P(outputNC,mytitle="No urban change")
g1 <- plotObsChange(outputNC)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=0)
  next_df <- extendData(df,beta1=0,change="uniform_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp_P(outputUC,mytitle="Uniform urban change")
g2 <- plotObsChange(outputUC)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1=0)
  next_df <- extendData(df,beta1=0,change="clustered_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q3 <- plotObsProp_P(outputCC,mytitle="Clustered urban change")
g3 <- plotObsChange(outputCC)


plot1 <- plot_grid(q1,q2,q3,nrow=1)
plot2 <- plot_grid(g1,g2,g3,nrow=1)

plot_grid(plot1,plot2,ncol=1,align='v',
          labels=c("A","B"),
          vjust=1)

ggsave("plots/SOM/Obs_change_neutralSpecies.png",width=10.5,height=6)

#combine all outputs
outputNC$Envion_change <- "no change"
outputUC$Envion_change <- "uniform change"
outputCC$Envion_change <- "clustered change"
outputNeutral <- rbind(outputNC,outputUC,outputCC) 
outputNeutral$Species <- "Neutral"

#### combine all ####

output <- rbind(outputNeutral,outputNegative,outputPositive)
output$Envion_change <- factor(output$Envion_change,
                               levels=c("no change","uniform change","clustered change"))

output$simNu <- 1:nrow(output)
output_melt <- reshape2::melt(output,id=c("simNu","Envion_change","Species"))
output_melt$scenario <- sapply(as.character(output_melt$variable),function(x){
  strsplit(x,"_")[[1]][1]})
output_melt$time <- sapply(as.character(output_melt$variable),function(x){
  strsplit(x,"_")[[1]][2]})
output_cast <- reshape2::dcast(output_melt,simNu+scenario + Envion_change+Species~time,value.var="value")
output_cast$Difference2 <- output_cast$Time2/output_cast$Time1

#option1
ggplot(output_cast,aes(x=scenario,y=Difference2))+
  geom_violin(aes(fill=Envion_change, colour=Envion_change),
              position="dodge",draw_quantiles=c(0.25,0.5,0.75),
              alpha=0.5)+
  theme_few()+
  scale_x_discrete("Sampling scenario", labels = c("Visit1" = "Full","Visit2" = "Random",
                                                   "Visit3" = "Bias","Visit4" = "Bias+"))+
  ylab("Occupancy change")+
  facet_wrap(~Species)+
  scale_fill_brewer("Urban change")+
  scale_color_brewer("Urban change")+
  geom_hline(yintercept=1,linetype="dashed")


#option2 
ggplot(output_cast,aes(x=Envion_change,y=Difference2))+
  geom_violin(aes(fill=scenario, colour=scenario),
              position="dodge",draw_quantiles=c(0.25,0.5,0.75),
              alpha=0.5)+
  theme_few()+
  ylab("Occupancy change")+
  facet_wrap(~Species)+
  scale_fill_brewer("Sampling scenario",labels=c("full","random","bias","bias+"))+
  scale_color_brewer("Sampling scenario",labels=c("full","random","bias","bias+"))+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(axis.text.x = element_text(angle=45, vjust=0.95, hjust=0.95),
        legend.position="top")+
  xlab("Urban change")

ggsave("plots/Obs_change_speciesPref.png",width=9,height=5)

### end ##################################################