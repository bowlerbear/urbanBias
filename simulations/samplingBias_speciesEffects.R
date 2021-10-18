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


### over range of values ###################################

#plot relationship between strength of the association
#and the degree of bias
#need to look at the effects on each time step

betaRange <- rep(seq(-3,3,by=0.1),1000)

outputNC <- ldply(betaRange,function(i){
  
  df <- generateData(beta1=i)
  next_df <- extendData(df,beta1=i,change="no_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputUC <- ldply(betaRange,function(i){
  
  df <- generateData(beta1=i)
  next_df <- extendData(df,beta1=i,change="uniform_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputCC <- ldply(betaRange,function(i){
  
  df <- generateData(beta1=i)
  next_df <- extendData(df,beta1=i,change="clustered_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})


# Plotting time points 
plot1 <- plotMeans(outputNC, mytitle="No urban change")
plot2 <- plotMeans(outputUC, mytitle="Uniform urban change")
plot3 <- plotMeans(outputCC, mytitle = "Clustered urban change")

plot_grid(plot1,plot2,plot3, ncol=3)


#Plotting occupancy change 
#logit difference
plot1 <- plotBias(outputNC, mytitle="No urban change") + ylim(-1.35,1.35)
plot2 <- plotBias(outputUC, mytitle="Uniform urban change")+ ylim(-1.35,1.35)
plot3 <- plotBias(outputCC, mytitle = "Clustered urban change")+ ylim(-1.35,1.35)
plot_grid(plot1,plot2,plot3, ncol=3)

ggsave("plots/Bias_plot_speciesEffects.png",width=11,height=3)


### mean occupancy effects ###############################

betaRange <- rep(seq(-3,3,by=0.1),1000)

outputNC <- ldply(betaRange,function(i){
  
  df <- generateData(beta0=i)
  next_df <- extendData(df,beta0=i,change="no_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputUC <- ldply(betaRange,function(i){
  
  df <- generateData(beta0=i)
  next_df <- extendData(df,beta0=i,change="uniform_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputCC <- ldply(betaRange,function(i){
  
  df <- generateData(beta0=i)
  next_df <- extendData(df,beta0=i,change="clustered_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

# Plotting time points 
plot1 <- plotMeans(outputNC, mytitle="No urban change")
plot2 <- plotMeans(outputUC, mytitle="Uniform urban change")
plot3 <- plotMeans(outputCC, mytitle = "Clustered urban change")

plot_grid(plot1,plot2,plot3, ncol=3)


#Plotting occupancy change 
#logit difference
plot1 <- plotBias(outputNC, myxlab="Mean occupancy (logit-scale)", 
                  mytitle="No urban change") + ylim(-1.6,0.1)
plot2 <- plotBias(outputUC, myxlab="Mean occupancy (logit-scale)",
                  mytitle="Uniform urban change")+ ylim(-1.6,0.1)
plot3 <- plotBias(outputCC, myxlab="Mean occupancy (logit-scale)",
                  mytitle = "Clustered urban change")+ ylim(-1.6,0.1)
plot_grid(plot1,plot2,plot3, ncol=3)

ggsave("plots/SOM/Bias_plot_meanOccuEffects.png",width=10,height=3)


### urban exploiters ###########################################

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1 = 2)
  next_df <- extendData(df,change="no_change",beta1 = 2)
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q1 <- plotObsProp(outputNC,mytitle="No urban change")+ylim(0,0.7)
g1 <- plotObsChange(outputNC)+ylim(-2.3,1)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1 = 2)
  next_df <- extendData(df,change="uniform_change",beta1 = 2)
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp(outputUC,mytitle="Uniform urban change")+ylim(0,0.7)
g2 <- plotObsChange(outputUC)+ylim(-2.3,1)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData(beta1 = 2)
  next_df <- extendData(df,change="clustered_change",beta1 = 2)
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q3 <- plotObsProp(outputCC,mytitle="Clustered urban change")
g3 <- plotObsChange(outputCC)


plot1 <- plot_grid(q1,q2,q3,nrow=1)
plot2 <- plot_grid(g1,g2,g3,nrow=1)

plot_grid(plot1,plot2,ncol=1,align='v',
          labels=c("A","B"),
          vjust=1)

ggsave("plots/Obs_change_urbanExploiter.png",width=10.5,height=6)

### end ##################################################