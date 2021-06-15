library(plyr)
library(ggplot2)
library(cowplot)
library(ggthemes)

source('simulations/spatialBias_functions.R')

### model and reweight data #####################################################

#ipw: Estimate Inverse Probability Weights
#Functions to estimate the probability to receive the observed treatment, based on individual characteristics. The inverse of these probabilities can be used as weights when estimating causal effects #from observational data via marginal structural models. Both point, treatment situations and longitudinal studies can be analysed. The #same functions can be used to correct for informative censoring

### naive time points #########################################

outputNC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

outputNC$Environ_change <- "no_change"
outputUC$Environ_change <- "uniform_change"
outputCC$Environ_change <- "clusetered_change"

allOutput_Simple <- rbind(outputNC,outputUC,outputCC)

### reweight time points ###################################

outputNC_RW <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC_RW <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC_RW <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})

outputNC_RW$Environ_change <- "no_change"
outputUC_RW$Environ_change <- "uniform_change"
outputCC_RW$Environ_change <- "clusetered_change"

allOutput_RW <- rbind(outputNC_RW,outputUC_RW,outputCC_RW)

### compare time points ##############################

allOutput_Simple$Analysis <- "Simple"
allOutput_RW$Analysis <- "Reweighted"
allOutput <- rbind(allOutput_Simple,allOutput_RW)

g1 <- ggplot(subset(allOutput,Environ_change=="no_change" & Time==2))+
  geom_violin(aes(x=factor(scenario),y=estimate,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.25),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "No urban change")


g2 <- ggplot(subset(allOutput,Environ_change=="uniform_change" & Time==2))+
  geom_violin(aes(x=factor(scenario),y=estimate,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.25),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Uniform urban change")


g3 <- ggplot(subset(allOutput,Environ_change=="clusetered_change" & Time==2))+
  geom_violin(aes(x=factor(scenario),y=estimate,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.25),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Clustered urban change")


plot_grid(g1,g2,g3,nrow=1)

ggsave("plots/reweighted_timepoints.png",width=10.6,height=3)

### trends ############################################

#no change in bias
outputNC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  fitDynamic(next_df)
})

outputUC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="uniform_change")
  fitDynamic(next_df)
})

outputCC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="clustered_change")
  fitDynamic(next_df)
})

#combine into a data frame
outputNC$Environ_change <- "no_change"
outputUC$Environ_change <- "uniform_change"
outputCC$Environ_change <- "clusetered_change"

ggsave("plots/SOM/Dynamicmodel_occupancychange.png",width=10.6,height=3)

allOutput_Simple <- rbind(outputNC,outputUC,outputCC)

### power ##########################################

powerNC <- ddply(outputNC,"scenario",summarise,nuSigs=mean(change_p<0.05))
powerUC <- ddply(outputUC,"scenario",summarise,nuSigs=mean(change_p<0.05))
powerCC <- ddply(outputCC,"scenario",summarise,nuSigs=mean(change_p<0.05))


p1 <- ggplot(powerNC)+
  geom_bar(aes(x=scenario,y=nuSigs),stat="identity",fill="grey")+
  theme_few()+
  xlab("sampling scenario")+
  ylab("Type 1 error rate")+
  labs(subtitle = "No urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))

p2 <- ggplot(powerUC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity",fill="grey")+
  theme_few()+
  xlab("sampling scenario")+
  ylab("Type II erorr rate")+
  labs(subtitle = "Uniform urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))

p3 <- ggplot(powerCC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity",fill="grey")+
  theme_few()+
  xlab("sampling scenario")+
  ylab("Type II error rate")+
  labs(subtitle = "Clustered urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))

plot_grid(p1,p2,p3,nrow=1)

ggsave("plots/SOM/power.png",width=7,height=2.5)

### reweight trends ###########################################

#single run
df <- generateData()
next_df <- extendData(df, change="no_change")
fitDynamicWeights(next_df)
fitDynamicWeightsS(next_df)

#replicate runs
outputNC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- fitDynamicWeightsS(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- fitDynamicWeightsS(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- fitDynamicWeightsS(next_df)
  temp$simNu <- i
  return(temp)
})

g1 <- ggplot(outputNC)+
  geom_boxplot(aes(x=factor(scenario),y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")
#ylim(-1.3,0.3)


g2 <- ggplot(outputUC)+
  geom_boxplot(aes(x=factor(scenario),y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")
#ylim(-1.5,0.3)

g3 <- ggplot(outputCC)+
  geom_boxplot(aes(x=factor(scenario),y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")
#ylim(-1.5,0.5)

plot_grid(g1,g2,g3,nrow=1)

#combine data
outputNC$Environ_change <- "no_change"
outputUC$Environ_change <- "uniform_change"
outputCC$Environ_change <- "clusetered_change"
allOutput_RW <- rbind(outputNC,outputUC,outputCC)

### compare trends ##############################

#and with simple analysis
allOutput_Simple$Analysis <- "Simple"
allOutput_RW$Analysis <- "Reweighted"
allOutput <- rbind(allOutput_Simple,allOutput_RW[,-8])

#exclude outliers
allOutput <- subset(allOutput,change < quantile(allOutput$change,0.999))

g1 <- ggplot(subset(allOutput,Environ_change=="no_change"))+
  geom_violin(aes(x=factor(scenario),y=change,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy change")+
  theme_few()+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.85),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "No urban change")


g2 <- ggplot(subset(allOutput,Environ_change=="uniform_change"))+
  geom_violin(aes(x=factor(scenario),y=change,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy change")+
  theme_few()+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.85),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Uniform urban change")


g3 <- ggplot(subset(allOutput,Environ_change=="clusetered_change"))+
  geom_violin(aes(x=factor(scenario),y=change,
                  fill=Analysis),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy change")+
  theme_few()+
  geom_hline(yintercept=1,linetype="dashed")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.2,0.85),legend.key.size=unit(0.5,"line"),
        legend.title = (element_text(size=10)))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Clustered urban change")


plot_grid(g1,g2,g3,nrow=1)

ggsave("plots/reweighted_trends.png",width=10.6,height=3)

