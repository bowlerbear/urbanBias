library(plyr)
library(ggplot2)
library(cowplot)
library(ggthemes)

source('C:/Users/db40fysa/Dropbox/CS spatial pattern/MS/urbanBias/spatialBias_functions.R')

#test the effect of the following possible options 
#- include covariate in state model (i.e. predict response at unsampled sites based on pattern in response at sampled sites)
#- include covariate in new site visitation model (i.e., predict which sites are overrepresented and downweight them)
#- which is better???

#as above:
#- include geographic coordinates as proxy for the covariate

### static scenario #######################################################

df <- generateData()

plotVisits(df)

fitStatic(df)

### reweight data ###########################################################

#ipw: Estimate Inverse Probability Weights
#Functions to estimate the probability to receive the observed treatment, based on individual characteristics. The inverse of these probabilities can be used as weights when estimating causal effects #from observational data via marginal structural models. Both point, treatment situations and longitudinal studies can be analysed. The #same functions can be used to correct for informative censoring

df <- getWeights(df)
fitStaticWeights(df)

temp <- ldply(1:1000,function(i){
  df <- generateData()
  df <- getWeights(df)
  fitStatic(df)
})

tempRW <- ldply(1:1000,function(i){
  df <- generateData()
  df <- getWeights(df)
  fitStaticWeights(df)
})

temp$Type <- "Simple"
tempRW$Type <- "Reweighted"
temp <- rbind(temp,tempRW)

estimatePlot <- ggplot(temp)+
  geom_violin(aes(x=factor(scenario),y=estimate,
                   fill=Type),
            position = position_dodge(width = 0.5),
            draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.1,0.9))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))
  

sePlot <- ggplot(temp)+
  geom_boxplot(aes(x=factor(scenario),y=se,
                   fill=Type),
               position = position_dodge(width = 0.5),
               width=0.4,
               outlier.shape = NA)+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("SE of total occupancy")+
  labs(subtitle = "SE")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position="top")+
  ylim(10,35)

plot_grid(estimatePlot,sePlot,labels=c("A","B"), nrow=1)
ggsave("plots/static_analysis.png",width=6.5,height=3)

### second time point ##################################

df <- getWeights(df)
fitStaticWeights(df)

temp <- ldply(1:1000,function(i){
  df <- generateData()
  df <- extendData(df)
  df <- subset(df,Time==2)
  df <- getWeights(df)
  fitStatic(df)
})

tempRW <- ldply(1:1000,function(i){
  df <- generateData()
  df <- extendData(df)
  df <- subset(df,Time==2)
  df <- getWeights(df)
  fitStaticWeights(df)
})

temp$Type <- "Simple"
tempRW$Type <- "Reweighted"
temp <- rbind(temp,tempRW)

estimatePlot <- ggplot(temp)+
  geom_violin(aes(x=factor(scenario),y=estimate,
                  fill=Type),
              position = position_dodge(width = 0.5),
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.15,0.88))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))

sePlot <- ggplot(temp)+
  geom_violin(aes(x=factor(scenario),y=se,
                  fill=Type),
              position = position_dodge(width = 0.75),
              draw_quantiles = c(0.25,0.5,0.75),
              trim = TRUE, scale="width")+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("Predicted occupancy SE")+
  theme_few()+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.15,0.88))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))

plot_grid(estimatePlot,sePlot,labels=c("A","B"), nrow=1)
ggsave("plots/static_analysis_second.png",width=10.5,height=4.5)

#repeat also for the other types of urban change.

### dynamic scenario ####################################

#urban change scenario
#assume each site is revisited, so current data is year 1

#assumptions about urban change
#no urban change - same urban bias and increasing urban bias
#urban change everywhere - same urban bias and increasing urban bias
#clustered urban change - same urban bias and increasing urban bias

#run same analysis as before except with
#(1) additional time step
#(2) additional time steps and higher urban bias in second time step

df <- generateData()

next_df <- extendData(df,change="no_change")
next_df <- extendData(df,change="uniform_change")
next_df <- extendData(df,change="clustered_change")

getTimePoints(next_df)
plotTimePoints(getTimePoints(next_df))
fitDynamic(next_df)
fitStaticWeights(subset(next_df,Time==1))

### sampling change #######################################

df <- generateData()
next_df <- extendData(df,change="clustered_change")#only variation in change with this option
plotChange(next_df)

### observed change ###################################

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="no_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q1 <- plotObsProp(outputNC,mytitle="No urban change")
g1 <- plotObsChange(outputNC)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp(outputUC,mytitle="Uniform urban change")
g2 <- plotObsChange(outputUC)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
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

ggsave("plots/Obs_change.png",width=10.5,height=6)

#absolute difference varies
#logit difference also varies
#but relative (ratio) change in the same
#for 1 to 3 under uniform change

### naive time points #########################################

outputNC <- ldply(1:500,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:500,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:500,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

g1 <- ggplot(outputNC)+
  geom_violin(aes(x=scenario,y=estimate,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupancy prop")+
              labs(subtitle = "no urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)

g2 <- ggplot(outputUC)+
  geom_violin(aes(x=scenario,y=estimate,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupied prop")+
              labs(subtitle = "uniform urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)

g3 <- ggplot(outputCC)+
  geom_violin(aes(x=scenario,y=estimate,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupied prop")+
              labs(subtitle = "clustered urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)


grid1 <- plot_grid(g1,g2,g3,nrow=1)

### reweight time points ###################################

outputNC <- ldply(1:100,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:100,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:100,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})

g1 <- ggplot(outputNC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
  theme_bw()+
  theme(legend.position = "top")+
  scale_fill_brewer("Time step",type="seq")+
  xlab("sampling scenario")+
  ylab("pred occupancy prop")+
  labs(subtitle = "No urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  ylim(0,0.7)

g2 <- ggplot(outputUC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
  theme_bw()+
  theme(legend.position = "top")+
  scale_fill_brewer("Time step",type="seq")+
  xlab("sampling scenario")+
  ylab("pred occupied prop")+
  labs(subtitle = "Uniform urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  ylim(0,0.7)

g3 <- ggplot(outputCC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
  theme_bw()+
  theme(legend.position = "top")+
  scale_fill_brewer("Time step",type="seq")+
  xlab("sampling scenario")+
  ylab("pred occupied prop")+
  labs(subtitle = "Clustered urban change")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  ylim(0,0.7)

plot_grid(g1,g2,g3,nrow=1)

### trends ############################################

#no change in bias
outputNC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  fitDynamicS(next_df)
})
powerNC <- ddply(outputNC,"scenario",summarise,nuSigs=mean(change_p<0.05))

outputUC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="uniform_change")
  fitDynamicS(next_df)
})
powerUC <- ddply(outputUC,"scenario",summarise,nuSigs=mean(change_p<0.05))

outputCC <- ldply(1:1000,function(i){
  df <- generateData()
  next_df <- extendData(df, change="clustered_change")
  fitDynamicS(next_df)
})
powerCC <- ddply(outputCC,"scenario",summarise,nuSigs=mean(change_p<0.05))

g1 <- ggplot(outputNC)+
  geom_violin(aes(x=scenario,y=change),trim = TRUE)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-2,2)


g2 <- ggplot(outputUC)+
  geom_violin(aes(x=scenario,y=change),trim = TRUE)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-2,2)

g3 <- ggplot(outputCC)+
  geom_violin(aes(x=scenario,y=change),trim = TRUE)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=1,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-2,2)

plot_grid(g1,g2,g3,nrow=1)

#combine into a data frame
outputNC$Environ_change <- "no_change"
outputUC$Environ_change <- "uniform_change"
outputCC$Environ_change <- "clusetered_change"

allOutput_Simple <- rbind(outputNC,outputUC,outputCC)

### power ##########################################

p1 <- ggplot(powerNC)+
  geom_bar(aes(x=scenario,y=nuSigs),stat="identity",fill="grey")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type 1 error rate")+
  labs(subtitle = "No urban change")

p2 <- ggplot(powerUC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity",fill="grey")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type II erorr rate")+
  labs(subtitle = "Uniform urban change")

p3 <- ggplot(powerCC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity",fill="grey")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type II error rate")+
  labs(subtitle = "Clustered urban change")

plot_grid(p1,p2,p3,nrow=1)
ggsave("plots/power.png",width=6.5,height=2.5)

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


### alt weights #################################################

#upweight sites that are sampled twice???

### sampling bias vs environ change bias ########################

#sampling bias with respect to change can be caused by either
#-constant spatial bias with heterogeneous land use change
#-changing spatial bias with uniform or heterogeneous land use change

#unbiased sampling of environmental change
#-uniform land use change and unbiased sampling
#-uniform land use change and constant biased sampling

#pull out how much each sampling approach samples the true urban change

sampleUrbanChange <- function(x,change="clustered_change"){
  
  df <- generateData()
  next_df <- extendData(df,change=change)
  
  #visit2
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits2==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits2==1]
  sample_urbanDiff2 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  
  #visit3
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits3==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits3==1]
  sample_urbanDiff3 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  
  #visit4
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits4==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits4==1]
  sample_urbanDiff4 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  
  data.frame(scenario=c("Random","Bias","Bias+"),
             urban_diff=c(sample_urbanDiff2,sample_urbanDiff3,sample_urbanDiff4))
  
}  
  
sampleBiasCC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="clustered_change"))
sampleBiasUC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="uniform_change"))
sampleBiasCC$scenario <- factor(sampleBiasCC$scenario, levels=c("Random","Bias","Bias+")) 
sampleBiasUC$scenario <- factor(sampleBiasUC$scenario, levels=c("Random","Bias","Bias+"))

#plot
g1 <- ggplot(sampleBiasUC)+
  geom_density(aes(urban_diff,fill=scenario),alpha=0.2)+
  theme_few()+
  labs(subtitle = "Uniform urban change")+
  scale_fill_discrete("Scenario")+
  xlab("Sampled urban change") + ylab("Density")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.845,0.85),legend.key.size=unit(0.35,"line"),
        legend.title = (element_text(size=8)),legend.text = (element_text(size=8)))
  

g2 <- ggplot(sampleBiasCC)+
  geom_density(aes(urban_diff,fill=scenario),alpha=0.2)+
  theme_few()+
  labs(subtitle = "Clustered urban change")+
  scale_fill_discrete("Scenario")+
  xlab("Sampled urban change") + ylab("Density")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position=c(0.845,0.85),legend.key.size=unit(0.35,"line"),
        legend.title = (element_text(size=8)),legend.text = (element_text(size=8)))

cowplot::plot_grid(g1,g2,nrow=1)

ggsave("plots/urbanChange.png",width=6,height=3)
  
#each sim - pull out bias of data and bias of estimate
sampleRun <- function(x){
df <- generateData()
next_df <- extendData(df, change="clustered_change")

#get urban cover sampled in each time step and the difference:
urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits4==1]
urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits4==1]
sample_urbanDiff <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
urbanCover_T1 <- next_df$urbanCover[next_df$Time==1]
urbanCover_T2 <- next_df$urbanCover[next_df$Time==2]
true_urbanDiff <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)

#bias of sample
#proportion in all sites - truth
propTrue_T1 <- mean(next_df$z[next_df$Time==1]) 
propTrue_T2 <- mean(next_df$z[next_df$Time==2]) 
trueChange <- propTrue_T2/propTrue_T1
#proportion in sample
propSample_T1 <- mean(next_df$z[next_df$Time==1 & next_df$Visits4==1]) 
propSample_T2 <- mean(next_df$z[next_df$Time==2 & next_df$Visits4==1]) 
sampleChange <- propSample_T2/propSample_T1

#bias in the sample
sampleBias <- sampleChange/trueChange
urbandiffBias <- sample_urbanDiff/true_urbanDiff

data.frame(rep=x,urbandiffBias,sampleBias)

}
sampleBias <- ldply(1:1000,sampleRun)
qplot(urbandiffBias,sampleBias,data=sampleBias)

### end ######################################################
