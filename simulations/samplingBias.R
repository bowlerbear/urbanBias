library(plyr)
library(ggplot2)
library(cowplot)
library(ggthemes)

source('simulations/spatialBias_functions.R')

#simulate occupancy data under different scenarios of urban cover change and sampling strategies

### static scenario #######################################################

df <- generateData()

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
q1 <- plotObsProp(outputNC,mytitle="No urban change")+ylim(0,0.7)
g1 <- plotObsChange(outputNC)+ylim(-2.3,1)

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q2 <- plotObsProp(outputUC,mytitle="Uniform urban change")+ylim(0,0.7)
g2 <- plotObsChange(outputUC)+ylim(-2.3,1)

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  #get proportion of sites observed to be occupied
  getObsProp(next_df)
  
})
q3 <- plotObsProp(outputCC,mytitle="Clustered urban change")+ylim(0,0.7)
g3 <- plotObsChange(outputCC)+ylim(-2.3,1)


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

g1 <- ggplot(outputNC)+
  geom_violin(aes(x=scenario,y=change_coef),trim = TRUE,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Occupancy change estimate")+
  ylim(-3,1.2)+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "No urban change")


g2 <- ggplot(outputUC)+
  geom_violin(aes(x=scenario,y=change_coef),trim = TRUE,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Occupancy change estimate")+
  ylim(-3,1.2)+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Uniform urban change")

g3 <- ggplot(outputCC)+
  geom_violin(aes(x=scenario,y=change_coef),trim = TRUE,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  ylab("Occupancy change estimate")+
  ylim(-3,1.2)+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
  scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                   "3" = "Bias","4" = "Bias+"))+
  labs(subtitle = "Clustered urban change")

plot_grid(g1,g2,g3,nrow=1)

ggsave("plots/SOM/Dynamicmodel_occupancychange.png",width=10.6,height=3)

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

### sampling bias vs environ change bias ########################

#sampling bias with respect to change can be caused by either
#-constant spatial bias with heterogeneous land use change
#-changing spatial bias with uniform or heterogeneous land use change

#unbiased sampling of environmental change
#-uniform land use change and unbiased sampling
#-uniform land use change and constant biased sampling

#pull out how much each sampling approach samples the true urban change

#each sim - pull out bias of data and bias of estimate
sampleRun <- function(change="clustered_change"){
  
df <- generateData()
next_df <- extendData(df, change=change)

#get urban cover sampled in each time step and the difference:
#sampled
urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits4==1]
urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits4==1]
#sample_urbanDiff <- as.numeric(statip::hellinger(urbanCover_T1, urbanCover_T2))
sample_urbanDiff <- median(urbanCover_T2) - median(urbanCover_T1)

#truth
urbanCover_T1 <- next_df$urbanCover[next_df$Time==1]
urbanCover_T2 <- next_df$urbanCover[next_df$Time==2]
#true_urbanDiff <- as.numeric(statip::hellinger(urbanCover_T1, urbanCover_T2))
true_urbanDiff <- median(urbanCover_T2) - median(urbanCover_T1)

#bias of sample
require(boot)
#proportion in sample
propSample_T1 <- mean(next_df$z[next_df$Time==1 & next_df$Visits4==1]) 
propSample_T2 <- mean(next_df$z[next_df$Time==2 & next_df$Visits4==1]) 
sampleChange <- logit(propSample_T2) - logit(propSample_T1)
#proportion in all sites - truth
propTrue_T1 <- mean(next_df$z[next_df$Time==1]) 
propTrue_T2 <- mean(next_df$z[next_df$Time==2]) 
trueChange <- logit(propTrue_T2)-logit(propTrue_T1)

#bias in the sample
sampleBias <- sampleChange - trueChange
urbandiffBias <- sample_urbanDiff - true_urbanDiff

data.frame(urbandiffBias,sampleBias)

}

sampleBias <- ldply(1:1000,function(i){
  sampleRun(change="clustered_change")})

qplot(urbandiffBias,abs(sampleBias),data=sampleBias)

sampleBias <- ldply(1:1000,function(i){
  sampleRun(change="uniform_change")})

qplot(urbandiffBias,abs(sampleBias),data=sampleBias)

sampleBias <- ldply(1:1000,function(i){
  sampleRun(change="no_change")})

qplot(urbandiffBias,abs(sampleBias),data=sampleBias)

### sample distr urban cover ###############################

sampleUrbanChange <- function(x,change="clustered_change"){
  
  df <- generateData()
  next_df <- extendData(df,change=change)
  
  #visit2
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits2==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits2==1]
  #sample_urbanDiff2 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  sample_urbanDiff2 <- as.numeric(statip::hellinger(urbanCover_T1, urbanCover_T2))
  
  #visit3
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits3==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits3==1]
  #sample_urbanDiff3 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  sample_urbanDiff3 <- as.numeric(statip::hellinger(urbanCover_T1, urbanCover_T2))
  
  #visit4
  urbanCover_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits4==1]
  urbanCover_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits4==1]
  #sample_urbanDiff4 <- as.numeric(ks.test(urbanCover_T1, urbanCover_T2)$statistic)
  sample_urbanDiff4 <- as.numeric(statip::hellinger(urbanCover_T1, urbanCover_T2))
  
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

### sampled mean urban cover ######################################

#calculate the sum urban cover being sampled by each sampling strategy

#also see section above 

sampleUrbanChange <- function(x,change="clustered_change"){
  
  df <- generateData()
  next_df <- extendData(df,change=change)
  
  #visit2
  urbanCover2_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits2==1]
  urbanCover2_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits2==1]
  
  #visit3
  urbanCover3_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits3==1]
  urbanCover3_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits3==1]
  
  #visit4
  urbanCover4_T1 <- next_df$urbanCover[next_df$Time==1 & next_df$Visits4==1]
  urbanCover4_T2 <- next_df$urbanCover[next_df$Time==2 & next_df$Visits4==1]
  
  temp <- data.frame(scenario=c("Random","Random",
                        "Bias","Bias",
                        "Bias+","Bias+"),
             time = c(1,2,1,2,1,2),
             urban_median=c(median(urbanCover2_T1),median(urbanCover2_T2),
                            median(urbanCover3_T1),median(urbanCover3_T2),
                            median(urbanCover4_T1),median(urbanCover4_T2)))
  
  temp$scenario <- factor(temp$scenario,levels=c("Random","Bias","Bias+"))
  return(temp)
  
}  

sampleBiasCC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="clustered_change"))
sampleBiasUC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="uniform_change"))
sampleBiasNC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="no_change"))

g1 <- ggplot(sampleBiasNC)+
  geom_violin(aes(x=scenario,y=urban_median,fill=factor(time)),alpha=0.2,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none") +
  scale_fill_manual("Time point", values = c("lightblue","blue"))+
  labs(subtitle = "No urban change")+
  xlab("Sampling scenario") + ylab ("Sampled urban cover")
#random and bias both sample similar mean urban cover at each time step
#bias + samples higher urban cover at the second time step

g2 <- ggplot(sampleBiasUC)+
  geom_violin(aes(x=scenario,y=urban_median,fill=factor(time)),alpha=0.2,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none") +
  scale_fill_manual("Time point", values = c("lightblue","blue"))+
  labs(subtitle = "Uniform urban change")+
  xlab("Sampling scenario") + ylab ("Sampled urban cover")
#all sample more urban cover in the second time step
#but the increase is smaller with constant bias
#maybe less space to sample more urban change
#check results are the same if we do not bound urban cover - yes they are

g3 <- ggplot(sampleBiasCC)+
  geom_violin(aes(x=scenario,y=urban_median,fill=factor(time)),alpha=0.2,
              draw_quantiles = c(0.25,0.5,0.75))+
  theme_few()+
  theme(legend.position = "none") +
  scale_fill_manual("Time point", values = c("lightblue","blue"))+
  labs(subtitle = "Clustered urban change")+
  xlab("Sampling scenario") + ylab ("Sampled urban cover")
#bias and bias+ both sample much higher urban cover in the second time step

plot_grid(g1,g2,g3,nrow=1)

ggsave("plots/urbanMeans.png",width=9,height=3)

### number of sampling sites #################################

outputNC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="no_change")
  getObsNu(next_df)
  
})

plotObsNu(outputNC,mytitle="No urban change")

outputUC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  getObsNu(next_df)
  
})

plotObsNu(outputUC,mytitle="Uniform urban change")

outputCC <- ldply(1:1000,function(i){
  
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  getObsNu(next_df)
  
})

plotObsNu(outputCC,mytitle="Clustered urban change")

### end ######################################################
