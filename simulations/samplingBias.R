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


g1 <- plotTrends(outputNC, mytitle="No urban change")
g2 <- plotTrends(outputUC, mytitle="Uniform urban change")
g3 <- plotTrends(outputUC, mytitle="Clustered urban change")

plot_grid(g1,g2,g3,nrow=1)

ggsave("plots/SOM/Dynamicmodel_occupancychange.png",width=10.6,height=3)

### power ##########################################

powerNC <- ddply(outputNC,"scenario",summarise,nuSigs=mean(change_p<0.05))
powerUC <- ddply(outputUC,"scenario",summarise,nuSigs=mean(change_p<0.05))
powerCC <- ddply(outputCC,"scenario",summarise,nuSigs=mean(change_p<0.05))


p1 <- plotPower(powerNC, mytitle="No urban change")
p2 <- plotPower(powerUC, mytitle="Uniform urban change")
p3 <- plotPower(powerCC, mytitle="Clustered urban change")

plot_grid(p1,p2,p3,nrow=1)

ggsave("plots/SOM/power.png",width=7,height=2.5)

### sampling bias vs environ change bias ########################

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

g1 <- plotUrbanDistr(sampleBiasUC, mytitle="Uniform urban change")
g1 <- plotUrbanDistr(sampleBiasCC, mytitle="Clustered urban change")

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
             urban_median=c(mean(urbanCover2_T1),mean(urbanCover2_T2),
                            mean(urbanCover3_T1),mean(urbanCover3_T2),
                            mean(urbanCover4_T1),mean(urbanCover4_T2)))
  
  temp$scenario <- factor(temp$scenario,levels=c("Random","Bias","Bias+"))
  return(temp)
  
}  

sampleBiasCC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="clustered_change"))
sampleBiasUC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="uniform_change"))
sampleBiasNC <- ldply(1:1000,function(x)sampleUrbanChange(x,change="no_change"))


g1 <- plotUrbanMean(sampleBiasNC, mytitle = "No urban change")+ylim(-0.4,1.1)
#random and bias both sample similar mean urban cover at each time step
#bias + samples higher urban cover at the second time step

g2 <- plotUrbanMean(sampleBiasUC, mytitle = "Uniform urban change")+ylim(-0.4,1.1)
#all sample more urban cover in the second time step
#but the increase is smaller with constant bias
#maybe less space to sample more urban change
#check results are the same if we do not bound urban cover - yes they are

g3 <- plotUrbanMean(sampleBiasCC, mytitle = "Clustered urban change")+ylim(-0.4,1.1)
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

### mean site visitation prob ###############################

betaRange <- rep(seq(-3,3,by=0.1),1000)

outputNC <- ldply(betaRange,function(i){
  
  df <- generateData(urban0=i)
  next_df <- extendData(df,urban0=i,change="no_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputUC <- ldply(betaRange,function(i){
  
  df <- generateData(urban0=i)
  next_df <- extendData(df,urban0=i,change="uniform_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})

outputCC <- ldply(betaRange,function(i){
  
  df <- generateData(urban0=i)
  next_df <- extendData(df,urban0=i,change="clustered_change")
  #get proportion of sites observed to be occupied
  temp <- getObsProp(next_df)
  temp$beta <- i
  return(temp)
})


#Plotting occupancy change 
#logit difference
plot1 <- plotBias(outputNC, myxlab="Mean visitation (logit-scale)", 
                  mytitle="No urban change") + ylim(-0.9,0.05)
plot2 <- plotBias(outputUC, myxlab="Mean occupancy (logit-scale)",
                  mytitle="Uniform urban change")+ ylim(-0.9,0.05)
plot3 <- plotBias(outputCC, myxlab="Mean occupancy (logit-scale)",
                  mytitle = "Clustered urban change")+ ylim(-0.9,0.05)
plot_grid(plot1,plot2,plot3, ncol=3)

ggsave("plots/SOM/Bias_plot_meanvisitEffects.png",width=10,height=3)

### end ######################################################
