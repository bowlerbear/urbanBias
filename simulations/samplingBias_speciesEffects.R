library(plyr)
library(ggplot2)
library(cowplot)
library(ggthemes)

source('C:/Users/db40fysa/Dropbox/CS spatial pattern/MS/urbanBias/spatialBias_functions.R')

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

### end ##################################################