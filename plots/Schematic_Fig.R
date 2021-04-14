library(ggplot2)
library(plyr)

#read in wrapper functions
source('simulations/spatialBias_functions.R')

#### UNIFORM CHANGE ####

#### generate df #######

df <- ldply(1:20,function(x){
  temp <- generateData(M=20,nuSamples=5)
  temp <- extendData(temp,change="uniform_change")
  temp$sim <- x
  return(temp)
})

#### color gradient ####

#specify green and grey colour gradient
library(dichromat)
colfunc <- colorRampPalette(c("lightgreen", "grey70", "grey10"))
coloursDF <- unique(df[,c('urbanCover',"Time")])
coloursDF <- arrange(coloursDF,urbanCover)
coloursDF$Colours <- colfunc(nrow(coloursDF))

#### random ###########

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits2==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Random - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits2==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Random - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

c1 <- cowplot::plot_grid(g1,g2,nrow=1)

#### bias ###########

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits3==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Bias - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")
  
g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits3==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Bias - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

 
c2 <- cowplot::plot_grid(g1,g2,nrow=1)

#### incr bias ##############

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits4==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Increased bias - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits4==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Increased bias - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

c3 <- cowplot::plot_grid(g1,g2,nrow=1)

#### CLUSTERED CHANGE ####

df <- ldply(1:20,function(x){
  temp <- generateData(M=20,nuSamples=5)
  temp <- extendData(temp,change="clustered_change")
  temp$sim <- x
  return(temp)
})

#### color gradient ####

#specify green and grey colour gradient
library(dichromat)
colfunc <- colorRampPalette(c("lightgreen", "grey70", "grey10"))
coloursDF <- unique(df[,c('urbanCover',"Time")])
coloursDF <- arrange(coloursDF,urbanCover)
coloursDF$Colours <- colfunc(nrow(coloursDF))

#### random ###########

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits2==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Random - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits2==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Random - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

c4 <- cowplot::plot_grid(g1,g2,nrow=1)

#### bias ###########

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits3==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Bias - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits3==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Bias - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")


c5 <- cowplot::plot_grid(g1,g2,nrow=1)

#### incr bias ##############

g1 <- ggplot(subset(df,Time==1))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
  geom_point(data = subset(df, Visits4==1 & Time==1), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Increased bias - Time 1") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

g2 <- ggplot(subset(df,Time==2))+
  geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
  scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
  geom_point(data = subset(df, Visits4==1 & Time==2), aes(x = sim, y = Site),
             shape=0,size=1)+
  ggtitle("Increased bias - Time 2") +
  theme_minimal() + 
  xlab("Replicate") + ylab("Site") + 
  theme(legend.position="none")

c6 <- cowplot::plot_grid(g1,g2,nrow=1)

### combine graphs ####

group1 <- cowplot::plot_grid(c1,c2,c3,nrow=3,scale=c(0.9,0.9,0.9))
group2 <- cowplot::plot_grid(c4,c5,c6,nrow=3,scale=c(0.9,0.9,0.9))
cowplot::plot_grid(group1,group2,ncol=2,
                   labels=c("A - Uniform urban change",
                            "B - Clustered urban change"),
                   scale=c(0.9,0.9))

