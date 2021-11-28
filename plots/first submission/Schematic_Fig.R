library(ggplot2)
library(plyr)
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra) 

#read in wrapper functions
source('simulations/spatialBias_functions.R')

#### UNIFORM CHANGE ####

#### generate df ####

df_Uniform <- ldply(1:20,function(x){
  temp <- generateData(M=20,nuSamples=5)
  temp <- extendData(temp,change="uniform_change")
  temp$sim <- x
  return(temp)
})

df_Clustered <- ldply(1:20,function(x){
  temp <- generateData(M=20,nuSamples=5)
  temp <- extendData(temp,change="clustered_change")
  temp$sim <- x
  return(temp)
})

df_Nochange <- ldply(1:20,function(x){
  temp <- generateData(M=20,nuSamples=5)
  temp <- extendData(temp,change="no_change")
  temp$sim <- x
  return(temp)
})

df <- rbind(df_Uniform,df_Clustered,df_Nochange)

#### color gradient ####

#specify green and grey colour gradient
library(dichromat)
colfunc <- colorRampPalette(c(terrain.colors(5)[c(1,2,3)],"grey60","grey30"))
coloursDF <- unique(df[,c('urbanCover',"Time")])
coloursDF <- arrange(coloursDF,urbanCover)
coloursDF$Colours <- colfunc(nrow(coloursDF))

### make legend ####

coloursDF$ID <- rep(1,nrow(coloursDF))

my_hist <- ggplot(coloursDF)+
            geom_point(aes(x=urbanCover, y=ID, colour=urbanCover))+
            scale_colour_gradientn("Urban cover",
                                   colors=coloursDF$Colours,
                                   breaks=c(-1,0,1),
                                   labels=c("low","medium","high"))
 
legend <- cowplot::get_legend(my_hist)
grid.newpage()
grid.draw(legend) 

ggsave("plots/Fig1_legend.png")

### uniform ####

plot_df_uni_change <- df_Uniform %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("T", Time)) %>%
  mutate(facet_label_bias=paste0("T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("T", Time))

cols <- plot_df_uni_change$Colours
names(cols) <- as.character(plot_df_uni_change$urbanCover)

random_uniform <- ggplot()+
  geom_tile(data=plot_df_uni_change, aes(x = sim, y = Site, fill =factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_uni_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Uniform urban change") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
random_uniform

bias_uniform <- ggplot()+
  geom_tile(data=plot_df_uni_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_uni_change %>%
               dplyr::filter(Visits3==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
bias_uniform

increase_bias_uniform <- ggplot()+
  geom_tile(data=plot_df_uni_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_increase_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_uni_change %>%
               dplyr::filter(Visits4==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("Replicate") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
increase_bias_uniform

### clustered change ####

plot_df_clust_change <- df_Clustered %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("T", Time)) %>%
  mutate(facet_label_bias=paste0("T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("T", Time))

cols <- plot_df_clust_change$Colours
names(cols) <- as.character(plot_df_clust_change$urbanCover)

random_clust <- ggplot()+
  geom_tile(data=plot_df_clust_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_clust_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Clustered urban change") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
random_clust

bias_clust <- ggplot()+
  geom_tile(data=plot_df_clust_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_clust_change %>%
               dplyr::filter(Visits3==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
bias_clust

increase_bias_clust <- ggplot()+
  geom_tile(data=plot_df_clust_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_increase_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_clust_change %>%
               dplyr::filter(Visits4==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
increase_bias_clust

### no change ####

plot_df_none_change <- df_Nochange %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("T", Time)) %>%
  mutate(facet_label_bias=paste0("T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("T", Time))

cols <- plot_df_none_change$Colours
names(cols) <- as.character(plot_df_none_change$urbanCover)

random_none <- ggplot()+
  geom_tile(data=plot_df_none_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_none_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("No urban change") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
random_none

bias_none <- ggplot()+
  geom_tile(data=plot_df_none_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_none_change %>%
               dplyr::filter(Visits3==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("Urban cover gradient") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
bias_none

increase_bias_none <- ggplot()+
  geom_tile(data=plot_df_none_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_increase_bias)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_none_change %>%
               dplyr::filter(Visits4==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  #ggtitle("Bias") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor=element_blank())+
  xlab("") + 
  ylab("") + 
  theme(legend.position="none")+
  theme(axis.text=element_blank())
increase_bias_none

### combine all ####

randomPlot <- plot_grid(random_none, random_uniform, random_clust,nrow=1)

biasPlot <- plot_grid(bias_none, bias_uniform, bias_clust,nrow=1)
  
biasplusPlot <- plot_grid(increase_bias_none, increase_bias_uniform,
                      increase_bias_clust,nrow=1)

plot_grid(randomPlot,
          biasPlot,
          biasplusPlot,
          labels=c("Random sampling","Biased sampling","Biased+ sampling"),
          align="hv",
          scale=c(0.89,0.89,0.89),
          nrow=3)

ggsave("plots/Fig1.png",width=9,height=6)



