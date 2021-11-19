library(ggplot2)
library(plyr)
library(tidyverse)
library(patchwork)

#read in wrapper functions
source('simulations/spatialBias_functions.R')

#### UNIFORM CHANGE ####

#### generate df #######

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
#colfunc <- colorRampPalette("#00A600" "#E6E600" "#EAB64E" "#EEB99F" "#F2F2F2")
colfunc <- colorRampPalette(c(terrain.colors(5)[c(1,2,3)],"grey60","grey30"))
coloursDF <- unique(df[,c('urbanCover',"Time")])
coloursDF <- arrange(coloursDF,urbanCover)
coloursDF$Colours <- colfunc(nrow(coloursDF))

### uniform ###

plot_df_uni_change <- df_Uniform %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("Random - T", Time)) %>%
  mutate(facet_label_bias=paste0("Bias - T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("Bias+ - T", Time))

cols <- plot_df_uni_change$Colours
names(cols) <- as.character(plot_df_uni_change$urbanCover)

random_uniform <- ggplot()+
  geom_tile(data=plot_df_uni_change, aes(x = sim, y = Site, fill =factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_uni_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  ggtitle("Uniform urban change") +
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

random_uniform + bias_uniform + increase_bias_uniform + plot_layout(ncol=1)

# Now clustered change

plot_df_clust_change <- df_Clustered %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("Random - T", Time)) %>%
  mutate(facet_label_bias=paste0("Bias - T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("Bias+ - T", Time))

cols <- plot_df_clust_change$Colours
names(cols) <- as.character(plot_df_clust_change$urbanCover)

random_clust <- ggplot()+
  geom_tile(data=plot_df_clust_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_clust_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  ggtitle("Clustered urban change") +
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

random_clust + bias_clust + increase_bias_clust + plot_layout(ncol=1)

# Now no change

plot_df_none_change <- df_Nochange %>%
  left_join(., coloursDF) %>%
  mutate(facet_label_random=paste0("Random - T", Time)) %>%
  mutate(facet_label_bias=paste0("Bias - T", Time)) %>%
  mutate(facet_label_increase_bias=paste0("Bias+ - T", Time))

cols <- plot_df_none_change$Colours
names(cols) <- as.character(plot_df_none_change$urbanCover)

random_none <- ggplot()+
  geom_tile(data=plot_df_none_change, aes(x = sim, y = Site, fill = factor(urbanCover)))+
  facet_wrap(~facet_label_random)+
  scale_fill_manual(values = cols)+
  geom_point(data = plot_df_none_change %>%
               dplyr::filter(Visits2==1), aes(x = sim, y = Site), 
             shape=0, size=0.5, color="white")+
  ggtitle("No urban change") +
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

random_none + bias_none + increase_bias_none + plot_layout(ncol=1)

# put all into one plot

random_none + random_uniform + random_clust +
  bias_none + bias_uniform + bias_clust +
  increase_bias_none + increase_bias_uniform + increase_bias_clust + plot_layout(ncol=3)


# ########### DIANA's ORIGINAL STUFF
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits2==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Random - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits2==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Random - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# c1 <- cowplot::plot_grid(g1,g2,nrow=1)
# c1
# 
# #### bias ###########
# 
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits3==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Bias - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
#   
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits3==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Bias - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
#  
# c2 <- cowplot::plot_grid(g1,g2,nrow=1)
# c2
# 
# #### incr bias ##############
# 
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits4==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Increased bias - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits4==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Increased bias - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# c3 <- cowplot::plot_grid(g1,g2,nrow=1)
# 
# #### CLUSTERED CHANGE ####
# 
# df <- ldply(1:20,function(x){
#   temp <- generateData(M=20,nuSamples=5)
#   temp <- extendData(temp,change="clustered_change")
#   temp$sim <- x
#   return(temp)
# })
# 
# #### color gradient ####
# 
# #specify green and grey colour gradient
# library(dichromat)
# colfunc <- colorRampPalette(c("lightgreen", "grey70", "grey10"))
# coloursDF <- unique(df[,c('urbanCover',"Time")])
# coloursDF <- arrange(coloursDF,urbanCover)
# coloursDF$Colours <- colfunc(nrow(coloursDF))
# 
# #### random ###########
# 
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits2==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Random - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits2==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Random - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# c4 <- cowplot::plot_grid(g1,g2,nrow=1)
# 
# #### bias ###########
# 
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits3==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Bias - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits3==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Bias - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# 
# c5 <- cowplot::plot_grid(g1,g2,nrow=1)
# 
# #### incr bias ##############
# 
# g1 <- ggplot(subset(df,Time==1))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==1])+
#   geom_point(data = subset(df, Visits4==1 & Time==1), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Increased bias - Time 1") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# g2 <- ggplot(subset(df,Time==2))+
#   geom_tile(aes(x = sim, y = Site, fill = urbanCover))+
#   scale_fill_gradientn("Urban cover", colors = coloursDF$Colours[coloursDF$Time==2])+
#   geom_point(data = subset(df, Visits4==1 & Time==2), aes(x = sim, y = Site),
#              shape=0,size=1)+
#   ggtitle("Increased bias - Time 2") +
#   theme_minimal() + 
#   xlab("Replicate") + ylab("Site") + 
#   theme(legend.position="none")
# 
# c6 <- cowplot::plot_grid(g1,g2,nrow=1)
# 
# ### combine graphs ####
# 
# group1 <- cowplot::plot_grid(c1,c2,c3,nrow=3,scale=c(0.9,0.9,0.9))
# group2 <- cowplot::plot_grid(c4,c5,c6,nrow=3,scale=c(0.9,0.9,0.9))
# cowplot::plot_grid(group1,group2,ncol=2,
#                    labels=c("A - Uniform urban change",
#                             "B - Clustered urban change"),
#                    scale=c(0.9,0.9))
# 
