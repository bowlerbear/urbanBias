library(plyr)
library(ggplot2)
library(cowplot)
require(ggthemes)

source('simulations/spatialBias_functions.R')

#test the effect of the following possible options 
#- include covariate in state model (i.e. predict response at unsampled sites based on pattern in response at sampled sites)
#- include covariate in observation model (i.e., this one doesnt work since detection probability is variation when sites are visited)
#- include covariate in new site visitation model (i.e., predict which sites are overrepresented and downweight them)

#as above:
#- include geographic coordinates as proxy for the covariate

### static scenario #######################################################

df <- generateData()
df <- getRepeatSurveys(df,constantDetection=TRUE)
fitStaticOccuModel(df,model="simulation_bias_intercepts.txt")

df <- generateData()
df <- getRepeatSurveys(df,constantDetection=FALSE)#lower effect of urban cover
fitStaticOccuModel(df,model="simulation_bias_urbanDetection.txt")

### replicate sims ########################################################

#constant detection and model
output <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df)
  temp <- fitStaticOccuModel(df)
  temp$simNu <- i
  return(temp)
})
plotEffects(output)


#constant detection and urban detection model
output <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df)
  temp <- fitStaticOccuModel(df,model="simulation_bias_urbanDetection.txt")
  temp$simNu <- i
  return(temp)
})
plotEffects(output)

#urban detection bias and constant detection model
output <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df,constantDetection=FALSE)
  temp <- fitStaticOccuModel(df,model="simulation_bias_urbanDetection.txt")
  temp$simNu <- i
  return(temp)
})
plotEffects(output)

#urban detection bias and urban detection model
output <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df,constantDetection=FALSE)
  temp <- fitStaticOccuModel(df,model="simulation_bias_urbanDetection.txt")
  temp$simNu <- i
  return(temp)
})
plotEffects(output)

### reweight data #########################################################

temp <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df)
  temp <- fitStaticOccuModel(df,model="simulation_bias_urbanDetection.txt")
  temp$simNu <- i
  return(temp)
})

tempRW <- ldply(1:300,function(i){
  df <- generateData()
  df <- getRepeatSurveys(df)
  temp <- fitStaticOccuModelWeights(df,model="simulation_bias_siteSelection.txt")
  temp$simNu <- i
  return(temp)
})


temp$Type <- "Naive"
tempRW$Type <- "Reweighted"
temp <- rbind(temp,tempRW)

estimatePlot <- ggplot(temp)+
  geom_boxplot(aes(x=factor(scenario),y=estimate/500,
                   fill=Type),
            position = position_dodge(width = 0.5),
            width=0.4,
            outlier.shape = NA)+
  theme_bw()+
  scale_fill_manual("Analysis",values=c("grey","white"))+
  xlab("sampling scenario")+
  ylab("pred occupancy prop")+
  labs(subtitle = "Estimate")+
  theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
        legend.position="top")+
  ylim(0.1,0.7)

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

ggsave("plots/static_analysis_occu.png",width=6.5,height=3)

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

### dynamic run #############################################

