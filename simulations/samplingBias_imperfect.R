library(plyr)
library(ggplot2)
library(cowplot)
require(ggthemes)

source('C:/Users/db40fysa/Dropbox/CS spatial pattern/MS/urbanBias/spatialBias_functions.R')

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

#ipw: Estimate Inverse Probability Weights
#Functions to estimate the probability to receive the observed treatment, based on individual characteristics. The inverse of these probabilities can be used as weights when estimating causal effects #from observational data via marginal structural models. Both point, treatment situations and longitudinal studies can be analysed. The #same functions can be used to correct for informative censoring

#add site visitation model to the occupancy model...

#use it to predict missing values

df <- getWeights(df)
fitStaticWeights(df)

temp <- ldply(1:300,function(i){
  df <- generateData()
  df <- getWeights(df)
  fitStatic(df)
})

tempRW <- ldply(1:300,function(i){
  df <- generateData()
  df <- getWeights(df)
  fitStaticWeights(df)
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
ggsave("plots/static_analysis.png",width=6.5,height=3)

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

#no change scenario
df <- generateData()
next_df <- extendData(df,change="no_change")
next_df <- getRepeatSurveys(next_df)

getOccuTimePoints(next_df)
plotTimePoints(getTimePoints(next_df))

fitOccuDynamic(next_df)

next_df <- extendData(df,change="uniform_change")
next_df <- extendData(df,change="clustered_change")

### replicate sims #########################################

outputNC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df,change="clustered_change")
  temp <- getTimePoints(next_df)
  temp$simNu <- i
  return(temp)
})

g1 <- ggplot(outputNC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupancy prop")+
              labs(subtitle = "no urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)

g2 <- ggplot(outputUC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupied prop")+
              labs(subtitle = "uniform urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)

g3 <- ggplot(outputCC)+
  geom_violin(aes(x=scenario,y=estimate/500,fill=Time),
              position = position_dodge(width = 0.5))+
              theme_bw()+
              theme(legend.position = "none")+
              xlab("sampling scenario")+
              ylab("pred occupied prop")+
              labs(subtitle = "clustered urban change")+
              theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
              ylim(0,0.7)


grid1 <- plot_grid(g1,g2,g3,nrow=1)

### urban cover check ######################################

g1 <- ggplot(outputUC)+
  geom_boxplot(aes(x=scenario,y=meanUrban,fill=Time))+
  ylim(0,1)+ggtitle('uniform urban change')

g2 <- ggplot(outputCC)+
  geom_boxplot(aes(x=scenario,y=meanUrban,fill=Time))+
  ylim(0,1)+ggtitle('clustered urban change')

cowplot::plot_grid(g1,g2,nrow=1)

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

grid1 <- plot_grid(g1,g2,g3,nrow=1)

### reweighing test ########################################

#reweighting each time point separely
out <- ldply(1:300,function(i){

df <- generateData()

next_df <- extendData(df, change="uniform_change")

next_df1 <- subset(next_df,Time==1)
next_df2 <- subset(next_df,Time==2)

summary(next_df1$urbanCover)
summary(next_df2$urbanCover)

#qplot(urbanCover,VisitPreds2,data=next_df1)+
#geom_point(aes(x=urbanCover,y=VisitPreds2),
#           data=next_df2,col="red")

#add weights
next_df1 <- getWeights(next_df1)
next_df2 <- getWeights(next_df2)

#plot weights
qplot(urbanCover,Weights4,data=subset(next_df1,Visits4==1))
qplot(urbanCover,Weights4,data=subset(next_df1,Visits4==0))
qplot(urbanCover,Weights4,data=subset(next_df2,Visits4==1))
qplot(urbanCover,Weights4,data=subset(next_df2,Visits4==0))
#direction of the weighing changes???

#weighting changes...

temp1 <- ipwpoint(exposure = Visits4, 
                 family = "binomial", link = "logit",
                 numerator = ~ 1, denominator = ~ urbanCover, 
                 data = next_df1)
next_df1$Weights <- temp1$ipw.weights
summary(next_df1$Weights)

temp2 <- ipwpoint(exposure = Visits4, 
                 family = "binomial", link = "logit",
                 numerator = ~ 1, denominator = ~ urbanCover, 
                 data = next_df2)
next_df2$Weights <- temp2$ipw.weights
summary(next_df2$Weights)

# data.frame(urbanCover1 = next_df$urbanCover[next_df1$Visits2==1],
#            urbanCover2 = next_df$urbanCover[next_df2$Visits2==1],
#            Weights1 = Weights1[next_df1$Visits2==1],
#            Weights2 = Weights2[next_df2$Visits2==1],
#            simNu=i)
# ggplot(out)+
#   geom_line(aes(x=urbanCover1,y=Weights1,group=simNu),
#             color="black")+
#   geom_line(aes(x=urbanCover2,y=Weights2,group=simNu),
#              color="red")

glm1 <- svyglm(z ~ 1,
               family=binomial,
               design = svydesign(~ 1, weights = ~ Weights,
                                  data = subset(next_df1,
                                                Visits4==1)))
preds <- as.data.frame(predict(glm1,
                               newdata=next_df1,type="response",se=T))

temp1 <- data.frame(time = 1, 
           estimate = sum(preds$response),
           se = sum(preds$SE),
           nuVisits = sum(next_df1$Visits4==1),
           num = temp1$num.mod$coefficients[1],
           denom1 = temp1$den.mod$coefficients[1],
           denom2 = temp1$den.mod$coefficients[2])

glm2 <- svyglm(z ~ 1,
               family=binomial,
               design = svydesign(~ 1, weights = ~ Weights,
                                  data = subset(next_df2,
                                                Visits4==1)))
preds <- as.data.frame(predict(glm2,
                               newdata=next_df2,type="response",se=T))

temp2 <- data.frame(time = 2, 
                    estimate = sum(preds$response),
                    se = sum(preds$SE),
                    nuVisits = sum(next_df2$Visits4==1),
                    num = temp2$num.mod$coefficients[1],
                    denom1 = temp2$den.mod$coefficients[1],
                    denom2 = temp2$den.mod$coefficients[2])

rbind(temp1,temp2)

})

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=nuVisits))#50 visits each time

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=estimate))

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=num))

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=denom1))

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=denom2))

#why is there a decrease
#there should be decrease!! 
#true change from 250 to 202/3

#slight underestimates with visits3
#greater understimates with visits4, especially for time point 2

#with standard glm
out <- ldply(1:300,function(i){
  
  df <- generateData()
  
  next_df <- extendData(df, change="uniform_change")
  
  next_df1 <- subset(next_df,Time==1)
  next_df2 <- subset(next_df,Time==2)
  
  #weights
  temp <- glm(Visits2 ~ urbanCover, family=binomial, 
                   data = next_df1)
  next_df1$Weights <- 1/(predict(temp,type="response"))
  
  temp <- glm(Visits2 ~ urbanCover, family=binomial, 
              data = next_df2)
  next_df2$Weights <- 1/(predict(temp,type="response"))
  
  #glms
  glm1 <- glm(z ~ 1,family=binomial,weights = Weights,
              data = subset(next_df1,Visits2==1))
  preds <- as.data.frame(predict(glm1,newdata=next_df1,type="response",se=T))
  
  temp1 <- data.frame(time = 1, 
                      estimate = sum(preds$fit),
                      se = sum(preds$se.fit))
  
  glm2 <- glm(z ~ 1,family=binomial,weights = Weights,
              data = subset(next_df2,Visits2==1))
  preds <- as.data.frame(predict(glm2,newdata=next_df2,type="response",se=T))
  
  temp2 <- data.frame(time = 2, 
                      estimate = sum(preds$fit),
                      se = sum(preds$se.fit))
  
  rbind(temp1,temp2)
  
})

ggplot(out)+
  geom_boxplot(aes(x=factor(time),y=estimate))

#Visit2 - as above
#Visit4 - same result as above

### trend plots ############################################

#no change in bias
outputNC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  fitDynamic(next_df)
})
powerNC <- ddply(outputNC,"scenario",summarise,nuSigs=mean(change_p<0.05))

outputUC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df, change="uniform_change")
  fitDynamic(next_df)
})
powerUC <- ddply(outputUC,"scenario",summarise,nuSigs=mean(change_p<0.05))

outputCC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df, change="clustered_change")
  fitDynamic(next_df)
})
powerCC <- ddply(outputCC,"scenario",summarise,nuSigs=mean(change_p<0.05))

g1 <- ggplot(outputNC)+
  geom_boxplot(aes(x=scenario,y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-1.3,0.3)


g2 <- ggplot(outputUC)+
  geom_boxplot(aes(x=scenario,y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-1.3,0.3)

g3 <- ggplot(outputCC)+
  geom_boxplot(aes(x=scenario,y=change),outlier.shape = NA)+
  theme_bw()+
  theme(legend.position = "none")+
  geom_hline(yintercept=0,linetype="dashed")+
  xlab("sampling scenario")+ylab("change estimate")+
  ylim(-1.3,0.3)

grid2 <- plot_grid(g1,g2,g3,nrow=1)

plot_grid(grid1,grid2,ncol=1,
         labels=c("A",
                  "B"),
         vjust =  1,
         hjust = -0.1,
         align="v")

ggsave("plots/naive_change.png",width=7,height=4.5)

### power plots ##########################################

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


### end ######################################################
