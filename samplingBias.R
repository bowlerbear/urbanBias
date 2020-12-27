setwd('C:/Users/db40fysa/Dropbox/CS spatial pattern/MS')
library(plyr)
library(ggplot2)
library(cowplot)

source('C:/Users/db40fysa/Dropbox/CS spatial pattern/MS/spatialBias_functions.R')

### missing data #########################################################

#site selection as a missing data problem:
#depend on the response value (species occurrence)

#https://cran.r-project.org/web/views/MissingData.html
#https://cran.r-project.org/web/packages/ipw/index.html
#https://blog.usejournal.com/missing-data-its-types-and-statistical-methods-to-deal-with-it-5cf8b71a443f

biodiversityX <- seq(-1,1,length.out=20)
environmentY <- seq(-1,1,length.out=20)
fullGrid <- expand.grid(biodiversityX=biodiversityX,
                        environmentY=environmentY)

#missing completely at random
fullGrid$MCAR <- plogis(0)
fullGrid$MCAR <- rbinom(nrow(fullGrid), 1, fullGrid$MCAR)

#missing at random
fullGrid$MAR <- plogis(0 + 3*fullGrid$environmentY)
fullGrid$MAR <- rbinom(nrow(fullGrid), 1, fullGrid$MAR)

#missing not at random
fullGrid$MNAR <- plogis(0 + 3*fullGrid$biodiversityX)
fullGrid$MNAR <- rbinom(nrow(fullGrid), 1, fullGrid$MNAR)

#draw grid - tile plot with probability that data is missing
library(tidyverse)
fullGrid_long <- fullGrid  %>%
  pivot_longer(cols = starts_with("M"), names_to = "Pattern", values_to = "Visited")

fullGrid_long$Pattern <- factor(fullGrid_long$Pattern,
                                levels=c("MCAR","MAR","MNAR"))
fullGrid_long$Visited <- ifelse(fullGrid_long$Visited==1,"Yes","No")

ggplot(fullGrid_long)+
  geom_tile(aes(x = environmentY, y = biodiversityX,fill=Visited))+
  facet_wrap(~Pattern,nrow=1)+
  theme_minimal()+
  scale_fill_manual(values=c("grey95","black"))+
  theme(axis.text = element_blank())+
  xlab("environmental gradient")+ylab("biodiversity gradient")

### static scenario #######################################################

df <- generateData()

plotVisits(df)

fitStatic(df)

### reweight data ###########################################################

#ipw: Estimate Inverse Probability Weights
#Functions to estimate the probability to receive the observed treatment, based on individual characteristics. The inverse of these probabilities can be used as weights when estimating causal effects #from observational data via marginal structural models. Both point, treatment situations and longitudinal studies can be analysed. The #same functions can be used to correct for informative censoring

df <- getWeights(df)
fitStaticWeights(df)

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

df <- generateData()

next_df <- extendData(df,change="no_change", 
                      biasChange = "no_change")
next_df <- extendData(df,change="uniform_change",
                      biasChange = "no_change")
next_df <- extendData(df,change="clustered_change",
                      biasChange = "no_change")

plotTimePoints(getTimePoints(next_df))
fitDynamic(next_df)
fitStaticWeights(subset(next_df,Time==1))

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

### reweight time points ###################################

outputNC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df, change="no_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})

outputUC <- ldply(1:300,function(i){
  df <- generateData()
  next_df <- extendData(df,change="uniform_change")
  temp <- getTimePoints_RW(next_df)
  temp$simNu <- i
  return(temp)
})


outputCC <- ldply(1:300,function(i){
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
         #labels=c("A - Occupancy proportions in each time step",
        #          "B - Estimated change"),
         #hjust = -0.2,
         align="v")

### power plots ##########################################

p1 <- ggplot(powerNC)+
  geom_bar(aes(x=scenario,y=nuSigs),stat="identity")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type 1 error rate")+
  ggtitle("no urban change")

p2 <- ggplot(powerUC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type II erorr rate")+
  ggtitle("uniform urban change")

p3 <- ggplot(powerCC)+
  geom_bar(aes(x=scenario,y=(1-nuSigs)),stat="identity")+
  theme_bw()+
  xlab("sampling scenario")+
  ylab("Type II error rate")+
  ggtitle("clustered urban change")

plot_grid(p1,p2,p3,nrow=1)

### reweight trends ###########################################








### jags check ##############################################

# Bundle data
win.data <- list(y = df$z[df$Visits2==1], M = length(df$z[df$Visits2==1]))

# Specify model in BUGS language
cat(file = "Bernoulli_GLM.txt","
    model {
    
    # Priors
    mean.psi ~ dunif(0,1)
    alpha <- logit(mean.psi)     # intercepts

    # Likelihood
    for (i in 1:M){
      y[i] ~ dbern(theta[i])
      logit(theta[i]) <- alpha 
    }
      
    totOccu <- sum(theta)
      
    }
    ")

# Initial values

# Parameters monitored
params <- c("alpha", "totOccu")

# MCMC settings
ni <- 10000   ;   nt <- 2   ;   nb <- 5000   ;  nc <- 3

# Call WinBUGS or JAGS from R (ART <1 min)
out1 <- jags(win.data, inits=NULL,params, "Bernoulli_GLM.txt", 
             n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(out1$summary, 2)

### end ######################################################
