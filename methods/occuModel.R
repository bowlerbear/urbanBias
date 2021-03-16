### simulate occupancy model ##################################

# simulate 50 sites with 1 species

# sites vary with urban cover

# assume species occupancy is affected by urban cover

# assume each year that only half of the sites are visits

#code taken from chapter 10.4 of AMH
#or use the function
?simOcc

### general libraries ######################################################

library(boot)
library(tidyverse)
library(unmarked)
#library(AHMbook)

### (1a) uncorrelated model ##################################

set.seed(1)                   # So we all get same data set

# Define number of sample sites
M <- 100                      

# Create a covariate called Year
T <- 20
Year <- 0:(T-1)

#Create a dataframe with sites and years
df <- data.frame(M=rep(1:M,each=T),
                 T=rep(0:(T-1),M))
N <- nrow(df)

#Number of surveys for each site and year
J <- 3                       
y <- matrix(NA, nrow = M*T, ncol = J) # the obs. data

# Choose parameter values for occupancy model and compute occupancy
beta0 <- 0                    # Logit-scale intercept - occupancy in year 1
beta1 <- 0                    # Logit-scale slope for Year
psi <- plogis(beta0 + beta1 * df$T) # Occupancy probability - no site variation
# plot(Year, psi, ylim = c(0,1), type = "l", lwd = 3) # Plot psi relationship

# Now visit each site and observe presence/absence perfectly
z <- rbinom(M*T, 1, psi)        # True presence/absence
df$z <- z

# Choose parameter values for measurement error model and compute detectability
alpha0 <- -1.4                        # Logit-scale intercept
#alpha1 <- -3                         # Logit-scale slope
p <- plogis(alpha0)                   # Detection probability c. 20%
# plot(p ~ wind, ylim = c(0,1))       # Look at relationship

# Take J = 3 presence/absence measurements at each site
for(j in 1:J) {
  y[,j] <- rbinom(M*T, z, p)
}
obs <- apply(y, 1, max)               # Number of sites with observed presences
df$obs <- obs

# Look at data so far: number of occupied sites per year
occProp <- df %>%
  group_by(T) %>%
  summarise(propZ=mean(z),propObs=mean(obs))

ggplot(occProp)+
  geom_point(aes(x=T,y=propZ))+
  geom_point(aes(x=T,propObs),color="red")+
  ylim(0,1)

# Create factors
time <- matrix(rep(as.character(1:J), M*T), ncol = J, byrow = TRUE)

# Load unmarked, format data and summarize
umf <- unmarkedFrameOccu(
  y = y,                               # Pres/Abs measurements
  siteCovs = data.frame(Year = df$T),  # site-specific covs.
  obsCovs = list(time = time))         # obs-specific covs.
summary(umf)

# Fit model and extract estimates
# Detection covariates follow first tilde, then occupancy covariates
fmSum <- summary(fm <- occu(~1 ~Year, data=umf))

#extract estimated detection probability
detectionEstimate <- plogis(fmSum$det$Estimate)   

#trend estimate
trendEstimate <- fmSum$state$Estimate[2]

# Predict occupancy and detection as function of covs (with 95% CIs)
newdat <- data.frame(Year=df$T)
df$pred.occ <- predict(fm, type="state", newdata=newdat)[,"Predicted"]

#derived parameters
df$pred.z <- bup(ranef(fm,stat="mean"))

occProp <- df %>%
  group_by(T) %>%
  summarise(propZ=mean(z),propObs=mean(obs),
            propPred=sum(pred.occ)/max(M),propPredZ=sum(pred.z)/max(M))

ggplot(occProp)+
  geom_point(aes(x=T,y=propZ))+
  geom_point(aes(x=T,y=propPredZ),colour="green")+
  geom_point(aes(x=T,propObs),color="red")+
  ylim(0,1)

### (1b) as a simulation function ###################################

simFun <- function(yearEffect=0, 
                   urbanEffect=0, 
                   urbanVariation=0,
                   propVisited=1){

# Define number of sample sites
M <- 100                      

#Define variation in urban cover
urbanCover <- sort(rnorm(M,0,urbanVariation))

# Create a covariate called Year
T <- 20
Year <- 0:(T-1)

#Create a dataframe with sites and years
df <- data.frame(M=rep(1:M,each=T),
                 T=rep(0:(T-1),M))
df$UrbanCover <- urbanCover[match(df$M,1:100)]
N <- nrow(df)

#Number of surveys for each site and year
J <- 3                       
y <- matrix(NA, nrow = M*T, ncol = J) # the obs. data

# Choose parameter values for occupancy model and compute occupancy
beta0 <- 0                    # Logit-scale intercept - occupancy in year 1
beta1 <- yearEffect           # Logit-scale slope for Year
beta2 <- urbanEffect
psi <- plogis(beta0 + beta1 * df$T + beta2 * df$UrbanCover) # Occupancy probability 

# Now visit each site and observe presence/absence perfectly
z <- rbinom(M*T, 1, psi)        # True presence/absence
df$z <- z

# Choose parameter values for measurement error model and compute detectability
alpha0 <- -1.4                        # Logit-scale intercept
#alpha1 <- -3                         # Logit-scale slope
p <- plogis(alpha0)                   # Detection probability c. 20%

# Take J = 3 presence/absence measurements at each site
for(j in 1:J) {
  y[,j] <- rbinom(M*T, z, p)
}
obs <- apply(y, 1, max)               # Number of sites with observed presences
df$obs <- obs

#assume some sites were not visited - those with the highest values
nuVisited <- M * propVisited
y[which(!df$M %in% 1:nuVisited),] <- NA

# Create factors
time <- matrix(rep(as.character(1:J), M*T), ncol = J, byrow = TRUE)

# Load unmarked, format data and summarize
umf <- unmarkedFrameOccu(
  y = y,                               # Pres/Abs measurements
  siteCovs = data.frame(Year = df$T, Site = df$M),  # site-specific covs.
  obsCovs = list(time = time))         # obs-specific covs.

# Fit model and extract estimates
# Detection covariates follow first tilde, then occupancy covariates
fmSum <- summary(fm <- occu(~1 ~Year, data=umf))

#extract estimated detection probability
detectionEstimate <- plogis(fmSum$det$Estimate)   

#initial Estimate
initialEstimate <- fmSum$state$Estimate[1]

#trend estimate
trendEstimate <- fmSum$state$Estimate[2]

# Predict occupancy and detection as function of covs (with 95% CIs)
newdat <- data.frame(Year=df$T)
df$pred.occ <- predict(fm, type="state", newdata=newdat)[,"Predicted"]

occProp <- df %>%
  group_by(T) %>%
  summarise(propZ=mean(z),propObs=mean(obs),propPred=sum(pred.occ)/max(M))

#add on detection prob and trend estimate
occProp$overall_detectionEstimate <- detectionEstimate
occProp$overall_trendEstimate <- trendEstimate
occProp$overall_initialEstimate <- initialEstimate

#return data frame
return(occProp)

}

#run 100 times
sumOutput <- plyr::ldply(1:50,function(x){
  temp = simFun()
  temp$simNu = x
  return(temp)
})


ggplot(sumOutput)+
  geom_line(aes(x=T,y=propPred,group=simNu),
            alpha=0.5,colour="mediumblue")+
  theme(legend.position="none")+
  geom_hline(yintercept=0.5,linetype="dashed",colour="black")+
  ylim(0,1)+
  theme_bw()

### (1c) uncorrelated JAGS #################

simFun <- function(yearEffect=0, urbanEffect=0, 
                   urbanVariation=2, urbanization=FALSE,
                   biasedVisits=FALSE,
                   model="simulation_bias_annual.txt"){
  
  # Define number of sample sites
  M <- 100                      
  
  #Define variation in urban cover
  urbanCover <- sort(rnorm(M,0,urbanVariation))
  
  # Create a covariate called Year
  T <- 20
  Year <- 0:(T-1)
  
  #Create a dataframe with sites and years
  df <- data.frame(M=rep(1:M,each=T),
                   T=rep(0:(T-1),M))
  df$UrbanCover <- urbanCover[match(df$M,1:100)]
  N <- nrow(df)
  
  #Number of surveys for each site and year
  J <- 3                       
  y <- matrix(NA, nrow = M*T, ncol = J) # the obs. data
  
  # Choose parameter values for occupancy model and compute occupancy
  beta0 <- 0                    # Logit-scale intercept - occupancy in year 1, 0.5 in year 1
  beta1 <- yearEffect           # Logit-scale slope for Year
  beta2 <- urbanEffect
  
  if(urbanization==TRUE){
    #add on the time to the urban cover variable
    
    df$UrbanCover <-df$UrbanCover + (df$T)/10
    
  }else if(urbanization=="clumped"){
    
    #assume highest urban cover sites are increasing in urban cover more
  
    range_urbanCover <- quantile(urbanCover,c(0.25,0.75))
    mySelect <- df$UrbanCover>=range_urbanCover[2]
    df$UrbanCover[mySelect] <- df$UrbanCover[mySelect] + (df$T[mySelect])/10
    
  }
  
  psi <- plogis(beta0 + beta1 * df$T + beta2 * df$UrbanCover) # Occupancy probability 
  
  # Now visit each site and observe presence/absence perfectly
  z <- rbinom(M*T, 1, psi)        # True presence/absence
  df$z <- z
  
  # Choose parameter values for measurement error model and compute detectability
  alpha0 <- -1.4                        # Logit-scale intercept
  #alpha1 <- -3                         # Logit-scale slope
  p <- plogis(alpha0)                   # Detection probability c. 20%
  
  # Take J = 3 presence/absence measurements at each site
  for(j in 1:J) {
    y[,j] <- rbinom(M*T, z, p)
  }
  obs <- apply(y, 1, max)               # Number of sites with observed presences
  df$obs <- obs
  
  #format observations into an array
  y <- as.data.frame(y)
  y$Site <- df$M
  y$Year <- df$T
  y <- reshape::melt(y,id=c("Site","Year"))
  y <- reshape2::acast(y,Site~Year~variable,value.var="value")
  
  #select sites to be visited
  df$Visited <- rep(0,nrow(df))
  
  if(biasedVisits==FALSE){
    
    visitedSites = c(1:M)[!c(1:M)%%2]
    
    #set sites outside of visisted sites as NAs
    y[which(!c(1:M) %in% visitedSites),,] <- NA
  
    df$Visited[df$M %in% visitedSites] <- 1 
    
  }else if (biasedVisits==TRUE){
    
    visitedSites = c(1:(M/2)) #all of first half - below average UC
    
    #set sites outside of visisted sites as NAs
    y[which(!c(1:M) %in% visitedSites),,] <- NA
    
    df$Visited[df$M %in% visitedSites] <- 1 
    
  }else if(biasedVisits=="Tendency"){
    
    #1/4 of sites come from above average UC
    #3/4 of sites from below average UC
    visitedSitesUpper = sample(((M/2)+1):M,floor((1/4)*(M/2)))
    visitedSitesLower = sample(1:(M/2),ceiling((3/4)*(M/2)))
    visitedSites = sort(c(visitedSitesLower,
                          visitedSitesUpper))
   
    #set sites outside of visisted sites as NAs
    y[which(!c(1:M) %in% visitedSites),,] <- NA   

    df$Visited[df$M %in% visitedSites] <- 1 
    
  }else if(biasedVisits=="Increasing"){
    
    #assume increasing bias towards the most urban sites
    #assume each year one less visit to the below average sites, and one more to above average sites
    
    
      for(t in 0:(T-1)){
        #sample half below, half above
        half <- (M/2) + t
        visitedSitesLower <- sample(1:half,(M/4))
        visitedSitesUpper <- sample((half+1):M,(M/4))
        visitedSites = sort(c(visitedSitesLower,
                              visitedSitesUpper))
        df$Visited[(df$T==t) & (df$M %in% visitedSites)] <- 1 
      }
    
    #set y to NAs when visited = 0
    for(m in 1:M){
      for(t in 1:T){
        y[m,t,] <- ifelse(df$Visited[df$M==m & df$T==(t-1)]==1,
                          y[m,t,],NA)   
      }
    }
  
  }
  
  
  #format for jags
  require(rjags)
  require(jagsUI)
  jags.data = list(n.site=M,
                   n.year=T,
                   J = J,
                   y = y,
                   covariate = urbanCover,
                   C = 10000)
  
  
  #add params for regression trend
  jags.data$sumX <- sum(1:jags.data$n.year)
  jags.data$sumX2 <- sum((1:jags.data$n.year)^2)
  
  #add site selection variable
  #jags.data$siteVisited <- reshape2::acast(df,M~T,value.var="Visited")
  
  #for ones
  #jags.data$ones <- jags.data$siteVisited 
  #jags.data$ones[jags.data$ones!=1] <- 1
  
  # Initial values
  zst <- apply(y, c(1,2), max)  
  zst[is.na(zst)] <- 0
  inits <- function(){list(z = zst)}
  
  # Parameters monitored
  params <- c("alpha0", "beta1", "beta0", "psi.fs",
              "beta.a","sV") 

  # MCMC settings
  ni <- 500   ;   nt <- 5   ;   nb <- 250   ;   nc <- 3
  
  # fit model
  out1 <- jags(jags.data, inits, params, 
               paste("models",model,sep="/"), 
               n.chains = nc,n.thin = nt, 
               n.iter = ni, n.burnin = nb,
               parallel = T) 
  
  #print(out1$summary,2)
  
  # Fit model and extract estimates
  #extract estimated detection probability
  detectionEstimate <- inv.logit(out1$mean$alpha0)   
  
  #initial occupancy
  if("beta0" %in% names(out1$mean)){
    initialEstimate <- inv.logit(out1$mean$beta0)
  }else if("beta.a" %in% names(out1$mean)){
    initialEstimate <- inv.logit(out1$mean$beta.a[1])
  }
  
  #trend estimate
  trendEstimate <- out1$mean$beta1
  
  # Predicted psi.fs
  occProp <- data.frame(T = 1:T,
                        psi.fs = out1$mean$psi.fs)
  
  #add on detection prob and trend estimate
  occProp$overall_initialEstimate <- initialEstimate
  occProp$overall_detectionEstimate <- detectionEstimate
  occProp$overall_trendEstimate <- trendEstimate
  
  #add on true occupancy and true trend
  zProp <- df %>%
            group_by(T) %>%
            summarise(z=mean(z))
  occProp$zProp <- zProp$z
  
  #return data frame
  return(occProp)
  
}

simFun(yearEffect=0, urbanEffect=0, urbanVariation=1,urbanization=FALSE,biasedVisits=FALSE)
simFun(yearEffect=0, urbanEffect=0, urbanVariation=2,urbanization=TRUE,biasedVisits=FALSE)
simFun(yearEffect=0, urbanEffect=0, urbanVariation=2,urbanization=TRUE,biasedVisits=TRUE)

#https://stackoverflow.com/questions/46098681/jags-logistic-regression-models-with-non-integer-weights

#urban effect
sumOutput0 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=0, urbanVariation=0,urbanization=FALSE,biasedVisits=FALSE)
  temp$simNu = x
  return(temp)
})


sumOutput1 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=FALSE,biasedVisits=FALSE)
  temp$simNu = x
  return(temp)
})
sumOutput2 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=FALSE,biasedVisits=TRUE)
  temp$simNu = x
  return(temp)
})



sumOutput5 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=TRUE,biasedVisits=FALSE)
  temp$simNu = x
  return(temp)
})
sumOutput6 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=TRUE,biasedVisits=TRUE)
  temp$simNu = x
  return(temp)
})




sumOutput3 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=FALSE,biasedVisits="Tendency")
  temp$simNu = x
  return(temp)
})


sumOutput4 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=FALSE,biasedVisits="Increasing")
  temp$simNu = x
  return(temp)
})




sumOutput7 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=TRUE,biasedVisits="Tendency")
  temp$simNu = x
  return(temp)
})

sumOutput8 <- plyr::ldply(1:100,function(x){
  temp = simFun(yearEffect=0, urbanEffect=-1, urbanVariation=2,urbanization=TRUE,biasedVisits="Increasing")
  temp$simNu = x
  return(temp)
})


#plot time-series
g1 <- ggplot(sumOutput1)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g2 <- ggplot(sumOutput2)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g3 <- ggplot(sumOutput3)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g4 <- ggplot(sumOutput4)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  theme_bw()
g5 <- ggplot(sumOutput5)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g6 <- ggplot(sumOutput6)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g7 <- ggplot(sumOutput7)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  ylim(0,1)+
  theme_bw()
g8 <- ggplot(sumOutput8)+         
  geom_line(aes(x=T,y=psi.fs,group=simNu),
            alpha=0.1,colour="mediumblue",size=2)+
  theme(legend.position="none")+
  theme_bw()+
  theme(axis.ticks.y = element_blank())
  


library(cowplot)
plot_grid(g7,g8,ncol=2)

 #results
#sumOutput0 - higher than 0

#urbanization - false
#sumOutput1 - like sumOutput0
#sumOutput2 - higher intercept, but no trend


#urbanzation - true
#sumOutput3 - trend - lower intercept
#sumOutput4 - trend - lower intercept but same trend
#sumOutput5 - same trend



#plot detection probability and trend estimate
detectionSummary <- subset(sumOutput1,!duplicated(simNu))
ggplot(detectionSummary)+
  geom_histogram(aes(x=overall_detectionEstimate))
ggplot(detectionSummary)+
  geom_histogram(aes(x=overall_trendEstimate))

#compile and compare results

#up weight sites with low probability of visitation
#down weight sites with high probability of visitation

#keep sample size constant
summary(1/out1$mean$sV)
sampSize <- M*T

### (2) auto-logistic model #######################################
##
#set general parameters:

#things we wont change much
set.seed(1)                   # So we all get same data set
M <- 50                       # Number of sites - assume arranged linearly
J <- 5                        # Number of presence/absence measurements

# Create a covariate called urban for occupancy
urbanProp <- sort(runif(M, -1, 1)) 
speciesBeta <- -10  #slope for covariate effect - assume species is negatively associated

### year 1 ######################################################

getYearOne <- function(mean.psi = 0.5){
  
  psi <- plogis(logit(mean.psi) + speciesBeta * urbanProp)
  z <- rbinom(M, 1, psi)  #psi is a probability
  return(z)
  
}

z1 <- getYearOne(mean.psi = 0.5)


### next years ##################################################

#https://code.usgs.gov/usgs/norock/irvine_k/ip-097746/
#auto-logistic model, page 313
#equation describing the dynamics
#logit(Pi) = intercept + beta.auto*Z + covariate.effect

#specify urban change
#assume all areas become a bit more urban over time  
  
getNextYear <- function(z_cur,intercept = 0, z.effect = 1){
 
  # current occupancy conditional on past
  # plus some effect of urban change
  urbanDiff <- 0.25
  
  #status quo
  pi <- intercept + z.effect*z_cur
  
  #effect of ongoing occupancy should be affected by urbanization still
  pi <- plogis(intercept + z.effect*z_cur + urbanDiff * speciesBeta)
  #pi <- plogis(intercept + z.effect*z_cur + siteEffect * beta.site)#including site effect
  
  z <- rbinom(M, 1, pi)# pi is a probability 
  
  return(z)
  
}

generateDynamics <- function(z_year1,nyear=10){
  
  z_all <- matrix(data=NA,nrow=M,ncol=nyear)
  z_all[,1] <- z_year1
  
  #simulate each of the following years
  for(i in 2:nyear){
    
    z_all[,i] <- getNextYear(z_cur = z_all[,i-1])
    
  }
  
  #return all
  return(z_all)
  
}

z <- generateDynamics(z_year1 = z1)


#number of observed sites over time
plot(1:10,colSums(z))

### sample data 

sampleData <- function(z,mean.p=0.5){
  
  p <- plogis(logit(mean.p))# constant detection prob
  
  #matrix to hold data
  y <- array(data = NA,dim =c(nrow(z),ncol(z),J)) 
  
  #repeat surveys
  for(i in 1:nrow(z)) {
    for(t in 1:ncol(z)) {
      
      y[i,t,] <- rbinom(J, z[i,t], p)
      
    }
  }
  
  #return data
  return(y)
}


obs <- sampleData(z = z, mean.p = 0.5)
dim(obs)
#50 10  5


### (3) dynamic model###########################################

#from https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf

#set.seed(13973) 
M <- 50                                # Number of sites 
J <- 3                                  # num secondary sample periods 
T <- 20                                 # num primary sample periods 
psi <- rep(NA, T)                       # Occupancy probability 
muZ <- z <- array(dim = c(M, T))        # Expected and realized occurrence> 
y <- array(NA, dim = c(M, J, T))  # Detection histories 

#assume all sites are the same
#urban cover
#urbanProp<- sort(runif(M, -1, 1)) 

#dynamic probabilities
mean.phi = 0.45
mean.gamma = 0.05
phi <- runif(n=T-1, min=0.4, max=0.5)
gamma <- runif(n=T-1, min=0.0, max=0.1)

#equilibrium occupancy
#page 302 on Royle and Dorazio
equilOccu <- mean.gamma/(mean.gamma + (1-mean.phi))
equilOccu
nuOccSites <- equilOccu * M

#initial occupancy probability
initialPsi <- equilOccu[1]  # set to be at equilbrium at the start
psi[1] <- initialPsi

#detection probabilities
p <- c(0.5) # Detection probability

#species covariate effects
# speciesBeta <- -3
# speciesPhi <- -0.5
# speciesGamma <- -2

# Generate latent states of occurrence

#scenario 1 - no effect of urban cover
for(i in 1:M){                          
# Loop over sites
  
  z[,1] <- rbinom(M, 1, psi[1])  

    for(k in 2:T){                        
  # Loop over years
      
      muZ[k] <- z[i, k-1]*phi[k-1] + (1-z[i, k-1])*gamma[k-1]
      z[i,k] <- rbinom(1, 1, muZ[k])
    }
}
  
#number of occupied sites over time
plot(1:T,colSums(z))

#prob site means
plot(rowMeans(z))

# Generate detection/non-detection data> 
for(i in 1:M){
  for(k in 1:T){
    prob <- z[i,k] * p
      for(j in 1:J){
        y[i,j,k] <- rbinom(1, 1, prob)
    }
  }
} 
  
# Compute annual population occupancy 
  for (k in 2:T){
    psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
  }

plot(1:T,psi)


### fit unmarked model ###################################

library(unmarked)

#melt to a 2d data frame
df <- y
dim(df)
dim(df) <- as.matrix(c(50*20,3))

umf <- unmarkedFrameOccu(y = df)  # Create unmarked data frame
summary(umf)                     # Summarize data frame
(fm1 <- occu(~1 ~1, data = umf)) # Fit model

backTransform(fm1, "state")      # Get estimates on probability scale
backTransform(fm1, "det")

# Empirical Bayes estimates of number of sites occupied in each year
re <- ranef(fm1)
modes <- bup(re, stat="mode")#Best Unbiased Predictors
plot(1:7, modes, xlab="Year", ylab="Sites occupied")

### scenarios #############################################

##species scenarios:

# species is indifferent to urban cover

# species is positively affeced by urban cover

# species is negatively affected by urban cover


##environmental change scenarios:

# no urban cover change

# increased urban cover over time


##visit scenarios (only half of the sites visited each year):

# no bias scenario - site visit probability doesn't depend on habitat. Each site is visited 3 times.

# bias visit - urban sites more likely to be visited. Each site is visited 3 times.

# bias visit change - urban sites more likely to be visited and this effect increases over time. Each site is visited 3 times.

# bias frequency - urban sites more frequently visited

# bias visit freq change - urban sites more frequently visited, and this bias increases over time




