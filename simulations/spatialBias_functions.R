generateData <- function(M=500,nuSamples=100, beta1=-2,urbanBias=2){
  
  # M is the number of sites
  
  #State process - occupancy is affected by urban cover
  
  #% between 0 and 1
  #shift 0 to 1 to -0.5 to 0.5
  #urbanCover <- seq(-0.5, 0.3,length.out = M)
  
  #or random between -1 and 1
  urbanCover <- seq(-1, 1,length.out = M)

  #simulate the relationships  
  beta0 <- 0  
  #beta1 is the regression coefficient of urban cover
  psi <- plogis(beta0 + beta1 * urbanCover) # Occupancy probability
  #plot(urbanCover, psi, ylim = c(0,1), type = "l", lwd = 3) 
  
  #draw some realizations of this
  z <- rbinom(M, 1, psi)        
  
  #combine into a data frame
  df <- data.frame(Site = 1:M,urbanCover,psi,z)
  
  #sum(df$z)#true number of occupied sites is 54
  
  ### visitation model (1 = a site is visited)
  
  #assume all sites are visited
  df$Visits1 <- rep(1,M)
  
  #assume % visited - at random
  probVisit <- rep(plogis(0),M) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  df$Visits2 <- ifelse(df$Site %in% Visits,1,0)
  
  #assume % visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * urbanCover) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  df$Visits3 <- ifelse(df$Site %in% Visits,1,0)
  
  #assume % visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * urbanCover) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  df$Visits4 <- ifelse(df$Site %in% Visits,1,0)
  
  df$Time <- 1
  
  return(df)
  
}

fitStatic <- function(df){
  require(boot)
  plyr::ldply(1:4,function(i){
    glm1 <- glm(z ~ 1, data = df[df[,paste0("Visits",i)]==1,],family=binomial)
    data.frame(scenario = i, 
               estimate = inv.logit(summary(glm1)$coefficients[1,1]),
               se = summary(glm1)$coefficients[1,2])
  #meanUrban = mean(df$urbanCover[df[,paste0("Visits",i)]==1]
  })
}

extendData <- function(df,beta1=-2,urbanBias=2,change="no_change"){
  
  M = length(unique(df$Site))
  nuSamples = sum(df$Visits2==1)
  
  #extract data for next time step
  if("p" %in% names(df)){
    next_df <- unique(df[,c("Site","urbanCover","psi","z","y","p")])
  }else{
    next_df <- unique(df[,c("Site","urbanCover","psi","z")])
  }
  
  #is there a change in urban cover
  if(change=="no_change"){
    
    next_df$urbanCover <- next_df$urbanCover
    
  }else if(change=="uniform_change"){
    
    next_df$urbanCover <- next_df$urbanCover + 0.2 
    
  }else if(change=="clustered_change"){
    
    #more urbanization of high urban sites
    next_df$urbanCover[next_df$urbanCover>0] <- 
      next_df$urbanCover[next_df$urbanCover>0] + 0.4
    
  }else if(change=="clustered_change2"){
    
    #more urbanization of high urban sites
    totalUrban <- M * 0.2
    urbanDF <- df[,c("Site","urbanCover")]
    urbanDF <- dplyr::arrange(df,desc(urbanCover))
    urbanDF$green <- 0.5-urbanDF$urbanCover
    urbanDF$cumsum <- cumsum(urbanDF$green)
    lastSite <- max(which(urbanDF$cumsum < totalUrban))
    remainder <- totalUrban - urbanDF$cumsum[lastSite]
    urbanDF$urbanCover[1:lastSite] <- 0.5
    urbanDF$urbanCover[lastSite+1] <- urbanDF$urbanCover[lastSite+1] + remainder
    next_df$urbanCover <- urbanDF$urbanCover[match(next_df$Site,urbanDF$Site)]
    
  }
  
  #generate new observations
  beta0 = 0
  next_df$psi <- plogis(beta0 + beta1 * next_df$urbanCover) 
  next_df$z <- rbinom(M, 1, next_df$psi)
  
  #revisit each site
  ### visitation model (1 = a site is visited)
  
  #rescale so that the median urban cover is still zero
  medianUrbanChange <- median(next_df$urbanCover)
  next_df$urbanCover <- next_df$urbanCover - medianUrbanChange
  
  #assume all sites are visited
  next_df$Visits1 <- rep(1,M)
  
  #assume % visited - at random
  probVisit <- rep(plogis(0),M) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  next_df$Visits2 <- ifelse(next_df$Site %in% Visits,1,0)
  
  #assume % visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * next_df$urbanCover) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  next_df$Visits3 <- ifelse(next_df$Site %in% Visits,1,0)
  
  #assume % visited - urban cover overrepresentated
  probVisit <- plogis(0 + (3*urbanBias) * next_df$urbanCover) 
  Visits <- sample(x = M,size = nuSamples, prob = probVisit)
  #Visits <- df$Site[rbinom(M,1,probVisit)==1]
  next_df$Visits4 <- ifelse(next_df$Site %in% Visits,1,0)
  
  next_df$urbanCover <- next_df$urbanCover + medianUrbanChange
  next_df$Time <- df$Time + 1
  df <- rbind(df,next_df)
  
}

# fitDynamic <- function(next_df){
#   
#   plyr::ldply(1:4,function(i){
#     glm1 <- glm(z ~ Time, 
#                 data = next_df[next_df[,paste0("Visits",i)]==1,],family=binomial)
#     temp <- data.frame(scenario = i, 
#                change = summary(glm1)$coefficients[2,1],
#                change_se = summary(glm1)$coefficients[2,2],
#                change_p = summary(glm1)$coefficients[2,4])
#     temp$scenario <- factor(temp$scenario)
#     return(temp)
#   })
#   
#   
# }


fitDynamicS <- function(next_df){
  
  plyr::ldply(1:4,function(i){
    glm1 <- glm(z ~ Time, 
                  data = next_df[next_df[,paste0("Visits",i)]==1,],family=binomial)
    
    pred1 <- predict(glm1,newdata=data.frame(Time=1),type="response")
    pred2 <- predict(glm1,newdata=data.frame(Time=2),type="response")
    
    temp <- data.frame(scenario = i, 
                       pred1 = pred1,
                       pred2 = pred2,
                       change = pred2/pred1,
                       change_coef = summary(glm1)$coefficients[2,1],
                       change_se = summary(glm1)$coefficients[2,2],
                       change_p = summary(glm1)$coefficients[2,4])
    temp$scenario <- factor(temp$scenario)
    return(temp)
  })
  
  
}


fitDynamic <- function(next_df){
  
  library(lme4)
  library(lmerTest)
  plyr::ldply(1:4,function(i){
    glm1 <- glmer(z ~ Time + (1|Site), 
                data = next_df[next_df[,paste0("Visits",i)]==1,],family=binomial)
    
    pred1 <- predict(glm1,newdata=data.frame(Time=1),re.form=NA,type="response")
    pred2 <- predict(glm1,newdata=data.frame(Time=2),re.form=NA,type="response")
    
    temp <- data.frame(scenario = i,
                       pred1 = pred1,
                       pred2 = pred2,
                       change = pred2/pred1,
                       change_coef = summary(glm1)$coefficients[2,1],
                       change_se = summary(glm1)$coefficients[2,2],
                       change_p = summary(glm1)$coefficients[2,4])
    temp$scenario <- factor(temp$scenario)
    return(temp)
  })
  
  
}

getTimePoints <- function(next_df){
  
  temp1 <- fitStatic(subset(next_df,Time==1))
  temp1$Time <- 1
  temp2 <- fitStatic(subset(next_df,Time==2))
  temp2$Time <- 2
  
  temp <- rbind(temp1,temp2)
  temp$Time <- as.factor(temp$Time)
  temp$scenario <- as.factor(temp$scenario)
  
  #relabel the scenarios
  
  return(temp)
  
}

plotTimePoints <- function(temp){
  
  ggplot(temp) +
    geom_pointrange(aes(x=scenario,y=estimate,ymin=estimate-se,ymax=estimate+se,
                        colour=Time),
                    position = position_dodge(width = 0.5))
}



getWeights <- function(df){
  
  require(ipw)
  
  #scenario 1
  df$Weights1 <- 1
  
  #scenario 2 
  temp <- ipwpoint(exposure = Visits2, 
                   family = "binomial", link = "logit",
                   numerator = ~ 1, denominator = ~ urbanCover, data = df)
  df$Weights2 <- temp$ipw.weights
  
  #scenario 3
  temp <- ipwpoint(exposure = Visits3, 
                   family = "binomial", link = "logit",
                   numerator = ~ 1, denominator = ~ urbanCover, data = df)
  df$Weights3 <- temp$ipw.weights
  
  #scenario 4
  temp <- ipwpoint(exposure = Visits4, 
                   family = "binomial", link = "logit",
                   numerator = ~ 1, denominator = ~ urbanCover, data = df)
  df$Weights4 <- temp$ipw.weights
  
  return(df)
  
}

getGLMWeights <- function(df){
  
  require(ipw)
  
  #scenario 1
  df$Weights1 <- 1
  
  #scenario 2 
  temp <- glm(Visits2 ~ urbanCover, family="binomial", data=df)
  df$Weights2 <- 1/(predict(temp,type="response"))
  
  #scenario 3 
  temp <- glm(Visits3 ~ urbanCover, family="binomial", data=df)
  df$Weights3 <- 1/(predict(temp,type="response"))
  
  #scenario 4 
  temp <- glm(Visits4 ~ urbanCover, family="binomial", data=df)
  df$Weights4 <- 1/(predict(temp,type="response"))
  
  return(df)
  
}


fitStaticWeights <- function(df){
  
  df <- getWeights(df)
  
  require(survey)
  
  plyr::ldply(1:4,function(i){
    sglm1 <- svyglm(z ~ 1,
                   family=binomial,
                   design = svydesign(~ 1, 
                                      weights = ~ df[df[,paste0("Visits",i)]==1,paste0("Weights",i)],
                                      data = df[df[,paste0("Visits",i)]==1,]))
    
    #preds <- as.data.frame(predict(glm1,newdata=df,type="response",se=T))
    require(boot)
    data.frame(scenario = i, 
               estimate = inv.logit(summary(sglm1)$coefficients[1,1]),
               se = summary(sglm1)$coefficients[1,2])
  })
  
}

fitGLMStaticWeights <- function(df){
  
  df <- getGLMWeights(df)
  
  require(survey)
  
  plyr::ldply(1:4,function(i){
    glm1 <- glm(z ~ 1,
                   family=binomial,
                   weights = df[df[,paste0("Visits",i)]==1,paste0("Weights",i)],
                    data = df[df[,paste0("Visits",i)]==1,])
    
    preds <- as.data.frame(predict(glm1,newdata=df,type="response",se=T))
    
    data.frame(scenario = i, 
               estimate = sum(preds$fit),
               se = sum(preds$se.fit))
  })
  
}


getTimePoints_RW <- function(next_df){
  
  
  temp1 <- fitStaticWeights(df = subset(next_df,Time==1))
  temp1$Time <- 1
  temp2 <- fitStaticWeights(df = subset(next_df,Time==2))
  temp2$Time <- 2
  
  temp <- rbind(temp1,temp2)
  temp$Time <- as.factor(temp$Time)
  temp$scenario <- as.factor(temp$scenario)
  
  #relabel the scenarios
  
  return(temp)
  
}

fitDynamicWeights <- function(next_df){
  
  #get weights for each time point
  temp1 <- getWeights(subset(next_df,Time==1))
  temp2 <- getWeights(subset(next_df,Time==2))
  next_df <- rbind(temp1,temp2)
  
  library(lme4)
  library(lmerTest)
  plyr::ldply(1:4,function(i){
    lmer1 <- glmer(z ~ Time + (1|Site), 
                  family=binomial,
                  weights = next_df[df[,paste0("Visits",i)]==1,paste0("Weights",i)],
                  data = next_df[next_df[,paste0("Visits",i)]==1,])
    
    pred1 <- predict(lmer1,newdata=data.frame(Time=1),re.form=NA,type="response")
    pred2 <- predict(lmer1,newdata=data.frame(Time=2),re.form=NA,type="response")
    
    
    #get robust standard errors
    #require(merDeriv)
    #temp <- bread.glmerMod(lmer1,full=FALSE)
    #sqrt(diag(temp))
    
    temp <- data.frame(scenario = i, 
                       pred1 = pred1,
                       pred2 = pred2,
                       change = pred2/pred1,
                       change_coef = summary(lmer1)$coefficients[2,1],
                       change_se = summary(lmer1)$coefficients[2,2],
                       change_p = summary(lmer1)$coefficients[2,4])
    
    temp$scenario <- factor(temp$scenario)
    return(temp)
  })
  
  
}


fitDynamicWeightsS <- function(next_df){
  
  #get weights for each time point
  temp1 <- getWeights(subset(next_df,Time==1))
  temp2 <- getWeights(subset(next_df,Time==2))
  next_df <- rbind(temp1,temp2)
  #next_df <- getWeights(next_df)
  
  require(survey)
  
  plyr::ldply(1:4,function(i){
    sglm1 <- svyglm(z ~ Time,
                    family=binomial,
                    design = svydesign(~ 1, 
                                       weights = ~ next_df[next_df[,paste0("Visits",i)]==1,paste0("Weights",i)],
                                       data = next_df[next_df[,paste0("Visits",i)]==1,]))
    
    pred1 <- predict(sglm1,newdata=data.frame(Time=1),type="response")
    pred2 <- predict(sglm1,newdata=data.frame(Time=2),type="response")
    
    require(boot)
    data.frame(scenario = i, 
               pred1 = pred1[1],
               pred2 = pred2[1],
               change = pred2[1]/pred1[1],
               change_coef = summary(sglm1)$coefficients[2,1],
               change_se = summary(sglm1)$coefficients[2,2],
               change_p = summary(sglm1)$coefficients[2,2])
  })
  
}


getRepeatSurveys <- function(df,meanP=0.5,nuReps=5,constantDetection=TRUE){
  
  # Choose parameter values for measurement error model and compute detectability
  require(boot)
  
  alpha0 <- logit(meanP)                       
  alpha1 <- 2                         
  
  if(constantDetection==TRUE){
    p <- plogis(alpha0)                   
  }else{
    p <- plogis(alpha0 + alpha1 * df$urbanCover) #detection is higher in urban areas
  }
  
  # Take J = nuReps presence/absence measurements at each site
  df$p <- p
  df$y <- rbinom(nrow(df),nuReps,p)*df$z

  
  #return section of the data frame that we will need
  df[,c("Site","urbanCover","psi","z","y","p","Visits1","Visits2","Visits3","Visits4","Time")]
  
}


fitStaticOccuModel <- function(df,model="simulation_bias_intercepts.txt"){
  
#defined in previous function - check it doesn't change 
#this is hard coded into the model script
nuReps = 5  

myfun <- function(i = 1){

#set y to NAs when visited = 0
df$y_Visits <- df$y
df$y_Visits[df[,paste0("Visits",i)]==0] <- NA

#format for jags
require(rjags)
require(jagsUI)
jags.data = list(n.site=nrow(df),
                 J = nuReps,
                 y = df$y_Visits,
                 covariate = df$urbanCover)


# Initial values
zst <- ifelse(jags.data$y>0,1,0)  
zst[is.na(zst)] <- 0
inits <- function(){list(z = zst)}

# Parameters monitored
params <- c("p.int", "psi.int","urbanEffect","psi.fs") 

# MCMC settings
ni <- 500   ;   nt <- 2   ;   nb <- 200   ;   nc <- 3

# fit model
out1 <- jags(jags.data, inits, params, 
             paste("models",model,sep="/"), 
             n.chains = nc,n.thin = nt, 
             n.iter = ni, n.burnin = nb,
             parallel = T) 

out1$summary

#export like this
data.frame(scenario = i, 
           estimate = out1$mean$psi.fs,
           se = out1$sd$psi.fs)
}

require(plyr)
ldply(1:4,function(i)myfun(i))
  
}


fitStaticOccuModelWeights <- function(df,model="simulation_bias_siteSelection.txt"){
  
  #defined in previous function - check it doesn't change 
  #this is hard coded into the model script
  nuReps = 5  
  
  myfun <- function(i = 1){
    
    #set y to NAs when visited = 0
    df$y_Visits <- df$y
    df$y_Visits[df[,paste0("Visits",i)]==0] <- NA
    
    #format for jags
    require(rjags)
    require(jagsUI)
    jags.data = list(n.site=nrow(df),
                     J = nuReps,
                     Visited = ifelse(!is.na(df$y),1,0),
                     y = df$y_Visits,
                     covariate = df$urbanCover)
    
    
    # Initial values
    zst <- ifelse(jags.data$y>0,1,0)  
    zst[is.na(zst)] <- 0
    inits <- function(){list(z = zst)}
    
    # Parameters monitored
    params <- c("p.int", "psi.int","urbanEffect","psi.fs") 
    
    # MCMC settings
    ni <- 500   ;   nt <- 2   ;   nb <- 200   ;   nc <- 3
    
    # fit model
    out1 <- jags(jags.data, inits, params, 
                 paste("models",model,sep="/"), 
                 n.chains = nc,n.thin = nt, 
                 n.iter = ni, n.burnin = nb,
                 parallel = T) 
    
    out1$summary
    
    #export like this
    data.frame(scenario = i, 
               estimate = out1$mean$psi.fs,
               se = out1$sd$psi.fs)
  }
  
  require(plyr)
  ldply(1:4,function(i)myfun(i))
  
}


getOccuTimePoints <- function(next_df,model="simulation_bias_intercepts.txt"){
  
  temp1 <- fitStaticOccuModel(subset(next_df,Time==1),model=model)
  temp1$Time <- 1
  temp2 <- fitStaticOccuModel(subset(next_df,Time==2),model=model)
  temp2$Time <- 2
  
  temp <- rbind(temp1,temp2)
  temp$Time <- as.factor(temp$Time)
  temp$scenario <- as.factor(temp$scenario)
  
  return(temp)
  
}


fitOccuDynamic <- function(next_df,model="simulation_bias_change_intercepts.txt"){
  
  nuReps = 5  
  
  myfun <- function(i = 1){
    
    #set y to NAs when visited = 0
    next_df$y_Visits <- next_df$y
    next_df$y_Visits[next_df[,paste0("Visits",i)]==0] <- NA
    
    #format for jags
    require(rjags)
    require(jagsUI)
    jags.data = list(n.site=length(unique(next_df$Site)),
                     J = nuReps,
                     y = reshape2::acast(next_df,Site~Time,value.var="y"),
                     time = sort(unique(next_df$Time)),
                     n.time = max(next_df$Time),
                     covariate = reshape2::acast(next_df,Site~Time,value.var="urbanCover"))
    
    
    # Initial values
    zst <- reshape2::acast(next_df,Site~Time,value.var="y") 
    zst[is.na(zst)] <- 0
    zst[zst>0] <- 1
    inits <- function(){list(z = zst)}
    
    # Parameters monitored
    params <- c("p.int", "psi.int","urbanEffect","beta.change","psi.change","psi.fs") 
    
    # MCMC settings
    ni <- 500   ;   nt <- 2   ;   nb <- 200   ;   nc <- 3
    
    # fit model
    out1 <- jags(jags.data, inits, params, 
                 paste("models",model,sep="/"), 
                 n.chains = nc,n.thin = nt, 
                 n.iter = ni, n.burnin = nb,
                 parallel = T) 
    
    #export like this
    temp <- data.frame(scenario = i,
                       pred1 = out1$mean$psi.fs[1],
                       pred2 = out1$mean$psi.fs[2],
                       change = out1$mean$psi.change,
                       change_coef = out1$mean$beta.change,
                       change_se = out1$sd$beta.change)
    temp$scenario <- factor(temp$scenario)
    return(temp)
  }
  
  require(plyr)
  ldply(1:4,function(i)myfun(i))
  
  
}




plotVisits <- function(df){
  require(cowplot)
  plotList <- lapply(1:4,function(i){
    ggplot(df)+
      geom_line(aes(x=urbanCover,y=df[,paste0("VisitPreds",i)]))+ ylab(paste0("VisitPreds",i))+ylim(0,1)
  })
  gridExtra::grid.arrange(grobs = plotList)
}



plotEffects <- function(output){
  
  ggplot(output)+
    geom_violin(aes(x=factor(scenario),y=estimate))+
    xlab("scenario")+theme_few()
  
}

plotChange <- function(next_df){
  
  next_df_time1 <- subset(next_df,Time==1)
  next_df_time2 <- subset(next_df,Time==2)
  names(next_df_time1) <- paste0(names(next_df_time1),"_Time1")
  names(next_df_time2) <- paste0(names(next_df_time2),"_Time2")
  allTimes <- cbind(next_df_time1,next_df_time2)
  allTimes$urbanChange <- allTimes$urbanCover_Time2 - allTimes$urbanCover_Time1
  
  #plot for each scenario
  allTimes$repeatSurvey <- ifelse(allTimes$Visits2_Time1==1 & allTimes$Visits2_Time2 == 1,1,0)
  g1 <- ggplot(allTimes,aes(x=urbanChange,y=repeatSurvey))+ylim(0,1)+
    geom_smooth(method = "glm", method.args = list(family = "binomial"))+ggtitle("scenario 2")
  
  allTimes$repeatSurvey <- ifelse(allTimes$Visits3_Time1==1 & allTimes$Visits3_Time2 == 1,1,0)
  g2 <- ggplot(allTimes,aes(x=urbanChange,y=repeatSurvey))+ylim(0,1)+
    geom_smooth(method = "glm", method.args = list(family = "binomial"))+ggtitle("scenario 3")
  
  allTimes$repeatSurvey <- ifelse(allTimes$Visits4_Time1==1 & allTimes$Visits4_Time2 == 1,1,0)
  g3 <- ggplot(allTimes,aes(x=urbanChange,y=repeatSurvey))+ylim(0,1)+
    geom_smooth(method = "glm", method.args = list(family = "binomial"))+ggtitle("scenario 4")
  
  cowplot::plot_grid(g1,g2,g3,nrow=1)
}



getObsProp <- function(next_df){
  data.frame(Visit1_Time1_prop = mean(next_df$z[next_df$Visits1==1 & next_df$Time==1]),
             Visit2_Time1_prop = mean(next_df$z[next_df$Visits2==1 & next_df$Time==1]),
             Visit3_Time1_prop = mean(next_df$z[next_df$Visits3==1 & next_df$Time==1]),
             Visit4_Time1_prop = mean(next_df$z[next_df$Visits4==1 & next_df$Time==1]),
             Visit1_Time2_prop = mean(next_df$z[next_df$Visits1==1 & next_df$Time==2]),
             Visit2_Time2_prop = mean(next_df$z[next_df$Visits2==1 & next_df$Time==2]),
             Visit3_Time2_prop = mean(next_df$z[next_df$Visits3==1 & next_df$Time==2]),
             Visit4_Time2_prop = mean(next_df$z[next_df$Visits4==1 & next_df$Time==2]))
}

getObsNu <- function(next_df){
  data.frame(Visit1_Time1_nu = length(next_df$z[next_df$Visits1==1 & next_df$Time==1]),
             Visit2_Time1_nu = length(next_df$z[next_df$Visits2==1 & next_df$Time==1]),
             Visit3_Time1_nu = length(next_df$z[next_df$Visits3==1 & next_df$Time==1]),
             Visit4_Time1_nu = length(next_df$z[next_df$Visits4==1 & next_df$Time==1]),
             Visit1_Time2_nu = length(next_df$z[next_df$Visits1==1 & next_df$Time==2]),
             Visit2_Time2_nu = length(next_df$z[next_df$Visits2==1 & next_df$Time==2]),
             Visit3_Time2_nu = length(next_df$z[next_df$Visits3==1 & next_df$Time==2]),
             Visit4_Time2_nu = length(next_df$z[next_df$Visits4==1 & next_df$Time==2]))
}


plotObsProp <- function(outputNC,mytitle){
  
  outputNC_melt <- reshape2::melt(outputNC)
  outputNC_melt$scenario <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][1]})
  outputNC_melt$time <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][2]})
  outputNC_melt$time <- ifelse(outputNC_melt$time=="Time1","1","2")
  
  ggplot(outputNC_melt,aes(x=scenario,y=value,fill=time))+
    geom_violin(position="dodge",draw_quantiles=c(0.25,0.5,0.75),
                alpha=0.5)+
    theme_few()+
    scale_fill_manual("Time point", values = c("lightblue","blue"))+
    scale_x_discrete("Sampling scenario", labels = c("Visit1" = "Full","Visit2" = "Random",
                                       "Visit3" = "Bias","Visit4" = "Bias+"))+
    ylab("Occupancy proportion")+
    theme(legend.position = c(0.85,0.85),legend.key.size=unit(0.5,"line"),
          legend.title = (element_text(size=10)))+
    labs(subtitle = mytitle)
    
}


plotObsProp_P <- function(outputNC,mytitle){
  
  outputNC_melt <- reshape2::melt(outputNC)
  outputNC_melt$scenario <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][1]})
  outputNC_melt$time <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][2]})
  outputNC_melt$time <- ifelse(outputNC_melt$time=="Time1","1","2")
  
  ggplot(outputNC_melt,aes(x=scenario,y=value,fill=time))+
    geom_violin(position="dodge",draw_quantiles=c(0.25,0.5,0.75),
                alpha=0.5)+
    theme_few()+
    scale_fill_manual("Time point", values = c("lightblue","blue"))+
    scale_x_discrete("Sampling scenario", labels = c("Visit1" = "Full","Visit2" = "Random",
                                                     "Visit3" = "Bias","Visit4" = "Bias+"))+
    ylab("Occupancy proportion")+
    theme(legend.position = c(0.2,0.85),legend.key.size=unit(0.5,"line"),
          legend.title = (element_text(size=10)))+
    labs(subtitle = mytitle)
  
}

plotObsNu <- function(outputNC,mytitle){
  
  outputNC_melt <- reshape2::melt(outputNC)
  outputNC_melt$scenario <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][1]})
  outputNC_melt$time <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][2]})
  outputNC_melt$time <- ifelse(outputNC_melt$time=="Time1","1","2")
  
  ggplot(outputNC_melt,aes(x=scenario,y=value,fill=time))+
    geom_violin(position="dodge",draw_quantiles=c(0.25,0.5,0.75),
                alpha=0.5)+
    theme_few()+
    scale_fill_manual("Time point", values = c("lightblue","blue"))+
    scale_x_discrete("Sampling scenario", labels = c("Visit1" = "Full","Visit2" = "Random",
                                                     "Visit3" = "Bias","Visit4" = "Bias+"))+
    ylab("Nu sites")+
    theme(legend.position = c(0.85,0.85),legend.key.size=unit(0.5,"line"),
          legend.title = (element_text(size=10)))+
    labs(subtitle = mytitle)
  
}


plotObsChange <- function(outputNC){
  
  outputNC$simNu <- 1:nrow(outputNC)
  outputNC_melt <- reshape2::melt(outputNC,id="simNu")
  outputNC_melt$scenario <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][1]})
  outputNC_melt$time <- sapply(as.character(outputNC_melt$variable),function(x){
    strsplit(x,"_")[[1]][2]})
  outputNC_cast <- reshape2::dcast(outputNC_melt,simNu+scenario~time,value.var="value")
  
  outputNC_cast$Difference <- outputNC_cast$Time2 -  outputNC_cast$Time1
  outputNC_cast$Difference2 <- boot::logit(outputNC_cast$Time2) - boot::logit(outputNC_cast$Time1)
  #outputNC_cast$Difference2 <- log(outputNC_cast$Time2/outputNC_cast$Time1)
  
  ggplot(outputNC_cast,aes(x=scenario,y=Difference2))+
    geom_violin(position="dodge",draw_quantiles=c(0.25,0.5,0.75),
                alpha=0.5)+
    theme_few()+
    scale_x_discrete("Sampling scenario", labels = c("Visit1" = "Full","Visit2" = "Random",
                                                     "Visit3" = "Bias","Visit4" = "Bias+"))+
    ylab("Occupancy change")+
    geom_hline(yintercept=0,linetype="dashed")
  
}




#function to get differences of Visit3 and Visit 4 from Visit1 at each time step
getDifferences <- function(outputNC){
  outputNC$Visit_4diff_Time1 <- log(outputNC$Visit4_Time1_prop/outputNC$Visit1_Time1_prop)
  outputNC$Visit_3diff_Time1 <- log(outputNC$Visit3_Time1_prop/outputNC$Visit1_Time1_prop)
  outputNC$Visit_4diff_Time2 <- log(outputNC$Visit4_Time2_prop/outputNC$Visit1_Time2_prop)
  outputNC$Visit_3diff_Time2 <- log(outputNC$Visit3_Time2_prop/outputNC$Visit1_Time2_prop)
  return(outputNC)
}

plotBiasT2 <- function(outputNC,mytitle){
  
  outputNC <- getDifferences(outputNC)[,9:13]
  
  outputNC_melt <- reshape::melt(outputNC,id="beta")
  outputNC_melt$Time <- as.factor(sapply(as.character(outputNC_melt$variable),
                               function(x)strsplit(x,"_")[[1]][3]))
  levels(outputNC_melt$Time) <- c("1","2")
  
  outputNC_melt$Sampling <- as.factor(sapply(as.character(outputNC_melt$variable),
                               function(x)strsplit(x,"_")[[1]][2]))
  levels(outputNC_melt$Sampling) <- c("Bias","Bias+")
  
  outputNC_melt_median <- plyr::ddply(outputNC_melt,.(beta,Time,Sampling),
                                      summarise,
                                      value=median(value))
  
  ggplot(subset(outputNC_melt_median,Time==2)) +
    geom_smooth(aes(x=beta,y=value,colour=Sampling),
                se = FALSE,position=position_dodge(width=0.2))+
    theme_few()+
    scale_color_brewer(type="qual")+
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    xlab("Species association with urban cover")+
    ylab("Bias of occupancy estimate")+
    theme(legend.position = c(0.8,0.25),legend.key.size=unit(0.7,"line"),
          legend.title = (element_text(size=12)))+
    labs(subtitle = mytitle)
  
}




getChange <- function(outputUC){
  require(boot)
  #change in each
  outputUC$change1 <- logit(outputUC$Visit1_Time2) - logit(outputUC$Visit1_Time1_prop)
  outputUC$change3 <- logit(outputUC$Visit3_Time2) - logit(outputUC$Visit3_Time1_prop)
  outputUC$change4 <- logit(outputUC$Visit4_Time2) - logit(outputUC$Visit4_Time1_prop)
  
  #growth rate bias
  outputUC$change4_bias <- outputUC$change4 - outputUC$change1
  outputUC$change3_bias <- outputUC$change3 - outputUC$change1  
  
  return(outputUC)
}

plotBias <- function(outputNC,mytitle){
  
  outputNC <- getChange(outputNC)[,c(9,13,14)]
  
  outputNC_melt <- reshape::melt(outputNC,id="beta")

  outputNC_melt$Sampling <- as.factor(sapply(as.character(outputNC_melt$variable),
                                             function(x)strsplit(x,"_")[[1]][1]))
  
  levels(outputNC_melt$Sampling) <- c("Bias","Bias+")
  
  outputNC_melt_median <- plyr::ddply(outputNC_melt,.(beta,Sampling),
                                      summarise,
                                      value=median(value))
  
  ggplot(outputNC_melt_median) +
    geom_smooth(aes(x=beta,y=value,colour=Sampling),
                se = FALSE,position=position_dodge(width=0.2),size=2)+
    theme_few()+
    scale_color_brewer(type="qual")+
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0)+
    xlab("Species association with urban cover")+
    ylab("Bias of occupancy change estimate")+
    theme(legend.position = c(0.8,0.25),legend.key.size=unit(0.7,"line"),
          legend.title = (element_text(size=12)))+
    labs(subtitle = mytitle)
  
}

plotTrends <- function(outputNC,mytitle="No urban change"){
  
  ggplot(outputNC)+
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
    labs(subtitle = mytitle)
}


plotPower <- function(powerNC,mytitle = "No urban change"){
  ggplot(powerNC)+
    geom_bar(aes(x=scenario,y=nuSigs),stat="identity",fill="grey")+
    theme_few()+
    xlab("sampling scenario")+
    ylab("Type 1 error rate")+
    labs(subtitle = mytitle)+
    theme(plot.subtitle = element_text(vjust=2,hjust=0.02))+
    scale_x_discrete("Sampling scenario", labels = c("1" = "Full","2" = "Random",
                                                     "3" = "Bias","4" = "Bias+"))
}

plotUrbanDistr <- function(sampleBiasUC, mytitle="Uniform urban change"){
  ggplot(sampleBiasUC)+
    geom_density(aes(urban_diff,fill=scenario),alpha=0.2)+
    theme_few()+
    labs(subtitle = mytitle)+
    scale_fill_discrete("Scenario")+
    xlab("Sampled urban change") + ylab("Density")+
    theme(plot.subtitle = element_text(vjust=2,hjust=0.02),
          legend.position=c(0.845,0.85),legend.key.size=unit(0.35,"line"),
          legend.title = (element_text(size=8)),legend.text = (element_text(size=8)))
}

plotUrbanMean <- function(sampleBiasNC, mytitle = "No urban change"){
  ggplot(sampleBiasNC)+
    geom_violin(aes(x=scenario,y=urban_median,fill=factor(time)),alpha=0.2,
                draw_quantiles = c(0.25,0.5,0.75))+
    theme_few()+
    scale_fill_manual("Time point", values = c("lightblue","blue"))+
    labs(subtitle = mytitle)+
    xlab("Sampling scenario") + ylab ("Sampled urban cover")+
    theme(legend.position = c(0.23,0.83),legend.key.size=unit(0.7,"line"),
          legend.title = (element_text(size=12)))
  
}
