generateData <- function(M=500,nuSamples=50, beta1=-3,urbanBias=2){
  
  # M is the number of sites
  #beta1 is the regression coefficient of urban cover
  
  #State process - occupancy is affected by urban cover
  urbanCover <- seq(-1, 1,length.out=M) 
  beta0 <- 0  # Logit-scale intercept vegHt
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
  
  #assume 50% visited - 50% systematic
  probVisit <- plogis(0) 
  Visits <- rbinom(M, 1, probVisit) 
  df$Visits2 <- 0
  df$Visits2[sample(which(Visits==1),nuSamples)] <- 1
  
  #assume 50% visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * urbanCover) 
  Visits <- rbinom(M, 1, probVisit) 
  df$Visits3 <- 0
  df$Visits3[sample(which(Visits==1),nuSamples)] <- 1
  
  
  #assume 50 visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * urbanCover) 
  Visits <- rbinom(M, 1, probVisit) 
  df$Visits4 <- 0
  df$Visits4[sample(which(Visits==1),nuSamples)] <- 1
  
  #model visitation probability
  for(i in 1:4){
    myFormula <- paste0("Visits",i," ~ urbanCover")
    glm1 <- glm(as.formula(myFormula), data = df,family=binomial)
    df[,paste0("VisitPreds",i)] <- predict(glm1,type="response")
  }
  
  df$Time <- 1
  
  return(df)
  
}

fitStatic <- function(df){
  
  plyr::ldply(1:4,function(i){
    glm1 <- glm(z ~ 1, data = df[df[,paste0("Visits",i)]==1,],family=binomial)
    data.frame(scenario = i, 
               estimate = sum(predict(glm1,newdata=df,type="response")),
               se = sum(predict(glm1,newdata=df,type="response",se=T)$se.fit),
               meanUrban = mean(df$urbanCover[df[,paste0("Visits",i)]==1]))
  })
}

plotVisits <- function(df){
  require(cowplot)
  plotList <- lapply(1:4,function(i){
    ggplot(df)+
      geom_line(aes(x=urbanCover,y=df[,paste0("VisitPreds",i)]))+ ylab(paste0("VisitPreds",i))+ylim(0,1)
  })
  gridExtra::grid.arrange(grobs = plotList)
}

extendData <- function(df,beta1=-3,urbanBias=2,change="no_change"){
  
  M = length(unique(df$Site))
  nuSamples = sum(df$Visits2==1)
  
  #extract data for next time step
  next_df <- df[,c("Site","urbanCover")]
  
  #is there a change in urban cover
  if(change=="no_change"){
    next_df$urbanCover <- next_df$urbanCover
  }else if(change=="uniform_change"){
    next_df$urbanCover <- next_df$urbanCover  + 0.2
  }else if(change=="clustered_change"){
    #more urbanization of intermediate urban sites
    next_df$urbanCover <- sapply(next_df$urbanCover,function(x){
      ifelse(x>mean(next_df$urbanCover),
             x+0.4,x)})
  }
  

  #generate new observations
  beta0 = 0
  next_df$psi <- plogis(beta0 + beta1 * next_df$urbanCover) 
  next_df$z <- rbinom(M, 1, next_df$psi)
  
  #revisit each site
  ### visitation model (1 = a site is visited)
  
  #assume all sites are visited
  next_df$Visits1 <- rep(1,M)
  
  #assume 50% visited - 50% systematic
  probVisit <- plogis(0)
  Visits <- rbinom(M, 1, probVisit) 
  next_df$Visits2 <- 0
  next_df$Visits2[sample(which(Visits==1),nuSamples)] <- 1
  
  #assume 50% visited - urban cover overrepresentated
  probVisit <- plogis(0 + (urbanBias) * next_df$urbanCover) 
  Visits <- rbinom(M, 1, probVisit) 
  next_df$Visits3 <- 0
  next_df$Visits3[sample(which(Visits==1),nuSamples)] <- 1
  
  #assume 50 visited - urban cover bias increases
  probVisit <- plogis(0 + (4 * urbanBias) * next_df$urbanCover) 
  Visits <- rbinom(M, 1, probVisit) 
  next_df$Visits4 <- 0
  next_df$Visits4[sample(which(Visits==1),nuSamples)] <- 1
  
  #model visitation probability
  for(i in 1:4){
    myFormula <- paste0("Visits",i," ~ urbanCover")
    glm1 <- glm(as.formula(myFormula), data = next_df,family=binomial)
    next_df[,paste0("VisitPreds",i)] <- predict(glm1,type="response")
  }
  
  next_df$Time <- df$Time + 1
  df <- rbind(df,next_df)
  
}

fitDynamic <- function(next_df){
  
  plyr::ldply(1:4,function(i){
    glm1 <- glm(z ~ Time, 
                data = next_df[next_df[,paste0("Visits",i)]==1,],family=binomial)
    temp <- data.frame(scenario = i, 
               change = summary(glm1)$coefficients[2,1],
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

fitStaticWeights <- function(df){
  
  df <- getWeights(df)
  
  require(survey)
  
  plyr::ldply(1:4,function(i){
    glm1 <- svyglm(z ~ 1,
                   family=binomial,
                   design = svydesign(~ 1, 
                                      weights = ~ df[df[,paste0("Visits",i)]==1,paste0("Weights",i)],
                                      data = df[df[,paste0("Visits",i)]==1,]))
    
    preds <- as.data.frame(predict(glm1,newdata=df,type="response",se=T))
    
    data.frame(scenario = i, 
               estimate = sum(preds$response),
               se = sum(preds$SE))
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
