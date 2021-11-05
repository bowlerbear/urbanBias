plotTS <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity_Annual <- samplingIntensity %>%
    as_tibble() %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(totalVisits = sum(Visited))
  
  ggplot(samplingIntensity_Annual, aes(x=Year, y=totalVisits))+
    geom_point()+
    geom_line()+
    theme_few()+
    ylab("Number of visits")+
    theme(axis.title = element_text(size = 11),
          axis.text = element_text(size=8))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))+
    scale_y_log10()
  
}


plotUrbanBias <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()

  #put numbers in 2 -year periods
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$Year2 <- samplingIntensity$Year
  samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
  samplingIntensity_Group <- samplingIntensity %>%
    dplyr::group_by(MTB_Q,Year2) %>%
    dplyr::summarise(Visited = max(Visited),
                     urban = median(urban))
  
  #left plot
  buttL <- ggplot(samplingIntensity_Group,
                  aes(x=urban*100,y=Visited,group=Year2))+
    stat_smooth(aes(color=Year2),
                method = "glm", 
                method.args = list(family = "binomial"),
                size=1.5,
                se=FALSE)+
    theme_few()+
    ylim(0,1)+
    theme(legend.position="none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_color_viridis_c("Year")+
    xlab("Urban cover (%)")+ylab("Visit probability")
  
  #right plot
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using glm
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    if(sum(tempData$Visited)>10){
    glm1 <- glm(Visited ~ urban, family= binomial, data = tempData)
    glmConfint <- confint(glm1)
    data.frame(Year = y, 
               estimate=summary(glm1)$coefficients[2,1],
               lower=glmConfint[2,1],
               upper=glmConfint[2,2])}
    
    else{
      data.frame(Year = y, 
                 estimate=NA,
                 lower=NA,
                 upper=NA)
    }
    
  })
  
  
  buttR <- ggplot(modelCoefs)+
    geom_crossbar(aes(x = Year, y = estimate, 
                      ymax = upper, ymin = lower, fill=Year))+
    geom_hline(yintercept = 0, colour="red", linetype="dashed")+
    theme_few()+ylab("Effect of urban cover")+
    scale_fill_viridis_c("Year")+
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
  
  
  cowplot::plot_grid(buttL,buttR,nrow=1)
  
  
}


plotUrbanCover1 <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  #plot median and upper and lower quantiles of urban cover in visited and unvisited sites each year
  
  urbanSummary <- samplingIntensity %>%
    as_tibble() %>%
    dplyr::group_by(Visited, Year) %>%
    dplyr::summarise(medianUrban = quantile(urban,0.5),
                     lowerUrban = quantile(urban,0.25),
                     upperUrban = quantile(urban,0.75))
  
  
  urbanSummary$Visited <- ifelse(urbanSummary$Visited==1,"Sampled","Unsampled")
  
  #left plot
  ggplot(urbanSummary)+
    geom_line(aes(x=Year, y = medianUrban, group=Visited, colour=Visited),size=1.5)+
    #geom_ribbon(aes(x=Year, ymin = lowerUrban, ymax=upperUrban,
    #                group=Visited),alpha=0.5)+
    theme_few()+
    theme(legend.position = "none",#purple is sampled, yellow is unsampled
          axis.title = element_text(size = 15),
          axis.text = element_text(size=12))+
    scale_color_viridis_d("Year")+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))+
    xlab("Year")+ylab("Mean urban cover")

}


plotUrbanCover2 <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity$Visited <- ifelse(samplingIntensity$Visited==1,"Sampled","Unsampled")
  
  urbanSummary <- samplingIntensity %>%
    as_tibble() 
    #group_by(Year) %>%
    #pivot_wider(.,names_from = Visited, values_from = urban)

  #left plot
  ggplot(urbanSummary)+
    geom_boxplot(aes(x=factor(Year), y = urban, colour=Visited),
                 size=1.5, outlier.color = NA)+
    theme_few()+
    theme(legend.position = "none",#purple is sampled, yellow is unsampled
          axis.title = element_text(size = 15),
          axis.text = element_text(size=12))+
    ylim(0,0.3)+
    scale_color_viridis_d()+
    scale_x_discrete(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))+
    xlab("Year")+ylab("Urban cover")
  
}

plotUrbanCover3 <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  #plot median and upper and lower quantiles of urban cover in visited and unvisited sites each year
  
  urbanSummary <- samplingIntensity %>%
    as_tibble() %>%
    dplyr::group_by(Visited, Year) %>%
    dplyr::summarise(medianUrban = quantile(urban,0.5),
                     lowerUrban = quantile(urban,0.25),
                     upperUrban = quantile(urban,0.75)) %>%
    dplyr::mutate(Visited = ifelse(Visited==1,"Sampled","Unsampled")) %>%
    select(Visited,Year,medianUrban) %>%
    group_by(Year) %>%
    pivot_wider(.,names_from=Visited, values_from=medianUrban) %>%
    ungroup() %>%
    dplyr::mutate(urbanDiff = Sampled-Unsampled)
  
  ggplot(urbanSummary,aes(x=Year, y = urbanDiff))+
    geom_point(size=1.5)+
    geom_smooth(method="lm")+
    theme_few()+
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size=12))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))+
    xlab("Year")+ylab("Difference in mean urban cover")+
    geom_hline(yintercept=0,linetype="dashed")
  
}


getAnnualBias <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  #right plot
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using glm
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    if(sum(tempData$Visited)>10){
      glm1 <- glm(Visited ~ urban, family= binomial, data = tempData)
      glmConfint <- confint(glm1)
      data.frame(Year = y, 
                 estimate=summary(glm1)$coefficients[2,1],
                 se = summary(glm1)$coefficients[2,2],
                 lower=glmConfint[2,1],
                 upper=glmConfint[2,2])}
    
    else{
      data.frame(Year = y, 
                 estimate=NA,
                 se=NA,
                 lower=NA,
                 upper=NA)
    }
    
  })
  

    return(modelCoefs)
  
}


getAnnualBiasSpace <- function(myfolder, mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))
  
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using spaMM
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    randomSample <- sample(1:nrow(tempData),5000)
    dat <- tempData[randomSample,]
    #model1 <- glm(Visited ~ urban, family= binomial, data = dat)
    #model_coefs <- broom::tidy(model1)
    
    if(sum(dat$Visited)>5){
      
      #fit glm model too
      glm1 <- glm(Visited ~ urban, family= binomial, data = dat)
      glmConfint <- confint(glm1)
      model1 <- data.frame(Year = y, 
                  model = "glm",
                 estimate=summary(glm1)$coefficients[2,1],
                 se = summary(glm1)$coefficients[2,2],
                 lower=glmConfint[2,1],
                 upper=glmConfint[2,2])
      
      
      #fit spatial model
      library(spaMM)
      # fit the model
      m_spamm1 <- fitme(Visited ~ urban + Matern(1 | x + y),
                        data = dat, family = "binomial", method="PQL")
      # model summary
      out1 <- summary(m_spamm1)
      model_coefs <- out1$beta_table
      
      model2 <- data.frame(Year = y, 
                  model = "spaMM",
                 estimate=model_coefs[2,1],
                 se=model_coefs[2,2],
                 lower=model_coefs[2,1] - 2*model_coefs[2,2],
                 upper=model_coefs[2,1] + 2*model_coefs[2,2])
      
      rbind(model1, model2)
      
      
      }else{
      data.frame(Year = y, 
                 model = "none",
                 estimate=NA,
                 se=NA,
                 lower=NA,
                 upper=NA)
    }
    
  })
  
}


testVisitBias <- function(myfolder,mytaxa,type="urban"){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  #non-spatial model
  if(type=="urban"){
  
  model1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity)
  mod1 <- broom::tidy(model1)
  mod1$model <- "urban"
    
  model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity)
  mod2 <- broom::tidy(model1)
  mod2$model <- "year*urban"
  
  allDF <- bind_rows(mod1, mod2) %>%
            add_column(dataset = myfolder, taxa = mytaxa)

  }else if (type=="PA"){
    
  #add on PA area
  samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))
    
  model1 <- glm(Visited ~ PA_area, family= binomial, data = samplingIntensity)
  mod1 <- broom::tidy(model1)
  mod1$model <- "PA"
  
  model1 <- glm(Visited ~ I(Year-1991) * PA_area, family= binomial, data = samplingIntensity)
  mod2 <- broom::tidy(model1)
  mod2$model <- "year*PA"  
  
  allDF <- bind_rows(mod1, mod2) %>%
    add_column(dataset = myfolder, taxa = mytaxa)
  
  }
  
  return(allDF)
}


testVisitBiasSpace <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))

  randomSample <- sample(1:nrow(samplingIntensity),5000)
  dat <- samplingIntensity[randomSample,]

  #fit spatial model
  library(spaMM)
  # fit the model
  m_spamm1 <- fitme(Visited ~ urban*I(Year-1991) + Matern(1 | x + y),
                        data = dat, family = "binomial", method="PQL")
  # model summary
  out1 <- summary(m_spamm1)
  model_coefs <- out1$beta_table
      
  modelDF <- data.frame(Year = y, 
                           model = "spaMM",
                           estimate=model_coefs[2,1],
                           se=model_coefs[2,2],
                           lower=model_coefs[2,1] - 2*model_coefs[2,2],
                           upper=model_coefs[2,1] + 2*model_coefs[2,2])
      
  return(modelDF)

  
}


readRecorders <- function(myfolder,mytaxa){
  
  readRDS(paste0(myfolder,"/recorders_revision_",myfolder,"_",mytaxa,".rds"))
  
}

readMTBQs <- function(myfolder,mytaxa){
  
  readRDS(paste0(myfolder,"/mtbqs_revision_",myfolder,"_",mytaxa,".rds"))
  
}
  

plotAllNew <- function(myfolder, mytaxa){
  
  annualDF <- getAnnualBias(myfolder, mytaxa)
  names(annualDF)[1] <- "year" 
  
  #get annual urban cover
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  annualUrbanCover <- samplingIntensity %>% 
                        as_tibble() %>%
                        dplyr::group_by(Year) %>%
                        dplyr::summarise(totalUrban = sum(urban)) %>%
                        dplyr::rename(year = Year)
          
  
  #get newly visited sites
  
  newSites <- readMTBQs(myfolder, mytaxa)
  
  # get new recorders 
  
  newRecorders <- readRecorders(myfolder, mytaxa)
  
  # merge all 
  
  all <- inner_join(newSites,newRecorders)
  all <- inner_join(all,annualDF)
  all <- inner_join(all,annualUrbanCover)
  
  p1 <- qplot(firstyearMTBQs/totalMTBQs,estimate,data=all)+
    xlab("prop new sites") + ylab("bias estimate") + theme_few()
  p2 <- qplot(firstyearObs/totalObs,estimate,data=all)+
    xlab("prop new recorders") + ylab("bias estimate") + theme_few()
  p3 <- qplot(totalUrban,estimate,data=all)+
    xlab("annual urban cover") + ylab("bias estimate") + theme_few()
  p4 <- qplot(totalMTBQs,estimate,data=all)+
    xlab("total sites") + ylab("bias estimate") + theme_few()
  
  cowplot::plot_grid(p1,p2,p3,p4,nrow=2)
  
}

testAllNew <- function(myfolder, mytaxa){
  
  annualDF <- getAnnualBias(myfolder, mytaxa)
  names(annualDF)[1] <- "year" 
  
  #get newly visited sites
  
  newSites <- readMTBQs(myfolder, mytaxa)
  
  # get new recorders 
  
  newRecorders <- readRecorders(myfolder, mytaxa)
  
  #get annual urban cover
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  annualUrbanCover <- samplingIntensity %>% 
    as_tibble() %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(totalUrban = sum(urban)) %>%
    dplyr::rename(year = Year)
  
  # merge all 
  
  all <- inner_join(newSites,newRecorders)
  all <- inner_join(all,annualDF)
  all <- all[complete.cases(all),]
  all <- inner_join(all,annualUrbanCover)
  
  all <- all %>%
          filter(year>1999)
  
  p1 <- cor(all$firstyearMTBQs/all$totalMTBQs,all$estimate)
  p2 <- cor(all$firstyearObs/all$totalObs, all$estimate)
  p3 <- cor(all$totalMTBQs,all$estimate)
  p4 <- cor(all$totalObs,all$estimate)
  p5 <- cor(all$totalUrban,all$estimate)
  
  df <- data.frame(type=c("prop new sites", 
                    "prop new recorders", 
                    "total sites",
                    "total recorders",
                    "urban cover"),
             cor = c(p1,p2,p3,p4,p5))
  df$dataset <- myfolder
  df$taxa <- mytaxa
  
  return(df)
  
}



plotNewUrbanCover <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity <- samplingIntensity %>%
    dplyr::arrange(MTB_Q,Year) %>%
    dplyr::group_by(MTB_Q) %>%
    dplyr::mutate(totalprevVisit = cumsum(Visited)) %>%
    dplyr::mutate(firstVisit = min(Year[Visited==1])) %>%
    dplyr::mutate(firstVisit = ifelse(is.infinite(firstVisit),
                                      2020,firstVisit)) %>%
    dplyr::mutate(prevVisit = ifelse(Year<=firstVisit, "new","old")) %>%
    dplyr::mutate(prevVisit = ifelse(firstVisit==2020, "never",prevVisit)) %>%
    dplyr::ungroup()
  
  ggplot(samplingIntensity)+
    geom_boxplot(aes(x=factor(Year),y=urban,fill=prevVisit),
                 outlier.shape = NA, coef =0)+
    theme_few()+
    ylim(0,0.15)
}


getAnnualDifferences <- function(myfolder, mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  #annual bias estimates
  annualDF <- getAnnualBias(myfolder, mytaxa)
  names(annualDF)[1] <- "year" 
  #and the difference
  annualDF$estimateDiff <- as.numeric(resid(lm(estimate~year,data=annualDF,na.action=na.exclude)))
  annualDF$estimateChange <- c(NA,diff(annualDF$estimate))
  
  #get annual urban cover
  annualUrbanCover <- samplingIntensity %>% 
    dplyr::group_by(MTB_Q) %>%
    dplyr::mutate(prevVisited = lag(Visited),
                  prevUrban = lag(urban))%>%
    dplyr::ungroup() %>%
    dplyr::group_by(MTB_Q,Year) %>%
    dplyr::mutate(urbanIncrease = urban > prevUrban) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(urbanIncrease = sum(urbanIncrease, na.rm=T), 
                     medianUrban = median(urban), meanUrban = mean(urban),
                     sumUrban = sum(urban)) %>%
    dplyr::mutate(urbanChange = c(NA,diff(log(sumUrban+1))))
  
  #urban cover differences
  annualUrbanCover$urbanDiff <-  as.numeric(resid(lm(sumUrban~Year,data=annualUrbanCover,na.action=na.exclude)))

  #get newly visited sites
  newSites <- readMTBQs(myfolder, mytaxa) %>%
    filter(year>1991 & year<2019)
  
  #and the difference
  newSites$firstMTBQsDiff <- as.numeric(resid(lm(firstyearMTBQs/totalMTBQs~year,
                                                 data=newSites,na.action=na.exclude)))
  newSites$totalMTBQsDiff <- as.numeric(resid(lm(totalMTBQs~year,
                                                 data=newSites,na.action=na.exclude)))
  newSites$firstMTBQsChange <- c(NA,diff(newSites$firstyearMTBQs/newSites$totalMTBQs))
  newSites$totalsMTBQsChange <- c(NA,diff(log(newSites$totalMTBQs+1)))
  #expand to full data range
  newSites <- data.frame(year=1992:2018) %>%
                  full_join(.,newSites)
  
  
  # get new recorders 
  newRecorders <- readRecorders(myfolder, mytaxa)%>%
    filter(year>1991 & year<2019)
  
  #and the difference
  newRecorders$firstObsDiff <- as.numeric(resid(lm(firstyearObs/totalObs~year,
                                                   data=newRecorders,na.action=na.exclude)))
  newRecorders$firstObsChange <- c(NA,diff(newRecorders$firstyearObs/newRecorders$totalObs))
  newRecorders$totalsObsChange <- c(NA,diff(log(newRecorders$totalObs+1)))
  newRecorders <- data.frame(year=1992:2018) %>%
                  full_join(.,newRecorders)
  
  #difference everywhere to look at change from one year to the next
  df <- data.frame(Year = 1992:2018,
                   annualBias = annualDF$estimate,
                   annualBiasChange = annualDF$estimateChange,
                   annualBiasDiff = annualDF$estimateDiff,
                   urbanCover = annualUrbanCover$sumUrban,
                   urbanCoverChange = annualUrbanCover$urbanChange,
                   urbanCoverDiff = annualUrbanCover$urbanDiff,
                   totalMTBQs = newSites$totalMTBQs,
                   totalMTBQsChange = newSites$totalsMTBQsChange,
                   totalMTBQsDiff = newSites$totalMTBQsDiff,
                   newMTBQs = newSites$firstyearMTBQs,
                   newMTBQsChange = newSites$firstMTBQsChange,
                   newMTBQsDiff = newSites$firstMTBQsDiff,
                   totalObs = newRecorders$totalObs,
                   totalObsChange = newRecorders$totalsObsChange,
                   newObs = newRecorders$firstyearObs,
                   newObsChange = newRecorders$firstObsChange,
                   newObsDiff = newRecorders$firstObsDiff)
  
  return(df)
  
}

plotAnnualDifferences <- function(getAnnualDifferences_DF, type="diff"){
  
  df <- getAnnualDifferences_DF %>% filter(Year > 1999)
  
  if(type=="diff"){
    GGally::ggpairs(df[,grepl("Diff",names(df))])
  }else if (type=="change"){
    GGally::ggpairs(df[,grepl("Change",names(df))])
  }
  
}

testAnnualDifferences <- function(getAnnualDifferences_DF){
  
  all <- getAnnualDifferences_DF[complete.cases(getAnnualDifferences_DF),]
  all <- all %>% filter(Year > 1999)
  
  p1 <- cor(all$newMTBQsChange,all$annualBiasChange)
  p2 <- cor(all$newObsChange, all$annualBiasChange)
  p3 <- cor(all$totalMTBQsChange,all$annualBiasChange)
  p4 <- cor(all$totalObsChange,all$annualBiasChange)
  p5 <- cor(all$urbanCoverChange,all$annualBiasChange)
  
  df <- data.frame(type=c("prop new sites", 
                          "prop new recorders", 
                          "total sites",
                          "total recorders",
                          "urban cover"),
                   cor = c(p1,p2,p3,p4,p5))
  
  return(df)
  
}

# testUrbanBias <- function(myfolder,mytaxa){
#   
#   samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
#   
#   samplingIntensity <- samplingIntensity %>% as_tibble()
#   
#   samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
#   samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
#   
#   samplingIntensity <- subset(samplingIntensity,!is.na(x))
#   
#   # #fit non-spatial models
#   # model1 <- glm(log(urban+0.001) ~ I(Year-1990), family= gaussian, data = samplingIntensity)
#   # mod1 <- broom::tidy(model1)
#   # mod1$model <- "year"
#   # 
#   # model1 <- glm(log(urban+0.001) ~ I(Year-1990)*Visited, family= gaussian, data = samplingIntensity)
#   # mod2 <- broom::tidy(model1)
#   # mod2$model <- "year*visited"
#   
#   ggplot(data = samplingIntensity,
#          aes(x = Year, y = log(urban+0.001), group = factor(Visited)))+
#     #geom_point(aes(colour = factor(Visited)))+
#     geom_smooth(method="lm",aes(colour = factor(Visited)))
#   
# }


#samplingIntensity[323504:323514,c("MTB_Q","Year","Visited",
#                                  "totalprevVisit","firstVisit","prevVisit")]

# #test spatial autocorrelation
# require(ncf)
# correlog1.1 <- correlog(samplingIntensity$x[randomSample], samplingIntensity$y[randomSample], 
#                         residuals(model1)[randomSample],
#                         na.rm=T, increment=1000, resamp=100)
# 
# plot(correlog1.1$correlation[1:20], type="b", pch=16, cex=1.5, lwd=1.5,
#      xlab="distance", ylab="Moran's I", cex.lab=2, cex.axis=1.5); abline(h=0)
# 
# # calculate Moran's I values explicitly for a certain distance,
# # and to test for its significance:
# require(spdep)
# snouter.nb <- dnearneigh(as.matrix(samplingIntensity[randomSample,c("x","y")]), 0, 10000) 
# snouter.listw <- nb2listw(snouter.nb, zero.policy=TRUE) 
# GlobMT1.1<- moran.test(residuals(model1)[randomSample], listw=snouter.listw,
#                        zero.policy=TRUE)


# # formal test
# library(DHARMa)
# sims <- simulateResiduals(model1)
# sims_Ag = recalculateResiduals(sims, group = dat$MTB_Q)
# dat_S <- subset(dat,!duplicated(MTB_Q))
# #arrow small error to
# testSpatialAutocorrelation(sims_Ag, 
#                            x = dat_S$x, 
#                            y = dat_S$y, plot = FALSE)


# #spatial model

# require(MASS)
# randomSample <- sample(1:nrow(samplingIntensity),10000)
# #define a grouping factor that assigns all observations to the same group
# group <- factor(rep("a",nrow(samplingIntensity)))
# samplingIntensity <- cbind(samplingIntensity, group)
# samplingIntensity_Subset <- samplingIntensity[randomSample,]
# attach(samplingIntensity_Subset) #For some reason, the data have to be attached AND specified in the formula!

# #exponential correlation structure
# model.e <- glmmPQL(Visited ~ urban*I(Year-1991), random=~1|Year,
#                    data=samplingIntensity_Subset,
#                    correlation=corExp(form=~x+y), family=binomial)
# 
# 
# infit <- HLCor(cbind(npos,ntot-npos)~
#                  1+Matern(1|longitude+latitude),data=Loaloa,
#                family=binomial(),ranPars=list(nu=0.5,rho=1/0.7))


# library(glmmTMB)
# 
# # first we need to create a numeric factor recording the coordinates of the sampled locations
# dat$pos <- numFactor(scale(dat$x), scale(dat$y))
# # then create a dummy group factor to be used as a random term
# dat$ID <- factor(rep(1, nrow(dat)))
# 
# # fit the model
# m_tmb <- glmmTMB(Visited ~ urban + mat(pos + 0 | ID), 
#                  dat,
#                  family=binomial) # take some time to fit
# # model summary of fixed effects
# summary(m_tmb)
s
# library(spaMM)
# randomSample <- sample(1:nrow(samplingIntensity),1000)
# 
# #fit basic model with same random samples
# model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity[randomSample,])
# mod2 <- broom::tidy(model1)
# mod2$model <- "year*urban"
# 
# 
# # fit the model
# m_spamm1 <- fitme(Visited ~ urban + Matern(1 | x + y %in% Year), 
#                  data = samplingIntensity[randomSample,], family = "binomial")
# # model summary
# summary(m_spamm1)
# 
# # fit the model
# m_spamm2 <- fitme(Visited ~ I(Year-1991) * urban + Matern(1 | x + y %in% Year), 
#                  data = samplingIntensity[randomSample,], family = "binomial")
# # model summary
# summary(m_spamm2)



# getNewSites <- function(myfolder,mytaxa){
#   
#   samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
#   
#   samplingIntensity <- samplingIntensity %>% as_tibble()
#   
#   samplingIntensity %>%
#     dplyr::arrange(MTB_Q,Year) %>%
#     dplyr::group_by(MTB_Q) %>%
#     #dplyr::mutate(totalprevVisit = cumsum(Visited)) %>%
#     dplyr::mutate(firstVisit = min(Year[Visited==1])) %>%
#     dplyr::mutate(firstVisit = ifelse(is.infinite(firstVisit),
#                                       2020,firstVisit)) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(firstVisit) %>%
#     dplyr::summarise(nuSites = length(unique(MTB_Q)))%>%
#     dplyr::filter(firstVisit!=2020)
#   
#   
# }
# 
# 
# plotNewSites <- function(myfolder,mytaxa){
#   
#   samplingIntensity <- getNewSites(myfolder,mytaxa)
#   
#   ggplot(samplingIntensity)+
#     geom_col(aes(x=firstVisit,y=nuSites))+
#     theme_few()+
#     xlab("Year")+
#     ylab("Number of sites visited for the first time")+
#     theme(axis.title = element_text(size = 15),
#           axis.text = element_text(size=12))+
#     scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
#   
# }


plotPABias <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  #put numbers in 2 -year periods
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$Year2 <- samplingIntensity$Year
  samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] <- samplingIntensity$Year2 [samplingIntensity$Year2 %% 2 == 1] + 1
  samplingIntensity_Group <- samplingIntensity %>%
    dplyr::group_by(MTB_Q,Year2) %>%
    dplyr::summarise(Visited = max(Visited))
  
  #add on PA area
  samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))
  samplingIntensity_Group <- inner_join(samplingIntensity_Group,protectedArea,by=c("MTB_Q"))
  
  #left plot
  buttL <- ggplot(samplingIntensity_Group,
                  aes(x=PA_area*100,y=Visited,group=Year2))+
    stat_smooth(aes(color=Year2),
                method = "glm", 
                method.args = list(family = "binomial"),
                size=1.5,
                se=FALSE)+
    theme_few()+
    ylim(0,1)+
    theme(legend.position="none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_color_viridis_c("Year")+
    xlab("Protected area(%)")+ylab("Visit probability")
  
  #right plot
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using glm
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    if(sum(tempData$Visited)>10){
      glm1 <- glm(Visited ~ PA_area, family= binomial, data = tempData)
      glmConfint <- confint(glm1)
      data.frame(Year = y, 
                 estimate=summary(glm1)$coefficients[2,1],
                 lower=glmConfint[2,1],
                 upper=glmConfint[2,2])}
    
    else{
      data.frame(Year = y, 
                 estimate=NA,
                 lower=NA,
                 upper=NA)
    }
    
  })
  
  
  buttR <- ggplot(modelCoefs)+
    geom_crossbar(aes(x = Year, y = estimate, 
                      ymax = upper, ymin = lower, fill=Year))+
    geom_hline(yintercept = 0, colour="red", linetype="dashed")+
    theme_few()+ylab("Effect of protected area")+
    scale_fill_viridis_c("Year")+
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size=10))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
  
  
  cowplot::plot_grid(buttL,buttR,nrow=1)
  
  
}