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
          axis.title = element_text(size = 15),
          axis.text = element_text(size=12))+
    scale_color_viridis_c("Year")+
    xlab("Urban cover (%)")+ylab("Visit probability")
  
  #right plot
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using glm
  modelCoefs <- ldply(myYears,function(y){
    
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
          axis.title = element_text(size = 15),
          axis.text = element_text(size=12))+
    scale_x_continuous(labels=c(1992,2002,2012),breaks=c(1992,2002,2012))
  
  
  cowplot::plot_grid(buttL,buttR,nrow=1)
  
  
}


plotUrbanCover <- function(myfolder,mytaxa){
  
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


testUrbanBias <- function(myfolder,mytaxa){
  
  samplingIntensity <- readRDS(paste0(myfolder,"/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))

  #fit non-spatial models
  model1 <- glm(Visited ~ urban, family= binomial, data = samplingIntensity)
  mod1 <- broom::tidy(model1)
  mod1$model <- "urban"
  
  model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity)
  mod2 <- broom::tidy(model1)
  mod2$model <- "year*urban"
  
  # # formal test
  # library(DHARMa)
  # sims <- simulateResiduals(model1)
  # sims_Ag = recalculateResiduals(sims, group = samplingIntensity$MTB_Q)
  # samplingIntensity_S <- subset(samplingIntensity,!duplicated(MTB_Q))
  # #arrow small error to 
  # testSpatialAutocorrelation(sims_Ag, x = samplingIntensity_S$x, y = samplingIntensity_S$y, plot = FALSE)
  # #seems to be spatial correlation
  
  #fit spatial models
  library(spaMM)
  randomSample <- sample(1:nrow(samplingIntensity),1000)
  
  #fit basic model with same randomsamoke
  model1 <- glm(Visited ~ I(Year-1991) * urban, family= binomial, data = samplingIntensity[randomSample,])
  mod2 <- broom::tidy(model1)
  mod2$model <- "year*urban"
  
  
  # fit the model
  m_spamm1 <- fitme(Visited ~ urban + Matern(1 | x + y %in% Year), 
                   data = samplingIntensity[randomSample,], family = "binomial")
  # model summary
  summary(m_spamm1)
  
  # fit the model
  m_spamm2 <- fitme(Visited ~ I(Year-1991) * urban + Matern(1 | x + y %in% Year), 
                   data = samplingIntensity[randomSample,], family = "binomial")
  # model summary
  summary(m_spamm2)
  
}



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

# 
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



