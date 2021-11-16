library(tidyverse)
library(broom)
library(mgcv)
library(sf)

### environ datasets ####

load("/data/idiv_ess/urbanBias/mtbqsDF.RData")
protectedArea <- readRDS("/data/idiv_ess/urbanBias/protectedarea_MTBQ.rds")

### functions #####

getAnnualBiasSpace <- function(myfolder, mytaxa){
  
  samplingIntensity <- readRDS(paste0("/data/idiv_ess/urbanBias/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity <- inner_join(samplingIntensity,protectedArea,by=c("MTB_Q"))

  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))
  
  myYears <- sort(unique(samplingIntensity$Year))
  
  #using spaMM
  modelCoefs <- plyr::ldply(myYears,function(y){
    
    tempData <- samplingIntensity[samplingIntensity$Year == y,]
    
    randomSample <- sample(1:nrow(tempData),4000)
    dat <- tempData[randomSample,]

    if(sum(dat$Visited)>5){
      
      #fit glm model too
      glm1 <- glm(Visited ~ PA_area, family= binomial, data = dat)
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
      m_spamm1 <- fitme(Visited ~ PA_area + Matern(1 | x + y),
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


### task IDs ####

#make TaskID array
taxa <- c("plants","birds",
          "butterflies","amphibians")
datasets <- c("GBIF","Obs","NG")
TaskID <- expand.grid(taxa,datasets)
TaskID$TaskID <- 1:nrow(TaskID)#12

#for this task  
task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
mytaxa <- as.character(TaskID$Var1[which(TaskID$TaskID==task.id)])
mydataset <- as.character(TaskID$Var2[which(TaskID$TaskID==task.id)])
                                
#run bias model
out2 <- getAnnualBiasSpace(myfolder = mydataset,
                           mytaxa = mytaxa)
saveRDS(out2,file=paste0("annualBiasSpace_PAarea_",mydataset,"_",mytaxa,".rds"))


# #explore models
# modelFiles <- list.files("model-outputs")
# allModels <- lapply(modelFiles, function(x){
#   temp <- readRDS(paste("model-outputs",x,sep="/"))
#   temp$Files <- x
#   temp$dataset <- strsplit(temp$Files,"_")[[1]][2]
#   temp$taxa <- strsplit(temp$Files,"_")[[1]][3]
#   temp$taxa <- gsub(".rds","",temp$taxa)
#   return(temp)
# })
# allModels <- do.call(rbind,allModels)
# 
# 
# 
# 
# #glm plots
# library(ggplot2)
# 
# ggplot(subset(allModels,model=="glm"))+
#   geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
#   geom_hline(yintercept=0,linetype="dashed")+
#   facet_wrap(dataset~taxa,scales="free")
# 
# ggplot(subset(allModels,model=="spaMM"))+
#   geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
#   geom_hline(yintercept=0,linetype="dashed")+
#   facet_wrap(dataset~taxa,scales="free")
# 
# #run again for for longer and more iterations
# ggplot(subset(allModels,model=="spaMM" & Year >2010))+
#   geom_crossbar(aes(x=Year, y = estimate, ymax=upper, ymin = lower))+
#   geom_hline(yintercept=0,linetype="dashed")+
#   facet_wrap(dataset~taxa,scales="free")

