library(tidyverse)
library(broom)
library(mgcv)
library(sf)

### environ datasets ####

load("/data/idiv_ess/urbanBias/mtbqsDF.RData")

### functions #####

getAnnualBiasSpace <- function(myfolder, mytaxa, myyear){
  
  samplingIntensity <- readRDS(paste0("/data/idiv_ess/urbanBias/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))
  
  #get data for one year and take a random sample
  tempData <- samplingIntensity[samplingIntensity$Year == as.numeric(myyear),]
  randomSample <- sample(1:nrow(tempData),4000)
  dat <- tempData[randomSample,]
    
  if(sum(dat$Visited)>5){
      
      #fit glm model too
      glm1 <- glm(Visited ~ urban, family= binomial, data = dat)
      glmConfint <- confint(glm1)
      model1 <- data.frame(Year = myyear, 
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
      
      model2 <- data.frame(Year = myyear, 
                           model = "spaMM",
                           estimate=model_coefs[2,1],
                           se=model_coefs[2,2],
                           lower=model_coefs[2,1] - 2*model_coefs[2,2],
                           upper=model_coefs[2,1] + 2*model_coefs[2,2])
      
      rbind(model1, model2)
      
      
  }else{
      data.frame(Year = myyear, 
                 model = "none",
                 estimate=NA,
                 se=NA,
                 lower=NA,
                 upper=NA)
    }
    
  
}


### task IDs ####

#make TaskID array
taxa <- c("plants","birds",
          "butterflies","amphibians")
datasets <- c("GBIF","Obs","NG")
years <- 1992:2018
TaskID <- expand.grid(taxa,datasets,years)
TaskID$TaskID <- 1:nrow(TaskID)#12

#for this task  
task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))
mytaxa <- as.character(TaskID$Var1[which(TaskID$TaskID==task.id)])
mydataset <- as.character(TaskID$Var2[which(TaskID$TaskID==task.id)])
myyear <- as.character(TaskID$Var3[which(TaskID$TaskID==task.id)])

#run bias model
out2 <- getAnnualBiasSpace(myfolder = mydataset,
                           mytaxa = mytaxa,
                           myyear = myyear)
saveRDS(out2,file=paste0("annualBiasSpace_",mydataset,"_",mytaxa,"_",myyear,".rds"))



