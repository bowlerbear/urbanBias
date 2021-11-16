library(tidyverse)
library(broom)
library(mgcv)
library(sf)

### environ datasets ####

load("/data/idiv_ess/urbanBias/mtbqsDF.RData")

### functions #####

getInteractionBiasSpace <- function(myfolder, mytaxa){
  
  samplingIntensity <- readRDS(paste0("/data/idiv_ess/urbanBias/surveys_revision_",myfolder,"_",mytaxa,".rds"))
  
  samplingIntensity <- samplingIntensity %>% as_tibble()
  
  samplingIntensity$x <- mtbqsDF$x[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  samplingIntensity$y <- mtbqsDF$y[match(samplingIntensity$MTB_Q,mtbqsDF$MTB_Q)]
  
  samplingIntensity <- subset(samplingIntensity,!is.na(x))
  
  #take a sample of 500 MTBQs per year
  samplingIntensity_subset <- samplingIntensity %>%
                                dplyr::group_by(Year) %>%
                                dplyr::mutate(sample = ifelse(1:n() %in% 
                                                                sample(1:n(),500),1,0)) %>%
                                dplyr::filter(sample==1)

  
  
  #fit spatial model
  library(spaMM)
  dat <- samplingIntensity_subset
  dat$fYear <- as.factor(dat$Year)
  dat$cYear <- dat$Year - 1991
  
  # fit the model
  m_spamm1 <- fitme(Visited ~ urban*cYear + Matern(1 | x + y %in% fYear),
                    data = dat, family = "binomial", method="PQL")
  # model summary
  out1 <- summary(m_spamm1)
  model_coefs <- out1$beta_table
  
  modelDF <- data.frame(model = "spaMM",
                        estimate=model_coefs[4,1],
                        se=model_coefs[4,2],
                        lower=model_coefs[4,1] - 2*model_coefs[4,2],
                        upper=model_coefs[4,1] + 2*model_coefs[4,2])
  
  return(modelDF)
  
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
out2 <- getInteractionBiasSpace(myfolder = mydataset,
                           mytaxa = mytaxa)

saveRDS(out2,file=paste0("interactionBiasSpace_",mydataset,"_",mytaxa,".rds"))
