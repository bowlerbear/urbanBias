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

  samplingIntensity_Summary <- samplingIntensity %>%
    dplyr::group_by(MTB_Q) %>%
    dplyr::summarise(totalVisits = sum(Visited),
                     urbanTrend = urban[Year==2018]-urban[Year==1992])
  
  #reduce by a quarter
  samplingIntensity_Summary$MTB <- sapply(as.character(samplingIntensity_Summary$MTB_Q),function(x){
    len <- nchar(x)
    substr(x,1,(len-1))})
  
  #add x and y
  samplingIntensity_Summary$x <- mtbqsDF$x_MTB[match(samplingIntensity_Summary$MTB,mtbqsDF$Value)]
  samplingIntensity_Summary$y <- mtbqsDF$y_MTB[match(samplingIntensity_Summary$MTB,mtbqsDF$Value)]
  
  #take a sample of 500 MTBQs per year
  samplingIntensity_subset <- samplingIntensity_Summary %>%
                                dplyr::filter(!duplicated(MTB), !is.na(x),!is.na(y)) 

  
  #fit spatial model
  library(spaMM)
  
  # fit the model
  m_spamm1 <- fitme(cbind(totalVisits, 27-totalVisits) ~ log(urbanTrend+0.01) + Matern(1 | x + y), 
                    data = samplingIntensity_subset, family = "binomial", method="PQL")
  
  # model summary
  out1 <- summary(m_spamm1)
  model_coefs <- out1$beta_table
  
  modelDF <- data.frame(model = "spaMM",
                        taxa = mytaxa,
                        dataset = myfolder,
                        estimate=model_coefs[2,1],
                        se=model_coefs[2,2],
                        lower=model_coefs[2,1] - 2*model_coefs[2,2],
                        upper=model_coefs[2,1] + 2*model_coefs[2,2])
  
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

saveRDS(out2,file=paste0("testpropYears_",mydataset,"_",mytaxa,".rds"))
