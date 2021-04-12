library(plyr)
library(ggplot2)
library(cowplot)
require(ggthemes)

source('simulations/spatialBias_functions.R')

#as before except we assume that we only have the presences at sampled sites
#other sites were either unsampled or sampled and absent

### static scenario #######################################################

df <- generateData()
