# urbanBias
A study of the strength of urban Bias in CS data and implications of changes in this bias for population trend estimation. 

### Simulations
In this folder are the main scripts:
- spatialBias.R - the main script to run each scenario under the standard settings (urban avoiding species, perfect detection)
- spatialBias_speciesEffects.R - as the main script except to generate the patterns for species either neutral or responding positively to urban cover.
- spatialBias_function.R - the functions to generate datasets and analyse. This script is called within the above scripts.


### Models
These are the text files for the JAGS occupancy models. These are not used in the urban bias paper.

### Methods
This folder just contains some bits of code for different methods.

### Empirical-data
This pulls together the data from the real-world datasets to analyse the spatial bias and draw the empirical data plot.
The first analysis was done within the empirical_bias_Plots.R
The analysis was extended using more datasets and using spAMM - which meant moving to the HPC
Hence all the other files are script to test
- the effect of urban cover and pa area on site visitation in each year (HPC_testPABias, HPC_testUrbanBias)
- test the main effect of urban cover across all years (HPC_testmainPABias, HPC_testmainUrbanBias)
- test the interaction between the effect of urban cover and year (HPC_testinteractionUrbanBias, HPC_testinteractionPABias)
HPC_models just pulls in the models from the HPC.
recordTS.R plots a time series
data_processing contains script to process each dataset that was downloaded.


