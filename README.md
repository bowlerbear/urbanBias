# urbanBias
A study of the strength of urban Bias in CS data and implications of changes in this bias for population trend estimation. 

### Simulations
In this folder are the main scripts:
- spatialBias.R - the main script to run each scenario under the standard settings (urban avoiding species, perfect detection)
- spatialBias_speciesEffects.R - as the main script except to generate the patterns for species either neutral or responding positively to urban cover.
- spatialBias_imperfect - as the main script except assuming imperfect detection (i.e., only a fraction of species present are seen) that may or may not related to urban cover.
- spatialBias_function.R - the functions to generate datasets and analyse. This script is called within the above scripts.


### Models
These are the text files for the JAGS occupancy models.

### Methods
This folder just contains some bits of code for different methods.

### Empirical-data
This pulls together the data from the real-world datasets to analyse the spatial bias and draw the empirical data plot.


