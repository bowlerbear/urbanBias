library(plyr)
library(ggplot2)
library(cowplot)
require(ggthemes)

source('simulations/spatialBias_functions.R')

#as before except we assume that we only have the presences at sampled sites
#other sites were either unsampled or sampled and absent

### static scenario #######################################################

df <- generatePP()



library(INLA)
library(inlabru)
library(ggplot2)

#+results="hide",warning=FALSE,message=FALSE
#init.tutorial()
bru_options_set(bru_verbose = TRUE, control.compute = list(dic = TRUE, waic = TRUE))


matern = inla.spde2.pcmatern(mesh, 
                             prior.sigma = c(0.1, 0.01), 
                             prior.range = c(5, 0.01))

#' 
#' **Task:** Fit an LGCP model with SPDE only to these data by using the `samplers=` argument of 
#' the function `lgcp( )`. 
#' 

#+results="hide",warning=FALSE,message=FALSE, echo = FALSE

cmp = coordinates ~ my.spde(main = coordinates, 
                            model = matern) 

#+results="hide",warning=FALSE,message=FALSE, echo = FALSE, eval=TRUE

fit = lgcp(cmp, sample$nests, samplers = sample$plots, domain = list(coordinates = mesh))

#'
#' **Task:** Plot the density surface from your fitted model
#' 

#+results="hide",warning=FALSE,message=FALSE, echo = FALSE

lambda.sample = predict(fit, pixels(mesh), ~ exp(my.spde + Intercept))

#' Your plot should look like this:

#+warning=FALSE,message=FALSE

lambda.sample.plot = ggplot() + 
  gg(lambda.sample) + 
  gg(sample$plots) + 
  gg(boundary, col = "yellow") + 
  coord_fixed() + 
  theme(legend.position = "bottom")

lambda.sample.plot

#'
#' **Task:** Estimate the integrated intensity lambda.
#' 

#+results="hide",warning=FALSE,message=FALSE, echo = FALSE

Lambda = predict(fit, ipoints(boundary, mesh), ~ sum(weight * exp(my.spde + Intercept)))
Lambda

#'
#' **Task:** Fit the same model to the full dataset (the points in `gorillas$nests`), or get your previous 
#' fit, if you kept it. Plot the intensity surface and estimate the integrated intensity
#' 

#+results="hide",warning=FALSE,message=FALSE, echo=FALSE

x = seq(0, 55, length = 50) # this sets mesh points - try others if you like
mesh1D <- inla.mesh.1d(x)


fit.all = lgcp(cmp, gorillas$nests, samplers = gorillas$boundary, domain = list(coordinates = mesh))
lambda.all = predict(fit.all, pixels(mesh), ~ exp(my.spde + Intercept))
Lambda.all = predict(fit.all,  ipoints(boundary, mesh), ~ sum(weight * exp(my.spde + Intercept)))

matern = inla.spde2.pcmatern(mrsea$mesh, 
                             prior.sigma = c(0.1, 0.01), 
                             prior.range = c(40, 0.01))

cmp = distance + coordinates ~ beta.df(main = distance^2, model = "linear") + 
  #offset(log(1/250)) +
  mySmooth(main = coordinates, model = matern) + 
  Intercept


#+results="hide",warning=FALSE,message=FALSE

fit.sp = lgcp(cmp, mrsea$points, samplers = mrsea$samplers, 
              domain = list(coordinates = mrsea$mesh, distance = c(0,250)))
