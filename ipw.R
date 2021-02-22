#measurements made in 1000 individuals on 
#a continuous confounder L, 
#a dichotomous exposure A and 
#a continuous outcome Y
set.seed(16)
n <- 1000
simdat <- data.frame(l = rnorm(n, 10, 5))
a.lin <- simdat$l - 10
pa <- exp(a.lin)/(1 + exp(a.lin))
simdat$a <- rbinom(n, 1, prob = pa)
simdat$y <- 10*simdat$a + 0.5*simdat$l + rnorm(n, -10, 5)
simdat[1:5,]
#The true parameter for the marginal causal effect of A on Y is 10.

library("ipw")

#To estimate the denominator of stablised weights, we use a logistic model regressing A on L.
#To estimate the numerator of (7), we use a logistic model regressing A on the constant only

temp <- ipwpoint(exposure = a, family = "binomial", link = "logit",
                    numerator = ~ 1, denominator = ~ l, data = simdat)
summary(temp$ipw.weights)


ipwplot(weights = temp$ipw.weights, logscale = FALSE,main = "Stabilized weights", xlim = c(0, 8))

summary(temp$num.mod)
summary(temp$den.mod)

#stablised weights - add to data frame
simdat$sw <- temp$ipw.weights

#https://stats.stackexchange.com/questions/411711/why-do-stabilized-ipw-weights-give-the-same-estimates-and-ses-as-unstabilized-we

library(survey)
#svglm #Fit a generalised linear model to data from a complex survey design, with inverse-probability weighting and design-based standard errors
?svydesign

msm <- (svyglm(y ~ a, design = svydesign(~ 1, weights = ~ sw,data = simdat)))
summary(msm)
#                 Estimate    Std. Error t value Pr(>|t|)    
#(Intercept)      -4.375      1.142  -3.832 0.000135 ***
#  a              10.647      1.190   8.948  < 2e-16 ***
  
#how does this vary with a standard lm?
summary(lm1 <- lm(y ~ a, data=simdat, weights=sw))
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -4.3755     0.2358  -18.56   <2e-16 ***
#  a             10.6466     0.3449   30.86   <2e-16 ***

#only the standard errors are different

#svyglm always returns 'model-robust' standard errors; the Horvitz-Thompson-type standard errors used everywhere in the survey package are a generalisation of the model-robust 'sandwich' estimators

library(sandwich)
sandwich::vcovHC(lm1, type = "HC3")
lmtest::coeftest(lm1, vcov=vcov(lm1))

parameters::standard_error_robust(lm1)
jtools::summ(lm1, robust = TRUE)
jtools::get_robust_se(lm1,type="HC3")#HC1 returns the same as survey_glm

#https://cran.r-project.org/web/packages/WeightIt/vignettes/WeightIt.html
#We'll use the jtools package to provide clean summaries of our results. #Similar output can be generated using summary() without installing jtools.

#https://stackoverflow.com/questions/56186735/does-the-sandwich-package-work-for-robust-standard-errors-for-logistic-regressio

#https://stats.stackexchange.com/questions/363340/inverse-probability-weighting-and-robust-estimation

#https://stats.stackexchange.com/questions/363340/inverse-probability-weighting-and-robust-estimation

#https://stackoverflow.com/questions/62614706/huber-white-robust-standard-errors-for-a-glmm-r

#weights in a glmm
library(lme4)

#BBS survey trend using maximum likelihood estimation with survey weights
library(survey)
bbs_design <- svydesign(ids = ~1, weights = ~weight, data = bbs_counts)
bbs_trend <- svyglm(count ~ year_factor + square, design = bbs_design, family=poisson)

#BBS survey trend using Bayesian weighted likelihood approach
library(brms)
bbs_trend_brms <- brm(count|weights(weight, scale = TRUE) ~ year_factor + square - 1, data = bbs_pf_counts, family = poisson)
