
#################################### HOMEWORK 6: ####################################
# Correlation and covariance
# due October 20, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 6)

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure correct functioning of function x11())
setwd("C:/Users/Me/MyWD")
getwd()

################## PART A ##################

# 1

df=foreign::read.spss("parenting.sav", to.data.frame = T)

checkNA=which(is.na(df), arr.ind=T) # look for NA
df[unique(checkNA[, "row"]),] # check result
# only one NA
rm(checkNA) # clean up

df=na.omit(df)

names(df)[-1] = tolower(names(df)[-1]) # can't stand capital letters in variable names. Changing them to lowercase, but i'll keep PARTNO because it's all capital

# install.packages("pastecs")
statdescDF_esteem=pastecs::stat.desc(df$esteem, basic = F, norm = T); statdescDF_esteem
# no severe kurtosis (-1 < kurt.2SE < 1)
# significant positive skew (1 < skew.2SE)

# install.packages("DescTools")
DescTools::Kurt(df$esteem, method = 1, conf.level = 0.95)
DescTools::Skew(df$esteem, method = 1, conf.level = 0.95)
# confirmed it w/ different method

stdDF=as.data.frame(scale(df[,-(1:4)])) # scale DF (numeric only)
outliers_stdDF=lapply(stdDF, function(x) which(x>=3)); outliers_stdDF # check for outliers in each variable
# two outliers in $esteem (in rows 367 and 368)
# one outlier in $psycho (in row 316)

x11();plot(density(stdDF$esteem)) # plot esteem distribution
x11();plot(density(stdDF$esteem[-c(outliers_stdDF$esteem)])) # plot esteem distribution WITHOUT outliers
# $esteem does include outliers but it seems they don't affect the distribution significantly, probably due to big size of the sample.

# furthermore distribution is NOT normal as both CI bounds computed in Skew() > 0. The distribution is positively skewed.

# 2

# upperCI
upperCI_esteem=mean(df$esteem)+unname(statdescDF_esteem["CI.mean.0.95"]); upperCI_esteem
# lowerCI
lowerCI_esteem=mean(df$esteem)-unname(statdescDF_esteem["CI.mean.0.95"]); lowerCI_esteem

# 3

range(df$esteem)
mean(df$esteem)
upperCI_esteem
lowerCI_esteem

################## PART B ##################

# 4

# centred dataset to simplify calculation
centrDF=as.data.frame(scale(df[,-(1:4)],scale=F)); head(centrDF)
# covariance:
sum(centrDF$esteem*centrDF$problems)/(length(df$esteem)-1) # manual cov
cov(df$esteem, df$problems, method="pearson") # check

#correlation:
sum(centrDF$esteem*centrDF$problems)/sqrt(sum(centrDF$esteem^2)*sum(centrDF$problems^2)) # manual cor
cor(df$esteem, df$problems, method="pearson") # check

# 5

cov(df$esteem, df$problems, method="pearson")
cor(df$esteem, df$problems, method="pearson")

# 6

summary(lm(data=df, esteem~problems))

# degrees of freedom are number of observations - number of parameters to be estimated -> 366 DF?

# 7

# recode to compute bivariate correlation:
df$gender=dplyr::recode(df$gender, "male" = 1, "female" = 0)
# male = 1, female = 0

cor.test(df$esteem,df$problems, method="pearson")
cor.test(df$esteem,df$gender)
cor.test(df$problems,df$gender)

# 8

# install.packages("PerformanceAnalytics")
x11(); PerformanceAnalytics::chart.Correlation(data.frame(Self_Esteem=df$esteem, Behavioural_Problems=df$problems), histogram = T)
x11(); PerformanceAnalytics::chart.Correlation(data.frame(Self_Esteem=df$esteem, Gender=df$gender), histogram = T)
x11(); PerformanceAnalytics::chart.Correlation(data.frame(Behavioural_Problems=df$problems, Gender=df$gender), histogram = T)

################## PART D ##################

install.packages("brms")
library(brms)
b_model = brm(problems~gender, df) 
summary(b_model)

# console output for PART D

# > install.packages("brms")
# Installing package into ‘C:/Users/Me/AppData/Local/R/win-library/4.5’
# (as ‘lib’ is unspecified)
# also installing the dependencies ‘ps’, ‘callr’, ‘processx’, ‘numDeriv’, ‘StanHeaders’, ‘inline’, ‘RcppParallel’, ‘pkgbuild’, ‘QuickJSR’, ‘RcppEigen’, ‘BH’, ‘tensorA’, ‘distributional’, ‘desc’, ‘ggridges’, ‘reshape2’, ‘Brobdingnag’, ‘globals’, ‘listenv’, ‘parallelly’, ‘rstan’, ‘loo’, ‘posterior’, ‘rstantools’, ‘bayesplot’, ‘bridgesampling’, ‘future’, ‘future.apply’, ‘matrixStats’, ‘nleqslv’, ‘coda’, ‘abind’
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/ps_1.9.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/callr_3.7.6.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/processx_3.8.6.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/numDeriv_2016.8-1.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/StanHeaders_2.32.10.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/inline_0.3.21.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/RcppParallel_5.1.11-1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/pkgbuild_1.4.8.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/QuickJSR_1.8.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/RcppEigen_0.3.4.0.2.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/BH_1.87.0-1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/tensorA_0.36.2.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/distributional_0.5.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/desc_1.4.3.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/ggridges_0.5.7.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/reshape2_1.4.4.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/Brobdingnag_1.2-9.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/globals_0.18.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/listenv_0.9.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/parallelly_1.45.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/rstan_2.32.7.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/loo_2.8.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/posterior_1.6.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/rstantools_2.5.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/bayesplot_1.14.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/bridgesampling_1.1-2.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/future_1.67.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/future.apply_1.20.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/matrixStats_1.5.0.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/nleqslv_3.3.5.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/coda_0.19-4.1.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/abind_1.4-8.zip'
# trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.5/brms_2.23.0.zip'
# package ‘ps’ successfully unpacked and MD5 sums checked
# package ‘callr’ successfully unpacked and MD5 sums checked
# package ‘processx’ successfully unpacked and MD5 sums checked
# package ‘numDeriv’ successfully unpacked and MD5 sums checked
# package ‘StanHeaders’ successfully unpacked and MD5 sums checked
# package ‘inline’ successfully unpacked and MD5 sums checked
# package ‘RcppParallel’ successfully unpacked and MD5 sums checked
# package ‘pkgbuild’ successfully unpacked and MD5 sums checked
# package ‘QuickJSR’ successfully unpacked and MD5 sums checked
# package ‘RcppEigen’ successfully unpacked and MD5 sums checked
# package ‘BH’ successfully unpacked and MD5 sums checked
# package ‘tensorA’ successfully unpacked and MD5 sums checked
# package ‘distributional’ successfully unpacked and MD5 sums checked
# package ‘desc’ successfully unpacked and MD5 sums checked
# package ‘ggridges’ successfully unpacked and MD5 sums checked
# package ‘reshape2’ successfully unpacked and MD5 sums checked
# package ‘Brobdingnag’ successfully unpacked and MD5 sums checked
# package ‘globals’ successfully unpacked and MD5 sums checked
# package ‘listenv’ successfully unpacked and MD5 sums checked
# package ‘parallelly’ successfully unpacked and MD5 sums checked
# package ‘rstan’ successfully unpacked and MD5 sums checked
# package ‘loo’ successfully unpacked and MD5 sums checked
# package ‘posterior’ successfully unpacked and MD5 sums checked
# package ‘rstantools’ successfully unpacked and MD5 sums checked
# package ‘bayesplot’ successfully unpacked and MD5 sums checked
# package ‘bridgesampling’ successfully unpacked and MD5 sums checked
# package ‘future’ successfully unpacked and MD5 sums checked
# package ‘future.apply’ successfully unpacked and MD5 sums checked
# package ‘matrixStats’ successfully unpacked and MD5 sums checked
# package ‘nleqslv’ successfully unpacked and MD5 sums checked
# package ‘coda’ successfully unpacked and MD5 sums checked
# package ‘abind’ successfully unpacked and MD5 sums checked
# package ‘brms’ successfully unpacked and MD5 sums checked
# 
# The downloaded binary packages are in
# C:\Users\Me\AppData\Local\Temp\RtmpoX0mY3\downloaded_packages
# > library(brms)
# Loading required package: Rcpp
# Loading 'brms' package (version 2.23.0). Useful instructions
# can be found by typing help('brms'). A more detailed introduction
# to the package is available through vignette('brms_overview').
# 
# Attaching package: ‘brms’
# 
# The following object is masked from ‘package:stats’:
#   
#   ar
# > b_model <- brm(Problems~Gender, parent) 
# Error: object 'parent' not found
# 
# > b_model = brm(problems~gender, df) 
# Compiling Stan program...
# Start sampling
# 
# SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
# Chain 1: 
#   Chain 1: Gradient evaluation took 1.3e-05 seconds
# Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
# Chain 1: Adjust your expectations accordingly!
#   Chain 1: 
#   Chain 1: 
#   Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
# Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
# Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
# Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
# Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
# Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
# Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
# Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
# Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
# Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
# Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
# Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
# Chain 1: 
#   Chain 1:  Elapsed Time: 0.015 seconds (Warm-up)
# Chain 1:                0.012 seconds (Sampling)
# Chain 1:                0.027 seconds (Total)
# Chain 1: 
#   
#   SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
# Chain 2: 
#   Chain 2: Gradient evaluation took 6e-06 seconds
# Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.06 seconds.
# Chain 2: Adjust your expectations accordingly!
#   Chain 2: 
#   Chain 2: 
#   Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
# Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
# Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
# Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
# Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
# Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
# Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
# Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
# Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
# Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
# Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
# Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
# Chain 2: 
#   Chain 2:  Elapsed Time: 0.016 seconds (Warm-up)
# Chain 2:                0.011 seconds (Sampling)
# Chain 2:                0.027 seconds (Total)
# Chain 2: 
#   
#   SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
# Chain 3: 
#   Chain 3: Gradient evaluation took 5e-06 seconds
# Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.05 seconds.
# Chain 3: Adjust your expectations accordingly!
#   Chain 3: 
#   Chain 3: 
#   Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
# Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
# Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
# Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
# Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
# Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
# Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
# Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
# Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
# Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
# Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
# Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
# Chain 3: 
#   Chain 3:  Elapsed Time: 0.016 seconds (Warm-up)
# Chain 3:                0.01 seconds (Sampling)
# Chain 3:                0.026 seconds (Total)
# Chain 3: 
#   
#   SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
# Chain 4: 
#   Chain 4: Gradient evaluation took 5e-06 seconds
# Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.05 seconds.
# Chain 4: Adjust your expectations accordingly!
#   Chain 4: 
#   Chain 4: 
#   Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
# Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
# Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
# Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
# Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
# Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
# Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
# Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
# Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
# Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
# Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
# Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
# Chain 4: 
#   Chain 4:  Elapsed Time: 0.015 seconds (Warm-up)
# Chain 4:                0.011 seconds (Sampling)
# Chain 4:                0.026 seconds (Total)
# Chain 4: 
#   > summary(b_model)
# Family: gaussian 
# Links: mu = identity 
# Formula: problems ~ gender 
# Data: df (Number of observations: 368) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept    10.25      0.25     9.74    10.73 1.00     4036     3101
# gender        2.90      0.35     2.19     3.59 1.00     3971     3143
# 
# Further Distributional Parameters:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     3.44      0.13     3.21     3.70 1.00     4155     3127
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
# > 
#   
#   



#####################################################################################
#####################################################################################

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################
