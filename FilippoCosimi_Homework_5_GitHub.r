
#################################### HOMEWORK 5: ####################################
# Linear regression
# due October 13, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 5)

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want you may download the script and run it to check if it works

#####################################################################################
#####################################################################################

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure correct functioning of function x11())
setwd("C:/Users/Me/WD")
getwd()

################## PART A ##################

# 1

library(foreign)
df=read.spss("parenting.sav", to.data.frame = T)
# knowing there is only one subject with NA variables, i'll exclude it for simplicity:
df=na.omit(df)
names(df)[6]="control" # can't stand capital letters in variable names

# install.packages("pastecs")
pastecs::stat.desc(df$control, basic = F, norm = T)
# kurtosis does not significantly vary from normality as: -1 < kurt.2SE < 1
# skew DOES significantly vary from normality as: 1 < skew.2SE

# install.packages("DescTools")
DescTools::Kurt(df$control, method = 1, conf.level = 0.95)
DescTools::Skew(df$control, method = 1, conf.level = 0.95)
# confirmed it w/ different method

x11();plot(density(df$control))
# visual confirmation: the distribution is NOT normal, and presents positive skew.

which(abs(df$control)>=(mean(df$control)+3*sd(df$control)))
# no outliers

# 2

DFstats=pastecs::stat.desc(df$control, basic = F, norm = T) # among other things, it returns 95% margins of error and mean.

# upper:
mean(df$control)+as.numeric(DFstats[4])
# lower:
mean(df$control)-as.numeric(DFstats[4])


# 3

length(which(df$Gender=="female")) 
length(which(df$Gender=="male"))

DFstats=pastecs::stat.desc(df$control, basic = F, norm = T)

# upper:
mean(df$control)+as.numeric(DFstats[4])
# lower:
mean(df$control)-as.numeric(DFstats[4])

range(df$control)

################## PART B ##################

# 4

# a
names(df)[9]="problems"

df$control_s=as.vector(scale(df$control, scale = F))
df$problems_s=as.vector(scale(df$problems, scale = F))

slope=sum((df$control_s)*(df$problems_s))/sum(df$control_s**2); slope
intercept=mean(df$problems)-(slope*mean(df$control)); intercept

# b
resid=df$problems-(intercept+slope*df$control)
SSresidual=sum(resid**2)
SStotal=sum(df$problems**2)-(length(df$control)*(mean(df$problems)**2))
Rsq=1-(SSresidual/SStotal)
  # c
# estimate SE
SEestimate=sqrt(SSresidual/length(df$control)-2)
  # d
# slope SE
SEslope=SEestimate/sqrt(sum(df$control**2)-length(df$control)*(mean(df$control))**2)

# intercept SE
SEintercept= SEestimate*sqrt(1/length(df$control) + mean(df$control)**2/sum(df$control_s**2))

# e
# print all
paste0("Percent explained variance: ", round(Rsq*100,2)); paste0("Estimate Standar Error: ", SEestimate); paste0("Slope Standar Error: ", SEslope);paste0("Intercept Standar Error: ", SEintercept)


regrModel_lm=lm(problems~control, data = df)
summary(regrModel_lm)
# all confirmed by lm()

# 5

SEestimate

# 6

df$Gender=as.factor(df$Gender)
genderProblemslm=lm(problems~Gender, data = df) # lm() automatically converts $Gender into dummy code
summary(genderProblemslm)

#####################################################################################
#####################################################################################

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want you may download the script and run it to check if it works

