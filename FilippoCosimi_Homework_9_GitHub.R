
#################################### HOMEWORK 9: ####################################

# Bayesian data analysis
# due November 17, 2025 at 10:20, (upload to Pitch2Peer portal on Brightspace --> Week 9)

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure correct functioning of function x11())
setwd("C:/Users/MyWD")
getwd()


# 1

library(brms)
options(mc.cores = parallel::detectCores())
options(contrasts = c("contr.sum", "contr.poly"))
df=foreign::read.spss("parenting.sav", to.data.frame = T)
df=na.omit(df)


names(df)[-1] = tolower(names(df)[-1]) # don't like capital letters in variable names. Changing them to lowercase, but i'll keep PARTNO because it's all capital

mean(df$control)

brmsModel=brm(control ~ 1, data = df)
summary(brmsModel)

x11(); plot(brmsModel)

# 2

# ggplot is not working (again). I have to default back to basic plot()
x11(); plot(brmsModel)

# 3

# 4

# 5

brmsModel_probContr=brm(problems ~ control, data = df)
summary(brmsModel_probContr)

# 6

# 7

brmsModel_probGend=brm(problems ~ gender, data = df)
summary(brmsModel_probGend)
plot(brmsModel_probGend)

contrasts(df$gender)

# 8

x11(); plot(conditional_effects(brmsModel_probGend, effects = 'gender'))

# 9

df_std=cbind(df[,1:4], lapply(df[,5:10], function(x) scale(x, center = T, scale = T))) # standardize

brmsModel_probEst=brm(problems ~ esteem, data = df_std)
summary(brmsModel_probEst)

cor(df$problems, df$esteem)

# 10

brmsModel_probEst2=brm(esteem ~ problems, data = df_std)
summary(brmsModel_probEst2)

#####################################################################################
#####################################################################################

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################


