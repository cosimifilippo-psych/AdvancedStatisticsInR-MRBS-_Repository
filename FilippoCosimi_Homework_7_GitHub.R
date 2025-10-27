
#################################### HOMEWORK 7: ####################################
# Effect sizes and statistical power
# due October 27, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 7)

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure correct functioning of function x11())
setwd("C:/Users/Me/MyWD")
getwd()


# 1a

df=read.csv("HW7_data.csv")

checkNA=which(is.na(df), arr.ind=T) # look for NA
df[unique(checkNA[, "row"]),] # check result
# only one NA
rm(checkNA) # clean up

df=na.omit(df) # omit NAs (optional). I'm not doing it.

# copy in wide format
df_wide=reshape::cast(df, formula = pp_code + gender + age ~ time, value = "score")

# sort and polish
df_wide$sortingValues=as.integer(sub("pp_", "", df_wide$pp_code))
df_wide=df_wide[order(df_wide$sortingValues), ]
rownames(df_wide)=c(1:length(df_wide$pp_code))
df_wide=df_wide[,-6]
df_wide

# 1b

library(ggplot2)

APAgraphics=
  ggthemes::theme_clean(base_size = 30, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(axis.title.y = element_text(face = "bold", margin = margin(r = 20)), axis.title.x = element_text(face = "bold", margin = margin(t = 20))) + # set axis labels margins
  theme(plot.margin = unit(c(0.5,1,0.5,1), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(legend.title = element_blank(), legend.background = element_rect(color = "black", linewidth = 1))

x11(); ggplot(data=df_wide) + 
  geom_density(mapping = aes(x=evening, fill="Morning"), alpha=0.5) +
  geom_density(mapping = aes(x=morning, fill="Evening"), alpha=0.5) +
  scale_fill_manual(values = c("Evening" = "royalblue4", "Morning" = "rosybrown")) +
  ylab("Density") + # set axis labels
  xlab("Scores") + # set axis labels
  APAgraphics

# 2

# install.packages("effsize")
# install.packages("pwr")

df_wide_naomit=na.omit(df_wide)

# Paired t-test
t_results = t.test(df_wide_naomit$morning, df_wide_naomit$evening, paired = TRUE, alternative = "two.sided")
t_results

# Descriptive statistics
mean_morning = mean(df_wide_naomit$morning); mean_morning
mean_evening = mean(df_wide_naomit$evening); mean_evening
mean_difference = mean(df_wide_naomit$evening - df_wide_naomit$morning); mean_difference
sd_difference = sd(df_wide_naomit$evening - df_wide_naomit$morning); sd_difference
n = sum(complete.cases(df_wide_naomit$morning, df_wide_naomit$evening)); n

# 95% CIs
t_crit = qt(0.975, df = n - 1); t_crit

se_m = sd(df_wide_naomit$morning) / sqrt(n); se_m
se_e = sd(df_wide_naomit$evening) / sqrt(n); se_e
ci_m = c(mean_morning - t_crit * se_m, mean_morning + t_crit * se_m); ci_m
ci_e = c(mean_evening - t_crit * se_e, mean_evening + t_crit * se_e); ci_e

se_difference = sd_difference / sqrt(n); se_difference
ci_difference = c(mean_difference - t_crit * se_difference, mean_difference + t_crit * se_difference); ci_difference

# Unstandardized effect size (mean difference)
mean_difference

# Standardized effect size (Cohen’s d)
d = mean_difference / sd_difference
d

# 3
sd_morning = sd(df_wide_naomit$morning)
sd_evening = sd(df_wide_naomit$evening)
n1 = length(df_wide_naomit$pp_code)
n2 = length(df_wide_naomit$pp_code)

# a
sd_pooled = sqrt(((n1 - 1)*sd_morning^2 + (n2 - 1)*sd_evening^2) / (n1 + n2 - 2))
cohen_d_ind = (mean_evening - mean_morning) / sd_pooled
cohen_d_ind

# b
J = 1 - (3 / (4*(n1 + n2) - 9))
hedges_g = J * cohen_d_ind
hedges_g

# c
delta_hat = (mean_evening - mean_morning) / sqrt((sd_morning^2 + sd_evening^2) / 2)
delta_hat

# d
# install.packages("MBESS")
library(MBESS)
# Hedges’ g using MBESS
hedges_g_mbess = smd(Mean.1 = mean_evening, Mean.2 = mean_morning, s.1 = sd_evening, s.2 = sd_morning, n.1 = n1, n.2 = n2, Unbiased = TRUE)  # TRUE applies Hedges' correction
hedges_g_mbess

delta_hat_mbess = smd(Mean.1 = mean_evening, Mean.2 = mean_morning, s.1 = sd_evening, s.2 = sd_morning, n.1 = n1, n.2 = n2, Unbiased = FALSE)  # FALSE gives Delta hat
delta_hat_mbess

# 4

# method 1
d_method1 = mean_difference / sd_difference
d_method1

# method 2
d_method2 = t_results$statistic / sqrt(n)
d_method2

# the sign just depends on the order of subtraction, but size is the same, so both methods yield same result.

# 5
# Load library
library(pwr)

# Power analysis: two-sided t-test, alpha = 0.05, desired power = 0.95
power_analysis = pwr.t.test(d = hedges_g, sig.level = 0.05, power = 0.95, type = "two.sample", alternative = "two.sided")
power_analysis

totalNumerOfSubjects=2*power_analysis$n
totalNumerOfSubjects

# power_analysis$n corresponds to number of subject for one group. power_analysis$n = 118 (approx). totalNumerOfSubjects = 235 (approx)

# 6

power_analysis = pwr.t.test(d = d_method1, sig.level = 0.05, power = 0.95, type = "two.sample", alternative = "two.sided")
power_analysis

totalNumerOfSubjects=2*power_analysis$n
totalNumerOfSubjects

# power_analysis$n corresponds to number of subject for one group. power_analysis$n = 4 (approx). totalNumerOfSubjects = 9 (approx)

# 7

power_analysis = pwr.t.test(d = hedges_g, sig.level = 0.05, power = 0.95, type = "paired", alternative = "two.sided")
power_analysis

totalNumerOfSubjects=2*power_analysis$n
totalNumerOfSubjects

# power_analysis$n corresponds to number of subject for one group. power_analysis$n = 60 (approx). totalNumerOfSubjects = 121 (approx)

# 8

samplesizes = seq(4, 400, by = 2)
powerValues = pwr.r.test(r = hedges_g, sig.level = 0.05, n = samplesizes, alternative = "two.sided")$power
x11();plot(samplesizes, powerValues)

#####################################################################################
#####################################################################################

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want, you may download the script and run it yourself to check if it works

#####################################################################################
#####################################################################################
