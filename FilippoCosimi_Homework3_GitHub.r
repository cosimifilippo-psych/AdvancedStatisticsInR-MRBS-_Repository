
# Lecture Notes ####################################

install.packages("pastecs")
library(pastecs)
stat.desc()
# 
# Description
# Compute a table giving various descriptive statistics about the series in a data frame or in a single/multiple time series
# 
# Usage
# stat.desc(x, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

complete.cases() # returns inverse values as na.omit()

install.packages("car")
library(car)
recode()
# 
# Description
# Recodes a numeric vector, character vector, or factor according to simple recode specifications. Recode is an alias for recode that avoids name clashes with packages, such as Hmisc, that have a recode function.
# 
# Usage
# recode(var, recodes, as.factor, as.numeric=TRUE, levels, 
#        to.value="=", interval=":", separator=";")




#################################### HOMEWORK 3: ####################################
# Data management and visualizing data with ggplot2
# due September 29, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 3)

# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want you may download the script and run it to check if it works


################## PART A ##################

# 1

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure c	correct functioning of function x11())
setwd("C:/documents/myWD")
getwd()

df=read.csv("newdf_melted_HW2.csv", sep=",", header = T)

# selecting subset of df w/ PARTNO id (to keep track of subjects) and the values of SES, and saving them on separate dataframe (dfSES) - need it in ifelse chain below:
dfSES=subset(df, subset = df$variable == "SES", select = c("PARTNO","value"))

# dividing subjects into 4 categories w/ series of nested elseif which check value of income and return 1 of 4 socioeconomic status labels:
# x < mean - sd ==> "low_income" (below -1 sd: I = ]-inf, -1sd[ )
# mean - sd <= x < mean ==> "mid-low_income" (between -1 sd and mean: I = [- 1sd, mean[ )
# mean <= x < mean + sd ==> "mid-high_income" (between mean and +1 sd: I = [mean, +1sd[ )
# x > mean + sd ==> "high_income" (over +1 sd: I = ]+1sd, + inf[ )

# w/ x = value of SES, mean = mean of SES, sd = standard deviation of SES, I = interval of f(x)

dfSES$SESlvl=ifelse(dfSES$value<(mean(dfSES$value)-sd(dfSES$value)),"low_income",
                    ifelse(dfSES$value<(mean(dfSES$value)) & dfSES$value>=(mean(dfSES$value)-sd(dfSES$value)),"mid-low_income",
                           ifelse(dfSES$value>=(mean(dfSES$value)) & dfSES$value<(mean(dfSES$value)+sd(dfSES$value)),"mid-high_income",
                                  ifelse(dfSES$value>(mean(dfSES$value)+sd(dfSES$value)),"high_income",NA))))

dfSES$SESlvl=factor(dfSES$SESlvl, levels = c("low_income", "mid-low_income", "mid-high_income", "high_income"))
dfSES

df$SESlvl=rep(NA, length(df$PARTNO)) # making the var here beacause making it directly in the for loop returns error. I assign NA for the moment

# make a for loop that looks for the matching PARTNO values in the two dataframe and assigns to df$SESlvl the corresponding value in the dfSES$SESlvl factor 
# i made it a for loop to make shure the algorithm is generalized and can be applied to any scenario (any vartype, any df size)

# a few tips to interpret the code:
# index i (for() loop index) corresponds to number level of factor variable in dfSES$SESlvl, thus:
#     1 = "low_income", 2 = "mid-low_income", 3 = "mid-high_income", 4 = "high_income"
# each for() cycle rewrites the value of df$SESlvl corresponding to the "i" level in dfSES$SESlvl, thus:
#     cycle 1 writes "low_income" in each df$SESlvl "slot" where the id of df and dfSES match,
# cycle 2 writes "mid-low_income" in each df$SESlvl "slot" where the id of df and dfSES match,
# cycle 3 writes "mid-high_income" in each df$SESlvl "slot" where the id of df and dfSES match,
# cycle 4 writes "high_income" in each df$SESlvl "slot" where the id of df and dfSES match,


for (i in 1:length(levels(dfSES$SESlvl))) {
  
  SESlvl_chrContent=paste0(levels(dfSES$SESlvl)[i])
  df$SESlvl[which(df$PARTNO %in% dfSES$PARTNO[dfSES$SESlvl==levels(dfSES$SESlvl)[i]])]=SESlvl_chrContent
  
}

df$SESlvl=factor(df$SESlvl, levels = c("low_income", "mid-low_income", "mid-high_income", "high_income"))
df$Gender=factor(df$Gender)
str(df)
# if successful, no need of dfSES anymore. To clean up:
rm(dfSES, i, SESlvl_chrContent)

# and rearrange cols, cuz i like it better when all the categorical vars are on one side:
df=df[,c(1,2,5,3,4)]


# 2

# checked the following on console:

# > length(which(df$SESlvl=="low_income"))
# [1] 32
# > length(which(df$SESlvl=="mid-low_income"))
# [1] 24
# > length(which(df$SESlvl=="mid-high_income"))
# [1] 52
# > length(which(df$SESlvl=="high_income"))
# [1] 12

# low and mid-high are the ranges with highest number of subjects so i'll keep those in my new df to have as many subjs as possible

df1=subset(df, subset = df$SESlvl == c("low_income","mid-high_income")) # leaving par "select" empty because i want all the cols
df1$SESlvl=droplevels(df1$SESlvl)

################## PART B ##################

# 3
# install.packages("ggplot2")
# install.packages("ggthemes")
library(ggplot2)
library(ggthemes)


mpgDF=mpg # saving it in my environment because i work with Rstudio and i like to have all the objects i work with in the environment window.
str(mpgDF)
head(mpgDF)
tail(mpgDF)
mpgDF$drv[which(mpgDF$drv!="f")]
mpgDF$fl[which(mpgDF$fl!="f")]

mpgDF$manufacturer=factor(mpgDF$manufacturer)
mpgDF$drv=factor(mpgDF$drv)
mpgDF$fl=factor(mpgDF$fl) # just because.

# install.packages("papaja")

# set APA graphics
APAgraphics=
  papaja::theme_apa(base_size = 14, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(text = element_text(face = "bold"), plot.margin = unit(c(0.25,0.5,0.25,0.5), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(legend.background = element_rect(color = "black",fill = "gray90"))

# city mileage
mpgDF_cty_manufacturer_boxplot=
  qplot(cty,manufacturer, data = mpgDF, geom = "boxplot", fill=manufacturer, ylab = "Manufacturer", xlab = "City mileage (miles per gallon)") +
  APAgraphics + # include graphics parameters
  labs(fill = "Manufacturers") # set legend title

x11(); mpgDF_cty_manufacturer_boxplot

# highway mileage
mpgDF_hwy_manufacturer_boxplot=
  qplot(hwy,manufacturer, data = mpgDF, geom = "boxplot", fill=manufacturer, ylab = "Manufacturer", xlab = "Highway mileage (miles per gallon)") +
  APAgraphics + # include graphics parameters
  labs(fill = "Manufacturers") # set legend title

x11(); mpgDF_hwy_manufacturer_boxplot

ggsave("mpgDF_cty_manufacturer_boxplot.jpg",mpgDF_cty_manufacturer_boxplot, width = 22, height = 16, units = "cm")
ggsave("mpgDF_hwy_manufacturer_boxplot.jpg",mpgDF_hwy_manufacturer_boxplot, width = 22, height = 16, units = "cm")

# 4

str(mpgDF)
levels(mpgDF$manufacturer) # check names of manufacturers

mpgsmall=mpgDF[which(mpgDF$manufacturer %in% c("volkswagen", "ford", "honda", "toyota")),]
levels(mpgsmall$manufacturer) # check names of manufacturers
mpgsmall$manufacturer=droplevels(mpgsmall$manufacturer)
str(mpgsmall)


# 5

x11(); qplot(cty, data = mpgsmall, geom = "density", color=manufacturer, ylab = "Density", xlab = "City mileage (miles per gallon)") +
  labs(fill="Manufacturers") +
  APAgraphics

# with current APAgraphics settings the histograms would look confusing, i'll set a box on each hist to separate it from the neighbour:
APAgraphics$panel.border=ggplot2::element_rect(color = "black", linewidth = 1) # change panel.border element in APAgraphics to give box to hist

mpgsmall_cty_manufacturer_hist=qplot(cty, data = mpgsmall, geom = "histogram", ylab = "Absolute frequency", xlab = "City mileage (miles per gallon)", bins = 15, facets = .~manufacturer) + # limited number of bins to give less accurate, but more meaningful results.
  APAgraphics

x11(); mpgsmall_cty_manufacturer_hist

# if desired:
APAgraphics$panel.border=ggplot2::element_blank() # to reset

ggsave("mpgsmall_cty_manufacturer_hist.jpg", mpgsmall_cty_manufacturer_hist, width = 22, height = 16, units = "cm")

# 6

mpgsmall_cty_hwy_scatter=qplot(cty, hwy, data = mpgsmall, geom = "point", ylab = "Highway mileage (miles per gallon)", xlab = "City mileage (miles per gallon)", color = manufacturer, shape = manufacturer) + # limited number of bins to give less accurate, but more meaningful results.
  labs(color = "Manufacturers", shape = "Manufacturers") +
  APAgraphics

x11(); mpgsmall_cty_hwy_scatter

ggsave("mpgsmall_cty_hwy_scatter.jpg", mpgsmall_cty_hwy_scatter, width = 22, height = 16, units = "cm")

# 7

APAgraphics= # let's modify APAgraphics for this ggplot
  ggthemes::theme_clean(base_size = 14, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(text = element_text(face = "bold"), plot.margin = unit(c(0.25,0.5,0.25,0.5), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(legend.background = element_rect(color = "black",fill = "gray90"))


mpgsmall_cty_hwy_scatter2=
  ggplot(data = mpgsmall, mapping = aes(cty, hwy)) +
  geom_point(aes(shape = manufacturer, color = manufacturer)) + 
  geom_smooth(aes(color = manufacturer, fill = manufacturer), method = "lm", formula = "y ~ x", alpha=0.25) +
  xlab("City mileage (miles per gallon)") + ylab("Highway mileage (miles per gallon)") +
  guides(colour = guide_legend("Car manufacturer"), shape = guide_legend("Car manufacturer"), fill = guide_legend("Car manufacturer")) +
  APAgraphics

x11(); mpgsmall_cty_hwy_scatter2

ggsave("mpgsmall_cty_hwy_scatter2.jpg", mpgsmall_cty_hwy_scatter2, width = 22, height = 16, units = "cm")


# 8

mpgsmall$class=factor(mpgsmall$class)
str(mpgsmall)

# a
mpgsmall_meanhwy_barplot=
  ggplot(data = mpgsmall, mapping = aes(class, hwy, fill = class)) +
  stat_summary(fun = mean, geom = "bar", color = "black", position = "dodge") +
  ylab("Highway mileage mean value (miles per gallon)") + xlab("") +
  labs(fill = "Vehicle class") +
  APAgraphics

x11(); mpgsmall_meanhwy_barplot

ggsave("mpgsmall_meanhwy_barplot.jpg", mpgsmall_meanhwy_barplot, width = 22, height = 16, units = "cm")

# b
mpgsmall_meanhwy_barplotERR=
  ggplot(data = mpgsmall, mapping = aes(class, hwy, fill = class)) +
  stat_summary(fun = mean, geom = "bar", color = "black", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", color = "black", position = position_dodge(width = 0.9), width = 0.2) +
  ylab("Highway mileage mean value (miles per gallon)") + xlab("") +
  labs(fill = "Vehicle class") +
  APAgraphics

x11(); mpgsmall_meanhwy_barplotERR

ggsave("mpgsmall_meanhwy_barplotERR.jpg", mpgsmall_meanhwy_barplotERR, width = 22, height = 16, units = "cm")


# 9

# install.packages("R.utils")
APAgraphics$plot.background@fill="seashell3" # change plot background colour

mpgsmall_meanhwy_barplot2=
  ggplot(data = mpgsmall, mapping = aes(class, hwy, fill = class)) +
  stat_summary(fun = mean, geom = "bar", color = "black", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", color = "black", position = position_dodge(width = 0.9), width = 0.2) +
  ylab("Highway mileage mean value (miles per gallon)") +
  scale_fill_manual(labels = c(R.utils::capitalize(levels(mpgsmall$class))),values = c("cadetblue4", "azure4", "orangered4", "yellow3", "orchid4")) +
  scale_x_discrete(labels = c(R.utils::capitalize(levels(mpgsmall$class))), name = "") +
  labs(fill = "Vehicle class") +
  APAgraphics

x11(); mpgsmall_meanhwy_barplot2

ggsave("mpgsmall_meanhwy_barplot2.jpg", mpgsmall_meanhwy_barplot2, width = 22, height = 16, units = "cm")


# 10
# install.packages("dplyr")
# install.packages("reshape")
library(dplyr)
library(reshape)

mpgsmall_DF = data.frame(as.integer(row.names(mpgsmall)),mpgsmall); names(mpgsmall_DF)[1]="id" # make df and id + correct name of var id
mpgsmall_DF = mpgsmall_DF %>% mutate_if(sapply(mpgsmall_DF, is.character), as.factor) # convert all char into factor

str(mpgsmall_DF)

mpgsmall_DF_melted=melt(mpgsmall_DF, id.vars = c("id",names(which(sapply(mpgsmall_DF, is.factor)))), measure.vars = c(names(which(sapply(mpgsmall_DF, is.factor)==F))[-1]), variable_name = "var", )

str(mpgsmall_DF_melted)

APAgraphics= # let's reset APAgraphics for this ggplot
  ggthemes::theme_clean(base_size = 18, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(text = element_text(face = "bold"), plot.margin = unit(c(0.5,1,0.5,1), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(legend.position = "top", plot.background = element_rect(fill = "seashell3"), legend.title = element_blank(), legend.background = element_rect(color = "black", fill = "snow3", linewidth = 1))


mpgsmall_cty_hwy_barplot=
  ggplot(subset(mpgsmall_DF_melted, subset = var == "hwy" | var == "cty", select = c("class","var","value")), aes(class, value, fill = var)) +
  stat_summary(fun = mean, geom = "bar", color = "black", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", color = "black", position = position_dodge(width = 0.9), width = 0.2) +
  ylab("Miles per gallon") +
  scale_fill_manual(labels = c("City mileage mean value", "Highway mileage mean value"), values = c("darkslategray4","tan4")) +
  scale_x_discrete(name = "", labels = c(R.utils::capitalize(levels(mpgsmall_DF_melted$class)))) +
  APAgraphics 

x11(); mpgsmall_cty_hwy_barplot

ggsave("mpgsmall_cty_hwy_barplot.jpg", mpgsmall_cty_hwy_barplot, width = 22, height = 16, units = "cm")

# Bonus

rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure c	correct functioning of function x11())
setwd("C:/documents/myWD")
getwd()

rDF=read.csv("Rating_ExampleData.csv", sep = ",", header = T)

# later i'll need to make a barplot confronting rating 1, 2 and 3, so i'll need a long DF, might as well melt it right now to avoid confusion later:
library(reshape)
names(rDF)[2]="gender"; rDF$gender=factor(rDF$gender) # quicker to type
rDFlong=melt(rDF, id=c("pp_code","gender"), measure.vars = c(names(rDF)[-c(1,2)]), variable_name = "var")
str(rDFlong)

# install.packages("ggrain")
library(ggrain)

rDFlong_rainplot=
  ggplot(data = rDFlong, mapping = aes(var, value, fill = var)) +
  geom_rain(alpha = 0.4, point.args = list(shape = 16, size = 2), boxplot.args.pos = list(width = 0.1, position = position_nudge(x = 0.1))) +
  ggthemes::theme_clean(base_size = 14, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(text = element_text(face = "bold"), plot.margin = unit(c(0.5,1,0.5,1), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(axis.title.y = element_blank(), legend.position = "top", plot.background = element_rect(fill = "seashell3"), legend.title = element_blank(), legend.background = element_rect(color = "black", fill = "snow3", linewidth = 1)) +
  scale_fill_manual(labels = c(paste0("Rating ",1:3)), values = c("aquamarine4", "brown4", "cadetblue3")) +
  scale_x_discrete(labels = c(paste0("Rating ",1:3))) +
  coord_flip() +
  ylab("Rating scores")

x11(); rDFlong_rainplot

ggsave("rDFlong_rainplot.jpg", rDFlong_rainplot, width = 16, height = 18, units = "cm")


rDFlong_barplot=
  ggplot(data = transform(rDFlong, pp_code=factor(pp_code, levels = paste0("pp_", 1:length(rDFlong$pp_code)))), mapping = aes(var, value, fill = var)) + # to display the pp_code in order i have to temporarily turn them into factors (using transform())
  geom_col() +
  facet_wrap(~pp_code) +
  theme_classic(base_size = 12, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(text = element_text(face = "bold"), plot.margin = unit(c(0.5,1,0.5,1), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(axis.title.x = element_blank(), legend.position = "top", plot.background = element_rect(fill = "seashell3"), legend.title = element_blank(), legend.background = element_rect(color = "black", fill = "snow3", linewidth = 1)) +
  ylab("Ratings") + 
  scale_fill_manual(labels = c("Neutral", "Happy", "Sad"), values = c("aquamarine4", "brown4", "cadetblue3")) +
  scale_x_discrete(labels = c("Neutral", "Happy", "Sad"))

x11(); rDFlong_barplot

ggsave("rDFlong_barplot.jpg", rDFlong_barplot, width = 25, height = 16, units = "cm")


# pdf and word formats mess with the text formatting, so to make the Homework review as smooth as possible I made the script available on a GitHub rep:

# https://github.com/cosimifilippo-psych/AdvancedStatisticsInR-MRBS-_Repository.git
# if you want you may download the script and run it to check if it works


