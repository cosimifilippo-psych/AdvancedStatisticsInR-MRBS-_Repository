

#################################### FILIPPO COSIMI – s1170314
#################################### HOMEWORK 2: ####################################
# Creating and manipulating dataframes in R
# due September 22, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 2)

################## PART A ##################

rm(list=ls()); graphics.off()
setwd("C:/Users/Me/Homework/WD")

install.packages("reshape")
library(reshape)
library(foreign)
library(lattice)
dfpar= read.spss("parenting.sav", use.value.labels = T, to.data.frame = T)

str(dfpar)

dfpar$Psychostd=dfpar$Psycho-mean(dfpar$Psycho)

density(dfpar$Psychostd)
x11(); densityplot(dfpar$Psychostd)

dfpar$SQPsychostd=dfpar$Psychostd^2
VARPsycho=sum(dfpar$SQPsychostd)/length(dfpar$SQPsychostd)

VARPsycho

var(dfpar$Psycho)


############# Reshaping DF #############

df=read.csv("Rating_ExampleData.csv")
df
df$f_gender=factor(df$f_gender)
str(df)
dfstack=stack(df, select = c("rating_1","rating_2","rating_3"))
unstack(dfstack)

dfmelt <- melt(df, id = c('pp_code', 'f_gender'), measured = c('rating_1', 'rating_2', 'rating_3'))
str(dfmelt)

dfcast=cast(dfmelt, formula=pp_code+f_gender~variable)
dfcast

names(dfmelt)[c(3,4)]=c("ratings","value")

dfreshaped=reshape(df, idvar=c("pp_code","f_gender"), varying = c("rating_1","rating_2","rating_3"), timevar = "variable", v.names="value", direction="long")
str(dfreshaped)
head(dfreshaped)

dfwide=reshape(dfreshaped, idvar=c("pp_code", "f_gender"), timevar = "variable", v.names = "value", direction="wide")
dfwide

?subset()

subset(dfmelt,subset=variable!="rating_1",select = c("variable","value"))


################## PART B ##################

# 1
rm(list=ls()); graphics.off() # clear work environment and reset graphic settings (it’s to ensure c	correct functioning of function x11())
setwd("C:/Users/Me/Homework/WD")

# install.packages("reshape")

library(reshape)
library(foreign)
library(lattice)
dfpar= read.spss("parenting.sav", use.value.labels = T, to.data.frame = T)
str(dfpar)

# 2
mean(dfpar$SES); median(dfpar$SES); 
min(dfpar$SES); max(dfpar$SES);
range(dfpar$SES)
IQR(dfpar$SES)

# 3

dfpar$SESstd=dfpar$SES-mean(dfpar$SES)

descvarSES=sum(dfpar$SESstd^2)/length(dfpar$SES)
descvarSES

infvarSES=sum(dfpar$SESstd^2)/(length(dfpar$SES)-1)
infvarSES

descSD_SES=sqrt(descvarSES); descSD_SES
infSD_SES=sqrt(infvarSES); infSD_SES

# 4

var(dfpar$SES)
sd(dfpar$SES)

# 5
set.seed(800)

randomSample=sample(dfpar$PARTNO, 30) # to extract 30 random subjects from the dfpar dataset (the whole set is too big) 
newdf=subset(dfpar, subset = PARTNO %in% randomSample, select = c("PARTNO", "Gender", "SES")) # new 	dataframe of 30 rows w/ demographic vars “PARTNO”, “Gender”, “SES”. I’ve taken them from the 	dataset used before for practicality (I’d rather recycle the data I already have, rather than write 	new data by hand)
newdf$measurement_1=rnorm(30,0,1) # simulation of the measurement of a random varible w/ standardized normal distr. (size = 30)
newdf$measurement_2=rnorm(30,0,1)
newdf$measurement_3=rnorm(30,1,1) # let's hypothesize that on the 3rd measurement the distribution's 	mean changes due to the effect of an event in between measurement 2 and 3.

newdf

# 6

# install.packages("ggpplot")
# install.packages("ggthemes")

library(ggplot2)
options(scipen=5)

# the following steps w/ “ # ** ” include the mass distribution of the observed data on top of the density distribution (added just to demonstrate the APA style graphics)
# Lines w/ ** can be skipped

dens=density(newdf$SES) # **
interpolMassSES=approx(dens$x,dens$y,xout = newdf$SES) # **
interpolMassSES_DF=data.frame(interpolMassSES$x,interpolMassSES$y); names(interpolMassSES_DF)=c("SESx","SESy") # **

SESplot=ggplot() + # ggplot() object creation
  geom_density(data = newdf, mapping = aes(x=SES, color = "Density distribution"), fill = "antiquewhite", linewidth = 1.25, alpha=0.5) + # draws density plot (density distr.) of SES var in 	newdf. Fill color of area under the curve = “antiquewhite”
  geom_line(data = interpolMassSES_DF, mapping = aes(x=SESx, y=SESy, color = "Observed data"), linewidth = 1, alpha = 0.75) + # lineplot w/ observed data (mass distr.) # **
  geom_point(data = interpolMassSES_DF, mapping = aes(x=SESx, y=SESy, color = "Observed data"), color= "black", size= 2.5) # lineplot's dots (highlight observations) # **
  scale_color_manual(values = c("Density distribution" = "royalblue4", "Observed data" = "orangered3")) # set legend

APAgraphics=
  ggthemes::theme_clean(base_size = 30, base_family = "sans") + # set plot graphic style, set font size 	and family (default sans-serif in ggplot should be Helvetica - as used in "APA_sample-figures.docx 	document")
  theme(axis.title.y = element_text(face = "bold", margin = margin(r = 20)), axis.title.x = element_text(face = "bold", margin = margin(t = 20))) + # set axis labels margins
  theme(plot.margin = unit(c(0.5,1,0.5,1), "cm")) + # set margins of plot from window (just because it 	looks better)
  theme(legend.title = element_blank(), legend.background = element_rect(color = "black", linewidth = 1))

x11(); SESplot + # display the plot in a new window
  xlim(min(newdf$SES)-50,max(newdf$SES)+50) + # set range of axis x
  xlab("Socioeconomic Status (SES)") + ylab("Density") + # set axis labels
  APAgraphics +
  theme(legend.position = c(0.85,0.75)) # set legend position # **

rm(dens, interpolMassSES) # clean up

# if desired font is not in ggplot, it can be imported - following function is only for Windows OS:
# windowsFonts(Helvetica=windowsFont("Helvetica Neue (Body)"), Times=windowsFont("Times New Roman")) # import font used in APA_sample-figures.docx (Helvetica Neue (Body))


# 7

# APAgraphics, library and options have already been set in previous exercise, so no need to repeat those steps here

measPLOT_1=ggplot() +
  geom_boxplot(data = newdf, mapping = aes(x="Measurement 1",y=measurement_1, fill = "Sample from population A")) + # I generated the data in such a way that mu_Measurement 1 = mu_Measurement 2 != 	mu_Measurement 3, see below (*)
  geom_boxplot(data = newdf, mapping = aes(x="Measurement 2",y=measurement_2, fill = "Sample from population A")) +
  geom_boxplot(data = newdf, mapping = aes(x="Measurement 3",y=measurement_3, fill = "Sample from population B")) + # *The boxplot should reflect this important bit of information
  scale_fill_manual(values=c("cornsilk", "rosybrown"))

x11(); measPLOT_1 +
  ylab("Density") + # set axis labels
  theme(axis.title.x = element_blank()) +
  APAgraphics +
  theme(axis.title.x = element_blank(),legend.position = c(0.5,0.85))

# 8

# reshape()

# install.packages("reshape")
library(reshape)

newdf_melted=reshape(newdf, idvar = c("PARTNO","Gender"), varying = c("SES","measurement_1","measurement_2","measurement_3"), timevar = "variable", v.names = "value", direction = "long" )

rownames(newdf_melted)=NULL; # reshape() messes up the rows' names, this line fixes the issue.
newdf_melted$variable=as.character(newdf_melted$variable)

# melt()

newdf_melted=melt(newdf, id = c("PARTNO","Gender"), measured = c("SES","measurement_1","measurement_2","measurement_3"))


# NOTE: the code in Q9 returns an error if the dataset has been melted through reshape(), since I find melt() function more practical I simply stuck to the dataframe melted through melt() and didn’t bother fixing the problem for reshape().


# 9

meltPLOT=
  ggplot(data = newdf_melted[-which(newdf_melted$variable %in% "SES"),-c(1,2)], mapping = aes(x=value, fill=variable)) + # data = (newdf_melted, but only rows where “variable” == “SES” and all cols except for the first and the second) 
  geom_density(alpha = 0.5) + # draws the density of the data declared in ggplot, with 50% transparency
  geom_vline(xintercept=mean(newdf_melted[which(newdf_melted$variable %in% "measurement_1"),"value"]), colour="pink") + # draws vertical line with x value = mean of the variable “measurement 1”
  geom_vline(xintercept=mean(newdf_melted[which(newdf_melted$variable %in% "measurement_2"),"value"]), colour="lightgreen") + # draws vertical line with x value = mean of the variable “measurement 2”
  geom_vline(xintercept=mean(newdf_melted[which(newdf_melted$variable %in% "measurement_3"),"value"]), colour="lightblue") # draws vertical line with x value = mean of the variable “measurement 3”

x11(); meltPLOT +
  xlim(min(newdf_melted[-which(newdf_melted$variable %in% "SES"),"value"])-2,max(newdf_melted[-which(newdf_melted$variable %in% "SES"),"value"])+2) + # set range of axis so that x goes from min of varible “SES” - 2 to max of variable “SES” + 2. (The +- 2 is arbitrary, it just looks right this way)
  APAgraphics +
  ylab("Density") + # set axis y label
  theme(axis.title.x = element_blank(),legend.position = c(0.85,0.85)) # erases x axis label and sets legend position
    
# 10

str(newdf_melted)
newdf_copy=cast(newdf_melted, formula = PARTNO + Gender ~ variable, value = newdf_melted$value)

# Bonus

write.csv(newdf, file = 'newdf_HW2.csv', row.names = FALSE)
write.csv(newdf_melted, file = 'newdf_melted_HW2.csv', row.names = FALSE)


