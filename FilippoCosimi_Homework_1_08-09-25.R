

#################################### HOMEWORK 1: ####################################
# Introduction to R
# due September 15, 2025 at 10:20 (upload to Pitch2Peer portal on Brightspace --> Week 1)

# NOTE: In the comments i usually make abbreviations:
#  - var = variable (vars = variables)
#  - w/ = with
#  -par = parameter
#  ....


    ################## PART A ##################

# installing R and Rstudio: DONE

    ################## PART B ##################

rm(list=ls()); graphics.off()
setwd("C:/Users/Vale/Documents/Documenti Importanti/Documenti Università/Università/Magistrale/Behavioural science - Radboud (Nijmegen)/LEZIONI/Advanced Statistics in R (SOW-BS086)/Period 1/Lesson Notes/WD")

# can use both console or script as a calculator:

2+2

#Ripasso Lezione 1 
a = 8; vec1=c(a,13,4,7,29,13); x11();hist(vec1); vec1

# ? or help() to check syntax or output of a function:
?x11()
?boxplot

vec1_b=rbinom(40,10,0.3)
x11(); hist(vec1_b, breaks = 5, col="red") # shows frequency of intervals of 2
x11(); hist(vec1_b, breaks = 10, col="lightgreen") # shows frequency of each mode
vec1_b
table(vec1_b)

x11(); hist(vec1, breaks=2)

#boxplot/plot
x11(); boxplot(vec1)
x11(); plot(vec1)

#New package for density plot
install.packages('lattice')
library(lattice)

x11();densityplot(vec1)
# descriptive indexes
mean(vec1)
median(vec1)
min(vec1)
max(vec1)
range(vec1)
summary(vec1)

####### INDEXING ####### 
# i = index of desired value in array, for example:

i=2
vec1
vec1[i]

# i can be invtervals or vectors:

vec1[3:5]
vec1[c(1,5,3)]

##### MAKE A MATRIX #####

matrix1 = matrix(NA,5,3)
matrix1

# turn it into a data frame:

df1=as.data.frame(matrix1)
df1
View(df1) #check
# matrix is no longer useful: clear up space:
rm(matrix1)

# assign column names and variable names:

names(df1)=c("pp_code","gender","age")

str(df1); df1

# assign var value:

df1$pp_code=c("pp01","pp02","pp03","pp04","pp05")
# or more efficient:
# df1$pp_code=paste0("pp0",1:5)

df1$gender = c("f","m","nb","f","f")
df1$age = c(19,3,28,22,9)

df1
head(df1)
tail(df1)

str(df1)
summary(df1)

df1$f_gender=as.factor(df1$gender)
str(df1)

df1[,2] # all rows, column: 2
df1[4,] # all cols, row: 4
df1[,] #same as df1 shows all values
df1[c(2,3),c("age","f_gender")]
# to find current working directory:

getwd()
# to set new WD: setwd("C:/Users/me/Documents/WD")


write.csv(df1, file="Mydf1.csv")

df2=read.csv("Mydf1.csv")

# if you don't need it anymmore, erase:

rm(df2)

#check it's gone:

ls()


x11(); demo(graphics)


    ################## PART C ##################

    #### 1.####

rm(list=ls()); graphics.off()
setwd("C:/Users/Vale/Documents/Documenti Importanti/Documenti Università/Università/Magistrale/Behavioural science - Radboud (Nijmegen)/LEZIONI/Advanced Statistics in R (SOW-BS086)/Period 1/Homework/HW1_08-09-25")
getwd()

# I use x11() to open a new graphic window on Windows OS

df=read.csv("pedometer.csv",header=T,sep=",") # header and sep should be already TRUE and "," but I specify to be shure


    #### 2.####

df
head(df,6)
tail(df,6)
str(df)

    #### 3.####

mean(df$Steps); sd(df$Steps)


    #### 4.####

# I use x11() to open a new graphic window (works only on Windows OS)

# boxplot of $Steps
x11(); boxplot(df$Steps)

# histogram w/ 10 breaks
myhist=hist(df$Steps, breaks=10, col="lightblue"); x11();myhist


    #### 5.####

# specify the levels when creating the new variable if you want the week days to be ordered properly:
df$f_Day=factor(df$Day, levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
x11();plot(df$f_Day)
# check absolute freq. table to make shure the plot is right:
table(df$f_Day)


    #### 6.####

# density plot of $Steps:
# install.packages("lattice") # keeping this line as comment, since the package is already installed on my device; erase "#" to run the line.
library("lattice")
x11(); densityplot(df$Steps)


    ################## PART D ##################

    #### 7.####

install.packages("foreign")
library(foreign)
#The file i need to open has a .sav extension ("parenting.sav"). I found through an online search .sav is the extension of SPSS data files, thus i'm looking for a function that allows me to open SPSS data files in Rstudio:
# I'm not familiar w/ package "foreign", thus:
help(package="foreign")  # to look for the function i need.

# thanks to "help(package="foreign")" i found a functions that reads SPSS files, it's called: read.spss:
# I'm also unfamiliar w/ the function, thus:
?read.spss # to understand its syntax and parameters
# first i check the file is in my WD and that the WD is set properly:
getwd()
#then

parDF=read.spss("parenting.sav", use.value.labels = T, to.data.frame = T); str(parDF)
View(parDF)
# parameter "use.value.labels" was set to TRUE, since: "[the parameter] convert[s] variables with value labels into R factors with those levels" meaning I need to set this parameter to TRUE to ensure that the categorical variables are imported in R as factor types, otherwise, they are imported as num types with a "value.labels" attribute (don't know what value.labels mean, but I just want factor type vars).
# parameter "to.data.frame" converts the SPSS list into a data frame. Thus i need to set it to TRUE.


    #### 8.####

head(parDF,6)
tail(parDF,6)


    #### 9.####
# (a)
x11();hist(parDF$Psycho, col="lightblue")
# (b)
x11();densityplot(parDF$Psycho)


  #### 10.####
# (a)
mean(parDF$Psycho)
# (b)
sd(parDF$Psycho)

    #### BONUS ASSIGNMENT.####
# don't remember how to erase row name in write.csv, thus:
?write.csv
# the parameter "row.names" can be used as logical to either include or erase the row.names:
write.csv(parDF, file="MyParenting.csv", row.names=F)



#################################### HOMEWORK 1 ####################################
####################################### END ####################################


################## OTHER EXERCISES I'VE DONE WHILE FOLLOWING "VIDEO 2" (https://www.youtube.com/watch?v=GcmorHXygoI)  ##################

# For example I might want to check the number of steps on one specific day of the week:
df$Steps[which(df$Day=="Fri")]
# or multiple days of the week:
df[c(which(df$Day==c("Fri")),which(df$Day==c("Tue"))),]


# plots $steps on y axis and $observation on x axis, useful to observe data and make fisrt hypothesis about possible correlation between vars plotted
x11();plot(df$Steps~df$Observation,col="red")
# creates a linear model:
LinearModel1=lm(df$Steps~df$Observation)
LinearModel1

summary(LinearModel1)

fitted(LinearModel1)
lines(fitted(LinearModel1))

