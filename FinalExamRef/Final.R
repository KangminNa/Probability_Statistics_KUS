####################
##   2022-12-19   ##
##   2019270664   ##
##     윤재현     ##
####################

## install library
setRepositories(ind = 1:7)
install.packages("ggplot2")
install.packages("devtools")
install.packages("data.table")
install.packages("ggpubr")

library(devtools)
install_github('jhk0530/Rstat')

## call library 
library(Rstat)
library(ggplot2)
library(ggpubr)
library(data.table)

## work dir setting
WORK_DIR <- "C:/Users/user/Desktop/Final_2019270664_윤재현"
setwd(WORK_DIR)
getwd()

## real work part 

#data load
data1_2020 <- data.frame(fread("Data1_PS_2020.txt", sep = "\t", head = T, stringsAsFactors = F))
data1_2021 <- data.frame(fread("Data1_PS_2021.txt", sep = "\t", head = T, stringsAsFactors = F))
data1_2022 <- data.frame(fread("Data1_PS_2022.txt", sep = "\t", head = T, stringsAsFactors = F))
data2 <- data.frame(fread("Data2.txt", sep="\t", head = T, stringsAsFactors = F))


## Q1-Q5 use data1_2020, 2021, 2022
#Q1, Q2, Q3, Q4
year = rep(0, time = length(data1_2020$HW1))
score = data1_2020$HW1
df_buffer1 <- data.frame(year, score)

year = rep(1, time = length(data1_2021$HW1))
score = data1_2021$HW1
df_buffer2 <- data.frame(year, score)

year = rep(2, time = length(data1_2022$HW1))
score = data1_2022$HW1
df_buffer3 <- data.frame(year, score)

df_compare <- rbind(df_buffer1, df_buffer2, df_buffer3)

summary(aov(score~year, data=df_compare))[[1]][5]
ggboxplot(df_compare, "year", "score", color="year",
          add = "jitter", main = "HW1")

year = rep(0, time = length(data1_2020$HW2))
score = data1_2020$HW2
df_buffer1 <- data.frame(year, score)

year = rep(1, time = length(data1_2021$HW2))
score = data1_2021$HW2
df_buffer2 <- data.frame(year, score)

year = rep(2, time = length(data1_2022$HW2))
score = data1_2022$HW2
df_buffer3 <- data.frame(year, score)

df_compare <- rbind(df_buffer1, df_buffer2, df_buffer3)

summary(aov(score~year, data=df_compare))[[1]][5]
ggboxplot(df_compare, "year", "score", color="year",
          add = "jitter", main = "HW2")


year = rep(0, time = length(data1_2020$HW3))
score = data1_2020$HW3
df_buffer1 <- data.frame(year, score)

year = rep(1, time = length(data1_2021$HW3))
score = data1_2021$HW3
df_buffer2 <- data.frame(year, score)

year = rep(2, time = length(data1_2022$HW3))
score = data1_2022$HW3
df_buffer3 <- data.frame(year, score)

df_compare <- rbind(df_buffer1, df_buffer2, df_buffer3)
summary(aov(score~year, data=df_compare))[[1]][5]

ggboxplot(df_compare, "year", "score", color="year",
          add = "jitter", main = "HW3")



year = rep(0, time = length(data1_2020$Midterm))
score = data1_2020$Midterm
df_buffer1 <- data.frame(year, score)

year = rep(1, time = length(data1_2021$Midterm))
score = data1_2021$Midterm
df_buffer2 <- data.frame(year, score)

year = rep(2, time = length(data1_2022$Midterm))
score = data1_2022$Midterm
df_buffer3 <- data.frame(year, score)

df_compare <- rbind(df_buffer1, df_buffer2, df_buffer3)
summary(aov(score~year, data=df_compare))[[1]][5]

ggboxplot(df_compare, "year", "score", color="year",
          add = "jitter", main = "Midterm")


##Q6~Q9
n <- length(data2$Gender)
data2$Gender <- replace(data2$Gender, data2$Gender=="Female", 1)
data2$Gender <- replace(data2$Gender, data2$Gender=="male", 0)
data2$Gender <- as.integer(data2$Gender)

#Q6
male <- subset(data2, Gender="Male", select="numTardy")
Female <- subset(data2, Gender="Female", select ="numTardy")

x_bar = mean(male[,1])
y_bar = mean(Female[,1])
obsKimStat = abs(x_bar-y_bar) / (x_bar+y_bar/8)


#Q7
wilcox.test(numTardy~Gender, data=data2, exact = FALSE)

#Q8
t.test(numTardy~Gender, data=data2)$p.value

#Q9
