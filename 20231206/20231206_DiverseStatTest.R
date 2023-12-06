##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-12-06                   ##
##################################


## ANOVA and kruskal test for Multiple group comparisons
## Kruskal Model is get 3group so not used t-test(because t-test using condition is 2 group and var...etc)
## This Example is that ToothGrowth is 상관관계 orange jucie and VC(what? i dont know) 
## 이는 t-test를 사용할 수 없기 때문에 Kruscal을 사용했는데 사용한 이유를 알고 t-test를 사용할 수 없었던 이유를 해당 데이터를 통해 알아야함
data("ToothGrowth")
View(ToothGrowth)
str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose)

hist(ToothGrowth$dose)
summary(ToothGrowth$supp)

summary(aov(len~dose, data = ToothGrowth))[[1]][1,5]

boxplot(len~dose, data = ToothGrowth)

combindValue

GroupA <- norm(20, 100, sd = 4)
GroupB <- norm(20, 100, sd = 4)
GroupC <- norm(20, 100, sd = 4)

testData.frame(combindValue, Group = factor(rep(c("phase1","phase2","phase3"), each = 20)))

boxplot((combinValue ~ Group, data = testData))

kruskal.test(len ~ dose, data = ToothGrowth)$p.value


## Association test for two continuous Random variables
data(mtcars)

A <- mtcars$mpg
B <- mtcars$wt


cor.test(mtcars$mpg, mtcars$wt, method = "pearson")$p.value
cor.test(mtcars$mpg, mtcars$wt, method = "spearman")$p.value




summary(lm(mpg~wt, data = mtcars))$coef[2,4]



## Association test for two cateogorical Random variables
data <- read.csv("https://goo.gl/j6lRXD")  #Reading CSV
table(data$treatment, data$improvement)

chisq.test(data$treatment, data$improvement)$p.value
fisher.test(data$treatment, data$improvement)$p.value


## Examples

Disease <- factor(c(rep("Covid-19", 50), rep("Normal", 50)))
Vaccine <- factor(c(rep("Shot", 50), rep("None", 50)))
Gender <- factor(rep(c(rep("Female", 25), rep("Male", 25)), 2))

table(Disease, Vaccine)
chisq.test(Disease, Vaccine)$p.value
fisher.test(Disease, Vaccine)$p.value

table(Disease, Gender)
chisq.test(Disease, Gender)$p.value
fisher.test(Disease, Gender)$p.value

############# 수업 내용##########################################


