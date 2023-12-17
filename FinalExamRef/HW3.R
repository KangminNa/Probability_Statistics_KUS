############################
# Probability & Statistics #
#       Assignment 2       #
#        2019270664        #
#         윤 재 현         #
############################

## Load library
library(Rstat)
library(data.table)
library(ggpubr)

## data load
WORK_DIR <- "F:/자료파일/고려대(세종)/2학년 2학기/확률및통계(영강)/Assignment/Assignment3"
setwd(WORK_DIR)
data1 <- data.frame(fread("Data1.txt", sep = "\t", head = T, stringsAsFactors = F))
data2 <- data.frame(fread("Data2.tsv", sep = "\t", head = T, stringsAsFactors = T))


## Q1~Q4 (data1)
dim(data1)
head(data1)

## Q1
n <- length(data1$Severity_Group)
data1$Severity_Group <- replace(data1$Severity_Group, data1$Severity_Group == "Healthy", 0)

for(i in 1:n) {
  if(data1$Severity_Group[i] != "0") {
    x <- strsplit(data1$Severity_Group[i], "COVID19_Phase")[[1]][2]
    data1$Severity_Group[i] <- x
  }
}
data1$Severity_Group <- as.integer(data1$Severity_Group)
data1$Severity_Group

## Q2
# two group test
t.test(Height_CM~Gender, data1, alternative="two.sided")$p.value
t.test(Height_CM~Race, data1, alternative="two.sided")$p.value
t.test(Height_CM~HaveCough, data1, alternative="two.sided")$p.value

#multi group test
summary(aov(Height_CM~Severity_Group, data = data1))[[1]][1, 5]

#continuous~continuous
cor.test(data1$Height_CM, data1$Age, method='pearson')$p.value
cor.test(data1$Height_CM, data1$Weight_KG, method='pearson')$p.value
cor.test(data1$Height_CM, data1$sysBP, method='pearson')$p.value
cor.test(data1$Height_CM, data1$HR, method='pearson')$p.value
cor.test(data1$Height_CM, data1$Resting_SaO2, method='pearson')$p.value
cor.test(data1$Height_CM, data1$BMI, method='pearson')$p.value
cor.test(data1$Height_CM, data1$FEV1pp_utah, method='pearson')$p.value
cor.test(data1$Height_CM, data1$FVCpp_utah, method='pearson')$p.value
cor.test(data1$Height_CM, data1$FEV1_FVC_utah, method='pearson')$p.value

## Q3
# two group test
wilcox.test(Height_CM~Gender, data = data1, exact = FALSE)$p.value
wilcox.test(Height_CM~Race, data = data1, exact = FALSE)$p.value
wilcox.test(Height_CM~HaveCough, data = data1, exact = FALSE)$p.value

#multi group test
kruskal.test(Height_CM~Severity_Group, data = data1)$p.value

#continuous~continuous
cor.test(data1$Height_CM, data1$Age, method='spearman')$p.value
cor.test(data1$Height_CM, data1$Weight_KG, method='spearman')$p.value
cor.test(data1$Height_CM, data1$sysBP, method='spearman')$p.value
cor.test(data1$Height_CM, data1$HR, method='spearman')$p.value
cor.test(data1$Height_CM, data1$Resting_SaO2, method='spearman')$p.value
cor.test(data1$Height_CM, data1$BMI, method='spearman')$p.value
cor.test(data1$Height_CM, data1$FEV1pp_utah, method='spearman')$p.value
cor.test(data1$Height_CM, data1$FVCpp_utah, method='spearman')$p.value
cor.test(data1$Height_CM, data1$FEV1_FVC_utah, method='spearman')$p.value

## Q4
#category~category
chisq.test(data1$Severity_Group, data1$Gender)
chisq.test(data1$Severity_Group, data1$Race)
chisq.test(data1$Severity_Group, data1$HaveCough)

#multi group test
summary(aov(Age~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(Height_CM~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(Weight_KG~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(sysBP~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(HR~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(Resting_SaO2~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(BMI~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(FEV1pp_utah~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(FVCpp_utah~Severity_Group, data=data1))[[1]][1, 5]
summary(aov(FEV1_FVC_utah~Severity_Group, data=data1))[[1]][1, 4]

## Q5~Q7 (data2)
dim(data2)
head(data2)
randomVariable <- colnames(data2)
Gen <- randomVariable[3:length(randomVariable)]

## Q5
Cancer <- which(data2$Disease == 'Cancer')
Healthy <- which(data2$Disease == 'Healthy')
length(Cancer)
length(Healthy)

significanceLevel = 0.01
differName <- c()
differValue <- c()
for(i in Gen) {
  sample1 = subset(data2, Disease == 'Cancer', select = i)
  sample2 = subset(data2, Disease == 'Healthy', select = i)
  pValue = wilcox.test(sample1[, 1], sample2[, 1], exact = FALSE)$p.value
  if(pValue < significanceLevel) {
    differName <- c(differName, i)
    differValue <- c(differValue, pValue)
  }
    
}
differName

## Q6
differGen <- data.frame(differName, differValue)

bottom <- c()
buffer = differGen
for(i in 1:5) {
  idx = which.max(buffer$differValue)
  bottom <- c(bottom, buffer[idx, 'differName'])
  buffer$differValue[idx] = 0
}
bottom

top <- c()
buffer = differGen
for(i in 1:5) {
  idx = which.min(buffer$differValue)
  top <- c(top, buffer[idx, 'differName'])
  buffer$differValue[idx] = 123456789
}
top

myComparision = c("Cancer", "Healthy")

#draw bottom
ggboxplot(data2, "Disease", "Gene_880", color = "Disease",
          add = "jitter", main = "Gene_880")
ggboxplot(data2, "Disease", "Gene_1249", color = "Disease",
          add = "jitter", main = "Gene_1249")
ggboxplot(data2, "Disease", "Gene_914", color = "Disease",
          add = "jitter", main = "Gene_914")
ggboxplot(data2, "Disease", "Gene_12", color = "Disease",
          add = "jitter", main = "Gene_12")
ggboxplot(data2, "Disease", "Gene_44", color = "Disease",
          add = "jitter", main = "Gene_44")

#draw top
ggboxplot(data2, "Disease", "Gene_395", color = "Disease",
          add = "jitter", main = "Gene_395")
ggboxplot(data2, "Disease", "Gene_391", color = "Disease",
          add = "jitter", main = "Gene_391")
ggboxplot(data2, "Disease", "Gene_13", color = "Disease",
          add = "jitter", main = "Gene_13")
ggboxplot(data2, "Disease", "Gene_19", color = "Disease",
          add = "jitter", main = "Gene_19")
ggboxplot(data2, "Disease", "Gene_231", color = "Disease",
          add = "jitter", main = "Gene_231")

## Q7
differ <- c()
sample1 = subset(data2, select = "Height")
for(i in Gen) {
  sample2 = subset(data2, select = i)
  pValue = cor.test(sample1[ ,1], sample2[ ,1], method="pearson")$p.value
  if(pValue < significanceLevel)
    differ <- c(differ, i)
}
differ
