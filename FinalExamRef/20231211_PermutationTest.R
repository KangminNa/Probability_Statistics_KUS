##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-12-11                   ##
##################################



## Permutation t-test
Gender <- factor(c(rep("Male", 30), rep("Female", 30)))
Height <- c(rnorm(30, 173, 2), rnorm(30, 173, 2))

Data <- data.frame(Height, Gender)
dim(Data)

obs_t_stat <- as.numeric(t.test(Height ~ Gender, Data)$statistic)

null_t_stat <- c()
numOfRepeat <- 1000

for(i in 1:numOfRepeat){
  null_t_stat[i] <- as.numeric(t.test(Height ~ sample(Gender), Data)$statistic)
}

Pval <- (sum(abs(null_t_stat) >= abs(obs_t_stat)) + 1) / numOfRepeat
Pval