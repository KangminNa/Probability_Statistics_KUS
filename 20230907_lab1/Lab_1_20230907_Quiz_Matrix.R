#########################################
## Quiz to help you understand         ##
## WarmUp_R_Programming
## for Probability & Statistics        ##
## Kangmin Na                          ##
## 2023-09-07                          ##
#########################################

# 빈 행렬 생성
my_matrix <- matrix(nrow = 20, ncol = 2)
colnames(my_matrix) <- c("gender", "height")

#  In Gender variable, first 10 samples are Male.
my_matrix[1:10, 1] <- "Male"

# Last 10 samples are Female.
my_matrix[11:20, 1] <- "Female"

# Male’s height should be generated from Normal distribution with mean = 173 and sd = 2.
# 남성(Male)의 키는 평균이 173이고 표준편차가 2인 정규 분포에서 생성되어야 합니다.
my_matrix[1:10, 2] <- rnorm(10, mean = 173, sd = 2)

# Female’s height should be generated from Normal distribution with mean = 162.3 and sd = 2.
# 여성(Female)의 키는 평균이 162.3이고 표준편차가 2인 정규 분포에서 생성되어야 합니다.
my_matrix[11:20, 2] <- rnorm(10, mean = 162.3, sd = 2)

## Get Descriptive Statistices
summary(my_matrix)
my_matrix[,2]
sd(my_matrix[,2])
median(my_matrix[,2])

mean(heightMale)
mean(heightFemale)
sd(heightMale)
sd(heightFemale)
