# 파일 경로 설정
Data1_PS_2020 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data1_PS_2020.txt"
Data1_PS_2021 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data1_PS_2021.txt"
Data1_PS_2022 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data1_PS_2022.txt"

# 텍스트 파일 읽기
Data1_PS_2020_read <- read.table(Data1_PS_2020, header = TRUE, sep = "\t")
Data1_PS_2021_read <- read.table(Data1_PS_2021, header = TRUE, sep = "\t")
Data1_PS_2022_read <- read.table(Data1_PS_2022, header = TRUE, sep = "\t")

# 년도 변수 추가
Data1_PS_2020_read$Year <- 2020
Data1_PS_2021_read$Year <- 2021
Data1_PS_2022_read$Year <- 2022

# 각 데이터셋 확인
print((Data1_PS_2020_read))
print((Data1_PS_2021_read))
print((Data1_PS_2022_read))

################################# 문제 1번 시작###################################################
##################################################################################################
##################################################################################################

# 2020년도 데이터에 대한 Sum_HW123_Midterm 계산
Sum_HW123_Midterm_2020 <- Data1_PS_2020_read$HW1 + Data1_PS_2020_read$HW2 + Data1_PS_2020_read$HW3 + Data1_PS_2020_read$Midterm

# 2021년도 데이터에 대한 Sum_HW123_Midterm 계산
Sum_HW123_Midterm_2021 <- Data1_PS_2021_read$HW1 + Data1_PS_2021_read$HW2 + Data1_PS_2021_read$HW3 + Data1_PS_2021_read$Midterm

# 2022년도 데이터에 대한 Sum_HW123_Midterm 계산
Sum_HW123_Midterm_2022 <- Data1_PS_2022_read$Sum_HW123_Midterm

# 결과 확인
print(Sum_HW123_Midterm_2020)
print(Sum_HW123_Midterm_2021)
print(Sum_HW123_Midterm_2022)

# 각 년도별로 Sum_HW123_Midterm의 하위 10% 데이터 선택
lower_10_2020 <- Sum_HW123_Midterm_2020[Sum_HW123_Midterm_2020 <= quantile(Sum_HW123_Midterm_2020, probs = 0.1)]
lower_10_2021 <- Sum_HW123_Midterm_2021[Sum_HW123_Midterm_2021 <= quantile(Sum_HW123_Midterm_2021, probs = 0.1)]
lower_10_2022 <- Sum_HW123_Midterm_2022[Sum_HW123_Midterm_2022 <= quantile(Sum_HW123_Midterm_2022, probs = 0.1)]

# 하위 10% 데이터를 모은 데이터프레임 생성
data_lower_10 <- data.frame(
  Year = c(rep(2020, length(lower_10_2020)), rep(2021, length(lower_10_2021)), rep(2022, length(lower_10_2022))),
  Average = c(lower_10_2020, lower_10_2021, lower_10_2022)
)

# One-way ANOVA 수행
result_lower_10 <- aov(Average ~ Year, data = data_lower_10)

# 문제 1번 ANOVA 결과를 summary 함수로 출력
anova_summary <- summary(result_lower_10)

# 문제 1번 P-value 출력
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
print(paste("The p-value of the ANOVA test is:", p_value))



################################# 문제 2번 시작##################################################
#################################################################################################
#################################################################################################

# 문제 2번 boxplot 생성
boxplot(Average ~ Year, data = data_lower_10, 
        xlab = "Year", ylab = "Average of Sum_HW123_Midterm", 
        main = "Comparison of Average Scores of Bottom 10% among Years",
        col = c("lightblue", "lightgreen", "lightpink"))




################################# 문제 3번 #####################################################
#################################################################################################
#################################################################################################

# 필요한 column만 선택하여 새로운 데이터프레임 생성
Data1_PS_2020_select <- Data1_PS_2020_read[, c("HW1", "HW2", "HW3", "Midterm", "Year")]
Data1_PS_2021_select <- Data1_PS_2021_read[, c("HW1", "HW2", "HW3", "Midterm", "Year")]
Data1_PS_2022_select <- Data1_PS_2022_read[, c("HW1", "HW2", "HW3", "Midterm", "Year")]

# 선택한 column으로 구성된 데이터프레임 합치기
data_all <- rbind(Data1_PS_2020_select, Data1_PS_2021_select, Data1_PS_2022_select)

# 각 항목에 대해 ANOVA 검정 수행
variables <- c("HW1", "HW2", "HW3", "Midterm")
for (var in variables) {
  result <- aov(as.formula(paste(var, "~ Year")), data = data_all)
  anova_summary <- summary(result)
  p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  print(paste("The p-value of the ANOVA test for", var, "is:", p_value))
}


################################# 문제 4번 ######################################################
#################################################################################################
#################################################################################################
# 필요한 column만 선택 "HW1"
Data1_PS_2020_select <- Data1_PS_2020_read[, c("HW1", "Year")]
Data1_PS_2021_select <- Data1_PS_2021_read[, c("HW1", "Year")]
Data1_PS_2022_select <- Data1_PS_2022_read[, c("HW1", "Year")]

# 선택한 column으로 구성된 데이터프레임 합치기
data_all <- rbind(Data1_PS_2020_select, Data1_PS_2021_select, Data1_PS_2022_select)

# HW1 점수에 대해 박스 플롯 그리기
boxplot(data_all$HW1 ~ data_all$Year, 
        xlab = "Year", ylab = "HW1", 
        main = "Comparison of HW1 Scores among Years",
        col = c("lightblue", "lightgreen", "lightpink"))

# 분포를 나타내는 점 추가하기
stripchart(data_all$HW1 ~ data_all$Year, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 20, 
           add = TRUE, 
           col = 'black')

# 필요한 column만 선택 "HW2"
Data1_PS_2020_select <- Data1_PS_2020_read[, c("HW2", "Year")]
Data1_PS_2021_select <- Data1_PS_2021_read[, c("HW2", "Year")]
Data1_PS_2022_select <- Data1_PS_2022_read[, c("HW2", "Year")]

# 선택한 column으로 구성된 데이터프레임 합치기
data_all <- rbind(Data1_PS_2020_select, Data1_PS_2021_select, Data1_PS_2022_select)

# HW1 점수에 대해 박스 플롯 그리기
boxplot(data_all$HW2 ~ data_all$Year, 
        xlab = "Year", ylab = "HW2", 
        main = "Comparison of HW2 Scores among Years",
        col = c("lightblue", "lightgreen", "lightpink"))

# 분포를 나타내는 점 추가하기
stripchart(data_all$HW2 ~ data_all$Year, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 20, 
           add = TRUE, 
           col = 'black')

# 필요한 column만 선택 "HW3"
Data1_PS_2020_select <- Data1_PS_2020_read[, c("HW3", "Year")]
Data1_PS_2021_select <- Data1_PS_2021_read[, c("HW3", "Year")]
Data1_PS_2022_select <- Data1_PS_2022_read[, c("HW3", "Year")]

# 선택한 column으로 구성된 데이터프레임 합치기
data_all <- rbind(Data1_PS_2020_select, Data1_PS_2021_select, Data1_PS_2022_select)

# HW1 점수에 대해 박스 플롯 그리기
boxplot(data_all$HW3 ~ data_all$Year, 
        xlab = "Year", ylab = "HW3", 
        main = "Comparison of HW3 Scores among Years",
        col = c("lightblue", "lightgreen", "lightpink"))

# 분포를 나타내는 점 추가하기
stripchart(data_all$HW3 ~ data_all$Year, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 20, 
           add = TRUE, 
           col = 'black')

# 필요한 column만 선택 "Midterm"
Data1_PS_2020_select <- Data1_PS_2020_read[, c("Midterm", "Year")]
Data1_PS_2021_select <- Data1_PS_2021_read[, c("Midterm", "Year")]
Data1_PS_2022_select <- Data1_PS_2022_read[, c("Midterm", "Year")]

# 선택한 column으로 구성된 데이터프레임 합치기
data_all <- rbind(Data1_PS_2020_select, Data1_PS_2021_select, Data1_PS_2022_select)

# HW1 점수에 대해 박스 플롯 그리기
boxplot(data_all$Midterm ~ data_all$Year, 
        xlab = "Year", ylab = "Midterm", 
        main = "Comparison of Midterm Scores among Years",
        col = c("lightblue", "lightgreen", "lightpink"))

# 분포를 나타내는 점 추가하기
stripchart(data_all$Midterm ~ data_all$Year, 
           vertical = TRUE, 
           method = "jitter", 
           pch = 20, 
           add = TRUE, 
           col = 'black')


################################# 문제 5번 ######################################################
#################################################################################################
#################################################################################################
# Grade를 숫자로 변환
Data1_PS_2021_read$GradeNum <- ifelse(Data1_PS_2021_read$Grade == "A+", 4.5,
                                      ifelse(Data1_PS_2021_read$Grade == "B+", 3.5,
                                             ifelse(Data1_PS_2021_read$Grade == "C+", 2.5,
                                                    ifelse(Data1_PS_2021_read$Grade == "C", 2.0,
                                                           ifelse(Data1_PS_2021_read$Grade == "D+", 1.5, 0)))))

# 선형 회귀 분석
model_2021 <- lm(GradeNum ~ Total, data = Data1_PS_2021_read)

# 회귀 계수 출력
print(summary(model_2021))

# Grade를 숫자로 변환
Data1_PS_2020_read$GradeNum <- ifelse(Data1_PS_2020_read$Grade == "A+", 4.5,
                                      ifelse(Data1_PS_2020_read$Grade == "B+", 3.5,
                                             ifelse(Data1_PS_2020_read$Grade == "C+", 2.5,
                                                    ifelse(Data1_PS_2020_read$Grade == "C", 2.0,
                                                           ifelse(Data1_PS_2020_read$Grade == "D+", 1.5, 0)))))

# 선형 회귀 분석
model_2020 <- lm(GradeNum ~ Total, data = Data1_PS_2020_read)

# 회귀 계수 출력
print(summary(model_2020))

# 2021년 모델을 이용한 예측
predicted_GradeNum_2021 <- model_2021$coefficients[1] + model_2021$coefficients[2] * 60
print(predicted_GradeNum_2021)

# 2020년 모델을 이용한 예측
predicted_GradeNum_2020 <- model_2020$coefficients[1] + model_2020$coefficients[2] * 60
print(predicted_GradeNum_2020)



################################# 문제 6번 ######################################################
#################################################################################################
#################################################################################################

# 파일 경로 설정
Data2 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data2.txt"

# 텍스트 파일 읽기
Data2_read <- read.table(Data2, header = TRUE, sep = "\t")

# 데이터 확인 및 출력
print(Data2_read)

# Kim의 검정 통계량을 계산하는 함수
kimStatFunc <- function(x, y) {
  abs(mean(x) - mean(y)) / ((sd(x) + sd(y)) / 8)
}

# 원래의 데이터를 사용하여 Kim의 검정 통계량 계산
obs_kimStat <- kimStatFunc(Data2_read[Data2_read$Gender == "Female", "numTardy"], 
                           Data2_read[Data2_read$Gender == "male", "numTardy"])

# 순열 검정을 위한 null 분포 생성
null_kimStat <- c()
numOfRepeat <- 1000

for(i in 1:numOfRepeat){
  shuffled_gender <- sample(Data2_read$Gender)
  null_kimStat[i] <- kimStatFunc(Data2_read[shuffled_gender == "Female", "numTardy"], 
                                 Data2_read[shuffled_gender == "male", "numTardy"])
}

# p-value 계산
Pval <- (sum(abs(null_kimStat) >= abs(obs_kimStat)) + 1) / (numOfRepeat + 1)
print(Pval)


################################# 문제 7번 ######################################################
#################################################################################################
#################################################################################################

# 파일 경로 설정
Data2 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data2.txt"

# 텍스트 파일 읽기
Data2_read2 <- read.table(Data2, header = TRUE, sep = "\t")

# 데이터 확인 및 출력
print(Data2_read2)


# wilcox.test 검정 수행
test_result <- wilcox.test(numTardy ~ Gender, data = Data2_read2)

# 결과 출력
print(test_result)


################################# 문제 8번 ######################################################
#################################################################################################
#################################################################################################
# 파일 경로 설정
Data2 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data2.txt"

# 텍스트 파일 읽기
Data2_read3 <- read.table(Data2, header = TRUE, sep = "\t")

# 데이터 확인 및 출력
print(Data2_read3)

# 두 독립 표본 t-검정 수행 (등분산 가정)
test_result <- t.test(numTardy ~ Gender, data = Data2_read3, var.equal = TRUE)

# 결과 출력
print(test_result)


################################# 문제 10번 #####################################################
#################################################################################################
#################################################################################################

# 파일 경로 설정
Data3 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data3.txt"

# 텍스트 파일 읽기
Data3_read <- read.table(Data3, header = TRUE, sep = "\t")

# 연속형 변수 리스트
cont_vars <- c('Age', 'Height_CM', 'Weight_KG', 'sysBP', 'HR', 'Resting_SaO2', 'BMI', 'FEV1pp_utah', 'FVCpp_utah', 'FEV1_FVC_utah')

# 결과를 저장할 빈 데이터프레임 생성
result <- data.frame(Variable = character(), P_value = numeric())

# 각 연속형 변수에 대해 독립 표본 t-검정 수행
for (var in cont_vars) {
  t_test <- t.test(as.formula(paste(var, "~ Gender")), data = Data3_read)
  result <- rbind(result, data.frame(Variable = var, P_value = t_test$p.value))
}

# p-value에 따라 정렬
result <- result[order(result$P_value), ]

# 결과 출력
print(result)

################################# 문제 11번 #####################################################
#################################################################################################
#################################################################################################

# 파일 경로 설정
Data3 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data3.txt"

# 텍스트 파일 읽기
Data3_read <- read.table(Data3, header = TRUE, sep = "\t")

# 결과를 저장할 빈 데이터프레임 생성
result_nonparam <- data.frame(Variable = character(), P_value = numeric())

# 각 연속형 변수에 대해 wilcox.test 검정 수행
for (var in cont_vars) {
  mw_test <- wilcox.test(as.formula(paste(var, "~ Gender")), data = Data3_read)
  result_nonparam <- rbind(result_nonparam, data.frame(Variable = var, P_value = mw_test$p.value))
}

# p-value에 따라 정렬
result_nonparam <- result_nonparam[order(result_nonparam$P_value), ]

# 결과 출력
print(result_nonparam)

################################# 문제 12번 #####################################################
#################################################################################################
#################################################################################################
# 파일 경로 설정
Data3 <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/HomeWork3/Data3.txt"

# 텍스트 파일 읽기
Data3_read <- read.table(Data3, header = TRUE, sep = "\t")

# 연속형 변수 리스트
cont_vars <- c('Age', 'Height_CM', 'Weight_KG', 'sysBP', 'HR', 'Resting_SaO2', 'BMI', 'FEV1pp_utah', 'FVCpp_utah', 'FEV1_FVC_utah')

# 결과를 저장할 빈 데이터프레임 생성
result <- data.frame(Variable = character(), P_value = numeric())

# 각 연속형 변수에 대해 Kruskal-Wallis H 검정 수행
for (var in cont_vars) {
  # 데이터에 NA 값이 있을 경우, KW test가 수행되지 않으므로 NA를 제거
  data_temp <- Data3_read[!is.na(Data3_read[,var]), c('Severity_Group', var)]
  kw_test <- kruskal.test(as.formula(paste(var, "~ Severity_Group")), data = data_temp)
  result <- rbind(result, data.frame(Variable = var, P_value = kw_test$p.value))
}

# p-value에 따라 정렬
result <- result[order(result$P_value), ]

# 결과 출력
print(result)
















