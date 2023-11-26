##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-11-22                   ##
##################################


## Self-Checking 3 (t-test)

# 데이터 생성, 성별 및 키
Gender <- factor(c(rep("남성", 100), rep("여성", 100)))
Height <- c(rnorm(100, 173, 2), rnorm(100, 162.3, 2))

# 성별과 키를 포함한 데이터 프레임 생성
Data <- data.frame(Height, Gender)

# 데이터 프레임의 차원 출력
dim(Data)

# 남성과 여성 간 평균 키의 차이에 대한 이분법 T-검정 수행
t_test_result <- t.test(Height ~ Gender, Data, alternative = "two.sided")
t_test_result

# 단측 T-검정 (낮은 쪽과 높은 쪽) 수행
t_test_less <- t.test(Height ~ Gender, Data, alternative = "less")
t_test_greater <- t.test(Height ~ Gender, Data, alternative = "greater")

# T-검정 결과 출력
t_test_less
t_test_greater


## 대응표본 T-검정
# 치료 전 쥐의 몸무게
before <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# 치료 후 쥐의 몸무게
after <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)

# 그룹 (before 또는 after) 및 몸무게를 포함한 데이터 프레임 생성
my_data <- data.frame(
  group = rep(c("before", "after"), each = 10),
  weight = c(before,  after)
)

# 치료 전과 후 몸무게의 차이에 대한 대응표본 T-검정 수행
paired_t_test_result <- t.test(before, after, paired = TRUE)
paired_t_test_result

# 또는 데이터 프레임을 사용하여 대응표본 T-검정 수행
paired_t_test_result_df <- t.test(weight ~ group, data = my_data, paired = TRUE)
paired_t_test_result_df


## 윌콕슨 순위 합 검정
# 남성과 여성 간 키의 차이에 대한 윌콕슨 순위 합 검정 수행
wilcox_test_result <- wilcox.test(Height ~ Gender, data = my_data, exact = FALSE)
wilcox_test_result


## 순열 T-검정
# 새로운 데이터를 생성하여 성별 및 키를 다시 정의
Gender <- factor(c(rep("남성", 30), rep("여성", 30)))
Height <- c(rnorm(30, 173, 2), rnorm(30, 173, 2))

# 성별과 키를 포함한 데이터 프레임 생성
Data <- data.frame(Height, Gender)

# 데이터 프레임의 차원 출력
dim(Data)

# 관측된 T-통계량 계산
obs_t_stat <- as.numeric(t.test(Height ~ Gender, Data)$statistic)

# 널 (무작위) T-통계량을 저장할 빈 벡터 초기화
null_t_stat <- c()

# 순열 횟수 설정
numOfRepeat <- 1000

# 순열 T-검정을 수행하고 널 T-통계량을 벡터에 저장
for(i in 1:numOfRepeat){
  null_t_stat[i] <- as.numeric(t.test(Height ~ sample(Gender), Data)$statistic)
}

# 순열 테스트의 p-값 계산
Pval <- (sum(abs(null_t_stat) >= abs(obs_t_stat)) + 1) / numOfRepeat
Pval
