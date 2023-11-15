##################################
## 확률과 통계를 위한 강의 자료 ##
## 서민석                       ##
## 2023-11-08                   ##
##################################

## 라이브러리 및 저장소 설정
setRepositories(ind = 1:7)

## 라이브러리 불러오기
#install.packages("devtools")
library(devtools)
#install_github('jhk0530/Rstat')

## 자가검사 1
testSampleMeanDist <- function (ns, mu = 0, sig = 1, N = 10000, ng = 50, seed = 9857, 
                                dig = 4) 
{
  
  xb <- NULL
  # 표본 평균을 저장할 벡터 초기화
  
  for (k in 1:N) xb <- c(xb, mean(rnorm(ns, mu, sig)))
  # N번의 시뮬레이션을 통해 표본 평균을 계산하고 벡터에 추가
  
  zb <- (xb - mu)/sig * sqrt(ns)
  # 표준화된 표본 평균을 계산하여 zb에 저장
  
  popd <- function(x) dnorm(x, mu, sig)
  # 이론적인 모집단 분포 함수
  
  smd <- function(x) dnorm(x, mu, sig/sqrt(ns))
  # 이론적인 표본 평균 분포 함수
  
  Ex1 <- round(mean(xb), dig)
  Dx1 <- round(sd(xb), dig)
  Ex2 <- mu
  Dx2 <- round(sig/sqrt(ns), dig)
  Ez <- round(mean(zb), dig)
  Dz <- round(sd(zb), dig)
  # 통계량의 기댓값과 표준편차 계산 및 반올림
  
  xp <- seq(floor(mu - 3 * sig/sqrt(ns)), ceiling(mu + 3 * sig/sqrt(ns)), by = 0.5 * sig)
  # 히스토그램 구간을 계산하기 위한 범위 설정
  
  Theory <- pnorm(xp, mu, sig/sqrt(ns))
  # 이론적인 누적 분포 함수 값 계산
  
  Simula <- sapply(xp, function(x) sum(xb < x))/N
  # 시뮬레이션을 통해 누적 분포 함수 값 계산
  
  cdf <- rbind(Theory, Simula)
  colnames(cdf) <- paste0("F(", xp, ")")
  # 이론적인 누적 분포 함수와 시뮬레이션 결과를 데이터 프레임으로 결합 및 열 이름 설정
  
  print(round(cdf, dig))

  
  par(mfrow = c(2, 1))
  par(mar = c(3, 4, 4, 2))
  x1 <- mu - 3 * sig
  x2 <- mu + 3 * sig
  
  hist(xb, breaks = ng, prob = T, col = 7, xlim = c(x1, x2), 
       ylab = "f(x)", xlab = "", main = bquote(bold("평균의 분포: ") ~ bar(X)[.(ns)] ~ ~bold(from) ~ ~N(.(mu), .(sig)^2)))
  # 표본 평균의 분포를 히스토그램으로 표시
  
  curve(popd, x1, x2, col = 4, add = T)
  curve(smd, x1, x2, col = 2, add = T)
  # 이론적인 모집단 및 표본 평균 분포 곡선 표시
  
  legend("topright", c("모수.  정확  시뮬.", 
                       paste("E(X) ", Ex2, Ex1, sep = "  "), paste("D(X)", 
                                                                   Dx2, Dx1, sep = "  ")), text.col = c(1, 4, 
                                                                                                        4))
  # 범례 표시
  
  hist(zb, breaks = 2 * ng, prob = T, col = "cyan", xlim = c(-4, 4), 
       ylab = bquote(phi(z)), xlab = "", main = "표준화된 표본 평균의 분포")
  # 표준화된 표본 평균의 분포를 히스토그램으로 표시
  
  curve(dnorm, -4, 4, col = 2, add = T)
  # 이론적인 표준 정규 분포 곡선 표시
  
  legend("topright", c("모수.  정확  시뮬.", 
                       paste("E(Z)    ", 0, "    ", Ez), paste("D(Z)    ", 
                                                               1, "    ", Dz)), text.col = c(1, 4, 4))
  # 범례 표시
}

testSampleMeanDist(ns=100, mu=100, sig=10, N=10000)
###################################################
# testSampleMeanDist 함수 설명

# 함수 인자 (Function Parameters):
#   ns: 표본 크기 (sample size)
#   mu: 정규분포의 평균 (population mean)
#   sig: 정규분포의 표준편차 (population standard deviation)
#   N: 시뮬레이션 횟수 (number of simulations)
#   ng: 히스토그램 구간의 수 (number of bins in the histogram)
#   seed: 난수 발생의 재현성을 위한 시드 (seed for random number generation)
#   dig: 결과의 유효 숫자 (number of digits for rounding)

# 표본 평균 및 표준화된 표본 평균의 계산:
#   xb: 표본 평균을 저장하는 벡터
#   zb: 표준화된 표본 평균을 계산하여 저장하는 벡터

# 이론적인 모집단 및 표본 분포 함수 정의:
#   popd: 이론적인 모집단의 확률 밀도 함수
#   smd: 이론적인 표본 평균의 확률 밀도 함수

# 통계량 계산 및 출력:
#   Ex1, Dx1: 표본 평균의 평균과 표준편차
#   Ex2, Dx2: 이론적인 모집단의 평균과 표준편차
#   Ez, Dz: 표준화된 표본 평균의 평균과 표준편차

# 이론적인 누적 분포 및 시뮬레이션 결과 계산 및 출력:
#   xp: 히스토그램의 범위를 설정하는 변수
#   Theory: 이론적인 누적 분포 함수 값 계산
#   Simula: 시뮬레이션을 통해 누적 분포 함수 값 계산

# 그래픽 표현:
#   par 함수를 사용하여 그래픽 디바이스 설정 및 여러 차트 출력
#   표본 평균의 분포를 히스토그램으로 표시하고, 이론적인 분포 및 표본 평균의 분포 곡선을 함께 표시
#   표준화된 표본 평균의 분포를 히스토그램으로 표시하고, 이론적인 표준 정규 분포 곡선을 함께 표시
#   결과에 대한 범례 추가

# 이 함수는 주어진 모집단의 특성을 기반으로 표본 평균 및 표준화된 표본 평균의 분포를 이해하고 시각적으로 확인하는 데 사용됩니다. 
# 결과적으로, 이 함수는 통계적인 개념과 시뮬레이션을 통한 직관적인 이해를 제공하며, 통계 강의에서는 중요한 확률과 통계적인 개념을 학습하는 데 활용.

# 무엇을 구하는 것인지:
#   이 함수는 주어진 표본 크기, 모집단 평균, 모집단 표준편차 등의 조건에서 표본 평균과 표준화된 표본 평균의 분포를 시뮬레이션하고 시각화하여,
#   표본 평균의 분포와 이론적인 분포의 차이, 표준화된 표본 평균의 분포와 표준 정규 분포의 차이를 확인하는 데 목적이 있습니다.

# 확률과 통계적인 개념:
#   - 표본 평균의 분포를 통해 중심극한정리를 시뮬레이션하고 확인합니다.
#   - 표준화된 표본 평균의 분포를 통해 표준 정규 분포와의 관계를 확인합니다.
#   - 이론적인 확률 밀도 함수와 시뮬레이션 결과를 비교하여 통계적 개념을 시각적으로 이해합니다.

#################################################################################################
# 가설 설정:
# 귀무가설 (H0): 2020년에 대한 남성 키의 평균은 169.3cm이다.
# 대립가설 (H1): 2020년에 대한 남성 키의 평균은 169.3cm보다 크다.

# 통계 검정:
#   양측 검정: 2020년 남성 키가 169.3cm인지 여부를 확인
#   단측 검정 (우측): 2020년 남성 키가 169.3cm보다 큰지 여부를 확인
#   단측 검정 (좌측): 2020년 남성 키가 169.3cm보다 작은지 여부를 확인

# 비교 검정:
#   귀무가설 (H0): 2023년과 2020년의 남성 키의 평균은 같다.
#   대립가설 (H1): 2023년과 2020년의 남성 키의 평균은 다르다.

# 이 코드에서 계산된 p-value는 유의수준 0.05에서 각 가설에 대한 결과를 확인하는 데 사용됩니다.
# p-value가 작을수록 귀무가설을 기각하고 대립가설을 채택할 수 있습니다.

#################################################################################################
# Z-통계량 계산 함수
z.test <- function(input, mu, var){
  z <- (mean(input) - mu) / (sqrt(var / length(input)))
  # Z 통계량 계산
  return(z)
}

# 남성 키 데이터 생성
Height_2020yr <- rnorm(100, mean = 173.6, sd = 5.62)
Height_2023yr <- rnorm(100, mean = 175.2, sd = 5.8)  # 2023년 남성 키 데이터 생성

# Z-통계량 계산
Zstat_2020 <- z.test(Height_2020yr, mu = 169.3, var = 5.4^2)

# 양측 Z-검정
p_value_2sided_2020 <- 2 * pnorm(-abs(Zstat_2020))
cat("양측 검정 결과 (p-value):", p_value_2sided_2020, "\n")

# 단측 Z-검정 (2020 키 >= 169.3)
p_value_1sided_greater_2020 <- 1 - pnorm(Zstat_2020)
cat("우측 검정 결과 (p-value):", p_value_1sided_greater_2020, "\n")

# 단측 Z-검정 (2020 키 <= 169.3)
p_value_1sided_less_2020 <- pnorm(Zstat_2020)
cat("좌측 검정 결과 (p-value):", p_value_1sided_less_2020, "\n")

# 2023년 키와 2020년 키 비교 Z-통계량 계산
Zstat_compare <- z.test(Height_2023yr, mu = mean(Height_2020yr), var = var(Height_2020yr))

# 양측 Z-검정
p_value_2sided_compare <- 2 * pnorm(-abs(Zstat_compare))
cat("2023년과 2020년 키 비교 양측 검정 결과 (p-value):", p_value_2sided_compare, "\n")

# 단측 Z-검정 (2023년 키 >= 2020년 키)
p_value_1sided_greater_compare <- 1 - pnorm(Zstat_compare)
cat("2023년과 2020년 키 비교 우측 검정 결과 (p-value):", p_value_1sided_greater_compare, "\n")

# 단측 Z-검정 (2023년 키 <= 2020년 키)
p_value_1sided_less_compare <- pnorm(Zstat_compare)
cat("2023년과 2020년 키 비교 좌측 검정 결과 (p-value):", p_value_1sided_less_compare, "\n")

##########################################################################################################
# Z-통계량 계산 함수 (z.test):

#   z.test 함수는 주어진 데이터셋의 평균과 주어진 모평균 간의 Z-통계량을 계산합니다.
#   Z-통계량은 표본 평균과 모평균 간의 표준화된 차이를 나타내며, 가설 검정에 활용됩니다.

# 남성 키 데이터 생성:

#   Height_2020yr은 평균 173.6, 표준편차 5.62인 정규 분포에서 추출한 100개의 랜덤한 남성 키 데이터를 나타냅니다.

# Z-통계량 계산:

#   Zstat은 위에서 정의한 z.test 함수를 사용하여 계산된 Z-통계량을 나타냅니다.
#   mean(input)은 데이터의 평균, mu=169.3은 가설로 설정한 모평균, var=5.4^2은 모분산을 나타냅니다.

# 양측 Z-검정:

#  2 * pnorm(-abs(Zstat))는 주어진 Z-통계량에 대한 양측 검정 결과를 계산합니다.
#  Z-통계량의 절댓값에 대한 누적 분포 함수를 사용하여 양측 검정을 수행합니다.
#  결과에 대한 주석 추가: 양측 검정 결과

# 단측 Z-검정:

#   1 - pnorm(Zstat)는 주어진 Z-통계량에 대한 우측 검정 결과를 계산합니다. (2020 키가 169.3 이상인 경우)
#   pnorm(Zstat)는 주어진 Z-통계량에 대한 좌측 검정 결과를 계산합니다. (2020 키가 169.3 이하인 경우)
#   결과에 대한 주석 추가: 우측 검정 결과 및 좌측 검정 결과

# 통계적 개념 설명:

#   가설 검정 (Hypothesis Testing):

#   가설 검정은 주어진 데이터로부터 모집단에 대한 주장이 유의미한지를 판단하는 통계적 방법입니다.
#   양측 검정은 주어진 가설이 모집단과 다르다는 것을 보이는데 사용되며, 단측 검정은 방향성이 있는 가설을 검증하는데 사용됩니다.

# Z-통계량 (Z-Statistic):

#   Z-통계량은 평균 차이를 표준 오차로 나눈 값으로, 가설 검정에서 표본 통계량을 표준화하여 사용합니다.
#   어떤 값이 주어진 가설을 지지하는지 또는 기각하는지를 판단하는 데 사용됩니다.

# 누적 분포 함수 (Cumulative Distribution Function, CDF):

#   누적 분포 함수는 확률 변수가 특정 값보다 작거나 같을 확률을 나타냅니다.
#   여기서는 pnorm 함수를 사용하여 Z-통계량이 특정 값 이하일 확률을 계산합니다.

# 이 코드의 목적:
#   주어진 남성 키 데이터를 사용하여 모평균이 169.3인 경우에 대한 가설을 검정하고, 이를 통해 남성 키의 통계적 특성을 평가하는 것입니다. 
#   결과는 양측 및 단측 검정을 통해 얻어진 p-값으로 제시되며, 이를 통해 가설이 통계적으로 유의미한지를 판단할 수 있습니다.
