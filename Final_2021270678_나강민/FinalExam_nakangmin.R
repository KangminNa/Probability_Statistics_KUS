# ggplot2 라이브러리 로드
install.packages("ggplot2")
library(ggplot2)

# 파일 경로 설정
Q1_Data <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/Final_2021270678_나강민/Q1_Data.txt"

# 텍스트 파일 읽기
Q1_Data_read <- read.table(Q1_Data, header = TRUE, sep = "\t")

# 데이터 확인
print(Q1_Data_read)

# 모수적 방법: 독립 표본 t-검정
t_test <- t.test(numCOVID ~ Job, data = Q1_Data_read)
print(t_test)

# 비모수적 방법: Wilcox 검정
mw_test <- wilcox.test(numCOVID ~ Job, data = Q1_Data_read)
print(mw_test)

# 히스토그램
hist(Q1_Data_read$numCOVID)


# 히스토그램 그리기
  geom_histogram(position="identity", alpha=0.5, bins=30) +
  labs(x="Number of COVID-19 confirmed cases", y="frequency", fill="Job") +
  theme_minimal() +
  theme(legend.position="top")


################################################################
# 파일 경로 설정
Q6_Data <- "/Users/mac_nkm/Documents/GitHub/Probability_Statistics_KUS/Final_2021270678_나강민/Q6_Data.txt"

# 텍스트 파일 읽기
Q6_Data_read <- read.table(Q6_Data, header = TRUE, sep = "\t")

# 데이터 확인
print(Q6_Data_read)



# 암 그룹과 정상 그룹 분리
cancer_group <- Q6_Data_read[Q6_Data_read$Disease == "Cancer", ]
normal_group <- Q6_Data_read[Q6_Data_read$Disease == "Healthy", ]

print(cancer_group)
print(normal_group)

############## 6번 #################

# 유의미한 차이가 있는 유전자를 저장할 벡터 초기화
significant_genes <- c()

# 본페로니 교정을 위한 유의수준 설정
p_value_threshold <- 0.01 / (20 + 1724)  # DNA 마커 수 + RNA 마커 수

# 각 DNA 마커에 대해 카이제곱 검정 수행
for (i in 1:20) {
  marker <- paste0("DNA_", i)
  table_data <- table(Q6_Data_read$Disease, Q6_Data_read[[marker]])
  
  # 모든 항목이 0 또는 음수인 경우 검정을 건너뜀
  if (all(table_data <= 0)) {
    next
  }
  
  chisq_test <- chisq.test(table_data)
  
  if (chisq_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}

# 각 RNA 마커에 대해 t-검정 수행
for (i in 1:1724) {
  marker <- paste0("Gene_", i)
  
  # 'y' 그룹에 대한 관측치가 충분한지 확인
  if (length(normal_group[[marker]]) < 2) {
    next
  }
  
  t_test <- t.test(cancer_group[[marker]], normal_group[[marker]])
  
  if (t_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}


# 찾아진 유전자의 개수 출력
length(significant_genes)

############## 7번 #################
# 유의미한 차이가 있는 유전자를 저장할 벡터 초기화
significant_genes <- c()

# 각 DNA 마커에 대해 상관 관계 검정 수행
for (i in 1:20) {
  marker <- paste0("DNA_", i)
  
  cor_test <- cor.test(cancer_group$Severity, cancer_group[[marker]], method = "pearson")
  
  if (cor_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}

# 각 RNA 마커에 대해 상관 관계 검정 수행
for (i in 1:1724) {
  marker <- paste0("Gene_", i)
  
  cor_test <- cor.test(cancer_group$Severity, cancer_group[[marker]], method = "pearson")
  
  if (cor_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}

# 찾아진 유전자의 개수 출력
length(significant_genes)

############## 8번 #################
# 유의미한 차이가 있는 유전자를 저장할 벡터 초기화
significant_genes <- c()

# 각 DNA 마커에 대해 상관 관계 검정 수행
for (i in 1:20) {
  marker <- paste0("DNA_", i)
  
  cor_test <- cor.test(cancer_group$Height, cancer_group[[marker]], method = "pearson")
  
  if (cor_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}

# 각 RNA 마커에 대해 상관 관계 검정 수행
for (i in 1:1724) {
  marker <- paste0("Gene_", i)
  
  cor_test <- cor.test(cancer_group$Height, cancer_group[[marker]], method = "pearson")
  
  if (cor_test$p.value < 0.01) {
    significant_genes <- c(significant_genes, marker)
  }
}

# 찾아진 유전자의 개수 출력
length(significant_genes)


