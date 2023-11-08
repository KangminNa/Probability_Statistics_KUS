##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-11-08                   ##
##################################

######################################################
## Lab code for Chap.5                              ##
######################################################

setRepositories(ind = 1:7)

#install.packages("devtools")
library(devtools)
#install_github('jhk0530/Rstat')
library(Rstat)
install.packages("TeachingDemos")
library(TeachingDemos)



## Self-Checking 1
mu <- c(0, 0, 2, 2)
sd <- c(1, 2, 1, 2)

getpdf <- function(dist, xa, para, para2) {
  np = length(xa)
  N = max(length(para), length(para2))
  
  # PDF name
  dpdf <- paste0("d", dist)
  pdf <- matrix(NA, nrow=np, ncol=N)
  
  # Vector of the PDF
  if (dist %in% c("exp", "t", "chisq")) { 
    for (k in 1:N) {
      pdf[, k] <- do.call(dpdf, list(xa, para[k]))
    }
  } else if (dist == "gamma") { 	
    for (k in 1:N) {
      pdf[, k] <- do.call(dpdf, list(xa, para[k], 1/para2[k]))
    }
  } else { 	for (k in 1:N) {
    pdf[, k] <- do.call(dpdf, list(xa, para[k], para2[k]))
  }
  }
  invisible(pdf)
}

cont.spdf("norm", -7, 7, mu, sd, xp=mu)


## Self-Checking 2
pnorm(1) - pnorm(-1.5)
norm.trans(mu = 2, sig = 2, a = -1, b = 4)


## Self-Checking 3
pv <- matrix(pnorm(0:299/100), ncol=10, byrow=T)
colnames(pv) <- 0:9/100
rownames(pv) <- 0:29/10
View(round(pv, 8))


## Self-Checking 4
pnorm(185, 175, 8) - pnorm(180, 175, 8)
60000000 * p


## Self-Checking 5
pbinom(45, 100, 0.5)-pbinom(39, 100, 0.5)

pnorm(-0.9)-pnorm(-2.1)

### Self-Checking5 샘플 중 pbinom이 더 확률이 높다.
### 밑에 있는 pnorm은 n이 크거나 확률이 0.5에 가까우면 위의 값과 비슷해지는 것임.


## Self-Checking 6
nu <- c(5, 10, 15, 20)
up <- qchisq(0.99, max(nu))

cont.spdf("chi", 0, up, para=nu, xp=nu)

### 카이제곱분포
### 자유도가 높을 수록 정규분포에 가깝게 다가가는 모습을 볼 수 있다.
### 그룹비교 


## Self-Checking 7
nu <- c(1, 5, 10, 30)
tnorm.comp(nu)

### t-분포


## Self-Checking 8

# Define function for CLT check
### CLT 까지는 코드를 자세히 몰라도 된다.
### 대신 이론은 자세히 알아야함
### 이번 시간까지 자세한 내용들을 전부 이론을 이해해야함
### 모집단 
plotForCLT <- function(r.dist, n, ...) {
  
  means <- c()
  
  for(i in 1:1000){
    means[i] <- mean(r.dist(n, ...))
  } 
  
  std.means <- scale(means)
  par(mfrow = c(1, 2))
  
  hist(std.means, prob = T, col = "light grey",
       border = "grey", main = NULL, ylim = c(0, 0.5))
  lines(density(std.means))
  box()
  
  curve(dnorm(x, 0, 1), -3, 3, col = 'blue', add = T)
  
  qqnorm(std.means, main="", cex = 0.8)
  abline(0, 1, lty = 2, col = "red")
  par(mfrow = c(1, 1))
}


# Sampling from the Chi-squared distribution
plotForCLT(rchisq, n = 1, df = 1)
plotForCLT(rchisq, n = 10, df = 1)
plotForCLT(rchisq, n = 20, df = 1)
plotForCLT(rchisq, n = 30, df = 1)
plotForCLT(rchisq, n = 100, df = 1)

# Sampling from the Binomial distribution
plotForCLT(rbinom, n = 1, size = 1, p = .5)
plotForCLT(rbinom, n = 10, size = 1, p = .5)
plotForCLT(rbinom, n = 20, size = 1, p = .5)
plotForCLT(rbinom, n = 30, size = 1, p = .5)
plotForCLT(rbinom, n = 50, size = 1, p = .5)
plotForCLT(rbinom, n = 100, size = 1, p = .5)

# Sampling from the Poisson distribution
plotForCLT(rpois, n = 1, lambda = 1)
plotForCLT(rpois, n = 10, lambda = 1)
plotForCLT(rpois, n = 20, lambda = 1)
plotForCLT(rpois, n = 30, lambda = 1)
plotForCLT(rpois, n = 40, lambda = 1)

# Sampling from the Negative Binomial distribution
plotForCLT(rnbinom, n = 1, size = 1, p = .5)
plotForCLT(rnbinom, n = 10, size = 1, p = .5)
plotForCLT(rnbinom, n = 20, size = 1, p = .5)
plotForCLT(rnbinom, n = 30, size = 1, p = .5)


