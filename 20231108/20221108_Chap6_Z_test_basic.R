##################################
## Lecture Material            ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-11-08                   ##
##################################
## SE에 대해 알고 넘어가기 : 표준오차
## 예시 중 1970~N(weight), 2022~N(weight) 이 예제를 통해서 z stat에 대해 예제와 함께 공식을 통해 정리하기
## 모평균, 모표준편차 우리는 모르지만!
## Set Repositories
setRepositories(ind = 1:7)

## Load library
#install.packages("devtools")
library(devtools)
#install_github('jhk0530/Rstat')

## Self-Checking 1
testSampleMeanDist <- function (ns, mu = 0, sig = 1, N = 10000, ng = 50, seed = 9857, 
                                dig = 4) 
{
  
  xb <- NULL
  for (k in 1:N) xb <- c(xb, mean(rnorm(ns, mu, sig)))
  zb <- (xb - mu)/sig * sqrt(ns)
  popd <- function(x) dnorm(x, mu, sig)
  smd <- function(x) dnorm(x, mu, sig/sqrt(ns))
  Ex1 <- round(mean(xb), dig)
  Dx1 <- round(sd(xb), dig)
  Ex2 <- mu
  Dx2 <- round(sig/sqrt(ns), dig)
  Ez <- round(mean(zb), dig)
  Dz <- round(sd(zb), dig)
  xp <- seq(floor(mu - 3 * sig/sqrt(ns)), ceiling(mu + 3 * 
                                                    sig/sqrt(ns)), by = 0.5 * sig)
  Theory <- pnorm(xp, mu, sig/sqrt(ns))
  Simula <- sapply(xp, function(x) sum(xb < x))/N
  cdf <- rbind(Theory, Simula)
  colnames(cdf) <- paste0("F(", xp, ")")
  print(round(cdf, dig))
  win.graph(7, 6)
  par(mfrow = c(2, 1))
  par(mar = c(3, 4, 4, 2))
  x1 <- mu - 3 * sig
  x2 <- mu + 3 * sig
  hist(xb, breaks = ng, prob = T, col = 7, xlim = c(x1, x2), 
       ylab = "f(x)", xlab = "", main = bquote(bold("Distribution of ") ~ 
                                                 bar(X)[.(ns)] ~ ~bold(from) ~ ~N(.(mu), .(sig)^2)))
  curve(popd, x1, x2, col = 4, add = T)
  curve(smd, x1, x2, col = 2, add = T)
  legend("topright", c("Para.  Exact  Simul.", 
                       paste("E(X) ", Ex2, Ex1, sep = "  "), paste("D(X)", 
                                                                   Dx2, Dx1, sep = "  ")), text.col = c(1, 4, 
                                                                                                        4))
  hist(zb, breaks = 2 * ng, prob = T, col = "cyan", xlim = c(-4, 
                                                             4), ylab = bquote(phi(z)), xlab = "", main = "Distribution of the Standardized Sample Mean")
  curve(dnorm, -4, 4, col = 2, add = T)
  legend("topright", c("Para.  Exact  Simul.", 
                       paste("E(Z)    ", 0, "    ", Ez), paste("D(Z)    ", 
                                                               1, "    ", Dz)), text.col = c(1, 4, 4))
}


testSampleMeanDist(ns=100, mu=100, sig=10, N=10000)




## Self-Checking 2 (Z-test)

# Implemenation to calculate Z-statistic
z.test <- function(input, mu, var){
  z <- (mean(input) - mu) / (sqrt(var / length(input)))
  return(z)
}

# Let's generate Men's Height
Height_2020yr <- rnorm(100, mean = 173.6, sd = 5.62)

# Get Z-statistic
Zstat <- z.test(Height_2020yr, mu=169.3, var=5.4^2)

2*pnorm(-abs(Zstat)) # two-sided Z-test
1-pnorm(Zstat) # one-sided Z-test (2020 Height >= 169.3)
pnorm(Zstat) # one-sided Z-test (2020 Height <= 169.3)





