#########################################
## Lecture Material                    ##
## for Probability & Statistics        ##
## Kangmin Na                          ##
## 2023-09-07                          ##
#########################################

setRepositories(ind = 1:7) 
## 서브파티 및 메소드를 사용할 건데, 그냥 다쓰겠다라고 정의

install.packages("devtools")
install_github("jhk0530/Rstat")
install.packages("ggVennDiagram")
install.packages("animation")

library(Rstat)
library(animation)
library(devtools)
library(dplyr)
library(ggVennDiagram)
## 처음 컴퓨터에 설치하고 다음에 설치하지 않아도 됨
## 이 부분은 처음이자 마지막으로 할 예정

## Set two fair die(Self-checking 1)
nsides <- 6
# nsides = 6 똑같이 작동함 그렇지만 <-이걸로 하자
## 주사위 면 nsides
times <- 2
## two dice

temp <- list()

for(i in 1:times){
  temp[[i]] <- 1:nsides
}
## R for => i는 1부터 times까지
## temp[[i]]에 1부터 nsides까지 넣기
## temp[[]] 인 이유는 times = 2이기 때문
S <- expand.grid(temp, KEEP.OUT.ATTRS = F)

###########################################################
## Chap1. Probability Theory(7페이지 Self-Checking1 까지)##
##                                                       ## 
##                                                       ##
###########################################################

## Sample Space S
S <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
names(S) <- c(paste(rep("Dice_", times), 1:times, seq))

