
setRepositories(ind = 1:7)



getwd()

WORK_DIR <- "E:/3.고려대_수업자료/1.확률및통계/2.Code/20230913_WarmUpRProgramming"
WORK_DIR

setwd(WORK_DIR)
getwd()

a <- 10
A <- 2

help("cbind")
?cbind

## Set two fair dice (self-checking 1)
nsides <- 6
times <- 2

temp <- list()

for(i in 1:times){
  temp[[i]] <- 1:nsides
}

## sample space S
S <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)

# names(s) <- c(paste(rep("Dice")))

## Get Event A
eventA <- subset(s, ((Dice_1 + Dice_2)%%2)==0)
## Get Event B

## Get Event C