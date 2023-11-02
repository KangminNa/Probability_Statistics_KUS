

## Set Repos.
setRepositories(ind = 1:7)


## Set working Dir.
WORK_DIR <- "E:\\3.고려대_수업자료\\2.융합정보학\\2.Code\\20231004_lectureCode"
setwd(WORK_DIR)

## Quiz

A <- 1:5
B <- A

length(A)
length(B)

cbind(A,B)
rbind(A,B)

C <- cbind(A,B)
dim(C)

C <- data.frame(C)
class(C)

View(C)

rm(C)
rm(B)
rm(A)

A <- c(1,2,3)
rep(A, 100)


c(rep(1, 100), rep(2, 100), rep(3, 100))

c(rep("Apple", 5), rep("Banana", 5))
rep(c("a", "b"), 5)




1:10
seq(1, 10)

10:1
seq(10, 1)

seq(1, 10000, by = 2)
seq(1,10000, len=100)
seq(1, 10000, length.out = 10)

hist(rnorm(100, mean=180, sd = 1))
rpois(100, 10)
rbinom(1, 100, 0.5)

A <- c(rep("Cancer 1st Phase", 5),
       rep("Cancer 2nd Phase", 5),
       rep("Cancer 3rd Phase", 5),
       rep("Cancer 4th Phase", 5),
       rep("Healthy", 5))
class(A)
A
as.numeric(A)

A <- factor(A)
A
class(A)
levels(A)
as.numeric(A)

## Quiz 2
Gender <- factor(c(rep("Male", 10), rep("Female", 10)))


Height <- c(rnorm(10, mean = 174.3, sd = 2),
rnorm(10, mean = 162.3, sd = 2))

#cbind(Gender, Height)
Data <- data.frame(Gender, Height)

dim(Data)
View(Data)
##


hist(rnorm(248361, mean = 174.3, sd = 5.5))





A <- factor()

rep()
