
setRepositories(ind = 1:7)
# 항상 최상단에 이 코드가 작동되어야함.
# 실행하면 이런 화면이 뜸.
# 1~7번까지 어떤걸 쓸지 결정하는데 그냥 다해버리기
# 내가 어디서 ggplot2를 어디 서버에 어디서 뭘쓸지 어떻게 알아??!?
# 선택은 1 2 3 4 5 6 7 이렇게 Enter하면 되는데
# 더 간단한 방법은 setRepositories(ind = 1:7) 이렇게 작성하면 됨.
# 1. + CRAN
# 2. BioC software
# 3. BioC annotation
# 4. BioC experiment
# 5. CRAN (extras)
# 6. R-Forge
# 7. rforge.net

# ggplot2 는 최고의 시각화 툴인데
# https://cran.r-project.org/web/packages/available_packages_by_name.html
# Google, chatGPT 쓰세요.
# ggplot 저 사이트에서 찾지 말고 열심히 찾아서 쓰세요.



# console에서 코딩을 하지마세요.
# File -> newFile -> R script클릭
# control + s를 꼭 눌러라... 교수님이 날린 적이 있나보다...
# 병적으로 control + s 로 눌러라... 별표가 있으면 무조건...


WORK_DIR <- "C:\\Github\\Probability_Statistics_KUS\\20230907_lab1"
# "\" 를 해결하기 위한 방법에 대해 설명\\ 하나를 더 추가하기.
WORK_DIR

setwd(WORK_DIR)
getwd()
# 현재 디렉토리에서 작업하기 위한 코드 작성

# int a = 10;
# a = 10
a <- 10 #그냥 R에서는 이렇게 쓰자.
A <- 2 # 소대문자를 구분한다.


help("cbind") 
?cbind
# 모든 R유저는 개발자이기에, 라이브러리를 가져와서 쓰는데,
# 이게 어떤 라이브러리인지 알려면 help("string")을 작성
# examples을 제공하고 이에 대한 설명을 해줌.
# a lot of 많이 사용할 것이다.
# 메뉴얼을 찾으려면 help 혹은 ?을친다.

# 남들이 개발한 걸 잘 찾아와서 잘 쓰면 굳.
# 시각화에 대한 패키지를 구성하면 돼!

##############
###수업정리###
##############
# R Basic에 집중해서 하자.
# 일단은 그러자 중간고사 까지
# 통계 및 확률에 사용하는 부분들을 코드로 작성할 줄 알아야 하는건 아님.
# 교수님이 올린 R code를 굳이 다 이해하지 않아도 된다.
# self-checking coin에 대한 코드를 다음주에 연습할거임.
# 다음주에 제대로 알려드리겠습니다.
# 오늘은 그냥 R 어떻게 사용하는지에 대해 설명만 하심.
# 오... 왕 멋짐
# 다음주 실습 코드 왕멋짐
# 이걸 이용해서 대수법칙을 이용하지 않았더라면 과제가 1000번을 던졌어야 했어.
