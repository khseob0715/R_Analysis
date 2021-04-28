#######################################
### R for the non-parametric analysis
### Since 21.04.28
### made by Hanseob Kim
#######################################

### install packages 
install.packages("tidyverse")
install.packages("ggplot2")  # for the visualization chart
install.packages("combinat")
install.packages("DescTools")
install.packages("psy")


### Add Library ###
library(psych)
library(psy)
library(tidyverse)
library(ggpubr)  # qqplot
library(rstatix)
library(datarium)
library(ggplot2)
library(combinat)
library(DescTools)



### Read Data ####
### import from CSV file

original <- read.csv(file = file.choose(), header = T) # call the file explore

### number of the participants
pid <- c(1:42) 


### pick the question index 
qid <- c(21,22,23,24)  # 17 18 19 20  distraction

qLength = length(qid)   ## 계산에 포함하는 질문 개수 
#qLength

# 각 조건별 시작 index
noIdx <- 1
loIdx <- 51
hiIdx <- 101

## np.zeros 랑 비슷한 개념.  
## 첫번째 인자의 값 크기 만큼의 두번째 인자의 값으로 이루어진 벡터 생성
noList <- replicate(qLength, noIdx)  
loList <- replicate(qLength, loIdx)
hiList <- replicate(qLength, hiIdx)

noList

## 계산에 포함되는 열의 벡터 
noList <- noList + qid
loList <- loList + qid
hiList <- hiList + qid

noList

cronbaList <- c(noList, loList, hiList)
cronbaList
aaaaaaaaaaa <- original[,unlist(cronbaList)]

cronbach(aaaaaaaaaaa)

print(cronbach(original[,unlist(cronbaList)])$alpha)




#print(alpha(cronbaList))

#no <- original[, c("no_1", "no_5", "no_11", "no_12")]  ## cp 
#no <- original[, c(noIdx + 2,noIdx + 4, noIdx + 6)] ## sp
#no <- original[, c(noIdx + 1,noIdx + 2)] ## sp 
#no
no <- original[, unlist(noList)]
no
no <- rowMeans(no, na.rm = T)  
no <- round(no, 3) # 소수점 자리수 올림
#no


lo <- original[, unlist(loList)]
lo
lo <- rowMeans(lo, na.rm = T)  
lo <- round(lo, 3)



hi <- original[, unlist(hiList)]
hi
hi <- rowMeans(hi, na.rm = T)
hi <- round(hi, 3)





spTable <- data.frame(pid, no, lo, hi)
#spTable
#######################################
# If you want to delete invalid data
#######################################
#dat <- dat[-c(2, 18, 21, 25, 26, 30),]  #   1  14 15  
### 10,12,13,14,21,29 CHI 결과 제외 참가자 
dat <- spTable
#dat <- spTable[-c(15),]





#######################################
# https://nittaku.tistory.com/467
#######################################

dat4test <- dat %>% gather(key = "cond", value = "score", no, lo, hi)   ## 데이터 샘플 합치기 
#dat4test
#head(dat4test, 3)
#dat4test


longData <- dat4test %>% convert_as_factor(pid, cond)
#head(longData, 3)

longData %>% 
  group_by(cond) %>%
  get_summary_stats(score, type = "common")

#longData

#######################################
# 원하는 조건 순서로 출력 값 지정하기 
#######################################

#longData$cond <- factor(longData$cond, levels = c("nn","bb","ss"))     # Specify data order 
longData$cond <- factor(longData$cond, levels = c("no","lo","hi"))     # Specify data order # 데이터 순서 정하기  

ggboxplot(longData, x = "cond", y = "score", add = "jitter")  # order=c("nn","bb","ss")

#######################################
# Data 정규성 검증  # https://jinmedi.tistory.com/339
#######################################

output=lm(score ~ cond,data=dat4test)
shapiro.test(resid(output))

#shapiro.test(dat$nn)
#shapiro.test(dat$bb)
#shapiro.test(dat$ss)


#hist(dat$bb)

#####################################
# Friedman test # Repeated measure ANOVA, 정규분포를 따르지 않을 때 
#######################################

# https://www.datanovia.com/en/lessons/friedman-test-in-r/
res.fried <- longData %>% friedman_test(score ~ cond|pid) ## rstatix packages
res.fried

print(res.fried)

res.fried$statistic
res.fried$p

print(res.fried$statistic)
print(res.fried$p)

longData %>% friedman_effsize(score ~ cond|pid)
longData

# pairwise comparisons
pwc <- longData %>%
  wilcox_test(score ~ cond, 
              paired = TRUE, 
              conf.level = 0.95,
              p.adjust.method = "bonferroni")
pwc
#%p.adjust.method = "bonferroni"
#print(pwc)
check <- "*" %in% pwc$p.adj.signif  

  if(check){
    print(qid)
    print(pwc)
  }else{
    check <- "**" %in% pwc$p.adj.signif  
    if(check){
      print(qid)
      print(pwc)
    }
  else{
    check <- "***" %in% pwc$p.adj.signif 
    if(check){
      print(qid)
      print(pwc)
    }
  }
}
#print(pwc$p.adj.signif)
#print(typeof(pwc$p.adj.signif))


# str(pwc$p.adj.signif) # If you want to read for results.


#########################################
#c("#00AFBB", "#E7B800", "#E7B800")
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = 'cond')
pwc
#########################################
#http://www.sthda.com/english/wiki/line-types-in-r-lty dashed line type 
# xlab = conditionName, main=conditionName,
#########################################

Figure <- ggboxplot(longData, 
                    x ='cond', 
                    y = "score", 
                    width=0.5,
                    xlab = F,
                    ylab = F,
                    bxp.errorbar = T,
                    bxp.errorbar.width = 0.1,
                    fill = 'cond', palette = "supp",
                    legend = "none") +
  stat_pvalue_manual(pwc, 
                     label="p.adj.signif", 
                     size=5,
                     bracket.size = 0.5,
                     label.size = 5, 
                     color="#2ECC71",
                     hide.ns = T, 
                     linetype = "solid",
                     tip.length = 0.01)  + 
  scale_y_continuous(expand = c(0, 0), limits = c(0.5, 8), breaks=c(1:7)) + 
  scale_x_discrete(labels=c("DF", "TP","STP"))

Figure