rm(list = ls())
setwd("C:/Users/cogus/OneDrive/바탕 화면/Rcode/논문(핀테크)")

library(readxl)
dt1<-read_excel("15~19dt_final.xlsx")
dt2<-read_excel("20~23dt_final.xlsx")

## 결측치 제거 전처리
dt1[is.na(dt1)] <- 0
dt2[is.na(dt2)] <- 0  

##코로나 이전 VaR도출 과정
# 평균 구하기
mean_1<-matrix(0,52,1)

for(i in 1:52){
  mean_1[i,1]<-sum(dt1[,i+1])/1227
  }

notech_mean_1 <-mean_1[-(22:23),]
notech_mean_1 <- matrix(notech_mean_1) #데이터 유형 통일

# 표준편차 구하기
sd_1<-matrix(0,52,1)

for(i in 1:52){
  sd_1[i,1]<-apply(dt1[, i+1, drop = FALSE], 2, sd) 
}

# 95% VaR 구하기
VaR_1<-matrix(0,52,1)

for(i in 1:52){
  VaR_1[i,1]<- qnorm(0.05,mean_1[i,1],sd_1[i,1])
}

notech_VaR1 <-VaR_1[-(22:23),1]
notech_VaR1 <-matrix(notech_VaR1) #데이터 유형 통일


# naver와 카카오 제외(가중평균 시총에서 제외)
dt1_n<-dt1[,-c(22,23,74,75)]

#가중평균한 등락률(각 기업에 대해 그 기업을 제외하고 계산한 금융시스템 전체의 수익률 계산하기 위함)
weighted_mean1<-matrix(0,1227,50)

dt1_n <- dt1_n[,-1] # 날짜 제외

for(z in 1:50){
  # 해당 기업 제외
  selected_cols <- setdiff(1:ncol(dt1_n), c(z, z+50))
  for(i in 1:1227){
    # 선택된 열에서 등락률과 시총을 추출
    rates1 <- dt1_n[i, selected_cols[1:49]] 
    market_caps1 <- dt1_n[i, selected_cols[50:98]]
    # 가중평균 계산
    weighted_mean1[i,z] <- sum(rates1 * market_caps1) / sum(market_caps1)
  }
}

#카카오 네이버에 넣을 시스템 등락률
notech_weighted_mean1 <-matrix(0,1227,1)
for(i in 1:1227){
  rates_1 <- dt1_n[i,1:50]
  market_caps_1 <- dt1_n[i,51:100]
  
  notech_weighted_mean1[i,1]<-sum(rates_1*market_caps_1)/sum(market_caps_1)
  }


#코로나 이전 coVaR도출(네이버, 카카오 제외)
library(quantreg) #분위수회귀하는 패키지

coefficients_A1<-matrix(0,50,1)
coefficients_B1<-matrix(0,50,1)

CoVaR_5_1<-matrix(0,50,1)
CoVaR_50_1<-matrix(0,50,1)
ΔCoVaR_1<-matrix(0,50,1)

for(i in 1:50){
coefficients_A1[i, 1] <- rq(weighted_mean1[ ,i] ~ dt1_n[[i]], tau=0.05)$coefficients[1]
coefficients_B1[i, 1] <- rq(weighted_mean1[, i] ~ dt1_n[[i]], tau=0.05)$coefficients[2]

CoVaR_5_1[i,1] <- coefficients_A1[i, 1]+coefficients_B1[i, 1]*notech_VaR1[i,1]
CoVaR_50_1[i,1]<- coefficients_A1[i, 1]+coefficients_B1[i, 1]*notech_mean_1[i,1]

ΔCoVaR_1[i,1] <- CoVaR_5_1[i,1] - CoVaR_50_1[i,1]
}

print(ΔCoVaR_1)
write.csv(ΔCoVaR_1, "코로나 이전 증분CoVaR 최종.csv") 

# 유의성 검정
summary(coefficients_A1, method = "boot") # 신뢰구간이 0을 포함하지 않으므로(-1.738 ~ -1.223) 유의미
summary(coefficients_B1, method = "boot") # 신뢰구간이 0을 포함하지 않으므로(0.02697 ~ 0.46015) 유의미
#quentreg를 이용한 분위수회귀에서는 특정 조건이 아닌 이상 p값이 제공되지 않으며 부트스트랩 방법을 이용하여 신뢰구간이 0을 포함하지 않는지를 검증하여 유의성 판단


#카카오의 코로나 이전 coVaR도출
reg_k_1 <- rq(notech_weighted_mean1~dt1[[22]],tau=0.05)

CoVaR_5_k_1 <- reg_k_1$coefficients[1]+reg_k_1$coefficients[2]*VaR_1[22,1]
CoVaR_50_k_1 <- reg_k_1$coefficients[1]+reg_k_1$coefficients[2]*mean_1[22,1]

ΔCoVaR_k_1 <- CoVaR_5_k_1 - CoVaR_50_k_1

print(ΔCoVaR_k_1)  # -0.3331355 

# 유의성 검정
summary(reg_k_1, method = "boot") #p값 0.00245 유의. 데이터가 간단해서 p값을 구할 수 있음.


#네이버의 코로나 이전 coVaR도출
reg_n_1 <- rq(notech_weighted_mean1~dt1[[23]],tau=0.05)

CoVaR_5_n_1 <- reg_n_1$coefficients[1]+reg_n_1$coefficients[2]*VaR_1[23,1]

CoVaR_50_n_1 <- reg_n_1$coefficients[1]+reg_n_1$coefficients[2]*mean_1[23,1]

ΔCoVaR_n_1 <- CoVaR_5_n_1 - CoVaR_50_n_1

print(ΔCoVaR_n_1)   #-0.3657696 

#유의성 검정
summary(reg_n_1, method = "boot") #p값 0.00038
 
 
########################################################################
##코로나 이후 VaR도출 
 
# 평균 구하기
mean_2<-matrix(0,52,1)
 
 for(i in 1:52){
   mean_2[i,1]<-sum(dt2[,i+1])/987
 }

notech_mean_2 <- mean_2[-(22:23),] # 날짜 포함 데이터에서 22행이 카카오, 23행이 네이버
notech_mean_2 <- matrix(notech_mean_2)
 
 
# 표준편차 구하기
sd_2<-matrix(0,52,1)
dt2_n <- dt2[ ,-1]

for(i in 1:52){
  sd_2[i,1]<-apply(dt2_n[, i, drop = FALSE], 2, sd) 
}

# 95% VaR 구하기
VaR_2<-matrix(0,52,1)

for(i in 1:52){
  VaR_2[i,1]<- qnorm(0.05,mean_2[i,1],sd_2[i,1])
}

notech_VaR2 <-VaR_2[-(22:23),1]
notech_VaR2 <-matrix(notech_VaR2)

# naver와 카카오 제외
dt2_n <-dt2[,-c(22,23,74,75)] 


# 가중평균한 등락률(각 기업에 대해 그 기업을 제외하고 계산한 금융시스템 전체의 수익률 계산하기 위함)
weighted_mean2 <-matrix(0,987,50)

dt2_n <- dt2_n[,-1] # 날짜 제외

for(z in 1:50){
  # 동 기업 제외
  selected_cols <- setdiff(1:ncol(dt1_n), c(z, z+50))
  for(i in 1:987){
    # 선택된 열에서 등락률과 시총을 추출
    rates2 <- dt2_n[i, selected_cols[1:49]]  
    market_caps2 <- dt2_n[i, selected_cols[50:98]]
    # 가중평균 계산
    weighted_mean2[i,z] <- sum(rates2 * market_caps2) / sum(market_caps2)
  }
}


# 카카오 네이버에 넣을 시스템 등락률
notech_weighted_mean2 <-matrix(0,987,1)

for(i in 1:987){
  rates2 <- dt2_n[i,1:50]
  market_caps2 <- dt2_n[i,51:100]
  
  notech_weighted_mean2[i,1]<-sum(rates2*market_caps2)/sum(market_caps2)
}


# 코로나 이후 coVaR도출(네이버, 카카오 제외)
library(quantreg)

coefficients_A2<-matrix(0,50,1)
coefficients_B2<-matrix(0,50,1)

CoVaR_5_2<-matrix(0,50,1)
CoVaR_50_2<-matrix(0,50,1)
ΔCoVaR_2<-matrix(0,50,1)

for(i in 1:50){
  coefficients_A2[i, 1] <- rq(weighted_mean2[, i] ~ dt2_n[[i]], tau=0.05)$coefficients[1]
  coefficients_B2[i, 1] <- rq(weighted_mean2[, i] ~ dt2_n[[i]], tau=0.05)$coefficients[2]
  
  CoVaR_5_2[i,1] <- coefficients_A2[i, 1]+coefficients_B2[i, 1]*notech_VaR2[i,1]
  CoVaR_50_2[i,1]<- coefficients_A2[i, 1]+coefficients_B2[i, 1]*notech_mean_2[i,1]
  
  ΔCoVaR_2[i,1] <- CoVaR_5_2[i,1] - CoVaR_50_2[i,1]
}

write.csv(ΔCoVaR_2, "코로나 이후 증분CoVaR 최종.csv")

# 유의성 검정
summary(coefficients_A2, method = "boot") # 신뢰구간에 0이 포함되지 않으므로 유의미함(-2.363 ~ -1.184)
summary(coefficients_B2, method = "boot") # 신뢰구간에 0이 포함되지 않으므로 유의미함(0.06065 ~ 0.81529)


#카카오의 코로나 이후 coVaR도출
reg_k_2 <- rq(notech_weighted_mean2~dt2[[22]],tau=0.05)

CoVaR_5_k_2 <- reg_k_2$coefficients[1]+reg_k_2$coefficients[2]*VaR_2[22,1]
CoVaR_50_k_2 <- reg_k_2$coefficients[1]+reg_k_2$coefficients[2]*mean_2[22,1]

ΔCoVaR_k_2 <- CoVaR_5_k_2 - CoVaR_50_k_2

print(ΔCoVaR_k_2)  #-0.8199009

# 유의성 검정
summary(reg_k_2 , se = "boot") # 0에 매우 가까운 값이 나와서 0으로 표기


#네이버의 코로나 이후 coVaR도출
reg_n_2 <- rq(notech_weighted_mean2~dt2[[23]],tau=0.05)

CoVaR_5_n_2 <- reg_n_2$coefficients[1]+reg_n_2$coefficients[2]*VaR_2[23,1]

CoVaR_50_n_2 <- reg_n_2$coefficients[1]+reg_n_2$coefficients[2]*mean_2[23,1]

ΔCoVaR_n_2 <- CoVaR_5_n_2 - CoVaR_50_n_2

print(ΔCoVaR_n_2)
# 결과값은 위에 뽑아낸 csv 파일에 추가하겠음(-0.8219968)

# 유의성 검정
summary(reg_n_2, se = "boot") # p값 0.00017
