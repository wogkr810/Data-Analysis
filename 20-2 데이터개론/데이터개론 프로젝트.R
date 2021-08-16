x<-read.csv(file.choose(),header=FALSE) 
head(x)                     
label<-c("middle","final","quiz","assignment")  
colnames(x)<-label                               
nrow(x)                                       
summary(x)                                    
attach(x)                                     

#필요한 라이브러리 설치 및 실행
install.packages("plotrix")
library(dplyr)
library(plotrix)

#최종성적 계산
total_sum<-c(1:nrow(x))
for (i in 1:nrow(x)){
  total_sum[i]<-0.4*middle[i]+0.4*final[i]+0.1*quiz[i]+0.1*assignment[i]
}
#데이터프레임에 비율환산한 최종성적 추가+최종성적 기준 내림차순 정리
x[,'total_sum']<-total_sum
x<-x[c(order(-x$total_sum)),]
rownames(x)<-NULL
head(x)
summary(x)

#각 열의 평가항목 분석
plot(x)
cor(x)    
boxplot(x)


#단순선형회귀 분석
res<-lm(total_sum~.,data=x)      
summary(res)      
par(mfrow=c(2,2)) 
plot(res)       



#각 항목별 정규분포 확인(히스토그램)
hist(middle, col="red", freq=FALSE, main="중간고사 점수",breaks=50)
lines(density(middle))
hist(final, col="orange", freq=FALSE, main="기말고사 점수", breaks=50)
lines(density(final))
hist(quiz, col="yellow", freq=FALSE, main="퀴즈 점수", breaks=50)
lines(density(quiz))
hist(assignment, col="green", freq=FALSE, main="과제 점수",breaks=10) 
lines(density(assignment))


#최종성적 
par(mfrow=c(1,1))
hist(total_sum,col='blue',main="최종 성적",breaks=100)
lines(density(total_sum))

#성적 부여
plot(x[,'total_sum'],main='최종 성적',xlab='등수',ylab='총점')
for(i in 1:nrow(x)){
  if((x[i,'total_sum']-x[i+1,'total_sum'])>0.5){
    print(i)
  }
}
#매우 촘촘한 데이터라 점수로 찾기에는 한계가 있음 -> 점찍고 index찾기

where=locator()
where
where=where$x
where<-round(where)
where[1]
where[2]



#성적 부여 

total_score<-c(1:nrow(x))
total_score
x[,'total_score']<-total_score
head(x)

#반복문을 통한 성적 부여

for (i in 1:nrow(x)){
  if (x[i,'total_sum']<20){
    x[i,'total_score']='F'
  }
  else if(i>round(0.9*nrow(x))){
    x[i,'total_score']='D'
  }
  else if(i>where[2]){
    x[i,'total_score']='C0'
  }
  else if(i>round(0.7*nrow(x))){
    x[i,'total_score']='C+'
  }
  else if(i>round(0.5*nrow(x))){
    x[i,'total_score']='B0'
  }
  else if(i>round(0.3*nrow(x))){
    x[i,'total_score']='B+'
  }
  else if(i>where[1]){
    x[i,'total_score']='A0'
  }
  else{
    x[i,'total_score']='A+'
  }
}

x['total_score']

#부여된 성적 분석
x['total_score']
table(x['total_score'])
per<-table(x['total_score'])
per<-as.data.frame(per)
per[,2]<-per[,2]/983*100
per<-per[,2]
per<-round(per,2)
per
final_percent<-data.frame(c("A+","A0","B+","B0","C+","C0","D","F"),per)
final_percent
colnames(final_percent)<-c("알파벳","퍼센트")
final_percent


#그림 그리기
par(mfrow=c(1,2))
pie(table(x['total_score']))
barplot(table(x['total_score']),col=table(x['total_score']))



