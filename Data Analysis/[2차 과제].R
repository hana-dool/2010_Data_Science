setwd('C:/Users/goran/Desktop/개인 자료')
data = read.csv('R.csv')
#data['rain']=exp(data['rain'])-1
#data['youtube'] = exp(data['youtube'])-1
data[,c(5,7,8,9,10,11)] = scale(data[,c(5,7,8,9,10,11)])
data[,'temp'] = abs(data[,'temp'])
data = data[-577,]
row.names(data) <- NULL
data

# 요일,월별 변화를 알아보기위해 subway, naver 변수를 삭제
# 이 상태에서 알아본 요일별 추이는 다음과 같음
model<-lm(order~.,data=subset(data,select = -c(rain_ox,rain1,rain2,rain3,corona,bok)))
summary(model) 
barplot(model$coefficients,cex.names = 0.7)

# 최종모델 (독립성이 크게 위배되어 일별 기울기는 믿을만하지 못하다.)
model<-lm(order~.,data=subset(data,select = -c(rain_ox,rain1,rain2,rain3,corona,bok)))
summary(model) 
barplot(model$coefficients,cex.names = 0.7)

library(gvlma)
gvlma(model)


# store 의 증가 = 인구도 같이 증가 
# 결국에는 비슷하다고 생각 가능합니다. 즉 초조해할 필요 없음

# 인구 왜뺏냐?
# store 과 엄청난 상관관계
# 자영업 포화상태 ~ 치킨집이 '살아남을 만큼' 만 생겨나고 있다고 생각 가능
# 즉 치킨집의 수는 인구수를 반영하고 있다는 것이다.
# naver의 경우는.. 별로 안좋은거같은데?


#---------- 데이터 discussion ---------------#
#많은 방법론을 보여주고, 그 결과를 비교, 보여줌
# 해석력을 유지하기 위해 regression 이용
# 그 가정사항에 대해 잘 만족하는지를 조사
# 그 한계를 설명하자. 

#------------------------ 성능시험 -------------------------------#
set.seed(12345)
#getting training data set sizes of .20 (in this case 20 out of 100)
train_size <- floor(0.80*nrow(data))

in_rows <- sample(c(1:nrow(data)), size = train_size, replace = FALSE)
train <- data[in_rows, ]
test <- data[-in_rows, ]

model_test<-lm(order~.,data=subset(train,select = -c(rain_ox,rain1,rain2,rain3,corona)))
pred_y<-predict(model_test,test)
mean((test$order - pred_y)^2)

# 오 100까지 낮아진 모습..! 

#------------------------ 검정 -----------------------------------#

library(corrplot)
corrplot(cor(data),method = 'number')

#------------Linearlity 조사 ----------#
# 빨간 선(residual 들의 추세) 가 0에서 직선 형태.
# 모델이 linear 하다고 볼 수 있겠다.
# 즉 에러는 크지만 X,y 간에 linear 관계가 있다고 볼 수 있다.
# 위의 OULIER 들은 model 이 예측 못하는 지점
plot(model,1)

#------------Normality 조사 -----------#
plot(model, 2) # 양 끝 지점에서 들리는 모습?
library(car)
qqPlot(model,main="Q-Q_ plot")
# distribution of studentized residuals
h = hist(model$residuals,breaks = 50)
g = model$residuals
# 어느정도 Normal 이라고 볼 수는 있을거 같다.
xfit <- seq(min(g), max(g), length = 40) 
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) 
yfit <- yfit * diff(h$mids[1:2]) * length(g) 
lines(xfit, yfit, col = "black", lwd = 2)



#------------오차의 독립성--------------#
# 시계열. 코로나 이전,이후가 상당히 연관있어보인다.
plot(model$residuals)
# 1 차이로 본 잔차이다. 서로 연관은없는듯
plot(model$residuals[c(1:990)],model$residuals[c(2:991)])
acf(model$residuals) # 자기상관여부는 괜찮은듯
crPlots(model)

plot(data$subway ,model$residuals )
plot(data$naver ,model$residuals )
#----------- 등분산 가정 ---------------#
plot(model, 1)
#커질수록 어느정도 오차가 커지는 느낌.
#약간의 이분산이 있다고 생각된다.

#----------- High leverage points , Outlier ----------#
# High leverage points
# 아마 우리가 집어넣지 못한 변수들에 대해서 이런게 나타나는거 같은
plot(model, 4)
data[1639,]
data[1422,] 
# 배달이 많은 경우였다. 즉 단체주문, 공휴일 등이 겹쳐서 매우 큰 값이 나온듯

#------------ 다중공선성----------------#
# 크게 이슈는 없어보인다.
library('car')
vif(model)
c = vif(model)
barplot(c,horiz = T,xlim=c(0,6))
abline(v=5,col='red')

barplot(c[c(1:7)])
# 그 떨어진 지점들을 조사해본 결과 이벤트들(반값할인 이벤트,크리스마스) 이였다.
# 그 부분을 제외한다면 이 그래프는 등분산성을 만족할 수 있을것이다.


#--------------Visualization of result------------------#
library(dotwhisker)
library(coefplot)
model<-lm(order~.,data=subset(data,select = -c(rain_ox,rain1,rain2,rain3,corona,bok)))
summary(model) 
coefplot(model,intercept=FALSE)

barplot(model$coefficients[c('mon','tue','wed','thu','sat','sun')],cex.names = 2,cex.axis = 2)

model<-lm(order~.,data=subset(data,select = -c(rain_ox,rain1,rain2,rain3,corona,bok,subway,naver,bok)))
summary(model)
coefplot(model,intercept=FALSE)

# 95% interval
confint(model, 'mon9', level=0.95)

