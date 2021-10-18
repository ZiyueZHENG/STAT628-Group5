# Bodyfat model
# Author: Ziyue Zheng, Yifan Du, Karen Abraham

## Read and Clean data ##
bodyfat <- read.csv('./data/BodyFat.csv')
outliers <- bodyfat[bodyfat$BODYFAT<=3,]$IDNO
bodyfat <- bodyfat[-outliers,]

## Take a brief look of the data ##
head(bodyfat)
summary(bodyfat)
hist(bodyfat$BODYFAT,main='Histogram of bodyfat',xlab = 'Bodyfat',ylab='Frequency')
grid(0,6)
cor(bodyfat$BODYFAT,bodyfat[,4:17])


## Using statistical method to construct a model ##
library(car)
bodyfat_t = bodyfat[,c(-1,-3)]
lm1 <- step(lm(BODYFAT~.,data=bodyfat_t),direction = "backward")
anova(lm(BODYFAT~AGE + WEIGHT + NECK + ABDOMEN + THIGH + FOREARM + WRIST,data=bodyfat))
anova(lm(BODYFAT~AGE + WEIGHT + NECK + ABDOMEN + FOREARM + WRIST,data=bodyfat))
anova(lm(BODYFAT~AGE + WEIGHT + NECK + ABDOMEN + WRIST,data=bodyfat))
anova(lm(BODYFAT~AGE + WEIGHT + NECK + ABDOMEN,data=bodyfat))
summary(lm(BODYFAT~AGE + WEIGHT + NECK + ABDOMEN,data=bodyfat))
summary(lm(BODYFAT~WEIGHT + NECK + ABDOMEN,data=bodyfat))


## Evaluation through R square ##
# AWN approach
model_A <- lm(BODYFAT~WEIGHT + NECK + ABDOMEN,data=bodyfat)
summary(model_A)
# BMI approach
model_B <- lm(BODYFAT~ADIPOSITY + AGE,data = bodyfat)
summary(model_B)
# YMCA approach
model_Y <- lm(BODYFAT~ADIPOSITY+ADIPOSITY/WEIGHT,data = bodyfat)
summary(model_Y)
# Navy approach
model_N <- lm(BODYFAT~log(ABDOMEN-NECK)+log(HEIGHT),data = bodyfat)
summary(model_N)


## Evaluation through RMSE ##
rmse <- function(actual,predicted){
  return(sqrt(mean((actual-predicted)^2)))
}
rmse(bodyfat$BODYFAT,predict(model_A))
rmse(bodyfat$BODYFAT,predict(model_B))
rmse(bodyfat$BODYFAT,predict(model_N))
rmse(bodyfat$BODYFAT,predict(model_Y))


## Model diagnostics ##
library(lmtest)
error <- predict(model_A)-bodyfat$BODYFAT
mean(error)
# testing Normality 
qqnorm(error)
qqline(rnorm(500,mean(error),sd(error)),col="red",lwd = 2)
grid(6,6)
# testing Homoscedasticity
plot(error,main='scatterplot of error')
grid(6,6)
bptest(model_A) # A insignificant p-value means there is no Heteroscedasticity


## Model performance ##
relative_error <- (error/bodyfat$BODYFAT)
hist(relative_error,breaks = 10)
grid(0,8)
mean(abs(relative_error))
# visualization
plot(predict(model_A),bodyfat$BODYFAT,
     main = 'Model Performance',ylab = 'ground truth',xlab = 'prediction',
     xlim = c(0,50),ylim = c(0,50),pch=3)
grid(5,5)
abline(a=0,b=1,col="red",lwd=3)


