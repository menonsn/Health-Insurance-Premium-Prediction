library(psych)
library("ggplot2")
library (DAAG)
library(latticeExtra)

insurance <- read.csv("C:/Users/siddh/OneDrive/Desktop/Classes/3rd Flex/DAM/insurance.csv")
head(insurance)

plot(insurance$charges, insurance$age, pch=2,col="red")
plot(insurance$sex, insurance$charges, pch=2,col="green")
plot(insurance$smoker, insurance$charges, pch=2,col="purple")

pairs.panels(insurance, pch=20)

table(as.numeric(insurance$sex))
table(as.numeric(insurance$smoker))

model1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data=insurance)
summary(model1)

model1 <- lm(charges ~ age + bmi + children + smoker, data=insurance)
anova(model,model1)

model_insurance_age <- lm(insurance$charges ~ insurance$age,data=insurance)
summary(model_insurance_age)

model_insurance_age <- lm(insurance$charges ~ insurance$sex,data=insurance)
summary(model_insurance_age)

model_insurance_age <- lm(insurance$charges ~ insurance$bmi,data=insurance)
summary(model_insurance_age)

model_insurance_age <- lm(charges ~ children, data=insurance)
summary(model_insurance_age)

model_insurance_age <- lm(charges ~ smoker, data=insurance)
summary(model_insurance_age)

model_insurance_age <- lm(charges ~ region, data=insurance)
summary(model_insurance_age)

model_insurance <- lm(insurance$charges ~ insurance$age + insurance$sex + insurance$bmi + insurance$children + insurance$smoker  , data=insurance)
summary(model_insurance)

model_new_1=lm(charges ~ age + children + bmi + bmi : as.numeric(smoker), data=insurance)
summary(model_new_1)

MSRes=summary(model_new_1)$sigma^2
MSRes
standardized_res=model_new_1$residuals/summary(model_new_1)$sigma
head(standardized_res)

par(mfrow=c(1,2))
# generate QQ plot
qqnorm(model_new_1$residuals,main="model1")
qqline(model_new_1$residuals)

par(mfrow=c(1,3))
plot(insurance$age,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$sex,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$bmi,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$children,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$smoker,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$region,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
plot(insurance$charges,model_new_1$residuals,pch=20)
abline(h=0,col="grey")
# generate residual plot, fitted values vs residual
plot(model_new_1$fitted.values,model_new_1$residuals,pch=20)
abline(h=0,col="grey")


KCV <- cv.lm(data = insurance, model_new_1, m=5, seed=123)
preddata2 <- data.frame(age = 56, sex = 'female', bmi = 39.82, children = 0, smoker = '1', region = 'southeast')
predi2 <- predict(model_new_1, preddata2, type="response")
predi2

n = dim(insurance)[1]
LOOCV <- cv.lm(data=insurance, model_new_1, m=n, seed=123)
preddata <- data.frame(age = 18, sex = 'male', bmi = 33, children = 2, smoker = 1, region = 'southeast')
predi <- predict(model_new_1, preddata, type="response")
predi

mspe <- sum((log(insurance$charges)-KCV$cvpred)^2)/n
mspe

press <- sum((log(insurance$charges)-KCV$cvpred)^2)
press

pred <- 1-sum((log(insurance$charges)-KCV$cvpred)^2)/sum((log(insurance$charges)-mean(log(insurance$charges)))^2)
pred

summary(model_new_1)$r.squared

preddata <- data.frame(age = 68, sex = 'male', bmi = 39, children = 2, smoker = 2, region = 'southeast')
predi <- predict(model_new_1, preddata, type="response")
predi

pred_model <- predict(model_new_1,insurance,interval = "prediction",level=0.95, type="response")
cbind(insurance, fit = pred_model)
plot(insurance$age + insurance$bmi + insurance$children + as.numeric(insurance$smoker), insurance$charges,xlab="Co-variates", ylab="Charges",pch=20)
abline(model_new_1,col="blue")
points(insurance$charges, insurance$fit.fit, pch=2,col="red")





