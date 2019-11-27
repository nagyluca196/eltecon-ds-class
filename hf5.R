library(data.table)
library(ggplot2)
library(readxl)
library(caret)
data <- read_excel("data.xls", 
                   col_types = c("numeric", "text", "text", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric"))
as.data.frame(data)

#Set of candidate models:
# lifeexp=gdppercap+current_health_exp
# lifeexp=gdppercap+urbanpop
# lifeexp_total= gdppercap + urabnpop + current_health_exp*gdpgrowth

set.seed(2460)

summary(data$country)
n <- 217
train_p <- 0.5
train <- sample(1:n, floor(n*train_p))
data_train <- data[train,]
data_val <- data[-train,]

pt <- ggplot(data_train, aes(lifeexp_total, gdppercap))+geom_point()+theme_classic()
pt2 <- ggplot(data_val, aes(gdppercap, lifeexp_total))+geom_point()+theme_classic()

formula1 <- as.formula(lifeexp_total~gdppercap+current_health_exp)
formula2 <- as.formula(lifeexp_total~gdppercap+urbanpop)
formula3 <- as.formula(lifeexp_total~gdppercap+urabnpop+currenthealth*gdpgrowth)

model1 <- lm(lifeexp_total~gdppercap+current_health_exp, data=data_train)
summary(model1)
plot(model1)
pred1 <- predict(model1, newdata = data)

cbind(data, pred1)

pt+
  geom_line(data=data, aes(pred1, gdppercap))+
  geom_point(data=data_val, mapping=aes(lifeexp_total), color="red")+xlim(50,98)

pt2+
  geom_line(data=data, aes(gdppercap, pred1))+
  geom_point(data=data_val, color="red")+xlim(0,100000)

model2 <- lm(lifeexp_total~gdppercap+urbanpop, data=data_train)
summary(model2)
plot(model2)
pred2 <- predict(model2, newdata = data)

cbind(data, pred2)

pt+
  geom_line(data=data, aes(pred2, gdppercap))+
  geom_point(data=data_val, mapping=aes(lifeexp_total), color="red")+xlim(50,98)

pt2+
  geom_line(data=data, aes(gdppercap, pred2))+
  geom_point(data=data_val, color="red")+xlim(0,100000)

model3 <- lm(lifeexp_total~gdppercap+urbanpop, data=data_train)
summary(model3)
plot(model3)
pred3 <- predict(model3, newdata = data)

cbind(data, pred3)

ggplot(data_train, aes(lifeexp_total))+
  geom_line(data=data, aes(pred3, gdppercap))+
  geom_point(data=data_val, mapping=aes(lifeexp_total, gdppercap), color="red")+xlim(50,98)

pt2+
  geom_line(data=data, aes(gdppercap, pred3))+
  geom_point(data=data_val, color="red")+xlim(0,100000)

#a három modellel generált predicted értékeket illesztettem a train és validation modellekre. 
#a tengelyek felcserélés szakadást okoz a predict line-ban, ezért a várható élettartam lett az x tengely



#cross validation (?)

folds=5
y=data$gdppercap
set.seed(2461)
holdout <- split(sample(1:n), 1:folds)
MSE1 <- function(y, pred) {
  mean((y - pred)**2)
}

CV_MSE <- imap(holdout, ~{
  model <- lm(formula1, data=data[-.x,])
  pred <- predict(model, newdata = data[.x,])
  MSE(data[.x,y],pred)
})


train_control <- trainControl(method="cv", number=5)

set.seed(2461)
model <- train(model1, 
               data=data, 
               trControl=train_control, 
               method="lm")

model$resample
model$results$RMSE**2
