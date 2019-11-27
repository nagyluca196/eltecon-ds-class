library(data.table)
library(magrittr)
library(ggplot2)
library(readr)

n<- 50
set.seed(1337)
x <- runif(n, min=0, max=1)
e <- rnorm(n,0,0.1)
y <- sin(2*pi*x)+e

data <- data.table(x=x, y=y)

fold <-  5

#5 egyforma nagyságú random alminta

cv_split <- split(sample(1:n), 1:fold)

formula <- as.formula("y~x+I(x^2)")

library(purrr)
imap(cv_split, ~ {
  model <- lm(formula=formula, data=data[-.x,])
  predict <- predict(model, newdata= data[.x,])
  # mean((p - data[.x, y])^2)
})

mean(unlist)

library(caret)
train_control <- trainControl(method="cv", number=5)
set.seed(2698)
model <- train(formula,
               data=data,
               trControl=train_control,
               method="lm")
model$resample
model$results$RMSE
