library(data.table)
library(glmnet)
library(ggplot2)
library(magrittr)
library(glue)
library(purrr)
data <- fread("OnlineNewsPopularity_mod.csv")

summary(data$shares)

plot1 <- ggplot(data, aes(x=shares))+geom_histogram(bins = 50)
plot1
plot2 <- ggplot(data, aes(x=log(shares+1)))+geom_histogram(bins= 50)
plot2                

test_proportion <- 0.2
test_data <- sample(1:n, floor(n*test_proportion))             

data_test <- data[test_data,]
data_train <- data[-test_data,]

var.names <- names(data)
x.images <- var.names[c(9,10)]
x.channel <- var.names[13:18]
x.words <- var.names[2:6]
x.sent <- var.names[40:47]
