library(data.table)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)

data <- read_csv("~/Documents/GitHub/eltecon-ds-class/sales_data_for_clustering.csv", 
                 col_types = cols(price = col_number(), 
                                  product_id = col_number(), purchase_time = col_skip(), 
                                  quantity = col_number(), user_id = col_number()))
data <- as.data.frame(data)
summary(data)

#K-means clustering, based on: https://uc-r.github.io/kmeans_clustering#fn:scale


data <- na.omit(data)

data2 <- scale(data)

set.seed(122)

data3 <- sample(1:nrow(data2), 2000, replace=FALSE)

data3 <- data2[data3,]

data3 <- na.omit(data3)

#memory is not enough for all 93000 rows
distance <- get_dist(data3)

set.seed(123)

fviz_nbclust(data3, kmeans, method = "wss")

#optimal number of clusters=3

data4 <- kmeans(data3, 3, nstart=25)

fviz_cluster(data4, data=data3)
          
print(data4)

##Szűrés, új adatbázis vásárlások száma, termék id, termékár és összes költés alapján

adat <- read_csv("~/Documents/GitHub/eltecon-ds-class/sales_data_for_clustering.csv", 
                         col_types = cols(price = col_number(), 
                                          product_id = col_number(), purchase_time = col_skip(), 
                                          quantity = col_number(), user_id = col_number()))
adat <- as.data.table(adat)
is.data.table(adat)

adat <- adat[, buy:=(count=.N), by=user_id]
adat <- adat[, full_buy:=(sum(price)), by=.(user_id)]

set.seed(1234)

adat2 <- sample(1:nrow(adat), 3000, replace=FALSE)
adat2 <- adat[adat2,]

adat2 <- scale(adat2)

fviz_nbclust(adat2, kmeans, method = "wss")

#k=4

adat3 <- kmeans(adat2, 4, nstart=25)

print(adat3)

fviz_cluster(adat3, data=adat2)

#A clusterezés hasonló ízlésű fogyasztókra (darabszám, termék, összes költés) irányult.