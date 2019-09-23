library(data.table)
library(ggplot2)
library(magrittr)
library(readxl)
#Hosszas próbálkozás után sem sikerült a data.csv oszlopait character-ről numericre állítani, ezért az xls fájl importálásánál használt, R által generált kódot adtam meg a scriptben.
data <- read_excel("data.xls", 
                      col_types = c("numeric", "text", "text", 
                                    "text", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))
summary(data)

data$`Time Code`=NULL
data$`Country Code`=NULL

summary(data)

data=na.omit(data)

summary(data)

ggplot(data, aes(lifeexp_total, gdppercap)) + geom_point()
ggplot(data, aes(gdppercap))+geom_histogram()

summary(data$gdppercap)

remove_high_gdp = function(data){
  data[ gdppercap >= 90000, gdppercap := NA]
}

remove_high_gdp(data)

ggplot(data, aes(lifeexp_total, current_health_exp)) + geom_point()
ggplot(data, aes(lifeexp_female, current_health_exp)) + geom_point()
ggplot(data, aes(lifeexp_male, current_health_exp)) + geom_point()
ggplot(data, aes(lifeexp_total))+geom_histogram()
summary(data$lifeexp_total)

ggplot(data, aes(lifeexp_total, urbanpop)) + geom_point()
