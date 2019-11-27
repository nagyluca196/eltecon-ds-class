library(data.table)
library(magrittr)
library(ggplot2)
library(readr)
data <- read_delim("data.csv", ";", escape_double = FALSE, 
                   col_types = cols(current_health_exp = col_number(), 
                                    gdpgrowth = col_number(), gdppercap = col_number(), 
                                    lifeexp_female = col_number(), lifeexp_male = col_number(), 
                                    lifeexp_total = col_number(), urbanpop = col_number()), 
                   trim_ws = TRUE)
as.data.frame(data)

#models: lifeexp(gdp, healthexp, urbanpop), lifeexp (gdp, healthexp, urbanpop, healthexp*urbanpop), 
#lifeexp(gdp, healthexp)

data_1 <- split(data, )