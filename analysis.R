library(magrittr)
library(data.table)
library(ggplot2)

sales_data= "data/data/sales.csv"
sales=fread(sales_data)

sales[, .(mean_quantity = mean(quantity)), by = customer_lifecycle_status]%>%
  ggplot(aes(customer_lifecycle_status, mean_quantity)) + geom_col(color="blue")
