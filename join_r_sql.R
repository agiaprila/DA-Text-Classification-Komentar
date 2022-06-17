install.packages("RMySQL")

library(RMySQL)

rconn <- dbConnect(MySQL(),
                  user="root",
                  password="",
                  host="127.0.0.1",
                  dbname="northwind")
dbListTables(rconn)


##

customers <- dbReadTable(rconn, name = "customers")
head(customers)

install.packages("dplyr")

library(dplyr)
customers <- customers %>%
  select(id, first_name, job_title)

colnames(customers) <- c("customer_id", "first_name", "job_title")

##
orders <- dbReadTable(rconn, name = "orders")
head(orders)

orders <- orders %>%
  select(id, customer_id, order_date)


data_inner <- inner_join(customers, orders, by = c("id" = "customer_id")) 

## inner
data_inner <- inner_join(customers, orders, by = "customer_id")    

## left
data_left <- left_join(customers, orders, by = "customer_id") 

## right
data_right <- right_join(customers, orders, by = "customer_id")    

## full
data_full <- full_join(customers, orders, by = "customer_id")    




