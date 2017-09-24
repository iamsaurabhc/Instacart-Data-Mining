library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

#Read all the Data availiable

orders <- fread('../Input/orders.csv')
products <- fread('../Input/products.csv')
order_products <- fread('../Input/order_products__train.csv')
order_products_prior <- fread('../Input/order_products__prior.csv')
aisles <- fread('../Input/aisles.csv')
departments <- fread('../Input/departments.csv')

# View the input data one by one
kable(head(orders,12))
kable(head(order_products,12))
kable(head(products,12))
kable(head(order_products_prior,12))
kable(head(aisles,12))
kable(head(departments,12))

glimpse(departments)

#Data Preprocessing: Converting Character variables to Factor
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day),eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

#Exploratory Data Analysis

##1.When do people order??
###1.1.Hour of the Day:
x11()
orders %>% 
  ggplot(aes(x = order_hour_of_day))+
  geom_histogram(stat="count",fill="green")
###1.1.Conclusion: Most orders are between 8:00am to 6:00pm

###1.2.Day of the Week
x11()
orders %>%
  ggplot(aes(x = order_dow))+
  geom_histogram(stat="count",fill="blue")
###1.2.Conclusion: Saturdays(0) and Sundays(1) are having highest count of orders
##1.Ends##

##2. When do People Order again?
x11()
orders %>%
  ggplot(aes(x = days_since_prior_order))+
  geom_histogram(stat = "count",fill="red")
##2.Conslusion: People seem to order more often after exactly 1 week

##3.How many prior orders are there?
x11()
orders %>%
  filter(eval_set=="prior") %>%
    count(order_number) %>%
      ggplot(aes(order_number,n))+
        geom_line(color="red",size=1)+
          geom_point(size=2,color="red")
##3.Conslusion: There are always atleast 3 prior orders

##4. How many items do People buy?
###4.1. From Train Set
x11()
order_products %>%
  group_by(order_id) %>%
    summarize(n_items = last(add_to_cart_order)) %>%
      ggplot(aes(x = n_items)) +
        geom_histogram(stat="count",fill="red")+
          geom_rug()+
            coord_cartesian(xlim = c(0,80))
###4.1.Conclusion: Most people buy around 5 items per order
###4.2. From Prior Set
x11()
order_products_prior %>%
  group_by(order_id) %>%
    summarize(n_items = last(add_to_cart_order)) %>%
      ggplot(aes(x = n_items)) +
        geom_histogram(stat="count",fill="red")+
          geom_rug()+
            coord_cartesian(xlim = c(0,80))
###4.2.Conclusion: Most people buy around 5 items per order
##4.Ends##

##5. Bestselling Product
tmp <- order_products %>%
        group_by(product_id) %>%
        summarize(count = n()) %>%
        top_n(10,wt = count) %>%
        left_join(select(products,product_id,product_name),by="product_id") %>%
        arrange(desc(count))
kable(tmp)
##5. Conclusion: Highest Selling -- (ID:24852)18,726 times:Banana
##               Lowest Selling -- (ID:49687)1 time: Smartblend Healthy Metabolism Dry Cat Food

View(products)
