library(stringr)        # Wrappers for Common String Operations
library(DT)             # DataTables library
library(scales)         # Functions for Visualization
library(readr)          # Read rectangular data like 'csv'
library(data.table)     # Fast aggregation of large data
library(ggplot2)        # Create Elegant Data Visualisations
library(gridExtra)      # To arrange ggplots in grids
library(scales)         # To adjust numeric scales in ggplots
library(dplyr)          # For Data manupulation


# Updating all the available packages!

#install.packages(
#  pkgs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors=FALSE)$Package,
#  type = 'source'
#)

# Load Data
setwd('C:/Users/Saurabh/Desktop/InstacartDataMining/')
orders <- fread('../Input/orders.csv')
products <- fread('../Input/products.csv')
head(products)
products <- subset(products, select = -c(V5,V6,V7,V8))
aisles <- fread('../Input/aisles.csv')
departments <- fread('../Input/departments.csv')
order_products_train <- fread('../Input/order_products__train.csv')
order_products_prior <- fread('../Input/order_products__prior.csv')

# Little Preprocessing
orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

# View the available Data
head(orders)
summary(orders$eval_set)
head(products)
head(aisles)
head(departments)
head(order_products_train)
head(order_products_prior)

## 1. Orders by hour of the day

x11()
ggplot(orders, aes(x = order_hour_of_day)) + 
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 4), "cyan3",
                    rep("grey25", 8))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Hour of the Day",
       y = "Number of Orders",
       title = "Distribution of Orders by Hour of the Day") +
  scale_y_continuous(labels = comma)

## 2. Orders by Hour and Day of Week

x11()
p0 <- ggplot(orders[orders$order_dow == 0, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 0")

p1 <- ggplot(orders[orders$order_dow == 1, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 1")

p2 <- ggplot(orders[orders$order_dow == 2, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 2")

p3 <- ggplot(orders[orders$order_dow == 3, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 3")

p4 <- ggplot(orders[orders$order_dow == 4, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 4")

p5 <- ggplot(orders[orders$order_dow == 5, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 10), "gold", rep("grey25", 13))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 5")

p6 <- ggplot(orders[orders$order_dow == 6, ], aes(x = order_hour_of_day)) +
  geom_bar(fill = c(rep("grey25", 14), "gold", rep("grey25", 9))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "Day 6",
       x = "Hour of the Day")



grid.arrange(p0, p1, p2, p3, p4, p5, p6, ncol = 1)

## 3. Orders by Day of the Week

x11()
ggplot(orders, aes(x = order_dow)) + 
  geom_bar(fill = c(rep("gold", 2), rep("grey25", 5))) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Day of Week (unknown)",
       y = "Count",
       title = "Distribution of Orders by Day of Week") +
  scale_y_continuous(labels = comma)

## 4. How many items do People buy

x11()
order_products_train %>%
  group_by(order_id) %>%
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x = n_items)) +
  geom_bar(fill = c(rep("cyan4", 4),"gold",rep("cyan4",4),rep("grey25",66)))+
  geom_rug()+
  coord_cartesian(xlim = c(0,80))+
  scale_x_continuous(name = "Items per Order",labels = comma)+
  scale_y_continuous(name = "Count", labels = comma)+
  theme_classic()
?summarize
## 5. Orders by Days since Prior Orders

x11()

ggplot(orders, aes(x = days_since_prior_order)) +
  geom_bar(fill = c("cyan3", rep("grey25", 6), 
                    "cyan3", rep("grey25", 6), 
                    "cyan3", rep("grey25", 6),
                    "cyan3", rep("grey25", 6),
                    "cyan3", "grey50", "cyan3")) +
  theme_minimal() +
  labs(x = "Days Since Prior Order",
       y = "Count",
       title = "Distribution of Orders by Days Since Prior Order") +
  scale_y_continuous(labels = comma)

## 6. Best Selling Products

x11()
head(order_products_train)
order_products_train %>% 
  group_by(product_id)%>%
  summarize(count = n())%>%
  top_n(10, wt = count)%>%
  left_join(select(products,product_id,product_name),by="product_id")%>%
  arrange(desc(count))%>%
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill=c("cyan3",rep("grey25",9)))+
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1, colour = "grey0"),
        legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(x = "Product Name",
       y = "Number of times Ordered",
       title = "Distribution of Best Selling Products") +
  scale_y_continuous(labels = comma)

## 7. Top 10 aisles

aisles$aisle_id = as.character(aisles$aisle_id)
departments$department_id = as.character(departments$department_id)

x11()

order_products_train %>%
  left_join(products,by="product_id") %>%
  left_join(aisles,by="aisle_id") %>%
  left_join(departments,by="department_id")%>%
  group_by(aisle,department)%>%
  tally(sort=TRUE)%>%
  mutate(perc = round(100*n/nrow(order_products_train),2))%>%
  ungroup() %>%
  top_n(10,n) %>%
  ggplot(aes(x=reorder(aisle, -n), y=n, fill=department)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1, size = 12),
        panel.grid.major = element_blank()) +
  labs(x = "Aisle Names",
       y = "Number of Orders",
       title = "Distribution of Number of Orders per Aisle") +
  scale_y_continuous(labels = comma)

## 8. Look on the customer behavior and find if we can cluster them by how often they buy.
summary(orders$order_number)
head(orders)
#Group the Data by Users
user_freq <- orders %>%
  select(user_id, order_number, days_since_prior_order)%>%
  group_by(user_id) %>%
  summarise(total = max(order_number), 
            frequency = mean(days_since_prior_order, na.rm = TRUE))
glimpse(user_freq)
glimpse(user_freq$order_number)
#How many users haven't bought again ?
user_freq %>%
  filter(total == 1) %>%
  glimpse

# Make the Cluster
?set.seed
set.seed(42)
clus <- kmeans(user_freq[,2:3], 4)
clus
clus$cluster <- as.factor(clus$cluster)

# See the Cluster
Clusters <- clus$cluster
Clusters
x11()
ggplot(user_freq,
       aes(total, frequency, shape = Clusters, color = Clusters)) +
  geom_point()+
  labs(x = "Order Sequence for User",
       y = "Frequency of Orders(Mean of Days since Prior Order)",
       title = "Distribution of People rebuying items")+
  scale_colour_discrete(name="Clusters",
                        labels = c("Buys twice a week",
                                   "Buys monthly",
                                   "Buys weekly",
                                   "Buys every two weeks"))+
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank())
