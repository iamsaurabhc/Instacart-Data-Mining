#Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)

order_products_prior <- read_csv("../Input/order_products__prior.csv")
products <- read_csv("../Input/products.csv")
head(order_products_prior)

##DATA MUNGING

#1.Get the Shopping baskets
order_baskets <- order_products_prior %>%
                  inner_join(products,by="product_id") %>%
                  group_by(order_id) %>%
                  summarise(basket=as.vector(list(product_name)))
#2.Compute Transactions
orderTransactions <- as(order_baskets$basket,"transactions")
#3.Gives us the most Frequent Items and Distribution of Transaction Sizes
summary(orderTransactions)
basketSizes<- size(orderTransactions)
quantile(basketSizes,probs = seq(0,1,0.1))
#4.Analyzing the baskets
x11()
hist(size(orderTransactions),
     breaks = 0:150, 
     xaxt= "n", 
     ylim = c(0,250000),
     main="Number of Items per basket",
     xlab = "Number of Items")
axis(1,at=seq(0,160,by=10),cex.axis=0.8)
mtext(paste("Total:",length(orderTransactions),"baskets,",sum(size(orderTransactions)),"items"))

#5. Relative Frequency of each product in transaction data
item_Frequencies <- itemFrequency(orderTransactions)
sum(item_Frequencies)

#5.1 Item Frequency Plot
itemFrequencyPlot(orderTransactions,support=0.1)

#Note that the frequencies don't sum to 1. 
#We can recover the number of times that each product occurred in the data by 
#normalizing the item frequencies and multiplying by the total number of items.
#6. Product Count
productCount <- (item_Frequencies/sum(item_Frequencies))*sum(basketSizes)
summary(productCount) #Highest being Banana
productCount[1:10]

#7. List 10 most popular products in the basket
orderedProducts <- sort(productCount,decreasing = T)
orderedProducts[1:10]

#8. Frequency of Best Selling product
(orderedProducts[1]/dim(orderTransactions)[1])*100
#The most popular product in the dataset occured in approx 15% of baskets

#9. Generate rules for Apriori
## With mining High-dimensional data:almost every event is rare.
# Keep support quite low.
support<- 0.01
itemSets <- apriori(orderTransactions,
                    parameter = list(target="frequent itemsets",supp=support,minlen=2),
                    control = list(verbose=FALSE))
inspect(itemSets)
sets_order_supp <- DATAFRAME(sort(itemSets,by="support",decreasing = F))
#10. Generate a Barplot of the rules
x11()
par(mar=c(5,18,2,2)+.1)
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:",support), padj = .8)

##Lets say we want to find products that occur together in each basket,
# we can't make any direct use of people who haven't yet 
# shown interest in multiple products. So we restrict the dataset to
# customers who have expressed interest in at least two products.
#11.
product_baskets <- orderTransactions[basketSizes>1]
10000/dim(product_baskets)[1]
#Let's try restricting the itemsets that we'll consider to those 
#that are supported by at least 10000 baskets. 
#This leads to a minimum support of: 0.0033

#12. Generate new rules
rules <- apriori(product_baskets,
                 parameter = list(supp=0.003269976,conf=0.01,maxlen=3),
                 control = list(verbose = FALSE))
plot(rules)
#13. Inspect the rules
inspect(sort(rules, by="lift")[1:5])
inspect(sort(rules, by="confidence")[1:5])
