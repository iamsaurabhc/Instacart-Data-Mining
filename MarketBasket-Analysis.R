#Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)
install.packages('methods')
getwd()
order_products_prior <- read_csv("../Input/order_products__prior.csv")
products <- read_csv("../Input/products.csv")
head(order_products_prior)

##DATA MUNGING

# Get the Shopping baskets
order_baskets <- order_products_prior %>%
                  inner_join(products,by="product_id") %>%
                  group_by(order_id) %>%
                  summarise(basket=as.vector(list(product_name)))
# Compute Transactions
orderTransactions <- as(order_baskets$basket,"transactions")

# See first 5 Transactions
inspect(orderTransactions[1:2,])

# count based product contigency matrix 
orderCM <- crossTable(orderTransactions[1:2,], measure="count", sort=TRUE) 

# Generate rules for Apriori
## With mining High-dimensional data:almost every event is rare.
# Keep support quite low.
support<- 0.01
itemSets <- apriori(orderTransactions,
                    parameter = list(target="frequent itemsets",supp=support,minlen=2),
                    control = list(verbose=FALSE))
inspect(itemSets)
sets_order_supp <- DATAFRAME(sort(itemSets,by="support",decreasing = F))
#1 Generate a Barplot of the rules
x11()
par(mar=c(5,18,2,2)+.1)
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .9, main = "Frequent Itemsets")
mtext(paste("Support:",support), padj = .8)

##Lets say we want to find products that occur together in each basket,
# we can't make any direct use of people who haven't yet 
# shown interest in multiple products. So we restrict the dataset to
# customers who have expressed interest in at least two products.
#11.
product_baskets <- orderTransactions[basketSizes>1]
#Let's try restricting the itemsets that we'll consider to those 
#that are supported by at least 10000 baskets.
#This leads to a minimum support of: 0.0033 (10k divided by 3million)
# conf of 0.01 (rule is correct 10% of time)

#12. Generate new rules
rules <- apriori(product_baskets,
                 parameter = list(supp=0.003269976,conf=0.01,maxlen=3),
                 control = list(verbose = FALSE))
plot(rules)
inspect(rules)
#Removing redundant rules
nonRedundantRules <- rules[unique(as.vector(is.subset(rules, rules)))]
plot(nonRedundantRules)
inspect(nonRedundantRules)

#13. Inspect the rules
inspect(sort(nonRedundantRules, by="lift")[1:5])
inspect(sort(rules, by="confidence")[1:5])

#Set support to be 0.001(1 in 1000) and conf of 0.25 (rule is correct 25% of time)
# Mining with different parameters
groceryRules <- apriori(orderTransactions,
                        parameter = list(support=0.001,confidence=0.25,minlen=2))
plot(groceryRules)
inspect(groceryRules)

#Removing redundant rules
groceryRules <- rules[unique(as.vector(is.subset(groceryRules, groceryRules)))]
plot(groceryRules)
inspect(groceryRules)

#Inspect the new Rules
inspect(sort(groceryRules, by="lift")[1:5])
inspect(sort(groceryRules, by="confidence")[1:5])

#Finding rules related to given items
#Rules that lead to buying Bananas (Items that lead to buying bananas)
# Suport 0.001 means 1 in 1000 Conf 0.08 means rule is correct 8% of time
bananaRules <- apriori (orderTransactions, 
                        parameter=list (supp=0.001,conf = 0.08,minlen=2), 
                        appearance = list (default="lhs",rhs="Banana"), 
                        control = list (verbose=F))
plot(bananaRules)
inspect(bananaRules)
#Remove redundant rules
bananaRules <- bananaRules[unique(as.vector(is.subset(bananaRules, bananaRules)))]
# See the rules
rules_conf <- sort (bananaRules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))
rules_supp <- sort (bananaRules, by="support", decreasing=TRUE) # 'high-support' rules.
inspect(head(rules_supp))
rules_lift <- sort (bananaRules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))

#To find out what products were purchased after/along with product X
# Here X is Banana
rulesAfterBanana <- apriori (orderTransactions, 
                             parameter=list (supp=0.001,conf = 0.05,minlen=2), 
                             appearance = list(default="rhs",lhs="Banana"), 
                             control = list (verbose=F))
plot(rulesAfterBanana)
inspect(rulesAfterBanana)

##TAKING SUBSETS OF RULES
#Need to market YOgurt
groceryRules <- apriori(orderTransactions,
                        parameter = list(support=0.001,confidence=0.25,minlen=2))

yogurtItemRules <- subset(groceryRules, items %pin% "Yogurt")
summary(yogurtItemRules)
inspect(sort(yogurtItemRules,by="supp")[1:5])
