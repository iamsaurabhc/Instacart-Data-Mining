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
head(order_products_prior)
# Get the Shopping baskets
order_baskets <- order_products_prior %>%
                  inner_join(products,by="product_id") %>%
                  group_by(order_id) %>%
                  summarise(basket=as.vector(list(product_name)))
head(order_baskets$basket[1])
head(products)
is.data.frame(order_baskets)
# Compute Transactions
orderTransactions <- as(order_baskets$basket,"transactions")

# See first 5 Transactions
inspect(orderTransactions[1:2,])
summary(orderTransactions)
#You can examine the distribution of transaction sizes

basketSizes<-size(orderTransactions)
summary(basketSizes)

# 1. Generate rules for Apriori (Frequent Items)
## With mining High-dimensional data:almost every event is rare.
# Keep support quite low.
support<- 0.01
itemSets <- apriori(orderTransactions,
                    parameter = list(target="frequent itemsets",supp=support,minlen=2),
                    control = list(verbose=FALSE))
inspect(itemSets)
summary(itemSets)

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
product_baskets <- orderTransactions[basketSizes > 1 ]
#Let's try restricting the itemsets that we'll consider to those 
#that are supported by at least 10000 baskets.
#This leads to a minimum support of: 0.0005 (1.5k divided by 3million)
# conf of 0.01 (rule is correct 1% of time)

#12. Generate new rules
  rules <- apriori(product_baskets,
                   parameter = list(supp=0.0005,conf=0.01,minlen=2),
                   control = list(verbose = FALSE))
summary(rules)
inspect(rules[1:4])
#Removing redundant rules
nonRedundantRules <- rules[unique(as.vector(is.subset(rules, rules)))]

# Create a Scatter Plot for rules
x11()
plot(nonRedundantRules)
inspect(nonRedundantRules[1:10])
summary(nonRedundantRules)

## Matrix shading
## --------------
## The following techniques work better with fewer rules
subrules <- sample(nonRedundantRules, 15)
subrules
## 2D matrix with shading
#plot(subrules, method="matrix")
## 3D matrix
#plot(subrules, method="matrix", engine = "3d")
## Matrix with two measures
plot(subrules, method="matrix", shading=c("lift", "confidence"))

## Parallel coordinates plot
## -------------------------
inspect(nonRedundantRules[1:5])
subrules <- sample(nonRedundantRules, 20)
?sample
inspect(subrules)
x11()
plot(subrules, method="paracoord")
#plot(subrules2, method="paracoord", reorder=TRUE)


# Inspect the rules
inspect(sort(nonRedundantRules, by="lift")[1:5])
inspect(sort(rules, by="confidence")[1:5])

#Set support to be 0.001(1 in 1000) and conf of 0.25 (rule is correct 25% of time)
# Mining with different parameters
#groceryRules <- apriori(orderTransactions, parameter = list(support=0.001,confidence=0.25,minlen=2))
#plot(groceryRules)
#inspect(groceryRules)

#Removing redundant rules
#groceryRules <- rules[unique(as.vector(is.subset(groceryRules, groceryRules)))]
#plot(groceryRules)
#inspect(groceryRules)

#Inspect the new Rules
#inspect(sort(groceryRules, by="lift")[1:5])
#inspect(sort(groceryRules, by="confidence")[1:5])

#Finding rules related to given items
#Rules that lead to buying Bananas (Items that lead to buying bananas)
# Suport 0.0005 means 1 in 2000 Conf 0.08 means rule is correct 5% of time

rules<-apriori(orderTransactions, parameter=list(supp=0.0005,conf = 0.05), 
               appearance = list(default="lhs",rhs="Banana"),
               control = list(verbose=F))

bananaRules <- bananaRules[unique(as.vector(is.subset(bananaRules, bananaRules)))]

bananaRules<-sort(bananaRules, decreasing=TRUE,by="confidence")
inspect(bananaRules[1:10])
#Remove redundant rules


#Plot graph-based visualisation:
subrules2 <- sample(sort(bananaRules, by="lift"), 10)
x11()
plot(subrules2, method="graph",control=list(type="items",main=""))
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
plot(rulesAfterBanana, method="graph",control=list(type="items",main=""))
inspect(rulesAfterBanana[1:5])
rules_lift <- sort (rulesAfterBanana, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(rules_lift)

##TAKING SUBSETS OF RULES
#Need to market YOgurt
groceryRules <- apriori(orderTransactions,
                        parameter = list(support=0.0005,confidence=0.25,minlen=2))

yogurtItemRules <- subset(groceryRules, items %pin% "Yogurt")
summary(yogurtItemRules)
inspect(sort(yogurtItemRules,by="supp")[1:5])
