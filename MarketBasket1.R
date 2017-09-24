#Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)

order_products_prior <- read_csv("../Input/order_products__prior.csv")
products <- read_csv("../Input/products.csv")
##DATA MUNGING
head(products)
#1.Get the Shopping baskets
order_baskets <- order_products_prior %>%
  inner_join(products,by="product_id") %>%
  group_by(order_id) %>%
  summarise(basket=as.vector(list(product_name)))
#2.Compute Transactions
orderTransactions <- as(order_baskets$basket,"transactions")

#3. See the summary
summary(orderTransactions)

#4. See first 5 transactions
inspect(orderTransactions[1:5])

#5. See the support level of first 3 items in the data
itemFrequency(orderTransactions[,1:3])

#6. Items with atleast 10% support
itemFrequencyPlot(orderTransactions,support=0.1)

#7. Or, Top 20 items with their frequency
x11()
itemFrequencyPlot(orderTransactions,topN=20)

#8. Sparse matrix for first 5 transactions
x11()
image(orderTransactions[1:15])
#NOT VISIBLE :(
image(sample(orderTransactions,100))

#Training a Model on Data

apriori(orderTransactions)
#no rules for support 0.1 and cof 0.8

#Set support to be 0.001(1 in 1000) and conf of 0.25 (rule is correct 25% of time)
groceryRules <- apriori(orderTransactions,
                        parameter = list(support=0.001,confidence=0.25,minlen=2))
groceryRules
#See the rules
rules_conf <- sort(groceryRules,by="conf",decreasing = TRUE)

inspect(head(rules_conf))

#Removing redundant rules
subsetRules <- which(colSums(is.subset(groceryRules, groceryRules)) > 1)
length(subsetRules)

groceryRules <- groceryRules[-subsetRules]

#Finding rules related to given items
#Rules that lead to buying Bananas
bananaRules <- apriori (orderTransactions, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="Banana"), control = list (verbose=F))

rules_conf <- sort (bananaRules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))
##Evaluating Model performance

#To find out what products were purchased after/along with product X

rulesAfterBanana <- apriori (orderTransactions, parameter=list (supp=0.001,conf = 0.15), appearance = list(default="rhs",lhs="Banana"), control = list (verbose=F)) 
# those who bought 'banana' also bought..
rules_conf <- sort(rulesAfterBanana, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

#10. High level overview of rules
summary(groceryRules)

#11. View 3 rules
inspect(groceryRules[1:3])

##IMPROVING THE MODEL PERFORMANCE

#12. Best 5 rules according to lift statistic
inspect(sort(groceryRules,by="lift")[1:5])

##TAKING SUBSETS OF RULES

#NEed to market YOgurt
yogurtRules <- subset(groceryRules, items %pin% "Yogurt")
summary(yogurtRules)
inspect(sort(yogurtRules,by="lift")[1:5])

#Saving the rules
write(yogurtRules,file = "yogurtRules.csv",sep=",",quote= TRUE, row.names=FALSE)
