order_products1 <- split(order_products$product_id,order_products$order_id)
View(order_products1)
order_products1 <- as(order_products[,1:2],"transactions")
#Most Frequent Item
itemFrequencyPlot(order_products1,topN=5)

# What are customers likely to buy before they purchase "Banana"

rules<-apriori(data=order_products)
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)
rules