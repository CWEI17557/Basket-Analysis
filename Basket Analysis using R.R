# import csv file 
setwd("C:\\Users\\cwei1\\OneDrive\\Documents\\Data Analysis\\Job\\Project")
groceries<- read.csv("trans_basket.csv",strip.white =TRUE, na.strings ="" )


# Expolre data
head(groceries,15)
sum(is.na(groceries))
summary(groceries)
distinct_count <- length(unique(groceries$COMMODITY_DESC))
# 307 COMMODITY_DESC


library(plyr)
library(dplyr)

# Transform data into a transactional dataset
itemList <- ddply(groceries, c("BASKET_ID"), function(df1)paste(df1$COMMODITY_DESC,collapse = ","))
head(itemList,6)

summary(itemList)
# 275542 combinations 

itemList %>% select(V1) %>% setNames(c("itemList")) %>% head
itemList<-itemList %>% select(V1) %>% setNames(c("itemList")) 
write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
# Convert CSV file to Basket Format
library(arules)
txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
inspect(txn[1:20])


library(arulesViz)
library(datasets)
# Create an item frequency plot for the top 10 items
itemFrequencyPlot(txn,topN=10,type="absolute")



# Extract the set of most frequent item sets
itemsets = apriori(txn,parameter = list(support = 0.03, target = 'frequent'))
length(itemsets)

# Inspect the five most popular items
inspect(sort(itemsets, by='support', decreasing = T))


# rules   
rules = apriori(txn,
                parameter = list(supp=0.03, conf=0.4, minlen=2),
                control = list(verbose=F))   
length(rules)
rules<- sort(rules,by="confidence",descreasing = FALSE)

inspect(rules)

plot(rules[1:10], method="paracoord", main="Parallel Coordinated plot Top 10 Confidence")
ruleExplorer(rules)

library(DT)
saveWidget(  inspectDT(rules) , "Basket Detail Report.html")


# rules 1  What product will be purchased with lunchmeat
rules1<-apriori(data=txn, parameter=list(supp=0.001,conf = 0.05,minlen=2), 
                appearance = list(default="rhs",lhs="LUNCHMEAT"),
                control = list(verbose=F))
length(rules1)
rules1<-sort(rules1, decreasing=TRUE,by="confidence")
inspect(rules1)


ruleExplorer(rules1)
plot(rules1[1:5], method="paracoord", main="What product will be purchased with lunchmeat")





# rules 2 Who is most likely to purchase cheese
rules2<-apriori(data=txn, 
                parameter=list(supp=0.01,conf = 0.3, minlen=2), 
                appearance = list(default="lhs",rhs="CHEESE"),
                control = list(verbose=F))
length(rules2)
rules2<-sort(rules2, decreasing=TRUE,by="confidence")
inspect(rules2[1:10])
ruleExplorer(rules2)
plot(rules2[1:5], method="paracoord", main="Who is most likely to purchase cheese")






