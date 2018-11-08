# importing data

data <- read.csv("BreadBasket.csv", T, ",")

# installing "plyr" package for manipulating data

library(plyr)

# preparing appropriate structure for apriori association
# merging item based on date, time and transaction

item_list <- ddply(data, c("Date", "Time", "Transaction"),
                   function(change)paste(change$Item,
                   collapse= ","))

item_list$Date <- NULL
item_list$Time <- NULL
item_list$Transaction <- NULL

names(item_list) <- c("Item")

# creating csv file for item_list

write.csv(item_list, "item_list.csv", row.names = T, quote = F)

# coverting csv file to transaction format

library(arules)

tranx <- read.transactions(file = "item_list.csv", rm.duplicates = T,
                           format = "basket", sep = ",", cols = 1)

# running the model

model <- apriori(tranx, parameter = list(sup= .001, conf= .7, target="rules",
                                         minlen= 2))

inspect(model)

# manually choice the objective

model1 <- apriori(tranx, parameter = list(sup=.0001, conf= 1, target="rules",
                                          minlen= 2), appearance = list(rhs= "Cake"))

inspect(model1)
inspect(model1[1:10])

# creating some useful visulization

library(arulesViz)
plot(model)
plot(model, method = "grouped", control = list(k = 5))
plot(model, method="graph", control=list(type="items"))
plot(model, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(model,measure=c("support","lift"),shading="confidence",interactive=T)

# top 10 products

itemFrequencyPlot(tranx, topN =10)
