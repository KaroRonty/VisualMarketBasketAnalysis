library(data.table)
library(dplyr)
library(arules)
library(arulesViz)
library(RColorBrewer)
options(scipen = 1e+06)

# Import and unzip sales and products data
url <- "https://s3.amazonaws.com/instacart-datasets/instacart_online_grocery_shopping_2017_05_01.tar.gz"
download.file(url, destfile = "temp.tar.gz")
untar("temp.tar.gz")
sales <- fread("instacart_2017_05_01\\order_products__train.csv")
products <- fread("instacart_2017_05_01\\products.csv")

# Remove sales that include only one product
sales_apriori <- sales[!unique(sales$order_id), ]

# Combine product names with sales data
sales_apriori <- sales_apriori[, 1:2]
products <- products[, 1:2]
sales_apriori <- left_join(sales_apriori, products, by = "product_id")

# For baskets from items bought together and format them into transactions and rules
baskets <- sales_apriori %>% group_by(order_id) %>% summarise(items = list(product_name))
transactions <- as(baskets$items, "transactions")
# Confidence: odds of A being bought after B was bought Adjust higher if too many rules were found
rules <- apriori(transactions, parameter = list(sup = 0.001, conf = 0.05, target = "rules"))

# Make a data frame containing rules selected by criteria
rules_df <- data.frame(A = labels(lhs(rules)), B = labels(rhs(rules)), rules@quality)
rules_df$A <- gsub("[{}]", "", rules_df$A)
rules_df$B <- gsub("[{}]", "", rules_df$B)

# Filter weak rules
rules_df <- filter(rules_df, count > 100)

# Plot histograms of quality criteria
par(mfrow = c(2, 2))
hist(rules_df$support, breaks = 100)
hist(rules_df$confidence, breaks = 100)
hist(rules_df$lift, breaks = 100)
hist(rules_df$count, breaks = 100)
par(mfrow = c(1,1))

# Plot the quality criteria as an interactive plot
plot(rules, measure = c("confidence", "lift"), engine = "interactive", shading = "support",
     control = list(col = brewer.pal(11, "Spectral")))

# Plot some rules
rules_top <- head(rules, 25)
plot(rules_top, method = "graph", control = list(type = "items"))
