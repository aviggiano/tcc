history1_filename <- "history1.csv"
history2_filename <- "history2.csv"
items_filename <- "items.csv"
customer_filename <- "customer.csv"
brand_filename <- "brand.csv"
color_filename <- "color.csv"

h1 <- read.csv(history1_filename, as.is=TRUE)
h2 <- read.csv(history2_filename, as.is=TRUE)
items <- read.csv(items_filename, as.is=TRUE)
customers <- read.csv(customer_filename, as.is=TRUE)
brand <- read.csv(brand_filename, as.is=TRUE)
color <- read.csv(color_filename, as.is=TRUE)

colnames(h2) <- colnames(h1)
history <- rbind(h1,h2)
rm(h1)
rm(h2)

source('functions.R')
