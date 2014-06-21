t0 <- Sys.time()
print("Setup started")

history1_filename <- "history1.csv"
history2_filename <- "history2.csv"
item_filename <- "item.csv"
customer_filename <- "customer.csv"
brand_filename <- "brand.csv"
color_filename <- "color.csv"
color_map_filename <- "color_map.csv"

h1 <- read.csv(history1_filename, as.is=TRUE)
h2 <- read.csv(history2_filename, as.is=TRUE)
item <- read.csv(item_filename, as.is=TRUE)
customer <- read.csv(customer_filename, as.is=TRUE)
brand <- read.csv(brand_filename, as.is=TRUE)
color <- read.csv(color_filename, as.is=TRUE)
color_map <- read.csv(color_map_filename, as.is=TRUE)

colnames(h2) <- colnames(h1)
history <- rbind(h1,h2)
rm(h1)
rm(h2)
gc()

source('functions.R')
print(paste("Setup finished after", format(round(Sys.time()-t0, 2), nsmall = 2)))
