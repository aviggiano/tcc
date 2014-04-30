history_filename <- "history.csv"
items_filename <- "items.csv"
customer_filename <- "customer.csv"
brand_filename <- "brand.csv"
color_filename <- "color.csv"

history <- read.csv(history_filename, as.is=TRUE)
items <- read.csv(items_filename, as.is=TRUE)
customers <- read.csv(customer_filename, as.is=TRUE)
brand <- read.csv(brand_filename, as.is=TRUE)
color <- read.csv(color_filename, as.is=TRUE)

size <- function(x.df) {
    length(x.df[,1])
}

where <- function(attr, val) {
    items[which(items[attr] == val),]
}

count.na <- function(xs) {
    length(which(is.na(xs) == TRUE))
}

count.not.na <- function(xs) {
    length(xs) - count.na(xs)
}

cdnna <- function(xs) {
    count.not.na(unique(xs))
}

color_id <- function(c) {
    which(color$color == c)
}

change_sth <- function(){
    i <- 1
    n = length(items$color)
    t0 <- Sys.time()
    while(i <= n) {
        items$color[i] <- color_id(items$color[i])
        if (i %% 1000 == 0) {
            print(i)
            print(Sys.time()-t0)
        }
        i <- i+1
    }
}
