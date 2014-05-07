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

delta <- function(x, y){
    as.numeric(x == y)
}

sim_delta <- function(xs, ys){
    ans <- 0
    for(i in 1:length(xs)){
        ans = ans + delta(xs[i], ys[i])
    }
    ans / length(xs)
}

sim_delta_bag_of_words <- function(xs, ys){
    if (length(xs) == 0 || length(ys) == 0)
        0
    else {
        m <- sapply(x, function(i){
            sapply(y, function(j){
                delta(i,j)
            })
        })
        # do not count duplicates
        sum(m[row(m)!=col(m)]) / max(length(xs), length(ys))
    }
}

sq_dist_L2 <- function(X,Y){
    ans <- 0
    for(i in 1:length(X)){
        ans <- ans + (X[i]-Y[i])*(X[i]-Y[i])
    }
    ans
}

dist_L2 <- function(X,Y){
    sqrt(sq_dist_L2(X,Y))
}

dist <- function(X,Y){
    dist_L2(X,Y)
}

red <- c(255,0,0)
green <- c(0,255,0)
blue <- c(0,0,255)
black <- c(0,0,0)
white <- c(255,255,255)

max_dist_color <- dist(black,white)

dc <- function(color1, color2){
    dist(color1, color2) / max_dist_color
}
