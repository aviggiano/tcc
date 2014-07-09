size <- function(x.df) {
    length(x.df[,1])
}

count.na <- function(xs) {
    length(which(is.na(xs) == TRUE))
}

count.not.na <- function(xs) {
    length(xs) - count.na(xs)
}

count.different.not.na <- function(xs) {
    count.not.na(unique(xs))
}

delta <- function(x, y){
    as.numeric(x == y)
}

delta_vector <- function(xs, ys){
    ans <- 0
    for(i in 1:length(xs)){
        ans = ans + delta(xs[i], ys[i])
    }
    ans / length(xs)
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
