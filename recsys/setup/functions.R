b0 = function(x){
  b(x,0)
}

b = function(x,y){
  1*(x > y)
}

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

delta = function(m, n){
  1*(m == n)
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

h = function(matrix, N=6){
  if(is.null(dim(matrix))){
    if(length(matrix) < N)
      N1 = length(matrix)
    else
      N1 = N
    matrix[1:N1]
  }
  else {
    if(dim(matrix)[1] < N) 
      N1 = dim(matrix)[1]
    else
      N1 = N
    if(dim(matrix)[2] < N) 
      N2 = dim(matrix)[2]
    else
      N2 = N  
    matrix[1:N1, 1:N2]    
  }
}

top.N = function(xs, N=10){
  xs.length = length(xs)
  x0 = if(N > xs.length) 1 else (xs.length-N+1)
  sort(xs)[x0:xs.length]
}

index.top.N = function(xs, N=10){
  nx <- length(xs)
  p <- nx-N
  if(p <= 0) p = 1
  xp <- sort(xs, partial=p)[p]
  which(xs > xp)
}

index.top.N.not.self = function(xs, self, N=10) {
  indexes = index.top.N(xs[-self], N)
  w = which(indexes > self)
  indexes[w] = indexes[w] + 1
  indexes
}

top.N.not.self = function(xs, self, N=10) {
  xs[index.top.N.not.self(xs, self, N)]
}