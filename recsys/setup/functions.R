elapsed = function(str){
  print(paste(str, format(round(Sys.time()-t0, 2), nsmall = 2)))
}

b0 = function(x){
  b(x,0)
}

# no artigo UP, Pt e' tal que x >= y. Aqui usamos x > y, entao t = t+1
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
  sort(xs, na.last=FALSE)[x0:xs.length]
}

index.top.N = function(xs, N=10, ys.remove=NULL){
  if(length(xs) > 0){
    o = order(xs, na.last=FALSE)
    if(!is.null(ys.remove)) o = setdiff(o, ys.remove)
    o.length = length(o)
    if (N > o.length) N = o.length
    if (length(o) > 0) 
      o[((o.length-N+1):o.length)]
    else 
      c()
  }
  else {
    0
  }
}

index.top.N.not.self = function(xs, self, N=10, ys.remove) {
  ys.remove = union(self, ys.remove)
  indexes = index.top.N(xs, N, ys.remove)
  #w = which(indexes > self)
  #indexes[w] = indexes[w] + 1
  indexes
}

top.N.not.self = function(xs, self, N=10) {
  xs[index.top.N.not.self(xs, self, N)]
}

list.length = function(l){
  sum(sapply(1:length(l), function(x) length(l[[x]])))
}

normalize = function(matrix, columns=FALSE){
  matrix[which(is.na(matrix))]=0
  if(columns)
    apply(a,2,function(matrix) matrix/max(abs(matrix), na.rm=TRUE))
  else
    matrix/max(abs(matrix), na.rm=TRUE)
}

get_U = function(r, debug){
  U = 1:length(r[,1])
  if(debug){
    print("U")
    print(U)
  }
  U
}