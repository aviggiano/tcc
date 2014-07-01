F = colnames(item)
U = customer$customer_id

sim = function(i,j){
  sum(sapply(F), function(f) {
    w(f) * (1 - d(f, i, j))
  })
}

d = function(f, i, j){
  delta(i,j)
}

b0 = function(x){
  b(x,0)
}

b = function(x,y){
  as.numeric(x > y)
}

delta = function(m, n){
  as.numeric(m == n)
}

r = function(u,i){
  has_bought_i = sapply(which(history$customer_id == u), function(hi){ history$item_id[hi] == i})
  any(has_bought_i, function(has) has == TRUE)
}

e = function(i,j){
  sum(sapply(U, b0(r(u,i) * r(u,j)))) 
}