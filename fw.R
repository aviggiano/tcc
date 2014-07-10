sim = function(i,j){
  sum(sapply(F), function(f) {
    w(f) * (1 - d(f, i, j))
  })
}

b0 = function(x){
  b(x,0)
}

b = function(x,y){
  1*(x > y)
}

delta = function(m, n){
  1*(m == n)
}

## SETUP OF E_IxJ
e = matrix(0,length(r[,1]), length(r[1,]))
for(i in 1:length(e[,1])){
  for(j in 1:length(e[1,])){
    e[i,j] = sum(
      b0(r[,i] * r[,j])
    )
  } 
}

## d_fij
max_distance_release_date = max(as.numeric(a[,1]), na.rm=T) - min(as.numeric(a[,1]), na.rm=T)
d = function(i, j, feature="gender"){
  if(feature == "release_date"){
    f = 1 # which(F==feature) # not necessary for now
    abs(as.numeric(a[i,f])-as.numeric(a[j,f]))/max_distance_release_date
  }
  else{
    aif = sapply(1:19, function(x) as.numeric(substr(a[i,2],x,x)))
    ajf = sapply(1:19, function(x) as.numeric(substr(a[j,2],x,x)))
    inters = sum(aif * ajf)
    union = sum(aif) + sum(ajf)
    jaccard = inters/union
    jaccard
  }
}

d1 = matrix(0,length(r[,1]), length(r[1,]))
d2 = matrix(0,length(r[,1]), length(r[1,]))
for(i in 1:length(d1[,1])){
  for(j in 1:length(d1[1,])){
    d1[i,j] = d(i,j,"release_date")
    d2[i,j] = d(i,j,"gender")
  } 
}

# best fit to W = [w0, w_release_date, w_gender]'
# Y = A X
# Y = as.vector(e)
# A = ONES D1 D2
D1 = as.vector(d1)
D2 = as.vector(d2)
ONES = rep(1, length(D1))
A = cbind(ONES, D1, D2, deparse.level = 0)
W = solve(t(A) %*% A) * t(A) * as.vector(e)