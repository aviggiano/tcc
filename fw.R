## SETUP OF E_IxJ
e=matrix(0,length(r[,1]), length(r[1,]))
for(i in 1:length(e[,1])){
  for(j in 1:length(e[1,])){
    e[i,j] = sum(
      b0(r[,i] * r[,j])
    )
  } 
}

sim = function(i,j){
  sum(sapply(F), function(f) {
    w(f) * (1 - d(f, i, j))
  })
}

d = function(i, j, feature="NA"){
  if(feature == "release_date"){
    f = 1 # which(F==feature) # not necessary for now
    abs(a[i,f]-a[j,f])
  }
  else{
    inters = sum(
      a[i,2:length(a[1,])] * a[j,2:length(a[1,])]
    )
    union = sum(a[i,2:length(a[1,])]) + sum(a[j,2:length(a[1,])])
    jaccard = inters/union
    jaccard
  }
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