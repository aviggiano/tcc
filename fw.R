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

d = function(i, j, feature="gender"){
  if(feature == "release_date"){
    f = 1 # which(F==feature) # not necessary for now
    abs(a[i,f]-a[j,f])
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

b0 = function(x){
  b(x,0)
}

b = function(x,y){
  1*(x > y)
}

delta = function(m, n){
  1*(m == n)
}