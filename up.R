## SETUP OF T_UxF
t = matrix(0,length(U), length(a[1,]))
M = 3
for(u in 1:length(t[,1])){
  for(f in 1:length(t[1,])){
    t[u,f] = sum(
      b(r[u,] * a[,f], M)
    , na.rm = TRUE)
  } 
}
p = t
qbar = sapply(1:length(t[1,]), function(f) sum(b0(t[,f])))
q = log(length(U)/qbar)
w = matrix(0, length(p[,1]), length(q))
for(u in 1:length(w[,1])){
  w[u,] = p[u,]*q
}
s = matrix(0, length(U), length(U))
for(u in 1:length(U)){
  for(v in 1:length(U)){
    Fu = which(t[u,]>0)
    Fv = which(t[v,]>0)
    Fuv = intersect(Fu, Fv)
    s[u,v] = sum((w[u,]*w[v,])[Fuv]) / (sqrt(sum((w[u,]*w[u,])[Fuv])) * sqrt(sum((w[v,]*w[v,])[Fuv])))
  }
}

k = 5
for(u in 1:length(U)){
  top_K = h(sort(s[u,], decreasing=TRUE), k)
  vuk = sapply(top_K, function(x) which(s[u,]==x))
  Ivuk = 
  
}
