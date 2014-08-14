## SETUP OF T_UxF
t = matrix(0,length(U), length(a[1,]))
M = 2
for(u in 1:length(t[,1])){
  for(f in 1:length(t[1,])){
    t[u,f] = sum(
      b(r[u,] * a[,f], M)
      , na.rm = TRUE)
  } 
}
p = t
qbar = sapply(1:length(t[1,]), function(f) sum(b0(t[,f])))
q = log(length(U)/qbar, 10)
w = matrix(0, length(p[,1]), length(q))
for(u in 1:length(w[,1])){
  w[u,] = p[u,]*q
}