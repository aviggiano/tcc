## get W_UF
source('up_ui_w.R')

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
fuf = matrix(0, length(U), length(a[1,]))
for(u in 1:length(U)){
  for(f in 1:length(a[1,])){
    top_K = h(sort(s[u,], decreasing=TRUE), k)
    vuk = sapply(top_K, function(x) which(s[u,]==x))
    Ivuk = sapply(vuk, function(v) which(r[v,]>M))
    Ivuk = unique(unlist(Ivuk))
    fuf[u,f] = sum(
      b0(a[,f])[Ivuk], na.rm = TRUE
    )
  }
}
omega = a %*% t(fuf)

iu = sapply(1:length(U), function(u){
  which(omega[u,]==max(omega[u,][which(r[u,]>0)], na.rm=TRUE))
})