## get W_UF
source('up_ui_w.R')

omega = w %*% t(a)

iu = sapply(1:length(U), function(u){
  which(omega[u,]==max(omega[u,][which(r[u,]>0)], na.rm=TRUE))
})