## SETUP OF T_UxF
get_t = function(a, U, M=2){
  t = matrix(0,length(U), length(a[1,]))
  for(u in 1:length(t[,1])){
    for(f in 1:length(t[1,])){
      t[u,f] = sum(
        b(r[u,] * a[,f], M)
        , na.rm = TRUE)
    } 
  } 
  t
}


get_q = function(t, U){
  qbar = sapply(1:length(t[1,]), function(f) sum(b0(t[,f])))
  q = log(length(U)/qbar, 10)
  q
}

get_w = function(t, q){
  w = matrix(0, length(t[,1]), length(q))
  for(u in 1:length(w[,1])){
    w[u,] = t[u,]*q
  }
  w
}

get_iu = function(omega, r, U){
  iu = sapply(1:length(U), function(u){
    which(omega[u,]==max(omega[u,][which(r[u,]>0)], na.rm=TRUE))
  })
}