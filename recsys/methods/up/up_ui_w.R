## SETUP OF T_UxF
source('recsys/setup/functions.R')

get_t = function(a, r, U, M, debug){
  t = matrix(0,length(U), length(a[1,]))
  for(u in 1:length(t[,1])){
    for(f in 1:length(t[1,])){
      t[u,f] = sum(
        b(r[u,] * a[,f], M)
        , na.rm = TRUE)
    } 
  } 
  
  if(debug) {
    print("t")
    print(t)
  }
  t
}


get_q = function(t, U, debug){
  qbar = sapply(1:length(t[1,]), function(f) sum(b0(t[,f])))
  q = log(length(U)/qbar, 10)
  if(debug) {
    print("q")
    print(q)
  }
  q
}

get_w = function(t, q, debug){
  w = matrix(0, length(t[,1]), length(q))
  for(u in 1:length(w[,1])){
    w[u,] = t[u,]*q
  }
  if(debug) {
    print("w")
    print(w)
  }
  w
}

get_iu = function(omega, r, U, N, debug){
  omega[union(which(!is.na(r)),  which(r != 0))] = NA # previne escolher itens repetidos
  iu = lapply(1:length(U), function(u){
    index.top.N(omega[u,], N)
  })
  if (debug){
    print("iu")
    print(iu)
  }
  iu
}