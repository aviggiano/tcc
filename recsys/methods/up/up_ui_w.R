## SETUP OF T_UxF
source('recsys/setup/functions.R')

get_U = function(r, debug){
  U = 1:length(r[,1])
  if(debug){
    print("U")
    print(U)
  }
}

get_TF = function(a, r, M, debug){
  TF = matrix(0,length(r[,1]), length(a[1,]))
  for(u in 1:length(TF[,1])){
    for(f in 1:length(TF[1,])){
      TF[u,f] = sum(
        b(r[u,] * a[,f], M)
        , na.rm = TRUE)
    } 
  } 
  
  if(debug) {
    print("TF")
    print(TF)
  }
  TF
}


get_IDF = function(TF, U, debug){
  IDFbar = sapply(1:length(TF[1,]), function(f) sum(b0(TF[,f])))
  IDF = log(length(U)/IDFbar, 10)
  if(debug) {
    print("IDF")
    print(IDF)
  }
  IDF
}

get_w = function(TF, q, debug){
  w = matrix(0, length(TF[,1]), length(q))
  for(u in 1:length(TF[,1])){
    w[u,] = TF[u,]*q
  }
  if(debug) {
    print("w")
    print(w)
  }
  w
}

get_iu = function(omega, r, rtrain.rtest, Utest, N, debug){
  repeated = which(rtrain.rtest[Utest,] == r[Utest,])
  omega[repeated] = NA # previne escolher itens repetidos
  iu = lapply(Utest, function(u){
    index.top.N(omega[u,], N) ## correto??
  })
  names(iu) = Utest
  if (debug){
    print("iu")
    print(iu)
  }
  iu
}