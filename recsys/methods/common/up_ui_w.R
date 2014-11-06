## SETUP OF T_UxF
source('recsys/setup/functions.R')

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

get_w = function(TF, IDF, debug){
  w = matrix(0, length(TF[,1]), length(IDF))
  for(u in 1:length(TF[,1])){
    w[u,] = TF[u,]*IDF
  }
  if(debug) {
    print("w")
    print(w)
  }
  w
}

get_iu = function(omega, r, rtrain.rtest, Utest, N, debug, repick){
  #omega[repeated] = NA # previne escolher itens repetidos
  iu = lapply(Utest, function(u){
    repeated = if(repick) NULL else which(rtrain.rtest[u,] > 0)
    index.top.N(omega[u,], N, repeated) ## correto??
  })
  names(iu) = Utest
  if (debug){
    print("iu")
    print(iu)
  }
  iu
}