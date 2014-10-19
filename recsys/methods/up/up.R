## get W_UF
source('recsys/methods/up/up_ui_w.R')

# s deve ser calculada tambem entre usuarios de teste??? 
get_s = function(w, TF, U, debug){
  U.length = length(U)
  s = matrix(0, U.length, U.length)
  for(u in 1:U.length){
    Fu = which(TF[u,]>0)
    for(v in 1:U.length){
      Fv = which(TF[v,]>0)
      Fuv = intersect(Fu, Fv)
      s[u,v] = if(length(Fuv) == 0) 0 else sum((w[u,]*w[v,])[Fuv], na.rm=TRUE) / (sqrt(sum((w[u,]*w[u,])[Fuv], na.rm=TRUE)) * sqrt(sum((w[v,]*w[v,])[Fuv], na.rm=TRUE)))
    }
  }
  if(debug) {
    print("s")
    print(s)
  }
  s
}

get_omega = function(a, r, s, Utrain.Utest, M, k, debug){
  Utrain = Utrain.Utest[[1]]
  Utest = Utrain.Utest[[2]]
  
  fuf = matrix(0, length(Utest)+length(Utrain), length(a[1,]))
  Ivuk_list = list()
  
  for(u in Utest){
    vuk = index.top.N.not.self(s[u,], u, k, Utest) # vizinhos devem ser selecionados dentre Utrain (?)
    Ivuk = unique(unlist(lapply(vuk, function(v) which(r[v,]>M))))
    #Iu = union(which(!is.na(r[u,])), which(r[u,] != 0))
    #Ivuk = setdiff(Ivuk,Iu)
    Ivuk_list[[u]] = Ivuk
    for(f in 1:length(a[1,])){
      fuf[u,f] = sum(
        b0(a[,f])[Ivuk], na.rm = TRUE
      )
    }
  }
  if(debug) {
    print("fuf")
    print(fuf)
  }
  omega = fuf %*% t(a)
#  for(vuk in Ivuk_list){
#    Ivuk = Ivuk_list[[vuk]]
#    if(is.null(Ivuk))
#      omega[vuk,] = 0
#    else
#      omega[vuk,][-Ivuk] = 0
#  }
  if(debug) {
    print("omega")
    print(omega)
  }
  omega
}

#up = function(a, r, U, M=2, k=2, N=1, debug=FALSE){
#  TF = get_t(a, r, U, M, debug)
#  IDF = get_q(TF, U, debug)
#  w = get_w(TF, IDF, debug)
#  s = get_s(w, TF, U, debug)
#  omega = get_omega(a, r, s, U, M, k, debug)
#  iu = get_iu(omega, r, U, N, debug)
#  iu  
#}

up = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
  # train: get IDF
  Utrain = Utrain.Utest[[1]]
  Utest = Utrain.Utest[[2]]
  
  rtrain = rtrain.rtest[Utrain,]
  rtest = rtrain.rtest[Utest,]
  
  U = get_U(rtrain, debug)
  TF = get_TF(a, rtrain, M, debug)
  IDF = get_IDF(TF, U, debug)
  
  # test
  U = get_U(rtrain.rtest, debug)
  TF = get_TF(a, rtrain.rtest, M, debug)
  w = get_w(TF, IDF, debug)
  s = get_s(w, TF, U, debug)
  omega = get_omega(a, r, s, Utrain.Utest, M, k, debug)
  iu = get_iu(omega, r, rtrain.rtest, Utest, N, debug)
  iu     
}
