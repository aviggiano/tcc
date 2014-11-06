## SETUP OF E_IxJ
source('recsys/setup/functions.R')
#library('MatrixModels')

setup_eij = function(r, M, debug) {
  e = b(t(r),M) %*% b(r,M)
  if(debug){
    print("e")
    print(e)
  }
  e
}


## d_fij
setup_dfij = function(a, r, debug, generic=TRUE){
  if(generic) {
    d = array(0,c(length(r[1,]), length(r[1,]), length(a[1,])))
    for(i in 1:length(d[1,,1])){
      for(j in 1:length(d[,1,1])){
        d[i,j,] = abs(a[i,]-a[j,])#1-delta(a[i,],a[j,])
      } 
    }
    d
  }
  else {
    #max_distance_release_date = max(a[,1], na.rm=T) - min(a[,1], na.rm=T)
    d = array(0,c(length(r[1,]), length(r[1,]), 6))

    for(i in 1:length(d[1,,1])){
      for(j in 1:length(d[,1,1])){
        d[i,j,1] = abs(a[i,21]-a[j,21])  
        d[i,j,2] = abs(a[i,22]-a[j,22])  
        d[i,j,3] = abs(a[i,23]-a[j,23])  
        d[i,j,4] = abs(a[i,24]-a[j,24])  
        d[i,j,5] = abs(a[i,25]-a[j,25])  
        d[i,j,6] = abs(a[i,1]-a[j,1])    
        d[i,j,7] = 1-jaccard(a[i,2:20],a[j,2:20])
      } 
    }
    d
  }
}

get_iu_fw = function(r, s, Utest, M, N, debug, repick){
  diag(s) = NA
  I.length = 1:length(r[1,])
  iu = lapply(Utest, function(u){
    positive = which(r[u,] > M)
    ans = c() # ans funciona como um conjunto de itens
    for(i in positive){
      # which(is.na(r[u,])) sao os itens ainda nao avaliados por u
      # index.top.N(s[i,], N) sao os melhores itens para u
      # a interseccao da os itens recomendados para u
      not.yet.chosen = if(repick) I.length else union(which(is.na(r[u,])), which(r[u,]==0))
      ans = union(ans, intersect(not.yet.chosen, index.top.N(s[i,], N)))
    }
    
    if(length(ans) < N) {
      ## se N e' uma lista de 6 elementos, devemos chutar outros 6 mesmo que nao haja similaridade: SIM
      ans = union(ans, setdiff(1:length(r[1,]), ans))
    }
    
    if(length(ans) < N) N = length(ans)
    ans[1:N] # escolhemos no maximo N elementos
  })
  names(iu) = Utest
  if (debug){
    print("iu")
    print(iu)
  }
  
  iu
}

get_W = function(d, e, r, debug){
  dim(d) = c(length(d[,,1]), length(d[1,1,]))
  
  # removing elements i=j
  e.nrow = dim(e)[1]
  IequalsJ = sapply(1:e.nrow, function(x) (x-1)*e.nrow+x)
    
  e = as.vector(e)
  e = e[-IequalsJ]
  d = d[-IequalsJ,]
  
  # cleaning memory
  rm(IequalsJ)
  rm(e.nrow)
  gc()
  
  # linear fit e ~ w0 + w (1-d)
  D = 1-d
  lm.W = lm(e ~ D, x=FALSE, y=FALSE, model=FALSE, qr=FALSE) # free some space
  W = as.vector(lm.W$coefficients)
  #rm(lm.W)
  #gc()
  
  # using sparse matrix
  #d=as(d,"sparseMatrix")
  #W = MatrixModels:::lm.fit.sparse(d,e)

  W
  
}

get_sij = function(W, d, debug, generic, only.top=FALSE){
  # SIMILARITY MATRIX
  W[which(is.na(W))] = 0
  s = matrix(0, length(d[,1,1]), length(d[,1,1]))
  
  set.F = 2:length(W)
  if(only.top) set.F = index.top.N(W, N=only.top, ys.remove=1)
  
  for(f in set.F){
    if(W[f] > 0) s = s + W[f] * (1-d[,,f-1])
  }  
  
  # NORMALIZED SIMILARITY
  s = s / max(abs(s), na.rm=TRUE)
  if(debug){
    print("s")
    print(s)
  }
  s 
}

fw = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug, WW, repick){
  print("Utrain.Utest")
  Utrain = Utrain.Utest[[1]]
  Utest = Utrain.Utest[[2]]
  
  print("rtrain.rtest")
  rtrain = rtrain.rtest[Utrain,]
  rtest = rtrain.rtest[Utest,]
  
  print("get_U")
  U = get_U(rtrain, debug)
  print("setup_eij")
  e = setup_eij(rtrain.rtest, M, debug)
  print("setup_dfij")
  d = setup_dfij(a, rtrain.rtest, debug)
  print("get_W")
  W = get_W(d, e, rtrain.rtest, debug)
  print("get_sij")
  s = get_sij(W, d, debug, only.top=WW)
  print("get_iu_fw")
  iu = get_iu_fw(rtrain.rtest, s, Utest, M, N, debug, repick)
  print("iu")
  iu
}  