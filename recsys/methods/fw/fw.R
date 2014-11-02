## SETUP OF E_IxJ
source('recsys/setup/functions.R')
library('MatrixModels')

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
    #for(f in 1:length(d[1,1,])) {
    for(i in 1:length(d[1,,1])){
      for(j in 1:length(d[,1,1])){
        d[i,j,] = abs(a[i,]-a[j,])#1-delta(a[i,],a[j,])
      } 
    }
    #}
    d
  }
  else {
    max_distance_release_date = max(a[,1], na.rm=T) - min(a[,1], na.rm=T)
    d = function(i, j, feature="gender"){
      if(feature == "release_date"){
        f = 1 # which(F==feature) # not necessary for now
        if(is.na(a[i,f]) || is.na(a[j,f]))
          1
        else
          abs(a[i,f]-a[j,f])/max_distance_release_date
      }
      else{
        aif = a[i,2:20]
        ajf = a[j,2:20]
        inters = sum(aif * ajf)
        union = sum(b0(aif+ajf))
        jaccard = inters/union
        if(union == 0) jaccard = 0
        1-jaccard
      }
    }
    
    d1 = matrix(0,length(r[1,]), length(r[1,]))
    d2 = matrix(0,length(r[1,]), length(r[1,]))
    for(i in 1:length(d1[,1])){
      for(j in 1:length(d1[1,])){
        d1[i,j] = d(i,j,"release_date")
        d2[i,j] = d(i,j,"gender")
      } 
    }
    if(debug){
      print("d")
      print(h(d1))
      print(h(d2))
    }
    list(d1, d2)    
  }
}

get_iu_fw = function(r, s, Utest, M, N, debug){
  diag(s) = NA
  iu = lapply(Utest, function(u){
    positive = which(r[u,] > M)
    ans = c() # ans funciona como um conjunto de itens
    for(i in positive){
      # which(is.na(r[u,])) sao os itens ainda nao avaliados por u
      # index.top.N(s[i,], N) sao os melhores itens para u
      # a interseccao da os itens recomendados para u
      not.yet.chosen = union(which(is.na(r[u,])), which(r[u,]==0))
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

get_W = function(d, e, r, debug, generic=TRUE){
  if(!generic){
    ones = matrix(1,length(r[1,]), length(r[1,]))
    # best fit to W = [w0, w_release_date, w_gender]'
    # Y = A X
    # Y = as.vector(e)
    # A = ONES D1 D2
    
    # equation not valid for i=j
    diag(d1) = NA
    diag(d2) = NA
    diag(ones) = NA
    D1 = cbind(as.vector(1-d1))
    D2 = cbind(as.vector(1-d2))
    ONES = cbind(as.vector(ones))
    Y = cbind(as.vector(e))
    
    A = cbind(ONES, D1, D2, deparse.level = 0)
    # removing elements i=j
    IequalsJ = which(is.na(A))
    A=A[-IequalsJ,]
    Y=cbind(Y[-IequalsJ])
    
    # resolve o sistema de equações
    W = solve(t(A) %*% A) %*% t(A) %*% Y
    if(debug){
      print("W")
      print(W)
    }
    W
  }
  else {
    #D = matrix(0, length(d[,,1]), length(d[1,1,]))
    #for(f in 1:length(d[1,1,])){
    #  diag(d[,,f]) = NA
    #  D[,f] = as.vector(d[,,f])
    #}
    dim(d) = c(length(d[,,1]), length(d[1,1,]))
    
    # removing elements i=j
    e.nrow = dim(e)[1]
    IequalsJ = sapply(1:e.nrow, function(x) (x-1)*e.nrow+x)
      
    #  which(is.na(d))
    #D=D[-IequalsJ,]
    #E = cbind(as.vector(e))
    #E=cbind(E[-IequalsJ])
    
    e = as.vector(e)
    e = e[-IequalsJ]
    d = d[-IequalsJ,]
    
    # cleaning memory
    rm(IequalsJ)
    rm(e.nrow)
    gc()
    
    # linear fit E ~ w0 + w D
    #lm.W = lm(e ~ d, x=FALSE, y=FALSE, model=FALSE, qr=FALSE) # free some space
    #W = as.vector(lm.W$coefficients)
    #rm(lm.W)
    #gc()
    
    # using sparse matrix
    d=as(d,"sparseMatrix")
    W = MatrixModels:::lm.fit.sparse(t(e),D)

    W
  }
  
}

get_sij = function(W, d, debug, generic){
  # SIMILARITY MATRIX
  W[which(is.na(W))] = 0
  s = matrix(0, length(d[,1,1]), length(d[,1,1]))
  for(f in 2:length(W)){
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

fw = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
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
  s = get_sij(W, d, debug)
  print("get_iu_fw")
  iu = get_iu_fw(rtrain.rtest, s, Utest, M, N, debug)
  print("iu")
  iu
}  