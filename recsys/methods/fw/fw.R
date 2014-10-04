source('recsys/methods/fw/fw_dij.R')

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
    D = matrix(0, length(d[,,1]), length(d[1,1,]))
    for(f in 1:length(d[1,1,])){
      diag(d[,,f]) = NA
      D[,f] = as.vector(d[,,f])
    }
    
    # removing elements i=j
    IequalsJ = which(is.na(D))
    D=D[-IequalsJ,]
    E = cbind(as.vector(e))
    E=cbind(E[-IequalsJ])
    
    # linear fit E ~ w0 + w D
    lm.W = lm(E ~ D)
    W = as.vector(lm.W$coefficients)
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

fw = function(r, a, M=2, N=1, debug=FALSE){
  e = setup_eij(r, M, debug)
  d = setup_dfij(a, r, debug)
  W = get_W(d, e, r, debug)
  s = get_sij(W, d, debug)
  iu = get_iu_fw(r, s, M, N, debug)
  iu
}  