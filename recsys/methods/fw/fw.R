source('fw_dij.R')



get_W = function(d1, d1){
  # best fit to W = [w0, w_release_date, w_gender]'
  # Y = A X
  # Y = as.vector(e)
  # A = ONES D1 D2
  
  # equation not valid for i=j
  diag(d1) = 1
  diag(d2) = 1
  diag(ones) = 0
  D1 = cbind(as.vector(1-d1))
  D2 = cbind(as.vector(1-d2))
  ONES = cbind(as.vector(ones))
  Y = cbind(as.vector(e))
  
  A = cbind(ONES, D1, D2, deparse.level = 0)
  # maneira idiota de remover da equação os elementos i=j
  IequalsJ = intersect(intersect(which(A[,1] == 0), which(A[,2] == 0)),which(A[,3] == 0))
  A=A[-IequalsJ,]
  Y=cbind(Y[-IequalsJ])
  
  # resolve o sistema de equações
  W = solve(t(A) %*% A) %*% t(A) %*% Y
  W
}

get_s = function(W, d1, d2){
  # SIMILARITY MATRIX
  s = W[1,1] + W[2,1]*(1-d1) + W[3,1]*(1-d2)
  # NORMALIZED SIMILARITY
  s = s / max(s)
  s
}

## for this dataset we have W = [8.0998016, -1.1060046, -0.2921785]
## either the implementation is wrong or this model can't be used!!!

fw = function(r, a){
  e = setup_eij(r)
  d1d2 = setup_dfij(a, r)
  W = get_W(d1d2[[1]], d1d2[[2]])
  s = get_s(W, d1d2[[1]], d1d2[[2]])
  s
}