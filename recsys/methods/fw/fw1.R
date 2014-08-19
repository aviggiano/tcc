source('methods/fw/fw_dij.R')

# wi = 1
get_W = function(F){
  W = matrix(1, length(F), 1)
  W
}

get_s = function(W, d1, d2){
  # SIMILARITY MATRIX
  s = W[1,1]*(1-d1) + W[1,1]*(1-d2)
  # NORMALIZED SIMILARITY
  s = s / max(s)
  s
}

fw1 = function(r, a, F){
  e = setup_eij(r)
  d1d2 = setup_dfij(a, r)
  W = get_W(F)
  s = get_s(W, d1d2[[1]], d1d2[[2]])
  s
}