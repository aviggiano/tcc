source('recsys/methods/fw/fw_dij.R')

# wi = 1
get_W = function(F, debug){
  W = matrix(1, length(F), 1)
  if(debug){
    print("W")
    print(W)
  }
  W
}

get_s = function(W, d1, d2, debug){
  # SIMILARITY MATRIX
  s = W[1,1]*(1-d1) + W[1,1]*(1-d2)
  # NORMALIZED SIMILARITY
  s = s / max(s)
  if(debug){
    print(s)
  }
  s
}

fw1 = function(r, a, F, debug=FALSE){
  e = setup_eij(r, debug)
  d1d2 = setup_dfij(a, r, debug)
  W = get_W(F, debug)
  s = get_s(W, d1d2[[1]], d1d2[[2]], debug)
  s
}