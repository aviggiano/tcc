source('recsys/methods/up/up.R')
source('recsys/methods/up/ui.R')
source('recsys/methods/fw/fw.R')

hide.data = function(R, kappa = 0.75){
  R.length = length(R)
  hide = setdiff(sample(R.length), which(is.na(R)))
  R[hide[1:round(kappa * length(hide))]] = NA
  R
}

performance = function(a, r, U, M=2, k=10, N=20, debug=FALSE){
  rtest = hide.data(r)
  if(debug){
    print("h(r)")
    print(h(r))
    print("h(rtest)")
    print(h(rtest))
  }
  up = performance.up(a, r, rtest, U, M, k, N, debug)
  ui = performance.ui(a, r, rtest, U, M, N, debug)
  fw = performance.fw(a, r, rtest, U, M, N, debug)
}

get.TP = function(iu, r, rtest, U, M){
  sum(sapply(1:length(U), function(u) {
    length(intersect(
      iu[[u]], 
      which(setdiff(r[u,],rtest[u,]) > M)
    ))
  }))
}

get.P = function(iu){
  list.length(iu)
}

get.R = function(r, rtest, U, M, N){
  # RELEVANT = length(which(rtest>M)) #length(which(!is.na(r))) - length(which(r == rtest)) #
  sum(sapply(1:length(U), function(u) {
    relevant = length(which(r[u,] > M)) - length(which(r[u,] == rtest[u,]))
    if(relevant < 0) relevant = 0
    if(relevant > N) relevant = N
    relevant
  }))
}

get.precision.recall.F1 = function(iu, r, rtest, U, M, N, debug){
  TP = get.TP(iu, r, rtest, U, M)
  P = get.P(iu)
  R = get.R(r, rtest, U, M, N)
  precision = TP/P
  recall = TP/R
  F1 = 2 * (precision * recall) / (precision + recall)
  if(debug||TRUE){
    print("precision")
    print(precision)
    print("recall")
    print(recall)
    print("F1")
    print(F1)
  }
  list(precision, recall, F1)
}

performance.up = function(a, r, rtest, U, M=2, k=2, N=10, debug=FALSE){
  cat("UP\n")
  iu = up(a, rtest, U, M, k, N, debug)
  get.precision.recall.F1(iu, r, rtest, U, M, N, debug)
}

performance.ui =  function(a, r, rtest, U, M=2, N=10, debug=FALSE){
  cat("UI\n")
  iu = ui(a, rtest, U, M, N, debug)
  get.precision.recall.F1(iu, r, rtest, U, M, N, debug)
}

performance.fw =  function(a, r, rtest, U, M=2, N=10, debug=FALSE){
  cat("FW\n")
  iu = fw(rtest, a, M, N, debug)
  get.precision.recall.F1(iu, r, rtest, U, M, N, debug)
}

cross.validation = function(r, a, U, M=2, k=2, N=10, K=10, debug=FALSE){
  require(caret)
  r[which(is.na(r))]=0
  folds <- createFolds(r, K, list = TRUE, returnTrain = FALSE)
  for(i in 1:length(folds)){
    rtest = r
    rtest[folds[[i]]]=0
    up = performance.up(a, r, rtest, U, M, k, N, debug)
  }
}