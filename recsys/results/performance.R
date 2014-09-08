source('recsys/methods/up/up.R')
source('recsys/methods/up/ui.R')

hide.data = function(R, kappa = 0.75){
  R.length = length(R)
  hide = setdiff(sample(R.length), which(is.na(R)))
  R[hide[1:round(kappa * length(hide))]] = NA
  R
}

performance = function(a, r, U, M=2, k=2, N=10, debug=FALSE){
  rtest = hide.data(r)
  if(debug){
    print("h(r)")
    print(h(r))
    print("h(rtest)")
    print(h(rtest))
  }
  up = performance.up(a, r, rtest, U, M, k, N, debug)
  ui = performance.ui(a, r, rtest, U, M, k, N, debug)
  print(up)
  print(ui)
}

performance.up = function(a, r, rtest, U, M=2, k=2, N=10, debug=FALSE){
  iu = up(a, rtest, U, M, k, N, debug=FALSE)
  TP = sum(sapply(1:length(U), function(u) length(intersect(iu[[u]], which(r[u,]>M)))))
  P = list.length(iu)
  RELEVANT = length(which(rtest>2)) #sum(sapply(1:length(U), function(u) length(which(rtest[u,]>M))))
  precision = TP/P
  recall = TP/RELEVANT
  F1 = 2 * (precision * recall) / (precision + recall)
  if(debug){
    print("UP")
    print("precision")
    print(precision)
    print("recall")
    print(recall)
    print("F1")
    print(F1)
  }
  list(precision, recall, F1)
}

performance.ui =  function(a, r, rtest, U, M=2, k=2, N=10, debug=FALSE){
  iu = ui(a, rtest, U, M, k, N, debug=FALSE)
  TP = sum(sapply(1:length(U), function(u) length(intersect(iu[[u]], which(r[u,]>M)))))
  P = list.length(iu)
  RELEVANT = length(which(rtest>2)) #sum(sapply(1:length(U), function(u) length(which(rtest[u,]>M))))
  precision = TP/P
  recall = TP/RELEVANT
  F1 = 2 * (precision * recall) / (precision + recall)
  if(debug){
    print("UI")
    print("precision")
    print(precision)
    print("recall")
    print(recall)
    print("F1")
    print(F1)
  }
  list(precision, recall, F1)  
}