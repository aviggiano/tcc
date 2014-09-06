source('recsys/methods/up/up.R')

hide.data = function(R, kappa = 0.75){
  R.length = length(R)
  hide = setdiff(sample(R.length), which(is.na(R)))
  R[hide[1:round(kappa * length(hide))]] = NA
  R
}

performance.up = function(a, r, U, M=2, k=2, debug=FALSE){
  rtest = hide.data(r)
  if(debug){
    print("r")
    print(r)
    print("rtest")
    print(rtest)
  }
  iu = up(a, rtest, U, M, k, debug)
  print(iu)
  TP = sum(sapply(1:length(U), function(u) length(intersect(iu[[u]], which(r[u,]>M)))))
  P = list.length(iu)
  RELEVANT = length(which(rtest>2)) #sum(sapply(1:length(U), function(u) length(which(rtest[u,]>M))))
  precision = TP/P
  recall = TP/RELEVANT
  F1 = 2 * (precision * recall) / (precision + recall)
  if(debug){
    print("precision")
    print(precision)
    print("recall")
    print(recall)
    print("F1")
    print(F1)
  }
  list(precision, recall, F1)
}