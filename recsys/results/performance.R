source('recsys/methods/up/up.R')
source('recsys/methods/up/ui.R')
source('recsys/methods/fw/fw.R')
SEED = 2

hide.data = function(r, Utrain.Utest, HIDDEN = 0.75, random = FALSE, has.na = TRUE){
  set.seed(SEED)
  Utest = Utrain.Utest[[2]]
  for(u in Utest){
    i.length = length(r[u,])
    already.hidden = if(has.na) which(is.na(r[u,])) else which(r[u,] == 0)
    hide = if(random) sample(i.length) else setdiff(sample(i.length), already.hidden)
    hide = hide[1:round(HIDDEN * length(hide))]
    r[u, hide] = if(has.na) NA else 0    
  }
  r
}

divide.train.test = function(r, TEST = 0.25){
  set.seed(SEED)
  U.length = length(r[,1])
  mix = sample(U.length)
  U.test = mix[1:round(TEST * length(mix))]
  U.train = mix[(round(TEST * length(mix))+1):length(mix)]
  list(U.train, U.test)
}

performance = function(a, r, M=2, k=10, N=20, debug=FALSE, normalize=TRUE){
  Utrain.Utest = divide.train.test(r)
  rtrain.rtest = hide.data(r, Utrain.Utest, has.na=FALSE)
  
  if(normalize){
    a[which(is.na(a))]=0
    a = normalize(a, columns = TRUE)
    a = a[,-c(1,21)]
  }
  #performance.up(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  #performance.ui(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  fw = performance.fw(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
}

get.TP = function(iu, r, rtrain.rtest, Utest, M){
  sum(sapply(Utest, function(u) {
    hidden = which(r[u,] != rtrain.rtest[u,])
    length(intersect(
      iu[[toString(u)]], 
      hidden[which(r[u, hidden] > M)]
    ))
  }))
}

get.P = function(iu){
  list.length(iu)
}

get.R = function(iu, r, rtrain.rtest, Utest, M, N){
  # RELEVANT = length(which(rtest>M)) #length(which(!is.na(r))) - length(which(r == rtest)) #
  iuz <<- iu
  sum(sapply(Utest, function(u) {
    length(setdiff(which(r[u,]>M),which(rtrain.rtest[u,]>M)))
    #length(intersect(which(r[u,]>M),iu[[toString(u)]]))
    #relevant = length(which(r[u,] > M)) - length(which(r[u,] == rtrain.rtest[u,]))
    #if(relevant < 0) relevant = 0
    #if(relevant > N) relevant = N
    #relevant
  }))
}

get.precision.recall.F1 = function(iu, r, rtrain.rtest, Utrain.Utest, M, N, debug){
  U = get_U(r, debug)
  Utest = Utrain.Utest[[2]]
  
  TP = get.TP(iu, r, rtrain.rtest, Utest, M)
  P = get.P(iu)
  R = get.R(iu, r, rtrain.rtest, Utest, M, N)
  
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

performance.up = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
  cat("UP\n")
  iu = up(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  get.precision.recall.F1(iu, r, rtrain.rtest, Utrain.Utest, M, N, debug)
}

performance.ui =  function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
  cat("UI\n")
  iu = ui(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  get.precision.recall.F1(iu, r, rtrain.rtest, Utrain.Utest, M, N, debug)
}

performance.fw = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
  cat("FW\n")
  iu = fw(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  get.precision.recall.F1(iu, r, rtest, U, M, N, debug)
}

plot.results = function(){
  Ns = c(1,5,10,15,20)
  
  ap = normalize(a, columns=TRUE)
  results = lapply(Ns, function(n) performance.up(ap, r, rtrain.rtest, Utrain.Utest, M=3, k=10, N=n, debug=TRUE))
  
  par(mfrow=c(3,1))
  par(mar=rep(2,4))
  
  plot(unlist(results)[c(1,4,7,10,13)])    
  plot(unlist(results)[c(1+1,4+1,7+1,10+1,13+1)])    
  plot(unlist(results)[c(1+1+1,4+1+1,7+1+1,10+1+1,13+1+1)])    
}

cross.validate = function(r, a, U, M=2, k=2, N=10, K=10, debug=FALSE){
  require(caret)
  folds = createFolds(U, K, list = TRUE, returnTrain = FALSE)
  for(i in 1:length(folds)){
    Uvalidate = folds[[i]]
    Utrain = unlist(unname(folds[-i]))
    
    rvalidate = r[Uvalidate,]
    rtrain = r[Utrain,]
    
    up(a, rtrain, Utrain, M, k, N, debug, TRAIN)
    iu = up(a, rvalidate, Uvalidate, M, k, N, debug, VALIDATE)
    get.precision.recall.F1(iu, , rtest, U, M, N, debug)
  }
}