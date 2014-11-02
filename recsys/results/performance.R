source('recsys/methods/up/up.R')
source('recsys/methods/up/ui.R')
source('recsys/methods/fw/fw.R')
library(ggplot2) 
library(reshape2)
options(digits=3)
SEED = 2

hide.data = function(r, Utrain.Utest, HIDDEN, random = FALSE, has.na = TRUE){
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

divide.train.test = function(r, TRAIN){
  TEST = 1 - TRAIN
  set.seed(SEED)
  U.length = length(r[,1])
  mix = sample(U.length)
  U.test = mix[1:round(TEST * length(mix))]
  U.train = mix[(round(TEST * length(mix))+1):length(mix)]
  list(U.train, U.test)
}

performance = function(a, r, M=2, k=10, N=20, debug=FALSE, 
                       normalize=TRUE, remove=c(1,21), method="up", 
                       TRAIN=0.75, HIDDEN=0.75){
  timer <<- Sys.time()
  Utrain.Utest = divide.train.test(r, TRAIN)
  rtrain.rtest = hide.data(r, Utrain.Utest, HIDDEN, has.na=FALSE)
  
  if(normalize){
    a[which(is.na(a))]=0
    a = normalize(a, columns = TRUE)
    if(remove) a = a[,-remove]
  }
  if("up" == method) performance.up(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  else if("ui" == method) performance.ui(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
  else if("fw" == method) performance.fw(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug)
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
  get.precision.recall.F1(iu, r, rtrain.rtest, Utrain.Utest, M, N, debug)
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

plot.T = function(){
  Ts = c(0,1,10,20,30,40,50,60,70,75,80,90,100)/100
  Ts = c(1,75)/100
  plot.results(Ts, "T")
}

plot.N = function(){
  Ns = c(1,10)#,20,30,40,50,60,70,80,90,100)
  plot.results(Ns, "N")
}

get.results = function(Xs, xl, a, r, method){
  
  sapply(Xs, 
         function(y){
           if(xl == "N") performance(a,r,N=y,remove=FALSE,method=tolower(method))                 
           else if(xl == "T") performance(a,r,TRAIN=y,remove=FALSE,method=tolower(method))                 
           else if(xl == "H") performance(a,r,HIDDEN=y,remove=FALSE,method=tolower(method))
           else if(xl == "M") performance(a,r,M=y,remove=FALSE,method=tolower(method))
           else if(xl == "k") performance(a,r,k=y,remove=FALSE,method=tolower(method))
           else -1
         })
  
}

plot.results = function(Xs, xl){
  methods = c("UP","UI")
  
  methods.length = length(methods)
  Xs.length = length(Xs)
  df = data.frame(precision = numeric(Xs.length*methods.length), 
                  recall = numeric(Xs.length*methods.length), 
                  F1 = numeric(Xs.length*methods.length), 
                  Xs = integer(Xs.length*methods.length), 
                  method = character(Xs.length*methods.length), 
                  stringsAsFactors = FALSE)
  i = 0
  for(method in methods){
    results = get.results(Xs, xl, a, r, method)
    precision = 100*unlist(results[1,])
    recall = 100*unlist(results[2,])
    F1 = 100*unlist(results[3,])
    
    df$precision[(1+ i*Xs.length):((i+1)*Xs.length)] = precision
    df$recall[(1+ i*Xs.length):((i+1)*Xs.length)] = recall
    df$F1[(1+ i*Xs.length):((i+1)*Xs.length)] = F1
    df$Xs[(1+ i*Xs.length):((i+1)*Xs.length)] = Xs
    df$method[(1+ i*Xs.length):((i+1)*Xs.length)] = method
    i = i+1
    
    ggplot.results(Xs, precision, aes(x=Xs, y=precision, colour=method), df, xl, method,
                   "_precision_", "Precisao (%)")
    ggplot.results(Xs, recall, aes(x=Xs, y=recall, colour=method), df, xl, method,
                   "_recall_", "Abrangencia (%)")
    ggplot.results(Xs, F1, aes(x=Xs, y=F1, colour=method), df, xl, method,
                   "_F1_", "F1 (%)")
  }
}

ggplot.results = function(Xs, Y, aes.f, df, xl, method, fl, yl){
  filename = paste("tese/img_temp/",method,fl,xl,".png",sep="")
  
  p = ggplot(df, aes.f) + 
    geom_line() + 
    geom_point(size=4, shape=21, fill="white") +
    scale_x_continuous(breaks=Xs) +
    #scale_y_continuous(breaks=Y) +
    labs(colour="Metodo") +
    xlab(xl) +
    ylab(yl)
  width = 10
  height = (9/16) * width
  ggsave(p, file=filename, width = width, height = height)  
}

plot.N.old = function(){
  methods = c("UP")#,"UI")
  Ns = c(1,10)#,20,30,40,50,60,70,80,90,100)0
  
  methods.length = length(methods)
  Ns.length = length(Ns)
  df = data.frame(precision = numeric(Ns.length*methods.length), 
                  recall = numeric(Ns.length*methods.length), 
                  F1 = numeric(Ns.length*methods.length), 
                  Ns = integer(Ns.length*methods.length), 
                  method = character(Ns.length*methods.length), 
                  stringsAsFactors = FALSE)

  i = 0
  for(m in methods){
    results = sapply(Ns, 
                     function(n){
                       performance(a,r,N=n,remove=FALSE,method=tolower(m))                 
                     })
    precision = 100*unlist(results[1,])
    recall = 100*unlist(results[2,])
    F1 = 100*unlist(results[3,])
    
    df$precision[(1+ i*Ns.length):((i+1)*Ns.length)] = precision
    df$recall[(1+ i*Ns.length):((i+1)*Ns.length)] = recall
    df$F1[(1+ i*Ns.length):((i+1)*Ns.length)] = F1
    df$Ns[(1+ i*Ns.length):((i+1)*Ns.length)] = Ns
    df$method[(1+ i*Ns.length):((i+1)*Ns.length)] = m
    i = i+1
  }

  i = 0
  xl = "N"
  for(Y in list(precision, recall, F1)){
    yl = if(i == 0) "Precisão (%)" else if (i == 1) "Abrangência (%)" else "F1"
    fl = if(i == 0) "precision_" else if (i == 1) "recall_" else "F1_"
    filename = paste("tese/img_temp/",fl,xl,".png",sep="")
    
    p = ggplot(df, aes(Ns, Y, colour=method)) + 
      geom_line() + 
      geom_point( size=4, shape=21, fill="white") +
      scale_x_continuous(breaks=Ns) +
      #scale_y_continuous(breaks=Y) +
      labs(colour="Método") +
      xlab(xl) +
      ylab(yl)
    p
    ggsave(p, file=filename)  
    
    i = i+1
  }
}
