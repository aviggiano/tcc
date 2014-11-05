## get W_UF
source('recsys/methods/common/up_ui_w.R')

get_omega_ui = function(w, a, debug){
  omega = w %*% t(a)  
  if(debug) {
    print("omega")
    print(omega)
  }
  omega
}

ui = function(a, r, rtrain.rtest, Utrain.Utest, M, k, N, debug){
  # train: get IDF
  Utrain = Utrain.Utest[[1]]
  Utest = Utrain.Utest[[2]]
  
  rtrain = rtrain.rtest[Utrain,]
  rtest = rtrain.rtest[Utest,]
  
  U = get_U(rtrain, debug)
  TF = get_TF(a, rtrain, M, debug)
  IDF = get_IDF(TF, U, debug)
  
  # test
  U = get_U(rtrain.rtest, debug)
  TF = get_TF(a, rtrain.rtest, M, debug)
  w = get_w(TF, IDF, debug)
  omega = get_omega_ui(w, a, debug)
  iu = get_iu(omega, r, rtrain.rtest, Utest, N, debug)
  iu
}