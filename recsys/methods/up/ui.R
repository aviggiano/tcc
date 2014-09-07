## get W_UF
source('recsys/methods/up/up_ui_w.R')

get_omega_ui = function(w, a, debug){
  omega = w %*% t(a)  
  if(debug) {
    print("omega")
    print(omega)
  }
  omega
}

ui = function(a, r, U, M=2, k=2, N=1, debug=FALSE){
  t = get_t(a, r, U, M, debug)
  q = get_q(t, U, debug)
  w = get_w(t, q, debug)
  omega = get_omega_ui(w, a, debug)
  iu = get_iu(omega, r, U, N, debug)
  iu
}