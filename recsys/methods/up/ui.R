## get W_UF
source('methods/up/up_ui_w.R')

get_omega = function(w, a){
  omega = w %*% t(a)  
  omega
}

ui = function(a, r, U, M){
  t = get_t(a, U, M)
  q = get_q(t, U)
  w = get_w(t, q)
  omega = get_omega(w, a)
  iu = get_iu(omega, r, U)
  iu
}