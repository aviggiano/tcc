## get W_UF
source('recsys/methods/up/up_ui_w.R')

get_s = function(w, t, U, debug){
  s = matrix(0, length(U), length(U))
  for(u in 1:length(U)){
    Fu = which(t[u,]>0)
    for(v in 1:length(U)){
      Fv = which(t[v,]>0)
      Fuv = intersect(Fu, Fv)
      s[u,v] = if(length(Fuv) == 0) 0 else sum((w[u,]*w[v,])[Fuv], na.rm=TRUE) / (sqrt(sum((w[u,]*w[u,])[Fuv], na.rm=TRUE)) * sqrt(sum((w[v,]*w[v,])[Fuv], na.rm=TRUE)))
    }
  }
  if(debug) {
    print("s")
    print(s)
  }
  s
}

get_omega = function(a, r, s, U, M, k, debug){
  fuf = matrix(0, length(U), length(a[1,]))
  Ivuk_list = list()
  for(u in 1:length(U)){
    vuk = index.top.N.not.self(s[u,], u, k)
    Ivuk = unique(unlist(lapply(vuk, function(v) which(r[v,]>M))))
    Iu = which(!is.na(r[u,]))
    Ivuk = setdiff(Ivuk,Iu)
    Ivuk_list[[u]] = Ivuk
    for(f in 1:length(a[1,])){
      fuf[u,f] = sum(
        b0(a[,f])[Ivuk], na.rm = TRUE
      )
    }
  }
  if(debug) {
    print("fuf")
    print(fuf)
  }
  omega = fuf %*% t(a)
  for(vuk in 1:length(Ivuk_list)){
    Ivuk = Ivuk_list[[vuk]]
    if(is.null(Ivuk))
      omega[vuk,] = 0
    else
      omega[vuk,][-Ivuk] = 0
  }
  if(debug) {
    print("omega")
    print(omega)
  }
}

up = function(a, r, U, M=2, k=2, N=1, debug=FALSE){
  t = get_t(a, r, U, M, debug)
  q = get_q(t, U, debug)
  w = get_w(t, q, debug)
  s = get_s(w, t, U, debug)
  omega = get_omega(a, r, s, U, M, k, debug)
  iu = get_iu(omega, r, U, N, debug)
  iu
}