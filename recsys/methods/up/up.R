## get W_UF
source('methods/up/up_ui_w.R')

get_s = function(w, t, U, debug){
  s = matrix(0, length(U), length(U))
  for(u in 1:length(U)){
    for(v in 1:length(U)){
      Fu = which(t[u,]>0)
      Fv = which(t[v,]>0)
      Fuv = intersect(Fu, Fv)
      s[u,v] = if(length(Fuv) == 0) 0 else sum((w[u,]*w[v,])[Fuv]) / (sqrt(sum((w[u,]*w[u,])[Fuv])) * sqrt(sum((w[v,]*w[v,])[Fuv])))
    }
  }
  if(debug) {
    print("s")
    print(s)
  }
  s
}

get_fuf = function(a, r, s, U, k, debug){
  fuf = matrix(0, length(U), length(a[1,]))
  for(u in 1:length(U)){
    for(f in 1:length(a[1,])){
      top_K = h(sort(s[u,], decreasing=TRUE), k)
      vuk = sapply(top_K, function(x) which(s[u,]==x))
      Ivuk = sapply(vuk, function(v) which(r[v,]>M))
      Ivuk = unique(unlist(Ivuk))
      fuf[u,f] = sum(
        b0(a[,f])[Ivuk], na.rm = TRUE
      )
    }
  }
  if(debug) {
    print("fuf")
    print(fuf)
  }
  fuf
}

get_omega = function(a, fuf, debug){
  omega = a %*% t(fuf)
  if(debug){
    print("omega")
    print(omega)
  }
  omega
}


up = function(a, r, U, M=2, k=5, debug=FALSE){
  t = get_t(a, r, U, M, debug)
  q = get_q(t, U, debug)
  w = get_w(t, q, debug)
  s = get_s(w, t, U, debug)
  fuf = get_fuf(a, r, s, U, k, debug)
  omega = get_omega(a, fuf, debug)
  iu = get_iu(omega, r, U, debug)
  iu
}