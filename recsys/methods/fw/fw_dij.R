## SETUP OF E_IxJ
source('recsys/setup/functions.R')

setup_eij = function(r, M, debug) {
  e = matrix(0,length(r[1,]), length(r[1,]))
  for(i in 1:length(e[,1])){
    for(j in 1:length(e[1,])){
      e[i,j] = sum(
        b(r[,i] * r[,j], M)
      , na.rm=TRUE)
    } 
  }
  if(debug){
    print("e")
    print(e)
  }
  e
}


## d_fij
setup_dfij = function(a, r, debug, generic=TRUE){
  if(generic) {
    d = array(0,c(length(r[1,]), length(r[1,]), length(a[1,])))
    for(f in 1:length(d[1,1,])) {
      for(i in 1:length(d[1,,1])){
        for(j in 1:length(d[,1,1])){
          d[i,j,f] = 1-delta(a[i,f],a[j,f])
        } 
     }
    }
    d
  }
  else {
    max_distance_release_date = max(a[,1], na.rm=T) - min(a[,1], na.rm=T)
    d = function(i, j, feature="gender"){
      if(feature == "release_date"){
        f = 1 # which(F==feature) # not necessary for now
        if(is.na(a[i,f]) || is.na(a[j,f]))
          1
        else
          abs(a[i,f]-a[j,f])/max_distance_release_date
      }
      else{
        aif = a[i,2:20]
        ajf = a[j,2:20]
        inters = sum(aif * ajf)
        union = sum(b0(aif+ajf))
        jaccard = inters/union
        if(union == 0) jaccard = 0
        1-jaccard
      }
    }
    
    d1 = matrix(0,length(r[1,]), length(r[1,]))
    d2 = matrix(0,length(r[1,]), length(r[1,]))
    for(i in 1:length(d1[,1])){
      for(j in 1:length(d1[1,])){
        d1[i,j] = d(i,j,"release_date")
        d2[i,j] = d(i,j,"gender")
      } 
    }
    if(debug){
      print("d")
      print(h(d1))
      print(h(d2))
    }
    list(d1, d2)    
  }
}

get_iu_fw = function(r, s, M, N, debug){
  diag(s) = NA
  iu = sapply(1:length(U), function(u){
    positive = which(r[u,] > M)
    ans = c() # ans funciona como um conjunto de itens
    for(i in positive){
      # which(is.na(r[u,])) sao os itens ainda nao avaliados por u
      # index.top.N(s[i,], N) sao os melhores itens para u
      # a interseccao da os itens recomendados para u
      ans = union(ans, intersect(which(is.na(r[u,])), index.top.N(s[i,], N)))
    }
    ans[1:N] # escolhemos no maximo N elementos
  })
  if (debug){
    print("iu")
    print(iu)
  }
  iu
}