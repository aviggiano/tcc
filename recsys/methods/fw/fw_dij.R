## SETUP OF E_IxJ
setup_eij = function(r) {
  e = matrix(0,length(r[1,]), length(r[1,]))
  for(i in 1:length(e[,1])){
    for(j in 1:length(e[1,])){
      e[i,j] = sum(
        b0(r[,i] * r[,j])
      )
    } 
  }
  e
}


## d_fij
setup_dfij = function(a, r){
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
  ones = matrix(1,length(r[1,]), length(r[1,]))
  for(i in 1:length(d1[,1])){
    for(j in 1:length(d1[1,])){
      d1[i,j] = d(i,j,"release_date")
      d2[i,j] = d(i,j,"gender")
    } 
  }
  list(d1, d2)
}