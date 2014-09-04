hide.data = function(R, kappa = 0.75){
  R.length = length(R)
  R[sample(R.length)[1:round(kappa * R.length)]] = NA
  R
}

