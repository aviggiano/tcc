source('fw_dij.R')

# wi = 1
W = matrix(1, length(F), 1)

# SIMILARITY MATRIX
s = W[1,1]*(1-d1) + W[1,1]*(1-d2)
# NORMALIZED SIMILARITY
s = s / max(s)