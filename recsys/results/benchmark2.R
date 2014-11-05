library(ggplot2)
data(movies)

m = movies #movies[which(movies$votes > 1000),]
m.title = paste(m$title, paste(paste("(",m$year,sep=""),")",sep=""))
inter = intersect(m.title, item$title)

keeps = c("year", "length", "budget", "rating", "votes")

for(k in keeps){
  item[k] =  rep(NA, length(item[,1]))
}
for(t in item$title){
  if(t %in% inter) {
    i = which(t == item$title)
    j = which(t == m.title)
    B = m[j, (names(m) %in% keeps)]

    for(k in keeps){
      item[i,k] =  B[k]
    }
  }
}

rm(m)
rm(inter)
rm(m.title)
rm(keeps)
rm(movies)