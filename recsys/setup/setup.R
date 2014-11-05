t0 <<- Sys.time()
library(ggplot2)
source('recsys/setup/functions.R')

directory = 'recsys/db/ml-100k/'
history.colnames = c("user_id", "item_id", "rating", "elapsedstamp")
item.colnames = c("id", "title", "release_date", "video_release_date", "IMDB_URL", 
                  "unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                  "Film_Noir", "Horror", "Musical", "Mystery", "Romance", "Sci_Fi", "Thriller", "War", "Western")
user.colnames = c("id", "age", "gender", "occupation", "zip_code")

read.data = function(filename, separator, header, col.names) {
  data = read.csv(filename, as.is=TRUE, sep=separator, header=header)
  if(typeof(col.names)=="character") colnames(data) = col.names
  data
}

read.history = function(filename=paste(directory,"u.data",sep=""), separator="\t", header=FALSE, col.names=history.colnames){
  read.data(filename, separator, header, col.names)
}

read.item = function(filename=paste(directory,"u.item",sep=""), separator="|", header=FALSE, col.names=item.colnames){
  item = read.data(filename, separator, header, col.names)
}

read.user = function(filename=paste(directory,"u.user",sep=""), separator="|", header=FALSE, col.names=user.colnames){
  read.data(filename, separator, header, col.names)
}

read.IMDB = function(item){
  data(movies)
  
  m = movies
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
  item
}

create.r = function(history){
  r = matrix(0,length(unique(history$user_id)),length(unique(history$item_id)))
  
  for(i in 1:size(history)) {
    hi = history[i,1:3]
    r[hi$user_id, hi$item_id] = hi$rating
  }
  r
}

create.a = function(item){
  item$release_date = as.numeric(as.Date(as.character(item$release_date), "%d-%b-%Y"))
  drops = c("id","title", "video_release_date", "IMDB_URL")
  a_temp = item[,!(names(item) %in% drops)]
  a = matrix(0, length(a_temp[,1]), length(a_temp[1,]))
  for(i in 1:length(a_temp[1,])) {
    a[,i] = a_temp[,i]
  }
  a
}

## GET 100k
history = read.history()
item = read.item()
user = read.user()
## GET IMDB
item = read.IMDB(item) 
## GET r_ui and a_if
r = create.r(history)
a = create.a(item)
  
rm(item)
rm(history)
rm(user)
gc()






