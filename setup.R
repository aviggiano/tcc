t0 = Sys.time()
print("Setup started")

directory = 'ml-100k'
history_filename = paste(directory,"u.data",sep='/')
item_filename = paste(directory,"u.item",sep='/')
user_filename = paste(directory,"u.user",sep='/')

history = read.csv(history_filename, as.is=TRUE, sep="\t", header=FALSE)
item = read.csv(item_filename, as.is=TRUE, sep="|", header=FALSE)
user = read.csv(user_filename, as.is=TRUE, sep="|", header=FALSE)

colnames(history)=c("user_id", "item_id", "rating", "timestamp")
colnames(item)=c("id", "title", "release_date", "video_release_date", "IMDB_URL", 
                 "unknown", "Action", "Adventure", "Animation", "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                 "Film_Noir", "Horror", "Musical", "Mystery", "Romance", "Sci_Fi", "Thriller", "War", "Western")
colnames(user)=c("id", "age", "gender", "occupation", "zip_code")

source('functions.R')

rating = matrix(0,length(unique(history$user_id)),length(unique(h$item_id)))
lapply(1:size(history), function(index) {
  rating[history[index,]$user_id][history[index,]$item_id] = history[index,]$rating
})

print(paste("Setup finished after", format(round(Sys.time()-t0, 2), nsmall = 2)))
