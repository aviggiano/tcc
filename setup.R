t0 = Sys.time()
print("Setup started")

## READ INPUT DATA
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

## READ AUXILIARY FUNCTIONS
source('functions.R')

## CREATE RATING MATRIX
r = matrix(0,length(unique(history$user_id)),length(unique(history$item_id)))
print(paste("Initialized rating matrix after", format(round(Sys.time()-t0, 2), nsmall = 2)))
for(i in 1:size(history)) {
  h = history[i,1:3]
  r[h$user_id, h$item_id] = h$rating
}
print(paste("Created rating matrix after", format(round(Sys.time()-t0, 2), nsmall = 2)))

## TRANSFORM ITEM MATRIX
item$release_date = as.numeric(as.Date(as.character(item$release_date), "%d-%b-%Y"))
drops = c("id","title", "video_release_date", "IMDB_URL")
a_temp = item[,!(names(item) %in% drops)]
a = matrix(0, length(a_temp[,1]), length(a_temp[1,]))
for(i in 1:length(a_temp[1,])) {
  a[,i] = a_temp[,i]
}
for(i in 1:length(a[,1])){
  a[i,2] = paste(rbind(a[i,2:20]), collapse='')
}
a = a[,1:2]
  
rm(a_temp)
rm(drop)
gc()

## SET OF FEATURES AND USERS
F = c("release_date", "gender")
U = user$id

## FINISHED
print(paste("Setup finished after", format(round(Sys.time()-t0, 2), nsmall = 2)))
