t0 = Sys.time()
print("Setup started")

## READ AUXILIARY FUNCTIONS
source('recsys/setup/functions.R')

## READ INPUT DATA
directory = 'recsys/db/ml-100k'
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

## GET MORE DATA
source('recsys/results/benchmark2.R')
time("Got more data after", t0)

## CREATE RATING MATRIX
r = matrix(0,length(unique(history$user_id)),length(unique(history$item_id)))
time("Initialized rating matrix after", t0)

for(i in 1:size(history)) {
  hi = history[i,1:3]
  r[hi$user_id, hi$item_id] = hi$rating
}
time("Created rating matrix after", t0)

## TRANSFORM ITEM MATRIX
item$release_date = as.numeric(as.Date(as.character(item$release_date), "%d-%b-%Y"))
drops = c("id","title", "video_release_date", "IMDB_URL")
a_temp = item[,!(names(item) %in% drops)]
a = matrix(0, length(a_temp[,1]), length(a_temp[1,]))
for(i in 1:length(a_temp[1,])) {
  a[,i] = a_temp[,i]
}
  
rm(a_temp)
rm(drops)
rm(hi)
gc()

## SET OF FEATURES, USERS AND ITEMS
F = c("release_date", "gender", "year", "length", "budget", "rating", "votes")
U = user$id
I = item$id

## FINISHED
time("Setup finished after", t0)
