size <- function(x.df) {
    length(x.df[,1])
}

where <- function(attr, val) {
    item[which(item[attr] == val),]
}

wheren <- function(attr,val) {
    item[which(item[attr] != val),]
}

count.na <- function(xs) {
    length(which(is.na(xs) == TRUE))
}

count.not.na <- function(xs) {
    length(xs) - count.na(xs)
}

cdnna <- function(xs) {
    count.not.na(unique(xs))
}

color_id <- function(c) {
    which(color$color == c)
}

change_sth <- function(){
    i <- 1
    n = length(item$color)
    t0 <- Sys.time()
    while(i <= n) {
        item$color[i] <- color_id(item$color[i])
        if (i %% 1000 == 0) {
            print(i)
            print(Sys.time()-t0)
        }
        i <- i+1
    }
}

delta <- function(x, y){
    as.numeric(x == y)
}

dk <- function(x,y){
    delta(x,y)
}

delta_vector <- function(xs, ys){
    ans <- 0
    for(i in 1:length(xs)){
        ans = ans + delta(xs[i], ys[i])
    }
    ans / length(xs)
}

sim_delta_bag_of_words <- function(xs, ys){
    if (length(xs) == 0 || length(ys) == 0)
        0
    else {
        m <- sapply(xs, function(i){
            sapply(ys, function(j){
                delta(i,j)
            })
        })
        # do not count duplicates
        sum(m[row(m)!=col(m)]) / max(length(xs), length(ys))
    }
}

sq_dist_L2 <- function(X,Y){
    ans <- 0
    for(i in 1:length(X)){
        ans <- ans + (X[i]-Y[i])*(X[i]-Y[i])
    }
    ans
}

dist_L2 <- function(X,Y){
    sqrt(sq_dist_L2(X,Y))
}

dist <- function(X,Y){
    dist_L2(X,Y)
}

red <- c(255,0,0)
green <- c(0,255,0)
blue <- c(0,0,255)
black <- c(0,0,0)
white <- c(255,255,255)

max_dist_color <- dist(black,white)

dc <- function(color1, color2){
    dist(color1, color2) / max_dist_color
}    

rgb <- function(color){
    color_rgb<- color_map[which(color_map['color']==tolower(color)),]
    c(color_rgb$r, color_rgb$g, color_rgb$b)
}

sim_color <- function(color1, color2){
    if(length(rgb(color1)) == 0 || length(rgb(color2)) == 0)
        0
    else
        sim_color_rgb(rgb(color1), rgb(color2))
}

sim_color_rgb <- function(color1rgb, color2rgb){
    1-dc(color1rgb, color2rgb)
}

max_dist_price <- dist(min(wheren('sale_price',0)$sale_price), max(item$sale_price))

dist_price <- function(price1, price2){
    dist(price1, price2) / max_dist_price
}

sim_price <- function(price1, price2){
    1 - dist_price(price1, price2)
}

dist_gender <- function(x,y){
    masc_fem <- c("Masculino", "Feminino")
    if (x == y)
        0
    else if (x %in% masc_fem && y %in% masc_fem) #um e' masc e o outro e' fem
        1
    else
        0.5 # falta completar
}

sim_gender <- function(x,y){
    1-dist_gender(x,y)
}

sim <- function(i, j){
    sim_general_item <- delta(i$general_item_id,j$general_item_id)
    sim_brand <- delta(i$brand, j$brand)
    sim_attribute_set <- delta(i$attribute_set, j$attribute_set)
    sim_type <- delta(i$type, j$type)
    sim_sport <- delta(i$sport, j$sport)
    sim_category <- delta(i$category, j$category)
    sim_subcategory <- delta(i$subcategory, j$subcategory)
    sim_gender <- sim_gender(i$gender, j$gender)
    sim_color <- sim_color(i$color, j$color)
    sim_sale_price <- sim_price(i$sale_price, j$sale_price)
    sim <- c(sim_general_item,
             sim_brand,
             sim_attribute_set,
             sim_type,
             sim_sport,
             sim_category,
             sim_subcategory,
             sim_gender,
             sim_color,
             sim_sale_price)
    sum(sim) / length(sim)       
}

get_sim_matrix <- function() {
    n <- 100#size(item)
    sapply(1:n, function(x) sapply(x:n, function(y) sim(item[x,],item[y,])))
}
sim_matrix <- get_sim_matrix()

sim_all <- function(index1, index2){
    if (index1 > index2){
        big <- index1
        small <- index2
    }
    else {
        big <- index2
        small <- index1
    }
    sim_matrix[[small]][big]
}
