movie <- read.csv("/Volumes/현듀/Hyundeww/빅데이터 장학금/data/movie_metadata.csv", header=TRUE)

#2000
movie <- subset(movie, movie$title_year >= 2000)
sum(complete.cases(movie))
attach(movie)

moviea <- movie[,-c(1, 17, 26)]
colnames(moviea)
movie.n <- moviea[,c("num_critic_for_reviews", "duration",
                     "director_facebook_likes", "gross",
                     "cast_total_facebook_likes", "facenumber_in_poster",
                     "num_user_for_reviews", "budget", "imdb_score",
                     "movie_facebook_likes")]

naf <- function(a){
  x <- c(rep(0, length(a)))
  for(i in 1:length(a)){
    x[[i]] <- sum(is.na(a[[i]]))
  }
  x.d <- as.data.frame(x)
  rownames(x.d) <- colnames(a)
  x.d}

# Perform mice imputation
library(mice)
mice_tt <- mice(movie.n, method="rf", maxit = 2)  

# Save the complete output 
mice_tt <- complete(mice_tt)
summary(mice_tt)

# dealing with NAs
movie.n$gross <- mice_tt$gross
movie.n$budget <- mice_tt$budget
naf(movie.n) #gross, budget NA deleted

movie.n <- na.omit(movie.n)
summary(movie.n)


boxplot(movie.n$gross)
hist(log(movie.n$gross))
