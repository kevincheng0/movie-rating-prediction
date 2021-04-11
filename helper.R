library(tidyverse)

data <- read.csv("User_profiles v2.csv")

ratings <- read.csv("ratings.csv")
grouped <- ratings %>% count(userId) %>% arrange(userId)
avg_ratings <- ratings %>% group_by(userId) %>% summarize(avg = mean(rating))

movies <- read.csv("movies.csv")
movie_avg <- ratings %>% 
  group_by(movieId) %>% 
  summarize(avg_rating = mean(rating)) %>%
  arrange(movieId)

head(movie_avg)




find_reviews <- function(Id) {
  user <- grouped %>% filter(userId == Id[1])
  return(user[2])
}

find_avg <- function(Id) {
  user <- avg_ratings %>% filter(userId == Id)
  return(round(user[2], digits = 2))
}

get_extremes <- function(Id) {
  genre_ratings <- data[-c(21:39)] %>% filter(userId == Id)
  genre_ratings[is.na(genre_ratings)] <- 0
  genre_ratings <- genre_ratings[3:20]
  high <- colnames(genre_ratings)[max.col(genre_ratings,ties.method="first")]
  genre_ratings[genre_ratings == 0] <- 5
  low <- names(genre_ratings)[apply(genre_ratings, MARGIN = 1, FUN = which.min)]
  return(c(high, low))
}

get_movie <- function(Id) {
  return(movies[Id[1], 2])
}

get_genres <- function(mId) {
  genres <- movies[mId, 3]
  return(paste(unlist(strsplit(genres, "\\|")), "_Mean", sep = ""))
}

predict_rating <- function(uId, mId) {
  genres <- gsub("\\-", "\\_", get_genres(mId[1]))
  user_ratings <- data[-c(21:39)] %>% filter(userId == uId)
  user_ratings[is.na(user_ratings)] <- 0
  user_ratings <- user_ratings[3:20] %>% select(genres)
  user_score <- rowMeans(user_ratings)
  movie_score <- movie_avg[mId[1], 2]
  print(user_score * 0.75 + movie_score * 0.25)
  return(round((user_score * 0.75 + movie_score * 0.25), digits = 2))
  
}


