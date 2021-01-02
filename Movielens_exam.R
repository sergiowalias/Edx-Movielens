#library(caret)
#library(tidyverse)
#library(data.table)
#library(ggthemes)
#library(scales)
#library(lubridate)

#DATA PREPARATION (EDX VS VALIDATION)

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# 'Validation' set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in 'validation' set are also in 'edx' set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from 'validation' set back into 'edx' set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

options(digits=10) 
#DATA PREPARATION 
#partition
set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
edx_test_prov <- edx[test_index,]
#assuring userId and movieId in both partitions
edx_test<-edx_test_prov%>%semi_join(edx_train,by="movieId")%>%semi_join(edx_train,by="userId")
train_2<-anti_join (edx_test_prov,edx_test)
edx_train<-rbind(edx_train,train_2)

#DATA EXPLORATION
#structure of the data
str(edx)
#genre
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>% arrange(-n)%>%
  head(10)
#date
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "red") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma)
#rating
edx %>% group_by(rating) %>% summarize(n=n())
edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  ggtitle("Rating Distribution") + 
  xlab("Rating") +
  ylab("Count") 
#movies
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  scale_x_log10() + 
  geom_histogram(color = "white") +
  ggtitle("Distribution of Movies") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") 
#user
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Users") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = comma)  

#DATA CLEANING.
edx_train <- edx_train %>% select(userId, movieId, rating, genres, title)
edx_test  <- edx_test  %>% select(userId, movieId, rating, genres, title)

#LINEAL MODEL CASE 1 (MOVIE. USER. WITHOUT REGULARIZATION)
#calculate overall average o_avg
  o_avg<-mean(edx_train$rating)
#calculate movie bias
  movie_avg<-edx_train%>%group_by(movieId)%>%summarize(b_mov = mean(rating - o_avg))
#distribution movie_avg
  movie_avg %>% ggplot(aes(x = b_mov)) + 
    geom_histogram(bins=10, col = I("red")) +
    ggtitle("Movie Effect Distribution") +
    xlab("Movie effect") +
    ylab("Count") +
    scale_y_continuous(labels = comma)  

#calculate user bias
  user_avg<-edx_train%>%left_join(movie_avg, by='movieId')%>%group_by(userId)%>%summarize(b_user = mean(rating - o_avg - b_mov))
#distribution user_avg
  edx_train %>% 
    group_by(userId) %>% 
    summarize(b_user = mean(rating)) %>% 
    filter(n()>=100) %>%
    ggplot(aes(b_user)) + 
    geom_histogram(color = "red") + 
    ggtitle("User Effect Distribution") +
    xlab("User Bias") +
    ylab("Count") +
    scale_y_continuous(labels = comma) 
#prediction with user and movie bias
  y_hat_bm_bu <- edx_test %>% 
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    mutate(pred = o_avg + b_mov + b_user) %>%
    .$pred
#RMSE (linear user and movie)  
  RMSE_1 <- sqrt(mean((edx_test$rating - y_hat_bm_bu)^2))
  RMSE_1
  RMSE_edx_test<-tibble(Method="Lineal mov+us",RMSE=RMSE_1)

#LINEAL MODEL CASE 2 (MOVIE. USER. AND GENRE)  
#calculate genres bias
  genres_avg<-edx_train%>%left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>% group_by(genres)%>%summarize(b_genres = mean(rating - o_avg - b_mov-b_user))
#distribution genres bias
  genres_avg %>% ggplot(aes(x = b_genres)) + 
    geom_histogram(bins=10, col = I("red")) +
    ggtitle("Genre Effect Distribution") +
    xlab("Genre effect") +
    ylab("Count") +
    scale_y_continuous(labels = comma)  
#prediction with user, movie and genre bias
  y_hat_bm_bu_g <- edx_test %>% 
    left_join(movie_avg, by='movieId') %>%
    left_join(user_avg, by='userId') %>%
    left_join(genres_avg, by='genres') %>%
    mutate(pred = o_avg + b_mov + b_user+b_genres) %>%
    .$pred
#RMSE (linear user, movie and genres)  
  RMSE_2 <- sqrt(mean((edx_test$rating - y_hat_bm_bu_g)^2))
  RMSE_2
  RMSE_edx_test<-bind_rows(RMSE_edx_test,tibble(Method="Linear mov+us+gen",RMSE=RMSE_2))

#REGULARIZATION.3 (MOVIE AND USER)
  lambda<-seq(0,10,0.25)
  RMSE_3<-vector(length=41)
  for (i in 1:41){
  #calculate movie bias
  movie_reg<-edx_train%>%group_by(movieId)%>%summarize(b_mov = sum(rating - o_avg)/(n()+lambda[i]))
  #calculate user bias
  user_reg<-edx_train%>%left_join(movie_reg, by='movieId')%>%
  group_by(userId)%>%summarize(b_user = sum(rating - o_avg - b_mov)/(n()+lambda[i]))
  #prediction with user and movie bias
  y_hat_reg <- edx_test %>% 
    left_join(movie_reg, by='movieId') %>%
    left_join(user_reg, by='userId') %>%
    mutate(pred = o_avg + b_mov + b_user) %>%
    .$pred
  #RMSE (linear user and movie)  
  RMSE_3[i] <- sqrt(mean((edx_test$rating - y_hat_reg)^2))
  }
  
  plot(lambda,RMSE_3)
  lambda_target<-lambda[which.min(RMSE_3)]
  
 #calculate RMSE_3 with lambda_target
  movie_reg<-edx_train%>%group_by(movieId)%>%summarize(b_mov = sum(rating - o_avg)/(n()+lambda_target))
  user_reg<-edx_train%>%left_join(movie_reg, by='movieId')%>%
  group_by(userId)%>%summarize(b_user = sum(rating - o_avg - b_mov)/(n()+lambda_target))
  y_hat_reg <- edx_test %>% 
    left_join(movie_reg, by='movieId') %>%
    left_join(user_reg, by='userId') %>%
    mutate(pred = o_avg + b_mov + b_user) %>%
    .$pred
  RMSE_3 <- sqrt(mean((edx_test$rating - y_hat_reg)^2))
  (RMSE_3)
  RMSE_edx_test<-bind_rows(RMSE_edx_test,tibble(Method="Regularization mov+us",RMSE=(RMSE_3)))
  
#REGULARIZATION.4 (MOVIE, USER AND GENRES)
  lambda_2<-seq(0,10,0.25)
  RMSE_4<-vector(length=41)
  for (i in 1:41){
    movie_regg<-edx_train%>%group_by(movieId)%>%summarize(b_mov = sum(rating - o_avg)/(n()+lambda_2[i]))
    user_regg<-edx_train%>%left_join(movie_reg, by='movieId')%>%
    group_by(userId)%>%summarize(b_user = sum(rating - o_avg - b_mov)/(n()+lambda_2[i]))
    
    genres_regg<-edx_train%>%left_join(movie_avg, by='movieId') %>%left_join(user_avg, by='userId') %>%
    group_by(genres)%>%summarize(b_genres = sum(rating - o_avg - b_mov-b_user)/(n()+lambda_2[i]))
    
    y_hat_bm_bu_lg <- edx_test %>% 
      left_join(movie_regg, by='movieId') %>%
      left_join(user_regg, by='userId') %>%
      left_join(genres_regg, by='genres') %>%
      mutate(pred = o_avg + b_mov + b_user+b_genres) %>%
      .$pred
    
    RMSE_4[i] <- sqrt(mean((edx_test$rating - y_hat_bm_bu_lg)^2))}
    plot(lambda_2,RMSE_4)
    lambda_target_2<-lambda[which.min(RMSE_4)]  
    
    #calculate RMSE_4 with lambda_target_2
    movie_regg<-edx_train%>%group_by(movieId)%>%summarize(b_mov = sum(rating - o_avg)/(n()+lambda_target_2))
    user_regg<-edx_train%>%left_join(movie_reg, by='movieId')%>%
      group_by(userId)%>%summarize(b_user = sum(rating - o_avg - b_mov)/(n()+lambda_target_2))
    genres_regg<-edx_train%>%left_join(movie_avg, by='movieId') %>%left_join(user_avg, by='userId') %>%
      group_by(genres)%>%summarize(b_genres = sum(rating - o_avg - b_mov-b_user)/(n()+lambda_target_2))
    y_hat_regg <- edx_test %>% 
      left_join(movie_regg, by='movieId') %>%
      left_join(user_regg, by='userId') %>%
      left_join(genres_regg, by='genres') %>%
      mutate(pred = o_avg + b_mov + b_user+b_genres) %>%
      .$pred
    RMSE_4 <- sqrt(mean((edx_test$rating - y_hat_regg)^2))
    (RMSE_4)
    RMSE_edx_test<-bind_rows(RMSE_edx_test,tibble(Method="Regularization mov+us+gen",RMSE=(RMSE_4)))
    
    
#MATRIX FACTORIZATION
set.seed(1)
#recosystem input format:
train_reco <-  with(edx_train, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_reco  <-  with(edx_test,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

r <-  recosystem::Reco()
#select best tuning parameters
opts <- r$tune(train_reco, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))
r$train(train_reco, opts = c(opts$min, nthread = 4, niter = 20))
y_hat_reco <-  r$predict(test_reco, out_memory())
RMSE_5 <- sqrt(mean((edx_test$rating - y_hat_reco)^2))
RMSE_5
RMSE_edx_test<-bind_rows(RMSE_edx_test,tibble(Method="Matrix factorization",RMSE=RMSE_5))
    
    
#VALIDATION. LINEAL VECTORS.
    o_avg_edx<-mean(edx$rating)
    movie_avg_edx<-edx%>%group_by(movieId)%>%summarize(b_mov_edx = mean(rating - o_avg_edx))
    user_avg_edx<-edx%>%left_join(movie_avg_edx,by='movieId')%>%group_by(userId)%>%
      summarize(b_user_edx = mean(rating - o_avg_edx-b_mov_edx))
    genres_avg_edx<-edx%>%left_join(movie_avg_edx,by='movieId')%>%left_join(user_avg_edx,by='userId')%>%
      group_by(genres)%>%summarize(b_genres_edx = mean(rating - o_avg_edx - b_mov_edx-b_user_edx))
    
#VALIDATION. REGULARIZED VECTORS (lambda_target)    
    movie_reg_edx<-edx%>%group_by(movieId)%>%summarize(b_mov_edx = sum(rating - o_avg_edx)/(n()+lambda_target))
    user_reg_edx<-edx%>%left_join(movie_reg_edx,by='movieId')%>%group_by(userId)%>%
      summarize(b_user_edx = sum(rating - o_avg_edx-b_mov_edx)/(n()+lambda_target))
    
#VALIDATION. REGULARIZED VECTORS (lambda_target_2)    
    movie_regg_edx<-edx%>%group_by(movieId)%>%summarize(b_mov_edx = sum(rating - o_avg_edx)/(n()+lambda_target_2))
    user_regg_edx<-edx%>%left_join(movie_reg_edx,by='movieId')%>%group_by(userId)%>%
      summarize(b_user_edx = sum(rating - o_avg_edx-b_mov_edx)/(n()+lambda_target_2))
    genres_regg_edx<-edx%>%left_join(movie_avg_edx,by='movieId')%>%left_join(user_avg_edx,by='userId')%>%
      group_by(genres)%>%summarize(b_genres_edx = sum(rating - o_avg_edx - b_mov_edx-b_user_edx)/(n()+lambda_target_2))
    
#CASE 1: LINEAL WITH MOVIE AND USER EFFECT    
    y_hat_bm_bu_f <- validation %>% 
    left_join(movie_avg_edx, by='movieId') %>%
    left_join(user_avg_edx, by='userId') %>%
    mutate(pred = o_avg_edx + b_mov_edx + b_user_edx) %>%
    .$pred
  
#RMSE (linear user and movie)  
  RMSE_1_v <- sqrt(mean((validation$rating - y_hat_bm_bu_f)^2))
  RMSE_1_v
  
  RMSE_validation<-tibble(Method="Lineal mov+us",RMSE=RMSE_1_v)
  
#CASE 2: LINEAL WITH MOVIE, USER AND GENRES EFFECTS 
    y_hat_bm_bu_fg <- validation %>% 
    left_join(movie_avg_edx, by='movieId') %>%
    left_join(user_avg_edx, by='userId') %>%
    left_join(genres_avg_edx, by='genres') %>%
    mutate(pred = o_avg_edx + b_mov_edx + b_user_edx+b_genres_edx) %>%
    .$pred
  
#RMSE (linear user and movie)  
  RMSE_2_v <- sqrt(mean((validation$rating - y_hat_bm_bu_fg)^2))
  RMSE_2_v
  
  RMSE_validation<-bind_rows(RMSE_validation,tibble(Method="Linear mov+us+gen",RMSE=RMSE_2_v))
  
  
#VALIDATION: CASE 3: REGULARIZATION. MOVIE AND USER EFFECT  
  y_hat_bm_bu_reg <- validation %>% 
    left_join(movie_reg_edx, by='movieId') %>%
    left_join(user_reg_edx, by='userId') %>%
    mutate(pred = o_avg_edx + b_mov_edx + b_user_edx) %>%
    .$pred
  
#RMSE (regularization)  
  RMSE_3_v <- sqrt(mean((validation$rating - y_hat_bm_bu_reg)^2))
  RMSE_3_v
  
  RMSE_validation<-bind_rows(RMSE_validation,tibble(Method="REg mov+us",RMSE=RMSE_3_v))
  
#VALIDATION: CASE 4: REGULARIZATION. MOVIE, USER AND GENRES EFFECT  
  y_hat_bm_bu_regg <- validation %>% 
    left_join(movie_regg_edx, by='movieId') %>%
    left_join(user_regg_edx, by='userId') %>%
    left_join(genres_regg_edx, by='genres') %>%
    mutate(pred = o_avg_edx + b_mov_edx + b_user_edx+b_genres_edx) %>%
    .$pred
  
 #RMSE (regularization_v2)  
  RMSE_4_v <- sqrt(mean((validation$rating - y_hat_bm_bu_regg)^2))
  RMSE_4_v
  
  RMSE_validation<-bind_rows(RMSE_validation,tibble(Method="REg mov+us+gen",RMSE=RMSE_4_v))
  
  #VALIDATION: CASE 5: MATRIX FACTORIZATION WITH RECO 
  set.seed(1)
  edx_reco <-  with(edx, data_memory(user_index = userId, 
                                     item_index = movieId, 
                                     rating = rating))
  validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                    item_index = movieId, 
                                                    rating = rating))
  r <-  recosystem::Reco()
  opts <-  r$tune(edx_reco, opts = list(dim = c(10, 20, 30), 
                                        lrate = c(0.1, 0.2),
                                        costp_l2 = c(0.01, 0.1), 
                                        costq_l2 = c(0.01, 0.1),
                                        nthread  = 4, niter = 10))
  
  r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))
  y_hat_final_reco <-  r$predict(validation_reco, out_memory())
  RMSE_5_v <- sqrt(mean((validation$rating - y_hat_final_reco)^2))
  
    RMSE_validation<-bind_rows(RMSE_validation,tibble(Method="Matrix fact",RMSE=RMSE_5_v))
    options(pillar.sigfig=6)
RMSE_validation  

# Results. Movies
# Regularization
# Best movies
validation %>% 
  left_join(movie_regg_edx, by='movieId') %>%
  left_join(user_regg_edx, by='userId') %>%
  left_join(genres_regg_edx, by='genres') %>%
  mutate(pred = o_avg_edx + b_mov_edx + b_user_edx+b_genres_edx) %>%
  arrange(-pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

## Worst movies
validation %>% 
  left_join(movie_regg_edx, by='movieId') %>%
  left_join(user_regg_edx, by='userId') %>%
  left_join(genres_regg_edx, by='genres') %>%
  mutate(pred = o_avg_edx + b_mov_edx + b_user_edx+b_genres_edx) %>%
  arrange(pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

# Factorization
#best
tibble(title = validation$title, rating = y_hat_final_reco) %>%
  arrange(-rating) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)

#worst
tibble(title = validation$title, rating = y_hat_final_reco) %>%
  arrange(rating) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)
 