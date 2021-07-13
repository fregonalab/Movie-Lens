library(tidyverse)
library(caret)
library(recosystem)

#Load data sets
load("rda/train_set.rda")
load("rda/test_set.rda")
load("rda/cv_set.rda")
load("rda/edx.rda")
load("rda/validation.rda")

#Load loss function - evaluation metric
source("functions/RMSE.R")

#-----------------------------------------------------------------------------------------------------------------#
#                                          Model 1 - Random variation                                             #
#-----------------------------------------------------------------------------------------------------------------#
#Simplest Model -> Total Variability due to random variation only 
mu_hat <- mean(train_set$rating)

#Model Accuracy
naive_rmse <- RMSE(test_set$rating, mu_hat)

#Keep track of RMSE results from different models
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#-----------------------------------------------------------------------------------------------------------------#
#                                 Model 2 - Random variation + Movie Effect                                       #
#-----------------------------------------------------------------------------------------------------------------#
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

#Distribution of b_i
movie_avgs %>% 
  ggplot(aes(b_i)) +
  geom_histogram(color = "black", fill = "white") +
  theme_bw() +
  xlab("Movie bias") +
  ylab("Frequency") +
  ggtitle("Distribution of Movie Effect", subtitle = "Normally distributed")

#Predicted rating - b_i + mu
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  summarise(pred = mu + b_i)

#Model accuracy
movie_rmse <- RMSE(test_set$rating, predicted_ratings$pred)

#Add model 2 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "Movie Effect",
                             RMSE = movie_rmse))
 
#-----------------------------------------------------------------------------------------------------------------#
#                         Model 3 - Random variation + Movie Effect + User Effect                                 #
#-----------------------------------------------------------------------------------------------------------------#
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

#Distribution of b_u
user_avgs %>% 
  ggplot(aes(b_u)) +
  geom_histogram(color = "black", fill = "white") +
  theme_bw() +
  xlab("User bias") +
  ylab("Frequency") +
  ggtitle("Distribution of User Effect", subtitle = "Normally distributed")

#Predicted rating - mu + b_i + b_u
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  summarise(pred = mu + b_i + b_u)

#Model accuracy
user_rmse <- RMSE(test_set$rating, predicted_ratings$pred)

#Add model 3 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "Movie + User Effects",
                             RMSE = user_rmse))

#-----------------------------------------------------------------------------------------------------------------#
#                  Model 4 - Random variation +  Movie Effect + User Effect + Genre Effect                        #
#-----------------------------------------------------------------------------------------------------------------#
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = mean(rating - mu - b_i - b_u))

#Distribution of b_g
genre_avgs %>% 
  ggplot(aes(b_g)) +
  geom_histogram(color = "black", fill = "white") +
  theme_bw() +
  xlab("Genre bias") +
  ylab("Frequency") +
  ggtitle("Distribution of Genre Effect", subtitle = "Left Skewed")

#Predicted rating -  mu + b_i + b_u + b_g
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  summarise(pred = mu + b_i + b_u + b_g)

#Model accuracy
genre_rmse <- RMSE(test_set$rating, predicted_ratings$pred)

#Add model 4 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "Movie + User + Genre Effects",
                             RMSE = genre_rmse))

#-----------------------------------------------------------------------------------------------------------------#
#            Model 5 - Random variation + Movie Effect + User Effect + Genre Effect + Time Effect                 #
#-----------------------------------------------------------------------------------------------------------------#
time_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(timediff) %>%
  summarise(b_t = mean(rating - mu - b_i - b_u - b_g))

#Distribution of b_u
time_avgs %>% 
  ggplot(aes(b_t)) +
  geom_histogram(color = "black", fill = "white") +
  theme_bw() +
  xlab("Time bias") +
  ylab("Frequency") +
  ggtitle("Distribution of Time Effect", subtitle = "Slight Normally Distributed")

#Predicted rating -  mu + b_i + b_u + b_g + b_t
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(time_avgs, by = "timediff") %>%
  summarise(pred = mu + b_i + b_u + b_g + b_t)

#Model accuracy
time_rmse <- RMSE(test_set$rating, predicted_ratings$pred)

#Add model 5 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "User + Movie + Genre + Time Effects",
                             RMSE = time_rmse))

#-----------------------------------------------------------------------------------------------------------------#
#     Model 6 - Random variation + Genre Effect + Time Effect + Movie Effect + User Effect + Regularization       #
#-----------------------------------------------------------------------------------------------------------------#

#Finding the optimal number of lambda based on rsme results
lambdas <- seq(0,10,0.25)

rsme_all <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  #Regularized movie effect
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(l+n()))
  
  #Regularized user effect
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i)/(l+n()))
  
  #Regularized genre effect
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarise(b_g = sum(rating - mu - b_i - b_u)/(l+n()))
  
  #Regularized time effect
  b_t <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    group_by(timediff) %>%
    summarise(b_t = sum(rating - mu - b_i - b_u - b_g)/(l+n()))
  
  #Predicted values
  predicted_rating <- cv_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by = "timediff") %>%
    summarise(pred = mu + b_i + b_u + b_g + b_t) %>%
    .$pred
  
  #Accuracy
  return(RMSE(predicted_rating, cv_set$rating))
    
})

#RSME X lambdas
qplot(lambdas,rsme_all)

#Optimal Lambda
lambda <- lambdas[which.min(rsme_all)]
lambda

#--------------------------------------------------------#
#      Recalculate predictions with optimal lambda       #
#--------------------------------------------------------#

#Regularized movie effect
b_i <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(lambda+n()))

#Regularized user effect
b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu - b_i)/(lambda+n()))

#Regularized genre effect
b_g <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu - b_i - b_u)/(lambda+n()))

#Regularized time effect
b_t <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  group_by(timediff) %>%
  summarise(b_t = sum(rating - mu - b_i - b_u - b_g)/(lambda+n()))

#Predictions
predicted_rating <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "timediff") %>%
  summarise(pred = mu + b_i + b_u + b_g + b_t) %>%
  .$pred

#Accuracy
reg_rmse <- RMSE(test_set$rating, predicted_rating)

#Add model 6 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "User + Movie + Genre + Time Effects + Regularization",
                             RMSE = reg_rmse))

#Add effects into Training and Test Set
reg_train_set <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "timediff") 

reg_test_set <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "timediff")
