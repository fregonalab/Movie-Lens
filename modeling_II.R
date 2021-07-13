library(tidyverse)
library(caret)
library(recosystem)

#Load data sets
load("rda/reg_train_set.rda")
load("rda/reg_test_set.rda")
load("rda/edx.rda")
load("rda/validation.rda")
load("rda/rmse.rda")

#Load loss function - evaluation metric
source("RMSE.R")

#-------------------------------------------------------------------------------------------------#
# Model 7 - Random variation + Genre Effect + Time Effect + Movie Effect + User Effect + 
#                             Regularization + Matrix Factorization
#-------------------------------------------------------------------------------------------------#

#Data Wrangling
mf_train <- reg_train_set %>%
  mutate(residual = rating - mu - b_i - b_u - b_g - b_t) 

mf_test <- reg_test_set %>%
  mutate(residual = rating - mu - b_i - b_u - b_g - b_t) 

#In recosystem, a DataSource class object is used as input data. In other words, the training and the test set have to be 
#converted to a DataSource object prior to modelling.

#Matrix factorization
set.seed(40, sample.kind = "Rounding")
train_data <-  with(mf_train, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating     = residual))
test_data  <-  with(mf_test,  data_memory(user_index = userId, 
                                    item_index = movieId, 
                                    rating     = residual))

#Create the model object
r <- Reco()

#Define the best tuning parameters
tune <- r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = 0.1,
                                       costp_l1 = 0, costq_l1 = 0,
                                       nthread = 2, niter = 10))

# Train the algorithm  
r$train(train_data, opts = c(tune$min, nthread = 5, niter = 25))

# Calculate the predicted values  
y_hat <- r$predict(test_data, out_memory())

#Predicted_values
mf_test <- mf_test %>%
  mutate(y_hat = y_hat + mu + b_i + b_u + b_g + b_t)

#Accuracy
mf_rmse <- RMSE(mf_test$rating, mf_test$y_hat)

#Add model 6 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(Method = "Random variation + Genre Effect + Time Effect + Movie Effect + User Effect + 
                             Regularization + Matrix Factorization",
                             RMSE = mf_rmse))

#Models RMSE results
rmse_results %>%
  kbl(booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(row = 0, bold = T) %>% 
  column_spec(1, width = "14cm")

#-----------------------------------------------------------------------------------------------------------------#
#                                  Model 8 - Matrix Factorization                                                 #
#-----------------------------------------------------------------------------------------------------------------#
set.seed(20, sample.kind = "Rounding")
train_data <-  with(mf_train, data_memory(user_index = userId, 
                                          item_index = movieId, 
                                          rating     = rating))
test_data  <-  with(mf_test,  data_memory(user_index = userId, 
                                          item_index = movieId, 
                                          rating     = rating))

#Create the model object
r <- Reco()

#Define the best tuning parameters
tune <- r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = 0.1,
                                       costp_l1 = 0, costq_l1 = 0,
                                       nthread = 2, niter = 10))

# Train the algorithm  
r$train(train_data, opts = c(tune$min, nthread = 5, niter = 25))

# Calculate the predicted values  
y_hat <- r$predict(test_data, out_memory())

#Accuracy
reco_rmse <- RMSE(test_set$rating, y_hat)

#Add model 6 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "Matrix Factorization",
                             RMSE = reco_rmse))

#-----------------------------------------------------------------------------------------------------------------#
#                                       Model 9 - Final Model                                                     #
#-----------------------------------------------------------------------------------------------------------------#
#Matrix Factorization Model
set.seed(30, sample.kind = "Rounding")
train_data <-  with(edx, data_memory(user_index = userId, 
                                     item_index = movieId, 
                                     rating     = rating))
test_data  <-  with(validation,  data_memory(user_index = userId, 
                                             item_index = movieId, 
                                             rating     = rating))

#Create the model object
r <- Reco()

#Define the best tuning parameters
tune <- r$tune(train_data, opts = list(dim = c(10, 20, 30), lrate = 0.1,
                                       costp_l1 = 0, costq_l1 = 0,
                                       nthread = 2, niter = 10))

# Train the algorithm  
r$train(train_data, opts = c(tune$min, nthread = 5, niter = 25))

# Calculate the predicted values  
y_hat <- r$predict(test_data, out_memory())

#Accuracy
final_rmse <- RMSE(validation$rating, y_hat)

#Add model 6 results to tracking table
rmse_results <- rbind(rmse_results, 
                      tibble(method = "Final Model",
                             RMSE = final_rmse))








