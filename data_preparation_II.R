library(caret)
library(tidyverse)
library(lubridate)

#Load .rda files
load("rda/edx.rda")
load("rda/validation.rda")

#Turn the "timestamp" feature into "diff_years" predictor by subtracting the release year of a movie
#by the user's rating year.
edx_modelling <- edx %>%
  mutate(rating_year = year(as_datetime(timestamp)),
         release_year = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(timediff = as.numeric(rating_year) - release_year) %>%
  select(userId, movieId, rating, timediff, title, genres)

#----------------------------------------------------#
#               Data sets - Modelling                #
#----------------------------------------------------#
#Separate the edx_modelling set into training and test set
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(edx_modelling$rating, times = 1, p = 0.1,
                                  list = FALSE)
train_temp <- edx_modelling[-test_index, ]
temp <- edx_modelling[test_index, ]

#To make sure we don't include users and movies in the cv set that do not appear 
#in the training set. 
test_set <- temp %>%
  semi_join(train_temp, by = "userId") %>%
  semi_join(train_temp, by = "movieId")

#Add removed rows from the test_set into the train_set
removed <- anti_join(temp, test_set)
train_temp <- rbind(train_temp, removed)

#----------------------------------------------------#
#               Data sets - Tuning                   #
#----------------------------------------------------#
#We will also split the train_set into train_set and cv_set 
#in order to perform cross-validation. 
set.seed(10, sample.kind="Rounding")

#Split train_temp into train_set and cv_set
test_index <- createDataPartition(train_temp$rating, times = 1, p = 0.1,
                                  list = FALSE)
train_set <- train_temp[-test_index, ]
temp <- train_temp[test_index, ]

#To make sure we don't include users and movies in the cv set that do not appear 
#in the training set. 
cv_set <- temp %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "movieId")

#Add removed rows from the test_set into the train_set
removed <- anti_join(temp, cv_set)
train_set <- rbind(train_set, removed)

#Clean Rstudio environment
rm(edx, edx_modelling, validation, temp, train_temp, removed, test_index)

#----------------------------------------------------#
#                  Saving Files                      #
#----------------------------------------------------#
#Save files as new .rda files for next step, modelling
save(train_set, file = "rda/train_set.rda")
save(test_set, file = "rda/test_set.rda")
save(cv_set, file = "rda/cv_set.rda")
