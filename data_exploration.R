library(caret)
library(tidyverse)
library(stringr)
library(lubridate)
library(kableExtra)

#Load edx data 
load("rda/edx.rda")

#Disable scientific notation
options(scipen = 999)

#-------------------------------------------------------------------------------------------------#
#                                 General Properties                                              #
#-------------------------------------------------------------------------------------------------#
#Dataset Dimensions
dim(edx)

#Dataset Structure
str(edx, vec.len = 2)

#Small sample of observations
head(edx)

#Distinct values of each predictors
edx %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_rating = n_distinct(rating)) %>%
  knitr::kable()

#-------------------------------------------------------------------------------------------------#
#                                  Data Exploration                                               #
#-------------------------------------------------------------------------------------------------#

## Movies ##

#Number of times distinct movies were rated by users - Movies Variability
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 100, color = "black") +
  scale_x_log10() +
  ylab("Number of movies") +
  xlab("Number of ratings") +
  ggtitle("Distribuition of movie ratings: Netflix Challenge") +
  theme_classic()

#Top 5 most rated movies
edx %>%
  group_by(title) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(5)

#Top 5 least rated movies
edx %>%
  group_by(title) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  slice_tail(n=5)

#Proportion of users who rated a specific movie
edx %>%
  count(movieId) %>%
  mutate(p_users = case_when(
    n <= 700 ~ "less than 1%",
    n >= 5000 ~ "more than 10%",
    TRUE ~ "between 1% and 10%"
  )) %>% 
  count(p_users)

#Average movie rating - variability
edx %>%
  group_by(movieId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(movieId, avg)) +
  geom_point() +
  theme_classic() + 
  ylab("Rating") +
  xlab("Movie Identification") +
  ggtitle("Average rating per movie", subtitle = "Strong variability between different movies")

## Users ##

#Users predictor - Variability
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 100, color = "black") +
  scale_x_log10() +
  ylab("Number of users") +
  xlab("Number of ratings") +
  ggtitle("Distribuition of user ratings: Netflix Challenge") +
  theme_classic()

#Top 5 most active users
edx %>%
  group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(5)

#Top 5 least active users
edx %>%
  group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  slice_tail(n=5)

#Average movie rating - variability
edx %>%
  group_by(userId) %>%
  summarise(avg = mean(rating)) %>%
  ggplot(aes(userId, avg)) +
  geom_point() +
  theme_classic() + 
  ylab("Rating") +
  xlab("Movie Identification") +
  ggtitle("User's average movie rating", subtitle = "Strong variability between different users")

## Ratings ##

#Rating distribuition - True Variability of our data - More 3's and 4's
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, colour = "black", fill = "white") +
  scale_x_continuous(breaks = seq(0, 5, by=0.5)) +
  stat_bin(binwidth = 0.5, aes(label = ..count..), vjust = -0.5, geom = "text")

## Genres ##

#Distribution of ratings per genre - Drama and Comedy were the most rated genres 
edx %>%
  count(genres) %>%
  arrange(desc(n)) %>%
  slice_head(n=30) %>%
  ggplot(aes(reorder(genres, n, sum), n)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") +
  coord_flip()

#Average rating per genre - Note that the avg values are confidently different from each other
edx %>%  
  group_by(genres) %>%
  mutate(n = n()) %>%
  filter(n >= 50000) %>%
  summarise(n = n(),
            avg = mean(rating),
            se = sd(rating)/sqrt(n)) %>%
  ggplot(aes(x = reorder(genres, avg), y = avg, ymin = avg - qnorm(0.975)*se, ymax = avg + qnorm(0.975)*se)) +
  geom_point() +
  geom_errorbar()
  
## Timestamp

##Calculating the difference between the released year of a movie, 
#and the time it was rated by an user.
edx <- edx %>%
  mutate(rating_year = year(as_datetime(timestamp)),
         release_year = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(diff_years = as.numeric(rating_year) - release_year) %>%
  select(userId, movieId, rating, diff_years, title, genres)

#Distribuition - Most ratings were recent to movie's release year
edx %>%
  ggplot(aes(diff_years)) +
  geom_histogram(binwidth = 1, colour = "black", fill = "white") 

#Number of movies
edx %>%
  count(diff_years)

#Release year - percentage of ratings
edx %>%
  count(diff_years) %>%
  mutate(rating_year = case_when(
    diff_years == 1 ~ "release year",
    diff_years <= 5 ~ "less than 5 years",
    TRUE ~ "more than 5 years"
    ), total = sum(n)) %>%
  group_by(rating_year) %>%
  summarise(ratio = unique(sum(n)/total)*100) %>%
  knitr::kable()

#Loess - geom_point (all points)
edx %>%
  group_by(diff_years) %>%
  summarise(n = n(),
            avg = mean(rating),
            se = sd(rating)/sqrt(n)) %>%
  ggplot(aes(diff_years, y = avg, ymin = avg - qnorm(0.975)*se, ymax = avg + qnorm(0.975)*se)) +
  geom_point() +
  geom_errorbar() +
  theme_classic() + 
  ylab("Average movie rating") +
  xlab("Differencial between movie launch year and user rating year") +
  ggtitle("Average rating per year after movie launch", subtitle = "Nostalgia effect")










