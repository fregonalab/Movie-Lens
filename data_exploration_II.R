library(tidyverse)
library(ggrepel)

#Load data sets
load("rda/reg_train_set.rda")

#-----------------------------------------------------------------------------------------------#
#                                Short Data Wrangling                                           #
#-----------------------------------------------------------------------------------------------#
#Rating average
mu <- mean(reg_train_set$rating)

#In order to have a better performance, we filtered movies with less than 100 rating, 
#and users that rated less than 100 times out of the data set
reg_exp_set <- reg_train_set %>%
  group_by(movieId) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  ungroup()

#Calculate model residuals
residuals_set <- reg_exp_set %>%
  mutate(residual = rating - mu - b_i - b_u - b_g - b_t) %>%
  select(movieId, userId, residual)

#MovieId X UserId residual matrix
y <- residuals_set %>%
  spread(movieId, residual) %>%
  as.matrix()

#Name Colunms and Rows for better interpretation
rownames(y) <- y[, 1]
y <- y[ ,-1]

movie_titles <- reg_train_set %>% 
  select(movieId, title) %>%
  distinct()
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
#-----------------------------------------------------------------------------------------------#
#                                Data Exploration                                               #
#-----------------------------------------------------------------------------------------------#
#Correlation
corr <- round(cor(y, use = "pairwise.complete"), 2)

#Organize the correlation results in a table
corr %>%  
  as.table %>%                                 # Start from the correlation matrix
  as.data.frame() %>%                         # Turn into 3-column table
  subset(Var1 != Var2 & abs(Freq) > 0.6) %>%
  sample_n(20) %>%                         
  arrange(Freq) %>%                      # Sort it
  knitr::kable() 

#Highly correlated movies
as.data.frame(y) %>%
  ggplot(aes(x = `Finding Nemo (2003)`, y = `Toy Story (1995)`)) +
  geom_point()

#Least correlated movies
as.data.frame(y) %>%
  ggplot(aes(x = `It Takes Two (1995)`, y = `Toy Story (1995)`)) +
  geom_point()
















