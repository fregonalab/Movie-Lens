#Load .rda files
load("rda/edx.rda")
load("rda/validation.rda")

#Turn the "timestamp" feature into "diff_years" predictor by subtracting the release year of a movie
#by the user's rating year.
edx_modelling <- edx %>%
  mutate(rating_year = year(as_datetime(timestamp)),
         release_year = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(diff_years = as.numeric(rating_year) - release_year) %>%
  select(userId, movieId, rating, diff_years, title, genres)

validation_modelling <- validation %>%
  mutate(rating_year = year(as_datetime(timestamp)),
         release_year = as.numeric(str_sub(title, -5, -2))) %>%
  mutate(diff_years = as.numeric(rating_year) - release_year) %>%
  select(userId, movieId, rating, diff_years, title, genres)

#Separate the edx_modelling set into training and test set (Cross-validation)


#Save as a new .rda file for next step, modelling
save(train_set, files = "rda/train_set.rda")
save(test_set, files = "rda/test_set.rda")
save(validation_modelling, files = "rda/validation_modelling.rda")
