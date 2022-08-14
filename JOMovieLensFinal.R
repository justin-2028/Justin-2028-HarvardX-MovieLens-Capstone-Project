################################################################################
# MovieLens Capstone Project - Justin Oh
# HarvardX Data Science Professional Certificate PH125.9x
# End Date: 8/13/22
################################################################################

######################################
# Creating the EdX and Validation Sets
######################################

# Required packages will install, please allow several minutes to complete.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

######################################
# End of Provided Code
######################################

######################################
# Data Exploration
######################################

# Load extra libraries for upcoming data analysis and visualization
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(dslabs)
library(ggplot2)
library(lubridate)

# Check any missing value in training data set "edx"
anyNA(edx)
# Check any missing value in final hold-out test data set "validation"
anyNA(validation)
# List amount of observations and variables in training data set "edx"
dim(edx)
# List amount of observations and variables in final hold-out test data set "validation"
dim(validation)
head(edx)
summary(edx)

######################################
# Data Pre-Processing
######################################

# Convert timestamp to year rated and add it to edx
edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))

# Double check any invalid value after convertion of timestamp
unique(edx$year_rated)

# Extract the year released from title and add it to edx
edx <- edx %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))

# Double check any invalid value of year released
unique(edx$year_released)

# Calculate the movie age when movie was rated and add it to edx
edx <- edx %>% mutate(ages = as.numeric(year_rated)-as.numeric(year_released))

# Double check any invalid value of ages
unique(edx$ages)

# Check odd values portion
sum(edx$ages == -1)/nrow(edx)
sum(edx$ages == -2)/nrow(edx)

# Do the same data pre-processing for validation set
validation <- validation %>% mutate(year_rated = year(as_datetime(timestamp)))
validation <- validation %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(ages = as.numeric(year_rated)-as.numeric(year_released))

######################################
# Methods/Analysis
######################################

### Data Analysis

# Head - Show some of the data
head(edx) %>%
  print.data.frame()

# Total unique movies and users
summary(edx)

# Number of unique movies and users in the edx dataset 
edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))

# Plot Ratings distribution
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "blue4") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution")

# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue4") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")


# Table 20 movies rated only once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()

# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue4") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue4") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()


######################################
# Modeling Approach
######################################

### Naive Model

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

# Check results
# Save prediction into data frame
rmse_results <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

### Movie Effect Model

# Simple model taking into account the movie effect b_m
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_m
movie_effects <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))
movie_effects %>% qplot(b_m, geom ="histogram", bins = 10, data = ., color = I("blue4"),
                     ylab = "Number of movies", main = "Number of movies with the computed b_m")


# Test and save RMSE results 
predicted_ratings <- mu +  validation %>%
  left_join(movie_effects, by='movieId') %>%
  pull(b_m)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_1_rmse ))
# Check results
rmse_results %>% knitr::kable()

### Movie + User Effect Model

# Plot simplifed user effect
user_effects<- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_m))
user_effects%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("blue4"))

# Estimating b_u (user effect)
user_effects <- edx %>%
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))

# Test and save RMSE results 
predicted_ratings <- validation%>%
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(prediction = mu + b_m + b_u) %>%
  pull(prediction)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_2_rmse))

# Check result
rmse_results %>% knitr::kable()

### Regularized Movie + User Effect Model

# Lambda is a tuning parameter
# Use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu_reg <- mean(edx$rating)
  
# Regulation movie effect
b_m_reg <- edx %>%
  group_by(movieId) %>%
  summarize(b_m_reg = sum(rating - mu_reg)/(n()+l))
  
# Regulation user effect
b_u_reg <- edx %>%
  left_join(b_m_reg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u_reg = sum(rating - b_m_reg - mu_reg)/(n()+l))
  
# Calculating predicted ratings
predicted_ratings_b_m_u <-
  validation %>%
  left_join(b_m_reg, by = "movieId") %>%
  left_join(b_u_reg, by = "userId") %>%
  mutate(prediction = mu_reg + b_m_reg + b_u_reg) %>%
  .$prediction
  return(RMSE(validation$rating,predicted_ratings_b_m_u))
})

qplot(lambdas, rmses)

# For the full model, the optimal lambda is given as
lambda <- lambdas[which.min(rmses)]
lambda

# Calculating regularization model RMSE: model_m_u_reg_rmse
model_m_u_reg_rmse <- min(rmses)
model_m_u_reg_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Regularization Model",
                                     RMSE = model_m_u_reg_rmse))
rmse_results %>% knitr::kable()


######################################
# Results
######################################

# RMSE results overview                                                          
rmse_results %>% knitr::kable()