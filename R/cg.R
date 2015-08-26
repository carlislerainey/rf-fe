# set working directory, e.g.,
# setwd("~/Dropbox/projects/rf-fe")

# clear workspace
rm(list = ls())

# parameters
n_iter <- 10  # number of iterations
proportion_training <- 0.9  # proportion of data to use in training set

# set seed 
set.seed(4897350)

# load packages
library(readr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)

# load and listwise delete data
cg <- read_csv("data/clark-golder.csv")
cg$case_id <- 1:nrow(cg)

# replicate Clark and Golder's main model and the rf (col 4, table 2)
f_int <- enep1 ~ eneg*log(avemag) + eneg*uppertier + enpres*proximity1
f_rf <- enep1 ~ eneg + avemag + uppertier + proximity1 + enpres

ls_fit <- lm(f_int, data = cg)  # clark and golder's model
rf_fit <- randomForest(f_rf, data = cg, ntree = 1000, mtry = 5)  # random forest

# in sample prediction
y_obs <- cg$enep1
mse <- numeric(2)
mse[1] <- mean((predict(ls_fit) - y_obs)^2)
mse[2] <- mean((predict(rf_fit) - y_obs)^2)
mse
mse_df1 <- data.frame(method = c("least squares", "random forest"),
                     prediction = "in-sample",
                     mse = mse)

# out-of-sample, within-country prediction
mse_mat <- NULL
for (i in 1:n_iter) {
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3)
  # set up training and test data
  training_set <- sample_frac(cg, size = proportion_training)
  test_set <- cg[!(cg$case_id %in% training_set$case_id), ]
  # fit models to training set
  ls_fit_t <- lm(f_int, data = training_set)  # clark and golder's model
  rf_fit_t <- randomForest(f_rf, data = training_set, ntree = 1000, mtry = 5)  # random forest
  # calculate out-of-sample prediction
  y_obs <- test_set$enep1
  mse <- numeric(2)
  mse[1] <- mean((predict(ls_fit_t, newdata = test_set) - y_obs)^2)
  mse[2] <- mean((predict(rf_fit_t, newdata = test_set) - y_obs)^2)
  mse_mat <- rbind(mse_mat, mse)
  # update progress bar
  setTxtProgressBar(pb, i)
}
mse <- apply(mse_mat, 2, mean)
mse_df2 <- data.frame(method = c("least squares", "random forest"),
                      prediction = "out-of-sample, within-country",
                      mse = mse)

# out-of-sample, out-of-country predictions
mse_mat <- NULL
for (i in 1:n_iter) {
  # set up progress bar
  pb <- txtProgressBar(min = 0, max = n_iter, style = 3)
  # set up training and test data
  training_countries <- sample(unique(cg$country), floor(proportion_training*length(unique(cg$country))))
  training_set <- filter(cg, country %in% training_countries)
  test_set <- filter(cg, !(country %in% training_countries))
  # fit models to training set
  ls_fit_t <- lm(f_int, data = training_set)  # clark and golder's model
  rf_fit_t <- randomForest(f_rf, data = training_set, ntree = 1000, mtry = 5)  # random forest
  # calculate out-of-sample prediction
  y_obs <- test_set$enep1
  mse <- numeric(2)
  mse[1] <- mean((predict(ls_fit_t, newdata = test_set) - y_obs)^2)
  mse[2] <- mean((predict(rf_fit_t, newdata = test_set) - y_obs)^2)
  mse_mat <- rbind(mse_mat, mse)
  # update progress bar
  setTxtProgressBar(pb, i)
  # update progress bar
  setTxtProgressBar(pb, i)
}
mse <- apply(mse_mat, 2, mean)
mse_df3 <- data.frame(method = c("least squares", "random forest"),
                      prediction = "out-of-sample, out-of-country",
                      mse = mse)
mse_df <- rbind(mse_df1, mse_df2, mse_df3)

# plot mses
ggplot(mse_df, aes(method, mse)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ prediction) + 
  labs(title = "Clark and Golder (2006)")
ggsave("clark-golder.png", height = 5, width = 10)
