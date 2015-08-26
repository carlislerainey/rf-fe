# set working directory, e.g.,
# setwd("~/Dropbox/projects/rf-fe")

# clear workspace
rm(list = ls())

# parameters
n_iter <- 3  # number of iterations
proportion_training <- 0.9  # proportion of data to use in training set

# set seed 
set.seed(89317420)

# load packages
library(readr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)

# load and listwise delete data
hj <- read_csv("data/hill-jones.csv")
hj$case_id <- 1:nrow(hj)

# clean up the data a bit and creat formula
hj <- hj %>% select(-one_of("latent_lag", "physint_lag", "amnesty_lag", "year",
                            "physint", "parcomp", "disap", "kill", "polpris", "tort", "amnesty",
                            "disap_lag", "kill_lag", "polpris_lag", "tort_lag", "latent_sd",
                            "hro_shaming_lag", "avmdia_lag", "ainr_lag", "aibr_lag", "polity2",
                            "cwar", "wbimfstruct", "lagus", "lagun", "iwar"))

hj <- hj %>% mutate(gdppc = log(gdppc), pop = log(pop)) %>% as.data.frame(.)
hj <- na.omit(hj)
ivar <- colnames(hj)[!(colnames(hj) %in% c("ccode", "latent"))]
f <- as.formula(paste0("latent ~ ", paste0(ivar, collapse = " + ")))

# replicate hill and jones' model
ls_fit <- lm(f, data = hj)  # clark and golder's model
rf_fit <- randomForest(f, data = hj, ntree = 1000, mtry = 5)  # random forest

# in sample prediction
y_obs <- hj$latent
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
  training_set <- sample_frac(hj, size = proportion_training)
  test_set <- hj[!(hj$case_id %in% training_set$case_id), ]
  # fit models to training set
  ls_fit_t <- lm(f, data = training_set)  # clark and golder's model
  rf_fit_t <- randomForest(f, data = training_set, ntree = 1000, mtry = 5)  # random forest
  # calculate out-of-sample prediction
  y_obs <- test_set$latent
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
  training_countries <- sample(unique(hj$ccode), floor(proportion_training*length(unique(hj$ccode))))
  training_set <- filter(hj, ccode %in% training_countries)
  test_set <- filter(hj, !(ccode %in% training_countries))
  # fit models to training set
  ls_fit_t <- lm(f, data = training_set)  # clark and golder's model
  rf_fit_t <- randomForest(f, data = training_set, ntree = 1000, mtry = 5)  # random forest
  # calculate out-of-sample prediction
  y_obs <- test_set$latent
  mse <- numeric(2)
  mse[1] <- mean((predict(ls_fit_t, newdata = test_set) - y_obs)^2)
  mse[2] <- mean((predict(rf_fit_t, newdata = test_set) - y_obs)^2)
  mse_mat <- rbind(mse_mat, mse)
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
  labs(title = "Hill and Jones (2014)")
ggsave("hill-jones.png", height = 5, width = 10)
