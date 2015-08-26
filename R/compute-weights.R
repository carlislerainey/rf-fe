# set working directory, e.g.,
# setwd("~/Dropbox/projects/rf-fe")

# clear workspace
rm(list = ls())

# parameters
n_iter <- 10  # number of iterations
proportion_training <- 0.9  # proportion of data to use in training set

# set seed 
#set.seed(4897350)

# load packages
library(readr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)

# load and listwise delete data
cg <- read_csv("data/clark-golder.csv")
cg$case_id <- 1:nrow(cg)
test_case <- sample(1:nrow(cg), 1)
test_set <- filter(cg, case_id == test_case)
training_set <- filter(cg, case_id != test_case)
#test_set <- filter(cg, country == "India" & year == 1999)
#training_set <- filter(cg, country != "India" | year != 1999)

# replicate Clark and Golder's main model using a rf (col 4, table 2)
f_rf <- enep1 ~ eneg + avemag + uppertier + proximity1 + enpres

rf_fit <- randomForest(f_rf, data = training_set, ntree = 1000, mtry = 3)  # random forest
rf_pred_train <- predict(rf_fit, newdata = training_set, predict.all = TRUE, nodes = TRUE)
rf_pred_test <- predict(rf_fit, newdata = test_set, predict.all = TRUE, nodes = TRUE)

weights <- matrix(0, nrow = nrow(cg), ncol = 1000)
for (i in 1:1000) {
  node_X <- attr(rf_pred_train, "nodes")[, i]
  node_x0 <- attr(rf_pred_test, "nodes")[, i]
  weights[which(node_x0 == node_X), i] <- 1/length(which(node_x0 == node_X))
}
cg$W_bar <- apply(weights, 1, mean)

cg_nz <- filter(cg, W_bar > 0 & W_bar > quantile(W_bar, .90))
cg_nz$country.year <- reorder(cg_nz$country.year, cg_nz$W_bar)
ggplot(cg_nz, aes(x = country.year, y = W_bar)) + 
  geom_bar(stat = "identity") + 
  labs(title = paste("Predicting", test_set$country.year)) + 
  coord_flip()

