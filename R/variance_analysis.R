# setwd("~/Dropbox/projects/rf-fe")
setwd("~/Documents/GitHub/rf-fe/")
# clear workspace
rm(list = ls())

# set seed 
set.seed(89317420)

# load packages
library(readr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plyr)
# load and listwise delete data
hj <- read_csv("data/hill-jones.csv")

glimpse(hj)


### create with and between country variance

# country means
cmeans = ddply(hj,.variables = hj$ccode,summarize, ddfun = mean(jd$tort_lag))
