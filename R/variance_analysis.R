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
hj$case_id <- 1:nrow(hj)

# clean up the data a bit and creat formula
hj <- hj %>% select(-one_of("latent_lag", "physint_lag", "amnesty_lag", "year",
                            "physint", "parcomp", "disap", "kill", "polpris", "tort", "amnesty",
                            "disap_lag", "kill_lag", "polpris_lag", "tort_lag", "latent_sd",
                            "hro_shaming_lag", "avmdia_lag", "ainr_lag", "aibr_lag", "polity2",
                            "cwar", "wbimfstruct", "lagus", "lagun", "iwar"))

hj <- hj %>% mutate(gdppc = log(gdppc), pop = log(pop)) %>% as.data.frame(.)
hj <- na.omit(hj)
glimpse(hj)


### create with and between country variance

# create country means & add to dataframe
cmeans = ddply(hj,.variables = "ccode",numcolwise(mean))
glimpse(cmeans)
numeric_vars = names(cmeans)
names(cmeans) = paste(names(cmeans),"cmean",sep="_")
names(cmeans)[1] = "ccode"
hj = join(hj,cmeans,by="ccode")


### make sure dimensions are correct
dim(cmeans)[1] == length(unique(hj$ccode))

demeaned = foreach(i = 2:dim(cmeans)[2],.combine = cbind)%do%(hj[,numeric_vars[i]] - hj[,names(cmeans)[i]])

demeaned = data.frame(demeaned)
names(demeaned) = paste(numeric_vars[-1],"demeaned",sep="_")

hj = data.frame(hj,demeaned)

btw_variances = data.frame(foreach(i = 2:length(numeric_vars),.combine = cbind)%do%(var(cmeans[,i])))
names(btw_variances) = numeric_vars[-1]

within_variances = data.frame(foreach(i = 2:length(numeric_vars),.combine = cbind)%do%(var(hj[,numeric_vars[i]])))
names(within_variances) = numeric_vars[-1]

