# setwd("~/Dropbox/projects/rf-fe")
# clear workspace
rm(list = ls())

# set seed 
set.seed(89317420)
setwd("~/Documents/GitHub/rf-fe/")
# load packages
library(readr)
library(randomForest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plyr)
library(foreach)
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
hj = data.frame(hj[,1],scale(hj[,-1]))
names(hj)[1] = "ccode"
### create with and between country variance

# create country means & add to dataframe
cmeans = ddply(hj[,-which(names(hj)%in%c("case_id"))],.variables = "ccode",numcolwise(mean))
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

btw_variances = data.frame(foreach(i = 2:length(numeric_vars),.combine = rbind)%do%(var(cmeans[,i])))
row.names(btw_variances) = numeric_vars[-1]

within_variances = data.frame(foreach(i = 2:length(numeric_vars),.combine = rbind)%do%(var(hj[,paste(numeric_vars[i],"demeaned",sep="_")])))
row.names(within_variances) = numeric_vars[-1]


variances = data.frame(btw_variances,within_variances,btw_variances/within_variances,factor(numeric_vars[-1]))
names(variances) = c("btw","within","ratio","vars")
variances$ratio[variances$vars == "britcol" | variances$vars == "common"] = NA

variances$vars1 = reorder(variances$vars, variances$ratio)
p = ggplot(data = variances)
p = p + geom_bar(aes(x = vars1,y = ratio),stat= "identity")
p = p + coord_flip() + theme_bw() 
p = p + labs(title = "", y="Ratio Betw Within Variance", x="Variables")+ theme(axis.text.x=element_text(size=12.5)) + theme(axis.text.y=element_text(size=15))+ theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) 
plot(p)
ggsave("~/Desktop/Btw_With_Ratio.pdf", width = 16, height = 16)




variances$vars2 = reorder(variances$vars, variances$btw)
p = ggplot(data = variances)
p = p + geom_bar(aes(x = vars2,y = btw),stat= "identity")
p = p + coord_flip() + theme_bw() 
p = p + labs(title = "", y="Between Variance", x="Variables")+ theme(axis.text.x=element_text(size=12.5)) + theme(axis.text.y=element_text(size=15))+ theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) 
plot(p)
ggsave("~/Desktop/Btw_Var.pdf", width = 16, height = 16)


variances$vars3 = reorder(variances$vars, variances$within)
p = ggplot(data = variances)
p = p + geom_bar(aes(x = vars3,y = within),stat= "identity")
p = p + coord_flip() + theme_bw() 
p = p + labs(title = "", y="Within Variance", x="Variables")+ theme(axis.text.x=element_text(size=12.5)) + theme(axis.text.y=element_text(size=15))+ theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20)) 
plot(p)
ggsave("~/Desktop/Within_Var.pdf", width = 16, height = 16)
