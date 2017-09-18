setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_per.rda")
library(dplyr)
names(player_stats_per)
drop <- c("FG%", "2P%", "3P%", "FT%", "#", "H", "P", "draft_class", "PER")
dat <- select(player_stats_per, -one_of(drop))
##############################################################
library(nnet)
set.seed(242626)
n = nrow(dat)
samp = sample(1:n, n/2)
train = dat[samp,]
test = dat[-samp,]

mlogit = multinom(label ~ ., data=train)
predict(mlogit, test, type='probs')
test$pred = predict(mlogit, test, type='class')
mean(test$label != test$pred)

################################################################
################################################################
load("player_stats_per.rda")
library(car)
player_stats_per$P <- recode(player_stats_per$P, 
                             "c('PG', 'SG', 'PG/SG')='A'; c('C', 'PF', 'PF/C') = 'B';
                            c('SF', 'SF/PF', 'SG/SF') = 'C'")
player_stats_per$P <- recode(player_stats_per$P, 
                             "c('PG', 'SG', 'PG/SG','SF', 'SF/PF', 'SG/SF')='A'; 
                             c('C', 'PF', 'PF/C') = 'B'")

POS = levels(player_stats_per$P)

library(dplyr)
drop <- c("FG%", "2P%", "3P%", "FT%", "#", "H", "draft_class", "PER")
dat <- select(player_stats_per, -one_of(drop))
head(dat)
ncol(dat)
mlr = matrix(rep(NA, nlevels(dat$P)))

set.seed(24262)
for(i in 1:nlevels(dat$P)){
    n = nrow(dat)
    samp = sample(1:n, n/2)
    train = dat[samp,]
    test = dat[-samp,]
    
    train = train[train$P == POS[i],-(ncol(dat)-1)]
    test = test[test$P == POS[i], -(ncol(dat)-1)]
    
    # multivariate logistic regression
    mlogit = multinom(label ~ ., data=train)
    predict(mlogit, test, type='probs')
    test$pred = predict(mlogit, test, type='class')
    mlr[i] <- mean(as.numeric(as.character(test$label)) != as.numeric(as.character(test$pred)), 
         na.rm = T)
}

mlr
#####################################################
rm(list = ls())
setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_per.rda")
library(dplyr)
names(player_stats_per)
drop <- c("FG%", "2P%", "3P%", "FT%", "#", "H", "P", "draft_class", "PER")
player_stats_per <- select(player_stats_per, -one_of(drop))

# LDA
library(ggplot2)
library(MASS)
library(class)
### LDA
classes = lapply(levels(player_stats_per$label), 
                 function(x) which(player_stats_per$label==x))
train = lapply(classes, function(class) sample(class, 0.7*length(class), replace = F))
train = unlist(train)
test = (1:nrow(player_stats_per))[-train]

playertrain = player_stats_per[train,]
playertest = player_stats_per[test,]
playerlda = lda(data=playertrain,label~.)
playerpred = predict(playerlda, playertest)
playertest$pred <- predict(playerlda, playertest)$class
table(playertest$label, playerpred$class)
mean(playertest$label != playertest$pred)

########################################################
setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_per.rda")
library(dplyr)
names(player_stats_per)
drop <- c("FG%", "2P%", "3P%", "FT%", "#", "H", "P", "draft_class", "PER")
dat <- select(player_stats_per, -one_of(drop))

# svm
library(e1071)
library(nnet)
set.seed(242626)
n = nrow(dat)
samp = sample(1:n, n/2)
train = dat[samp,]
test = dat[-samp,]

train_x <- select(train, -label)
train_y <- train$label

svm_tune_linear <- tune(svm, train.x=train_x, train.y=train_y,
                        kernel="linear", ranges=list(cost=10^(-1:1)))
svm_tune_gauss <- tune(svm, train.x=train_x, train.y=train_y,
                       kernel="radial", ranges=list(cost=10^(-1:1), gamma=c(0.5,1,2,3,4)))
svm_tune_poly <- tune(svm, train.x=train_x, train.y=train_y,
                      kernel="polynomial",
                      ranges=list(cost=10^(-1:1), gamma=c(0.5,1,2,3,4), degree = c(2:4)))

nba_svm <- svm(label~., data=train, kernel="radial", cost=10, gamma = 0.5)
table(test$label, predict(nba_svm, test))
mean(test$label != predict(nba_svm, test))

nba_svm <- svm(label~., data=train, kernel="polynomial", cost=0.1, gamma = 0.5, degree = 3)
table(test$label, predict(nba_svm, test))
mean(test$label != predict(nba_svm, test))

#############################################################
#############################################################
# by position
setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_per.rda")
library(dplyr)
names(player_stats_per)
library(car)
player_stats_per$P <- recode(player_stats_per$P, 
                             "c('PG', 'SG', 'PG/SG')='A'; c('C', 'PF', 'PF/C') = 'B';
                            c('SF', 'SF/PF', 'SG/SF') = 'C'")
# player_stats_per$P <- recode(player_stats_per$P, 
#                              "c('PG', 'SG', 'PG/SG','SF', 'SF/PF', 'SG/SF')='A'; 
#                              c('C', 'PF', 'PF/C') = 'B'")

POS = levels(player_stats_per$P)

library(dplyr)
drop <- c("FG%", "2P%", "3P%", "FT%", "#", "H", "draft_class", "PER")
dat <- select(player_stats_per, -one_of(drop))
head(dat)
# svm
i = 1
set.seed(242626)
n = nrow(dat)
samp = sample(1:n, n/2)
train = dat[samp,]
train = train[train$P == POS[i],]
train = select(train, -P)
test = dat[-samp,]
test = test[test$P == POS[i],]
test = select(test, -P)

train_x <- select(train, -label)
train_y <- train$label

tune(svm, train.x=train_x, train.y=train_y,
                        kernel="linear", ranges=list(cost=10^(-1:1)))
tune(svm, train.x=train_x, train.y=train_y,
                       kernel="radial", ranges=list(cost=10^(-1:1), gamma=c(0.5,1,2,3,4)))
tune(svm, train.x=train_x, train.y=train_y,
                      kernel="polynomial",
                      ranges=list(cost=10^(-1:1), gamma=c(0.5,1,2,3,4), degree = c(2:4)))

nba_svm <- svm(label~., data=train, kernel="radial", cost=10, gamma = 0.5)
table(test$label, predict(nba_svm, test))
mean(test$label != predict(nba_svm, test))

nba_svm <- svm(label~., data=train, kernel="polynomial", cost=0.1, gamma = 0.5, degree = 3)
table(test$label, predict(nba_svm, test))
mean(test$label != predict(nba_svm, test))







































