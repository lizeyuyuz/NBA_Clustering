setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_use.rda")

data <- player_stats_use
data <- na.omit(data)
nrow(data)
ind <- data$draft_class
class10 <- data[ind == 2010, ]
class11 <- data[ind == 2011, ]
class12 <- data[ind == 2012,]
class13 <- data[ind == 2013,]
class14 <- data[ind == 2014,]
class15 <- data[ind == 2015,]

x = class10$PER
p_draft_lab <- cut(x, breaks=seq(min(x),max(x), length.out=5), include.lowest=T, labels=F)
class10 <- cbind(class10, label = p_draft_lab)
x = class11$PER
p_draft_lab <- cut(x, breaks=seq(min(x, na.rm =T), 
                                 max(x, na.rm =T), length.out=5), include.lowest=T, labels=F)
class11 <- cbind(class11, label = p_draft_lab)
x = class12$PER
p_draft_lab <- cut(x, breaks=seq(min(x, na.rm =T),
                                 max(x, na.rm =T), length.out=5), include.lowest=T, labels=F)
class12 <- cbind(class12, label = p_draft_lab)
x = class13$PER
p_draft_lab <- cut(x, breaks=seq(min(x, na.rm =T),
                                 max(x, na.rm =T), length.out=5), include.lowest=T, labels=F)
class13 <- cbind(class13, label = p_draft_lab)
x = class14$PER
p_draft_lab <- cut(x, breaks=seq(min(x, na.rm =T),
                                 max(x, na.rm =T), length.out=5), include.lowest=T, labels=F)
class14 <- cbind(class14, label = p_draft_lab)
x = class15$PER
p_draft_lab <- cut(x, breaks=seq(min(x, na.rm =T),
                                 max(x, na.rm =T), length.out=5), include.lowest=T, labels=F)
class15 <- cbind(class15, label = p_draft_lab)


player_stats_per <- rbind(class10,class11,class12,class13,class14,class15)
names(player_stats_per)

head(player_stats_per)
player_stats_per$label <- as.factor(player_stats_per$label)
player_stats_per$P <- as.factor(player_stats_per$P)

# save(player_stats_per, file = "player_stats_per.rda")



load("player_stats_use.rda")
player_stats_use <- na.omit(player_stats_use)




