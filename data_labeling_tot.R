setwd("~/Dropbox/umich_2017_winter/stats503/project")
library(stringr)
load("player_stats_tot.rda")
head(player_stats_tot)
playername <- rownames(player_stats_tot)
# add hyphen between first and last name
player_name <- str_replace_all(playername, " ", "-")
# to lower
player_name <- tolower(player_name)

library(XML)
player_html <- vector("list", length(player_name))
for(i in 574:length(player_name)){
    print(i)
    player_html[[i]] <- htmlTreeParse(
        str_c("http://www.sports-reference.com/cbb/players/", 
              player_name[i], 
              "-1.html"), 
        useInternalNodes=T, isURL = T)
}
player_html <- player_html[-c(573, 627)]
player_name <- player_name[-c(573, 627)]

per_extract <- function(player = player_html[[1]]){
    dat <- xpathSApply(player,"//div[@id='all_players_advanced']//comment()", xmlValue)
    dat = unlist(strsplit(dat, '\\n'))
    start <- str_which(dat, "<tbody>") + 1
    end <- str_which(dat, "</tbody>") - 2
    dat <- dat[start:end]
    
    Year = rep(NA, length(dat))
    Per = rep(NA, length(dat))
    for(i in 1:length(dat)){
        dt <- unlist(strsplit(dat[i], '\\<td class=\"right \" data-stat=\"'))
        # dt <- unlist(strsplit(dat[3], '\\<td class=\"right \" data-stat=\"'))
        dt[1] <- str_replace_all(dt[1], "[^0-9]", "")
        dt[1] <- substr(dt[1], 1,4)
        Year[i] <- as.numeric(dt[1])
        per <- unlist(strsplit(dt[-1], "\" >"))
        per <- str_replace_all(per, "</td><", "")
        num <- which(per == "per") + 1
        Per[i] = as.numeric(per[num])
    }
    dff <- as.data.frame(cbind(Year, Per))
    colnames(dff) <- NULL
    return(dff)
}

per_list <- lapply(1:length(player_html), function(e) per_extract(player_html[[e]]))

per_last <- lapply(1:length(player_html), function(e) tail(per_list[[e]], n = 1))

per_year = unlist(sapply(1:length(player_html), function(e) per_last[[e]][1]))
per_per = unlist(sapply(1:length(player_html), function(e) per_last[[e]][2]))

per_data <- data.frame(Player = playername[1:length(player_html)], 
                       Year = per_year, 
                       PER = per_per)

# PER data for each player
per_data_tot <- per_data
# save(per_data_tot, file = "per_data_tot.rda")

load("per_data_tot.rda")
head(per_data_tot)

# # combining with the draft class, find draft class ranking/label
# library(stringr)
# load("player_list.rda")
# sapply(player_list, nrow)
# draft_class = matrix(as.numeric(rep(str_c("201", 0:5), each = 60)), ncol = 6)
# for(i in 1:6){
#     player_list[[i]] <- cbind(player_list[[i]], draft_class=draft_class[,i])
# }
# player_list <- rbind(player_list[[1]], player_list[[2]], player_list[[3]], 
#                      player_list[[4]], player_list[[5]], player_list[[6]])
# player_list$Player <- str_replace_all(player_list$Player, " ", "-")
# player_list$Player <- tolower(player_list$Player)
# ind <- player_list$Player %in% player_name
# 
# player_list <- player_list[ind, ]
# player_list_df <- player_list
# # save(player_list_df, file = "player_list_df.rda")
# load("player_list_df.rda")
# 
# player_label <- player_list_df[,c("#", "Player", "draft_class", "P")]

library(dplyr)
load("player_stats_tot.rda")
head(player_stats_tot)
player_stats_tot <- player_stats_tot[-c(454, 653), ]
player_lab <- cbind(player_stats_tot, per_data)
sum(rownames(player_stats_tot) == player_lab[,26])
head(player_lab)
player_lab <- player_lab[,-c(26,27)]
head(player_lab)

player_complete_per <- player_lab
save(player_complete_per, file = "player_complete_per.rda")
load("player_complete_per.rda")
head(player_complete_per)

player_lab$draft_class
cut_point <- which(player_lab$`#` == 1)
length(cut_point)
class09 <- player_lab[cut_point[1]:(cut_point[2]-1),]
class10 <- player_lab[cut_point[2]:(cut_point[3]-1),]
class11 <- player_lab[cut_point[3]:(cut_point[4]-1),]
class12 <- player_lab[cut_point[4]:(cut_point[5]-1),]
class13 <- player_lab[cut_point[5]:(cut_point[6]-1),]
class14 <- player_lab[cut_point[6]:(cut_point[7]-1),]
class15 <- player_lab[cut_point[7]:(cut_point[7]-1),]
class00 <- player_lab[cut_point[8]:(cut_point[7]-1),]
class01 <- player_lab[cut_point[9]:(cut_point[7]-1),]
class02 <- player_lab[cut_point[10]:(cut_point[7]-1),]
class03 <- player_lab[cut_point[11]:(cut_point[7]-1),]
class04 <- player_lab[cut_point[12]:(cut_point[7]-1),]
class05 <- player_lab[cut_point[6]:(cut_point[7]-1),]
class06 <- player_lab[cut_point[6]:(cut_point[7]-1),]
class07 <- player_lab[cut_point[6]:(cut_point[7]-1),]
class08 <- player_lab[cut_point[6]:(cut_point[7]-1),]

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


player_per_lab <- rbind(class10,class11,class12,class13,class14,class15)
player_stats_per <- cbind(player_stats, 
                          label = player_per_lab$label, Draft_class = player_per_lab$draft_class,
                          PER = player_per_lab$PER, PER_year = player_per_lab$Year, 
                          Position = player_per_lab$`#`, P = player_per_lab$P)

player_stats_per
player_stats_per$label <- as.factor(player_stats_per$label)
player_stats_per$P <- as.factor(player_stats_per$P)

# player_stats_per_4 <- player_stats_per
# save(player_stats_per_4, file = "player_stats_per_4.rda")
# player_stats_per_5 <- player_stats_per
# save(player_stats_per_5, file = "player_stats_per_5.rda")









