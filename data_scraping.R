setwd("~/Dropbox/umich_2017_winter/stats503/project")

library(stringr)
library(rvest)
########################################################
# players drafted 
start_year = 2010
end_year = 2015
seasons <- seq(start_year, end_year, 1)
# scrape player names and ranking from nbadraft.net
draftplayer_read <- function(){
    urls <- sapply(seasons, function(e)
        str_c("http://www.nbadraft.net/nba_final_draft/",e))
    page <- lapply(urls, read_html)
    draftees <- lapply(1:length(seasons),
                       function(e) html_table(html_nodes(page[[e]], "table"))[[1]])
    return(draftees)
}
player_list <- do.call(rbind, draftplayer_read())
names(player_list)
# convert wide format to long format
player_list <- rbind(player_list[,1:8], player_list[,9:16])

names(player_list)
# select only useful columns
player_list <- player_list[,c("#", "Player", "H", "P")]
sapply(draftplayer_read(), nrow)
# include draft_class lable
draft_class = as.numeric(rep(str_c("201", 0:5), each = 60))
# complete 360 rows
player_list_df <- cbind(player_list, draft_class)
# incomplete 
player_name <- player_list_df$Player
player_name <- str_replace_all(player_name, " ", "-")
# to lower
player_name <- tolower(player_name)
# create list container
player_html <- vector("list", length(player_name))
# load each player's college stat into each element in the list
for (i in 1:length(player_name)){
    player_html[[i]] <- tryCatch(read_html(str_c("http://www.sports-reference.com/cbb/players/",
                                                 player_name[i], "-1.html")),
                                 error = function(w){print(str_c("error!", i, player_name[i]))},
                                 warning = function(e){print(str_c("warning!",i, player_name[i]))}
    )
}
player_html_orginal <- player_html
# save(player_html_orginal, file = "player_html_orginal.rda")
# leave only draf players whose college stats available
ind_original <- unlist(lapply(player_html_orginal, function(e){length(e) == 2}))
player_list_df <- player_list_df[ind_original, ]
head(player_list_df)
# save(player_list_df, file = "player_list_df")
load("player_list_df.rda")

# find college statistics
# convert html table to dataframes
player_ind <- player_html[ind_original]
playercollege_stats <- lapply(1:length(player_ind),
                              function(e) html_table(html_nodes(player_ind[[e]], "table"))[[1]])
# save(playercollege_stats, file = "playercollege_stats.rda")
load("playercollege_stats.rda")

# player names
playername <- player_list_df$Player
# check each player's stat's dimension
a <- sapply(1:length(playername), function(e) ncol(playercollege_stats[[e]]))
min(a) # 24
max(a) # 26

player_stats <- matrix(rep(NA, n = min(a)*149), ncol = min(a), nrow = length(playername))
player_stats <- as.data.frame(player_stats)
for(i in 1:length(playercollege_stats)){
    player_stats[i,1:24]<- tail(playercollege_stats[[i]][,1:24], n = 1)
}
names(player_stats) <- names(playercollege_stats[[1]][,1:24])
# remove conference variable
player_stats <- player_stats[,-3]
rownames(player_stats) <- playername
head(player_stats)

# combine draft list and college stats
player_stats_original <- cbind(player_stats[, -c(1,2)], 
                               player_list_df[, c("#", "H", "P", "draft_class")])
x = player_stats_original$H
a <- str_replace_all(x, "-[0-9]+", "")
a <- cm(as.numeric(a)*12) 
b <- str_replace_all(x, "[0-9]+-", "")
b <- cm(as.numeric(b)) 
player_stats_original$H <- a + b
# save(player_stats_original, file = "player_stats_original.rda")
load("player_stats_original.rda")
head(player_stats_original)
nrow(player_stats_original)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################



########################################################
# players drafted 
start_year = 2009
end_year = 2015
seasons <- seq(start_year, end_year, 1)
# scrape player names and ranking from nbadraft.net
draftplayer_read_1 <- function(){
    urls <- sapply(seasons, function(e)
        str_c("http://www.nbadraft.net/nba_final_draft/",e))
    page <- lapply(urls, read_html)
    draftees <- lapply(1:length(seasons),
        function(e) html_table(html_nodes(page[[e]], "table"))[[1]])
    return(draftees)
}
player_list_1 <- do.call(rbind, draftplayer_read_1())
player_list_1 <- rbind(player_list_1[,1:8], player_list_1[,9:16])
names(player_list_1)
draft_class = as.numeric(rep(c("2009",str_c("201", 0:5)), each = 60))
player_list_1 <- cbind(player_list_1, draft_class)
player_list_1 <- player_list_1[,c("#", "Player", "H", "P", "draft_class")]
names(player_list_1)

# players drafted 
start_year = 2000
end_year = 2008
seasons <- seq(start_year, end_year, 1)
# scrape player names and ranking from nbadraft.net
draftplayer_read_2 <- function(){
    urls <- sapply(seasons, function(e)
        str_c("http://www.nbadraft.net/nba_draft_history/",e, ".html"))
    page <- lapply(urls, read_html)
    draftees <- lapply(1:length(seasons),
                       function(e) html_table(html_nodes(page[[e]], "table"))[[1]])
    draftees
    return(draftees)
}
ll <- draftplayer_read_2()
for(i in 1:9){
    ll[[i]] <- cbind(ll[[i]], 
                     draft_class = rep(str_c("200", i-1), each = nrow(ll[[i]])))
    x <- ll[[i]]
    x <- x[,-c(2, 6)]
    names(x) <- c("#", "Team", "info", "#", "Team", "info", "draft_class")
    player_list_21 <- cbind(x[,1:3], draft_class = x[,7])
    player_list_22 <- cbind(x[,4:6], draft_class = x[,7])
    ll[[i]] <- rbind(player_list_21, player_list_22)
}
player_list_2 <- do.call(rbind, ll)

# extract player names
x4 <- player_list_2$info
# x4 <- x4[-c(47, 82, 145)]
x4 <- str_replace_all(x4, "\\n", " ")
x4 <- str_replace_all(x4, "[0-9]-[0-9]*[[:print:]]*$", "")
x4 <- str_replace_all(x4, "[[:blank:]]*$", "")
x4 <- str_replace_all(x4, "\\*", "")
# extract player height
hei <- player_list_2$info
hei <- str_replace_all(hei, "\\n", " ")
hei <- as.vector(str_extract_all(hei, "[0-9]-[0-9]", ""))
# extract player position
pos <- player_list_2$info
pos <- str_replace_all(pos, "\\n", " ")
pos <- str_replace_all(pos, "^[[:print:]]*[0-9]-[0-9]*", "")
pos <- str_replace_all(pos, "^[[:blank:]]+", "")
pos <- str_replace_all(pos, "[[:digit:]]*", "")
pos <- str_replace_all(pos, "^[[:blank:]]+", "")
pos <- str_replace_all(pos, "[[:punct:]]+", "")
pos <- str_replace_all(pos, "^[[:blank:]]+", "")
pos <- strsplit(pos, " ")
pos <- unlist(lapply(pos, function(e) e[1]))

remov <- str_which(x4, "[[:punct:]]")
x4 <- x4[-remov] # remove everything irregular
hei <- hei[-remov]
pos <- pos[-remov]
draft_class <- player_list_2$draft_class
draft_class <- draft_class[-remov] 
player_list_2 <- player_list_2[-remov,]
player_list_2 <- data.frame(player_list_2$`#`, x4, hei, pos, draft_class)
names(player_list_2) <- c("#", "Player", "H", "P", "draft_class")
# combine 2000-2015
player_list_tot <- rbind(player_list_1, player_list_2)
# save(player_list_tot, file = "player_list_tot.rda")

########################################################

load("player_list_tot.rda")
# webscraping format:
# add hyphen between first and last name
dt <- player_list_tot
player_name <- dt$Player
player_name <- str_replace_all(player_name, " ", "-")
# to lower
player_name <- tolower(player_name)
# create list container
player_html <- vector("list", length(player_name))
# load each player's college stat into each element in the list
for (i in 1:684){
    player_html[[i]] <- tryCatch(read_html(str_c("http://www.sports-reference.com/cbb/players/",
                                           player_name[i], "-1.html")),
                           error = function(w){print(str_c("error!", i, player_name[i]))},
                           warning = function(e){print(str_c("warning!",i, player_name[i]))}
    )
}
for (i in 685:length(player_name)){
    player_html[[i]] <- tryCatch(read_html(str_c("http://www.sports-reference.com/cbb/players/",
                                                 player_name[i], "-1.html")),
                                 error = function(w){print(str_c("error!", i, player_name[i]))},
                                 warning = function(e){print(str_c("warning!",i, player_name[i]))}
    )
}

# leave only players whose college stats available
ind_tot <- unlist(lapply(player_html, function(e){length(e) == 2}))
# save(ind_tot, file = "ind_tot.rda")
load("ind_tot.rda")
player_list_tot <- player_list_tot[ind_tot, ]
head(player_list_tot)
# save(player_list_tot, file = "player_list_tot.rda")

player_ind <- player_html[ind_tot]
################################################################################
# convert html table to dataframes
playercollege_stats_tot <- lapply(1:length(player_ind),
                              function(e) html_table(html_nodes(player_ind[[e]], "table"))[[1]])
# save(playercollege_stats_tot, file = "playercollege_stats_tot.rda")
load("playercollege_stats_tot.rda")

load("player_list_tot.rda")
playername <- player_list_tot$Player
length(playername)

a <- sapply(1:length(playername), function(e) ncol(playercollege_stats_tot[[e]]))
min(a) # 24
max(a) # 26

player_stats <- matrix(rep(NA, n = min(a)*length(playername)), 
                       ncol = min(a), nrow = length(playername))
dim(player_stats)
player_stats <- as.data.frame(player_stats)
for(i in 1:length(playercollege_stats_tot)){
    player_stats[i,1:24]<- tail(playercollege_stats_tot[[i]][,1:24], n = 1)
}

names(player_stats) <- names(playercollege_stats_tot[[1]][,1:24])
# remove conference variable
player_stats <- player_stats[,-3]
# rownames
dupl <- which(duplicated(playername) == TRUE)
dup <- which(playername == playername[dupl])
rownames(player_stats)[1:dup[1]] <- playername[1:dup[1]]
rownames(player_stats)[dup[1]] <- "Marcus Williams.1"
rownames(player_stats)[(dup[1]+1):dup[2]] <- playername[(dup[1]+1):dup[2]]
rownames(player_stats)[dup[2]] <- "Marcus Williams.2"
rownames(player_stats)[(dup[2]+1):length(playername)] <- 
    playername[(dup[2]+1):length(playername)]

head(player_stats)

# combine draft list and college stats
player_stats <- cbind(player_stats[, -c(1,2)], 
                               player_list_tot[, c("#", "H", "P", "draft_class")])
player_stats_tot <- player_stats

x = player_stats_tot$H
a <- str_replace_all(x, "-[0-9]+", "")
a <- cm(as.numeric(a)*12) 
b <- str_replace_all(x, "[0-9]+-", "")
b <- cm(as.numeric(b)) 
player_stats_tot$H <- a + b

head(player_stats_tot)
# save(player_stats_tot, file = "player_stats_tot.rda")
load("player_stats_tot.rda")



##############################################################################
# original 2010 - 2015
load("player_stats_original.rda")
head(player_stats_original)
nrow(player_stats_original)
##############################################################################
# expanded 2000 - 2015
load("player_stats_tot.rda")
head(player_stats_tot)
nrow(player_stats_tot)
