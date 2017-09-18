setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_stats_original.rda")
player_stats_original
head(player_stats_original)

player_name <- rownames(player_stats_original)
# add hyphen between first and last name
player_name <- str_replace_all(player_name, " ", "-")
# to lower
player_name <- tolower(player_name)

library(XML)
player_html <- vector("list", length(player_name))
for(i in 1:length(player_name)){
    print(i)
    player_html[[i]] <- htmlTreeParse(
        str_c("http://www.sports-reference.com/cbb/players/", 
              player_name[i], 
              "-1.html"), 
        useInternalNodes=T)
}

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

per_list <- lapply(1:length(player_name), function(e) per_extract(player_html[[e]]))

per_last <- lapply(1:length(player_name), function(e) tail(per_list[[e]], n = 1))

per_year = unlist(sapply(1:length(player_name), function(e) per_last[[e]][1]))
per_per = unlist(sapply(1:length(player_name), function(e) per_last[[e]][2]))

per_data <- data.frame(Player = player_name, Year = per_year, PER = per_per)
# PER data for each player
head(per_data)
# save(per_data, file = "per_data.rda")

#################################### 
library(stringr)
load("per_data.rda")
nrow(per_data)
head(per_data)
load("player_stats_original.rda")
nrow(player_stats_original)
head(player_stats_original)

# check if two dataset player names match
players <- str_replace_all(rownames(player_stats_original), " ", "-")
players <- tolower(players)
ind <- players %in% per_data$Player


player_stats_use <- cbind(player_stats_original, PER = per_data$PER)
save(player_stats_use, file = "player_stats_use.rda")
data <- player_stats_use
data <- data[, -ncol(data)]
data <- na.omit(data)
nrow(data)

load("player_stats_use.rda")



















