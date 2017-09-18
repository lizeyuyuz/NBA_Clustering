setwd("~/Dropbox/umich_2017_winter/stats503/project")
load("player_list_df.rda")
load("player_stats.rda")
library(dplyr)
library(cluster)
library(matrixStats)

data = cbind(player_stats, player_list_df[, c("H", "P")])

x = data[,ncol(data)-1]
a <- str_replace_all(x, "-[0-9]+", "")
a <- cm(as.numeric(a)*12) 
b <- str_replace_all(x, "[0-9]+-", "")
b <- cm(as.numeric(b)) 
data[,ncol(data)-1] <- a + b

colnames(data) = c("Season","School","G","MP",
                   "FG","FGA","FGP","P2","P2A","PP2",
                   "P3","P3A","PP3","FT","FTA","FTP","TRB",
                   "AST","STL","BLK","TOV","PF","PTS", 
                   "H", "P")
data = na.omit(data)

data1 = data %>%
    select(-Season, -School, -FGP, -PP2, -PP3, -FTP, -H, -P)

data1_norm = scale(data1)
dat_norm = data1_norm
kpp_init = function(dat, K) {
    x = as.matrix(dat)
    n = nrow(x)
    
    # Randomly choose a first center
    centers = matrix(NA, nrow=K, ncol=ncol(x))
    centers[1,] = as.matrix(x[sample(1:n, 1),])
    
    for (k in 2:K) {
        # Calculate dist^2 to closest center for each point
        dists = matrix(NA, nrow=n, ncol=k-1)
        for (j in 1:(k-1)) {
            temp = sweep(x, 2, centers[j,], '-')
            dists[,j] = rowSums(temp^2)
        }
        dists = rowMins(dists)
        
        # Draw next center with probability propor to dist^2
        cumdists = cumsum(dists)
        prop = runif(1, min=0, max=cumdists[n])
        centers[k,] = as.matrix(x[min(which(cumdists > prop)),])
    }
    return(centers)
}

k = 5
clust_kmeans = kmeans(dat_norm, kpp_init(dat_norm, k), iter.max=100, algorithm='Lloyd')
data_cluster = cbind(data, as.factor(clust_kmeans$cluster))
colnames(data_cluster)[ncol(data_cluster)] = c('cluster')
data_cluster$name = row.names(data_cluster)

# height histgram
ggplot(data_cluster, aes(H)) + 
    geom_histogram(binwidth = 2, color = "white") + 
    scale_x_continuous(breaks = seq(175, 220, by = 2)) + 
    facet_wrap(~cluster, ncol = 1)

# player-position label
player_pos <- str_c(rownames(data_cluster), data_cluster$P, sep =" (")
player_pos <- str_c(player_pos, rep(")", nrow(data_cluster)))
save(player_pos, file = "player_pos.rda")





