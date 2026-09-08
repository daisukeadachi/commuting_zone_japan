# this file reads data, generate distance matrix and clusters with hierarchical agglomerative clustering

# one can modify `height` parameters to examine different tree heights. 

# housekeeping ------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggdendro) 
# library(devtools)
# install_github("cran/Nippon")   # Nippon package is not in CRAN as of 200208
library(Nippon)

# raw directory
rawdir <- 'data/'

# parameter for zero lower bound dissimilarity
small <- 0.001

# MODIFY THIS PARAMETER TO EXAMINE DIFFERENT TREE HEIGHTS. 
height <- 0.98

# function of reading and clustering -------------------------------------

year <- 2015
theight <- height
munic.code <- 'harmonized'

clustering <- function(year, theight, munic.code){
  
  print(paste('working on', year, '...', ', threshold', theight))
  
  # first attempt is 2005, the natural beginning year of 2001-2017 analysis
  df <- read_csv(paste0(rawdir, 'commute_', year, '_', munic.code, '_conceal.csv'))
  
  
  # to define the distance first, generate bilateral flow variable
  df.ij <- df %>% 
    rename(i = living_mun, j = commute_mun, flow.ij = pop) %>% 
    select(i, j, flow.ij)
  df.ji <- df %>% 
    rename(j = living_mun, i = commute_mun, flow.ji = pop) %>% 
    select(i, j, flow.ji)
  df.bil <- df.ij %>% 
    left_join(df.ji, by = c('i', 'j'))
  
  # to fill NA, define the complete set (expand.grid)
  set.munic <- df %>% select(living_mun) %>% unique %>% unlist
  df.full <- expand.grid(set.munic, set.munic) %>% 
    rename(i = Var1, j = Var2) %>% 
    left_join(df.bil, by = c('i', 'j'))
  
  # fill NA by zeros
  df.full <- df.full %>% replace(., is.na(.), 0)
  
  # add total labor force
  df.lab <- df %>% 
    select(living_mun, tot_pop_living_mun) %>% 
    unique
  df.full <- df.full %>% 
    left_join(df.lab, by = c('i' = 'living_mun')) %>% rename(rlf.i = tot_pop_living_mun) %>% 
    left_join(df.lab, by = c('j' = 'living_mun')) %>% rename(rlf.j = tot_pop_living_mun)
  
  # define distance
  df.full <- df.full %>% 
    mutate(dist = ifelse(
      i == j,
      0,
      # sometimes less than zero when the size of i and j is very different and the larger send a lot to the smaller.
      pmax(small, 1 - (flow.ij + flow.ji)/pmin(rlf.i, rlf.j))
    ))
  
  # then spread
  df.mat <- df.full %>%
    select(i, j, dist) %>%
    spread(j, dist)
  
    # check the order of matrix
    sum(as.integer(colnames(df.mat)[2:length(colnames(df.mat))]) == df.mat$i) == nrow(df.mat)
    print(paste0('in ', year, ', the number of municipalities is ', nrow(df.mat)))
  
  
  # cluster -------------------------------------------------------------
  
  
  # for better dendrogram plot, get the label of each municipality
  name <- read_csv(paste0('data/mmm/codelist_', year, '1001and', year, '1001.csv'), locale = locale(encoding = 'CP932')) %>%
    mutate(munic_eng = kakasi(ifelse(is.na(GNAME1), paste(PNAME1, CNAME1), paste(PNAME1, GNAME1, CNAME1)))) %>%
    mutate(i = as.integer(JISCODE1)) %>%
    select(i, munic_eng)
  # sort in the order of df.
  name <- df.mat %>%
    left_join(name, by = 'i') %>%
    select(i, munic_eng)
  
  df_mat <- df.mat[,2:(nrow(df.mat)+1)]
  
  # 'dist' object, and define cluster
  colnames(df_mat) <- c(name$munic_eng)
  dendro <- df_mat %>% as.dist %>% hclust(method = 'average') 
  
    # check heights
    print(paste('highest dissimilarity value:', max(dendro$height)))
    dendro$height[length(dendro$height)-10:length(dendro$height)]
    
  # clustering
  clusters <- dendro %>% cutree(h = theight) # tolbert and sizer value
  print(paste('count of CZs:', max(clusters))) # number of clusters
  
  # save --------------------------------------------------------------------
  
  df.out <- data.frame(
    municipality = df.mat$i,
    cz = clusters,
    size = df.lab$tot_pop_living_mun
  )
  
  write_csv(df.out, paste0('output/replication_by_tree_heights/', year, '_', munic.code, '_small-', small, '_tree_height-', theight, '_concealed.csv'))
  
}

# GO! ---------------------------------------------------------------------


expand.grid(
  year = seq(1980, 2015, 5), 
  theight = height,
  munic.code = c('original', 'harmonized')
) %>% pmap(clustering)



