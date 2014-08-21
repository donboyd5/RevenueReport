# findstateneighbors.r
# Don Boyd
# 8/21/2014

# https://www.census.gov/geo/reference/county-adjacency.html

ubase <- "https://www.census.gov/geo/reference/docs/"
fn <- "county_adjacency.txt"

download.file(paste0(ubase, fn), paste0(qtd, fn))


# state centroids
fn <- "CenPop2010_Mean_ST.txt"

df <- read.csv(paste0(qtd, fn))
