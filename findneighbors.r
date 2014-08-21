# findstateneighbors.r
# Don Boyd
# 8/21/2014

# https://www.census.gov/geo/reference/county-adjacency.html

ubase <- "https://www.census.gov/geo/reference/docs/"
fn <- "county_adjacency.txt"

download.file(paste0(ubase, fn), paste0(qtd, fn))

data()
db <- data()
# state.abb (state)                    US State Facts and Figures
# state.area (state)                   US State Facts and Figures
# state.center (state)                 US State Facts and Figures
# state.division (state)               US State Facts and Figures
# state.name (state)                   US State Facts and Figures
# state.region (state)                 US State Facts and Figures
# state.x77 (state)                    US State Facts and Figures

# state centroids
fn <- "CenPop2010_Mean_ST.txt"
df <- read.csv(paste0(qtd, fn))
str(df)
names(df) <- tolower(names(df))
df$stfips <- zpad(df$statefp, 2)
df
df2 <- merge(df, stcodes, by="stfips")
df2
df2 <- df2 %>% mutate(stname.x=as.character(stname.x), stname.y=as.character(stname.y))
df2 %>%  filter(stname.x!=stname.y)




