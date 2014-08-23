# findstateneighbors.r
# Don Boyd
# 8/21/2014

# https://www.census.gov/geo/reference/county-adjacency.html

# https://github.com/ubikuity/List-of-neighboring-states-for-each-US-state
# from ubikuity
fn <- "neighbors-states.csv"
df <- read.csv(paste0(qtd, fn), sep=";")
str(df)
names(df) <- c("stabbr", "nstabbr")
df[, 1:2] <- colwise(as.character) (df[, 1:2])
str(df)

# this is odd, he does not have duplicates
st <- "RI"
filter(df, stabbr==st)
filter(df, nstabbr==st)

df2 <- df[, c("nstabbr", "stabbr")]
names(df2) <- c("stabbr", "nstabbr")
df3 <- rbind(df, df2)
df3 <- unique(df3)

st <- "CO"
filter(df3, stabbr==st)
filter(df3, nstabbr==st)

saveRDS(df3, paste0(qtd, "stateneighbors.rds"))
str(df3)


# ubase <- "https://www.census.gov/geo/reference/docs/"
# fn <- "county_adjacency.txt"
# 
# download.file(paste0(ubase, fn), paste0(qtd, fn))
# 
# data()
# db <- data()
# state.abb (state)                    US State Facts and Figures
# state.area (state)                   US State Facts and Figures
# state.center (state)                 US State Facts and Figures
# state.division (state)               US State Facts and Figures
# state.name (state)                   US State Facts and Figures
# state.region (state)                 US State Facts and Figures
# state.x77 (state)                    US State Facts and Figures

# state centroids
# fn <- "CenPop2010_Mean_ST.txt"
# df <- read.csv(paste0(qtd, fn))
# str(df)
# names(df) <- tolower(names(df))
# df$stfips <- zpad(df$statefp, 2)
# df
# df2 <- merge(df, stcodes, by="stfips")
# df2
# df2 <- df2 %>% mutate(stname.x=as.character(stname.x), stname.y=as.character(stname.y))
# df2 %>%  filter(stname.x!=stname.y)




