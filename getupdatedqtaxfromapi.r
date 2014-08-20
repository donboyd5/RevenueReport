# getupdatedqtaxfromapi.r
# Don Boyd
# 8/20/2014

# http://niaaa.census.gov/data.html
# http://api.census.gov/data/eits/eits_program_code_QTAX.xml
# http://www.census.gov/developers/


#****************************************************************************************************
#
#                RUN AS NEEDED: Get latest version of 1992+ from Census API ####
#
#****************************************************************************************************
# get qtax - EVERYTHING from 1992 forward; eventually figure out how to just get recent
# censusapikey<-"b27cb41e46ffe3488af186dd80c64dce66bd5e87"
pre <- "http://api.census.gov/data/eits/qtax?key="
post <- "&get=cell_value,time_slot_name,seasonally_adj,geo_level_code,category_code,data_type_code,error_data"
url <- paste0(pre, censusapikey, post) # note that a censusapikey is needed
system.time(qtdat <- jsonlite::fromJSON(url)) # return results as a matrix # about 40-50 secs

# save the matrix as a permanent file
fn <- paste0("qtfromapi_", Sys.Date(), ".rds") # format(Sys.Date(), "%d%b%Y")
saveRDS(qtdat, paste0(qtd, fn))
str(qtdat)


#****************************************************************************************************
#
#                Clean and save Census API data ####
#
#****************************************************************************************************
# define filename of latest raw Census API data or use the fn in memory from step above
# fn <- paste0("qtfromapi_",format(Sys.Date(),"%d%b%Y"),".rds")
# fn <- "qtfromapi_03Jul-2014.rds"
# fn <- "qtfromapi_2014-07-21.rds"

qtdat <- readRDS(paste0(qtd, fn))
str(qtdat)
df <- data.frame(qtdat)
ht(df)
names(df) <- t(df[1, ])
df <- df[-1, ]
str(df)
ht(df)
count(df, "time_slot_name")
count(df, "seasonally_adj")
count(df, "geo_level_code") # includes US and DC
count(df, "category_code")
count(df, "error_data")
count(df, "data_type_code")

df$date <- qy(as.numeric(substr(df$time_slot_name, 2, 2)), as.numeric(substr(df$time_slot_name, 3, 6)))
subset(df, date==max(df$date) & geo_level_code=="US") # look at latest data, for US
count(df, c("date", "time_slot_name")) # in CY 1992 and 1993 we only have 40 obs each
subset(df, date=="1992-01-01") # we only have US totals by tax type

# states do not begin until 1994q1
# dcast(df, list(.(date), .(geo_level_code=="US")))
# do the same thing with dplyr geo_level_code=="US"
df %>% group_by(date, usrec=geo_level_code=="US") %>%
  summarise(n=n()) %>%
  mutate(usrec=ifelse(usrec, "US", "notUS")) %>%
  spread(usrec, n)

df$time_slot_name <- NULL
df$seasonally_adj <- NULL
df$error_data <- NULL
df <- gdata::rename.vars(df, c("geo_level_code","data_type_code"), c("stabbr","ic"))
df$stabbr <- as.character(df$stabbr)
df$value <- cton(df$cell_value)
df$cell_value <- NULL
# get rid of unnecessary records
df <- subset(df, !(grepl("4QE", ic) | ic=="T24254")) # all of the 4QEs are 4-quarter ending sums, as is T24254; KEEP T24T25, which is combined mvfee, lic
df <- subset(df, ic!="ALLOTH") # we'll compute any "all other" categories that we want

# QTAXCAT1" start="1992-Q1" end="2013-Q2" description="Table 1 - Latest National Totals of State and Local Tax Revenue
# QTAXCAT2" start="1992-Q1" end="2013-Q2" description="Table 2 - Latest National Totals of State Tax Revenue
# QTAXCAT3" start="1994-Q1" end="2013-Q2" description="Table 3 - Latest State Tax Collections by State and Type of Tax
df$geolev <- ifelse(df$category_code %in% c("QTAXCAT1","QTAXCAT2"), "natl", ifelse(df$category_code=="QTAXCAT3", "state", "BAD"))
count(df, "geolev")
df$level[df$category_code=="QTAXCAT1"] <- "slg"
df$level[df$category_code=="QTAXCAT2"] <- "sg"
df$level[df$category_code=="QTAXCAT3"] <- "sg"
uvars <- c("geolev","level","stabbr","ic","date") # we want unique records for these categories combined (only 1 rec per combination)
anyDuplicated(df[, uvars])
df$source <- "QTAX_API"
df$category_code <- NULL
ht(df)
saveRDS(df, paste0(qtd, "qtapiclean.rds"))