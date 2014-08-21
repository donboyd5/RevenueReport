


library(RCurl) 
library(rjson)

qtax <- getdata("qtax")
gdppi <- getdata("gdpq")

qtax <- readRDS(paste0(qtd, "qtaxall.rds"))

# prepare to get NIPA table
p1 <- "http://www.bea.gov/api/data?&UserID="
p2 <- beaapikey

# first some detective work to find out how to get the 
# # get general format of NIPA data
# p3<-"&method=getparameterlist&datasetname=NIPA&"
# u <- paste0(p1, p2, p3)
# tmp <- getURL(u, .opts=curlOptions(followlocation=TRUE))
# (df <- ldply(fromJSON(tmp)$BEAAPI$Results$Parameter, data.frame))
# 
# # get parameters describing each table
# p3 <- "&method=GetParameterValues&datasetname=NIPA&ParameterName=TableID&"
# u <- paste0(p1, p2, p3)
# tmp <- getURL(u, .opts=curlOptions(followlocation=TRUE))
# (df <- ldply(fromJSON(tmp)$BEAAPI$Results$ParamValue, data.frame))
# # TableID 4 is price indexes

# now get the specific desired data
p3<-"&method=GetData&datasetname=NIPA&TableID=4&Frequency=Q&Year=X&ResultFormat=JSON&" # Year=X gets all years
u <- paste0(p1, p2, p3)
tmp <- getURL(u, .opts=curlOptions(followlocation=TRUE))
head(df <- ldply(fromJSON(tmp)$BEAAPI$Results$Data, data.frame))
count(df, c("SeriesCode", "LineDescription"))
ht(df)
gdppi <- df %>% filter(SeriesCode=="B191RG") %>%
  mutate(TimePeriod=as.character(TimePeriod),
         year=as.numeric(substr(TimePeriod, 1, 4)),
         qtr=as.numeric(substr(TimePeriod, 6, 6)),
         gdppi=cton(DataValue),
         date=qy(qtr, year)) %>%
  select(ydate, gdppi)

ht(gdppi)
gdppi$pmult<-with(gdppi, gdppi[which.max(date)]/gdppi)
# gdppi$pmult <- with(gdppi, gdppi[date=="2014-01-01"] / gdppi)
idx <- match(qtax$date, gdppi$date)
qtax$rvalue <- qtax$value * gdppi$pmult[idx]
count(qtax, c("ic", "vname"))

qtax<-arrange(qtax,tax,stabbr,date) # must be sorted by date for ma to be correct
# system.time(qtax2<-ddply(qtax, c("tax","stabbr"), transform, valma4=ma4(value), rvalma4=ma4(rvalue), rvalpcma4=ma4(rvaluepc), .progress="text")) # moving average
system.time(qtax2 <- qtax %>% group_by(tax,stabbr) %.% transform(valma4=ma4(value), rvalma4=ma4(rvalue), rvalpcma4=ma4(rvaluepc)))
# dplyr a bit faster than ddply; transform actually faster than mutate
qtax<-arrange(qtax,tax,stabbr,date)

a <- proc.time()
qtax2 <- qtax %>% group_by(level, stabbr, vname) %>% 
  mutate(valma4=order_by(date, ma4(value)), 
         rvalma4=order_by(date, ma4(rvalue)))
proc.time() - a


ht(qtax2)
saveRDS(qtax2, file=paste0(qtd, "qtaxrma.rds"))


# myts<-function(df, var, freq){ # decompose time series; assume "date" var exists; has minor error handling
#   if(nrow(df) < 2 * freq){
#     dfout<-df
#     return(dfout)
#   }
#   varts <- ts(df[,var], start=c(year(min(df$date)), qtr(min(df$date))), frequency=freq)
#   decomp <- stl(varts, s.window=freq+1, na.action=na.approx) # na.approx replaces missing values with interpolated values  
#   tsr <- data.frame(trend=as.vector(decomp$time.series[, "trend"]), 
#                     seasonal=as.vector(decomp$time.series[, "seasonal"]), 
#                     remainder=as.vector(decomp$time.series[, "remainder"]))
#   if(nrow(tsr) != nrow(df)) { # an alternative would be to pad tsr with NAs
#     tsr<-data.frame(trend = rep(NA, nrow(df)), seasonal = rep(NA, nrow(df)), remainder = rep(NA, nrow(df)))
#   }
#   dfout <- cbind(df,tsr)
#   return(dfout)
# }
# 
# ht(qtax)
# # get an "all other category"
# count(qtax, "vname")
# subset(qtax, stabbr=="AK" & vname=="cit")
# # qt2 <- subset(qtax, level=="sg" & vname %in% c("tottax", "iit", "gst", "cit"))
# qt2 <- qtax %>% filter(level=="sg" & vname %in% c("tottax", "iit", "gst", "cit"))
# ht(qt2)