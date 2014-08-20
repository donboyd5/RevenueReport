# combineqtaxhistapi.r
# Don Boyd
# 8/20/2014

# get qtax data from the Census qtax api
# get qthistory, combine best from both
# adjust property tax
# compute revised totals
# put item code descriptors on file


#****************************************************************************************************
#
#                Directories, constants, and setup ####
#
#****************************************************************************************************



#****************************************************************************************************
#
#                Get codes, history, and combine Census API data with historical data ####
#
#****************************************************************************************************
# df %>% group_by("stabbr","ic") %>% summarise(n=n())

# read both data files, explore which records to keep, conform file formats, and keep unique values that then can be combined
# start with the new data
qtapi <- readRDS(file=paste0(qtd, "qtapiclean.rds"))
ht(qtapi)
# dcast(qtapi, geolev~level) # the slg data are only at the national level; there are no lg data
# dcast(qtapi, stabbr~level) # we have slg data ONLY for the US, not for states
# dcast(subset(qtapi, stabbr=="US"), geolev~level) # the natl data ONLY has US (of course); the state data does have US
# dcast(subset(qtapi, stabbr!="US"), geolev~level) # non-US data is only state level, sg (not lg)
# dcast(subset(qtapi, stabbr=="US" & level=="sg"), date~geolev) # sg US: natl recs start 1992-01-01, state recs start in 1994-01-01
# dcast(subset(qtapi, stabbr=="US" & level=="slg"), date~geolev) # slg US: natl recs start 1992-01-01, no state recs
# dcast(subset(qtapi, stabbr!="US" & level=="sg"), date~geolev) # sg US: state recs start 1994-01-01
# so we have overlapping US recs from 1994-01-01 onward - explore and then delete dups

uvars <- c("level","stabbr","ic","date") # we will want unique observations for these dimensions
anyDuplicated(qtapi[, uvars])

# compare duplicated values
tmp <- dcast(subset(qtapi, stabbr=="US" & level=="sg"), date+ic~geolev)
tmp <- subset(tmp, !(is.na(natl) | is.na(state)) )
tmp$diff <- tmp$natl - tmp$state
tmp$pctdiff <- tmp$diff/tmp$state*100
tmp <- arrange(tmp, -abs(diff))
head(tmp)
# when we have dup values, the differences are truly trivial and I think we can pick one; I'll use the state levels for the Us when there are dups
# because state has data for some itemcodes that are not available for the US, but double-check, first
tmp <- dcast(subset(qtapi, stabbr=="US" & level=="sg"), date+ic~geolev)
tmp2 <- subset(tmp, !is.na(natl) & is.na(state) & date>="1994-01-01") # this is just ic T24T25; appears to be sum of state T24, T25
tmp3 <- subset(tmp, is.na(natl) & !is.na(state) & date>="1994-01-01") # a lot of the lesser-used ics are in the state data but not natl
count(tmp3, c("ic"))
# based on this, I can safely pick the state data when we have dups

idx <- gdata::duplicated2(qtapi[ ,uvars]) # check for dups
qta2 <- qtapi
qta2$dup <- idx*1
qta2 <- subset(qta2, !(dup==1 & geolev=="natl"))
qta2$dup <- NULL
anyDuplicated(qta2[, uvars])
qta2$source <- "qtaxapi"
qta2$geolev <- NULL
count(qta2, "ic")
qta2$ic <- as.character(qta2$ic)
qta2 <- arrange(qta2, stabbr, level, ic, date)
str(qta2)
summary(qta2)
# all done with qtapi data


# now the historical data
qthist <- readRDS(file=paste0(qtd, "qtaxhistory_best.rds"))
ht(qthist)
anyDuplicated(qthist[, uvars]) # no dups
qth2<-qthist
str(qth2)
# all done with qth data

# combine the two unique files, look for dups, favor qtapi
qtall <- rbind(qta2, qth2)
ht(qtall)
anyDuplicated(qtall[, uvars]) # no dups - wow!

# simple checks
dcast(qtall, source~level) # the slg data are only at the national level; there are no lg data
dcast(qtall, stabbr~level) # we have slg data ONLY for the US, not for states
dcast(subset(qtall, stabbr=="US"), date~level) # the natl data ONLY has US (of course); the state data does have US
dcast(qtall, ic~level) # non-US data is only state level, sg (not lg)

# create adjusted property tax and total tax variables - only where needed
dfw <- dcast(subset(qtall, stabbr=="US"), stabbr + ic + date ~ level)
dfw <- subset(dfw, !(is.na(slg) | is.na(sg)))
dfw$lg <- dfw$slg - dfw$sg
summary(dfw)
subset(dfw, lg<0) # this seems odd; get rid of negative local tax data
dfw <- subset(dfw, lg>=0)
ht(dfw)
dflcl <- subset(dfw, select=c(stabbr, ic, date, lg))
dflcl$level <- "lg"
dflcl$source <- "calc"
dflcl <- gdata::rename.vars(dflcl, c("lg"), c("value"))
ht(dflcl)

qtall2<-rbind(qtall, dflcl)

# now adjust property tax and total tax
lastoldsampdate <- as.Date("2008-07-01")
# get the reported prop tax amount and set it aside. unfortunately we don't have lg before 1988, although we do
# have slg - eventually I could estimate what local was relative to state-local and adjust that or better still
# do it based on slg total minus sum of sg prop tax
proptaxadj <- subset(qtall2, ic=="T01" & level=="lg" & stabbr=="US" & date<=lastoldsampdate, select=c(date,value)) # calc adjust from this
proptaxadj$lgptadjust <- proptaxadj$value * 0.077 # based on my analysis of Census Bureau's bridge study
# lgptadjust is the amount to add to reported property and total tax

# get all the values for which we'll want to create adjusted versions
vta <- subset(qtall2, stabbr=="US" & ic %in% c("T01", "TOTAL") & level %in% c("slg", "lg")) # values to adjust
head(vta); head(proptaxadj)
vta <- merge(vta, proptaxadj[,c("date","lgptadjust")], all=TRUE)
vta$ic<-paste0(vta$ic, "adj")
vta$value <- vta$value + naz(vta$lgptadjust)
vta$lgptadjust <- NULL
ht(vta)

qtall3<-rbind(qtall2, vta)

# put vname and vdesc on the file
qtc <- read.xls(paste0(qtdrop, "qtaxitemcodes.xlsx"), sheet="qtaxitemcodes", colClasses="character") # get codes
idx<-match(qtall3$ic, qtc$ic)
qtall3[, c("vname","vdesc")] <- qtc[idx, c("vname","vdesc")]
ht(qtall3)
count(qtall3, c("ic", "vname", "vdesc"))
qtall<-arrange(qtall, stabbr, level, ic, date)
saveRDS(qtall3, file=paste0(qtd, "qtaxall.rds"))
################# All done creating complete qtax file ##################################





