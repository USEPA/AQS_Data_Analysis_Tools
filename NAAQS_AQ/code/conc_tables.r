setwd("C:/Users/bwells01/Documents/NAAQS_AQ/")
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(plyr); library(reshape2);
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)

## Functions used to generate table summaries
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
get.quarter <- function(d) { 
  q <- (as.integer(substr(d,6,7)) - 1) %/% 3 + 1
  qc <- paste(q,ifelse(q == 1,"st",ifelse(q == 2,"nd",ifelse(q == 3,"rd","th"))),sep="")
  return(paste(qc,"quarter"))
}
get.region <- function(id) {
  return(switch(paste("s",substr(id,1,2),sep=""),
    s01="Southeast",s02="Northwest",s04="Southwest",s05="South",s06="West",s08="Southwest",
    s09="Northeast",s10="Northeast",s11="Northeast",s12="Southeast",s13="Southeast",s15="West",
    s16="Northwest",s17="Central",s18="Central",s19="East North Central",s20="South",s21="Central",
    s22="South",s23="Northeast",s24="Northeast",s25="Northeast",s26="East North Central",
    s27="East North Central",s28="South",s29="Central",s30="West North Central",
    s31="West North Central",s32="West",s33="Northeast",s34="Northeast",s35="Southwest",
    s36="Northeast",s37="Southeast",s38="West North Central",s39="Central",s40="South",
    s41="Northwest",s42="Northeast",s44="Northeast",s45="Southeast",s46="West North Central",
    s47="Southeast",s48="South",s49="Southwest",s50="Northeast",s51="Southeast",s53="Northwest",
    s54="Central",s55="East North Central",s56="West North Central",s72="Southeast",NA))
}
get.season <- function(d) {
  return(switch(paste("m",substr(d,6,7),sep=""),
    m01="winter",m02="winter",m03="spring",m04="spring",m05="spring",m06="summer",
    m07="summer",m08="summer",m09="autumn",m10="autumn",m11="autumn",m12="winter"))
}
get.stats <- function(df,metric,region,season,digits=0) {
  df$val <- df[,metric]
  if (region != "all") { df <- df[which(df$region == region),] }
  if (season != "all") { df <- df[which(df$season == season),] }
  pct <- ddply(df,"site",summarize,pct=round(100*sum(!is.na(val))/length(val)))
  sites.keep <- pct$site[which(pct$pct >= 75)]
  x <- df$val[which(df$site %in% sites.keep)]
  q <- round(quantile(x,probs=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.98,0.99,1),na.rm=TRUE),digits)
  names(q) <- c("min",paste("p",c(1,5,10,25,50,75,90,95,98,99),sep=""),"max")
  return(data.frame(metric,region,season,N.sites=length(sites.keep),
    N.obs=sum(!is.na(x)),mean=round(mean(x,na.rm=TRUE),digits),SD=round(sd(x,na.rm=TRUE),digits),
    t(q),max.site=df$site[which.max(df$val)]))
}

########################################################################
## CO: Calculate summary statistics based on MDA1, MDA8 and DA24 metrics
########################################################################
load(paste("data/",curr.year,"/COdaily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(temp,c("site","date"),summarize,MDA1=max.na(conc.mda1),
  MDA8=max.na(conc.mda8),DA24=max.na(conc))
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])
co.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",digits=1),
  get.stats(df=daily,metric="MDA8",region="all",season="all",digits=1),
  get.stats(df=daily,metric="MDA8",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="MDA8",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="MDA8",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="MDA8",region="all",season="autumn",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",digits=1))

########################################################################################################
## Lead: Calculate summary statistics based on daily samples, monthly averages, 3-month rolling averages
########################################################################################################
load(paste("data/",curr.year,"/Pbdaily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(pb.daily,c("site","date"),summarize,conc=max.na(conc))
temp$season <- sapply(temp$date,get.quarter)
temp$month <- substr(temp$date,1,7)
daily <- subset(temp,!is.na(conc))
monthly <- ddply(daily,c("site","month"),summarize,conc=mean(conc),conc.3mo=NA,season=season[1])
for (i in 3:nrow(monthly)) {
  if (monthly$site[i] != monthly$site[i-2]) { next }
  monthly$conc.3mo[i] <- mean(monthly$conc[(i-2):i])
}
pb.table1 <- rbind(
  get.stats(df=daily,metric="conc",region="all",season="all",digits=3),
  get.stats(df=daily,metric="conc",region="all",season="1st quarter",digits=3),
  get.stats(df=daily,metric="conc",region="all",season="2nd quarter",digits=3),
  get.stats(df=daily,metric="conc",region="all",season="3rd quarter",digits=3),
  get.stats(df=daily,metric="conc",region="all",season="4th quarter",digits=3),
  get.stats(df=monthly,metric="conc",region="all",season="all",digits=3),
  get.stats(df=monthly,metric="conc",region="all",season="1st quarter",digits=3),
  get.stats(df=monthly,metric="conc",region="all",season="2nd quarter",digits=3),
  get.stats(df=monthly,metric="conc",region="all",season="3rd quarter",digits=3),
  get.stats(df=monthly,metric="conc",region="all",season="4th quarter",digits=3),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="all",digits=3),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="1st quarter",digits=3),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="2nd quarter",digits=3),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="3rd quarter",digits=3),
  get.stats(df=monthly,metric="conc.3mo",region="all",season="4th quarter",digits=3))
pb.table1$metric <- rep(c("daily","monthly","3-month"),each=5)
colnames(pb.table1)[3] <- "quarter"

###################################################################
## NO2: Calculate summary statistics based on MDA1 and DA24 metrics
###################################################################
load(paste("data/",curr.year,"/NO2daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(no2.daily,c("site","date"),summarize,MDA1=max.na(conc.max),DA24=max.na(conc.mean))
temp$region <- sapply(temp$site,get.region)
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA1))/length(MDA1)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Daily 1-hr max and 24=hr mean aggregated by season 
no2.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",digits=1))

## Table 2: Daily 1-hr max and 24=hr mean aggregated by NOAA climate region
no2.table2 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="Central",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="East North Central",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="Northeast",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="Northwest",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="South",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="Southeast",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="Southwest",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="West",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="West North Central",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="Central",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="East North Central",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="Northeast",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="Northwest",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="South",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="Southeast",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="Southwest",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="West",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="West North Central",season="all",digits=1))

#######################################################################################
## Ozone: Calculate summary statistics based on 'year-round' and 'warm season' datasets
#######################################################################################
load(paste("data/",curr.year,"/O3daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- subset(o3.daily,as.numeric(substr(site,1,2)) <= 56)
temp <- ddply(temp,c("site","date"),summarize,MDA1=max.na(conc.mda1),
  MDA8=max.na(conc.mda8),DA24=max.na(conc.mean))
temp$region <- sapply(temp$site,get.region)
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily.yr <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])

## Table 1: Year-round dataset aggregated by season
o3.table1 <- rbind(
  get.stats(df=daily.yr,metric="MDA1",region="all",season="all"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="winter"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="spring"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="summer"),
  get.stats(df=daily.yr,metric="MDA1",region="all",season="autumn"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="all"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="winter"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="spring"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="summer"),
  get.stats(df=daily.yr,metric="MDA8",region="all",season="autumn"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="all"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="winter"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="spring"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="summer"),
  get.stats(df=daily.yr,metric="DA24",region="all",season="autumn"))

## Table 2: Warm season dataset aggregated by NOAA climate region
temp <- subset(temp,as.numeric(substr(date,6,7)) %in% c(5:9))
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA8))/length(MDA8)))
daily.ws <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])
o3.table2 <- rbind(
  get.stats(df=daily.ws,metric="MDA1",region="all",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="Central",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="East North Central",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="Northeast",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="Northwest",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="South",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="Southeast",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="Southwest",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="West",season="all"),
  get.stats(df=daily.ws,metric="MDA1",region="West North Central",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="all",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="Central",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="East North Central",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="Northeast",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="Northwest",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="South",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="Southeast",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="Southwest",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="West",season="all"),
  get.stats(df=daily.ws,metric="MDA8",region="West North Central",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="all",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="Central",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="East North Central",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="Northeast",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="Northwest",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="South",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="Southeast",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="Southwest",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="West",season="all"),
  get.stats(df=daily.ws,metric="DA24",region="West North Central",season="all"))

###############################################################################
## PM: Calculate summary statistics for PM10, PM2.5, PM10-2.5 and PM2.5 species
###############################################################################
load(paste("data/",curr.year,"/PM10daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("data/",curr.year,"/PM25daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("data/",curr.year,"/PM10_25daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("data/",curr.year,"/PM25spec_daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
pm10.mean <- na.omit(ddply(pm10.daily,c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm10.mda1 <- na.omit(ddply(pm10.dmax,c("site","date"),summarize,MDA1=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm25.mean <- na.omit(ddply(pm25.daily,c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm25.mda1 <- na.omit(ddply(pm25.dmax,c("site","date"),summarize,MDA1=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1])))
pm10_25 <- ddply(pm10_25.daily,c("site","date"),summarize,DA24=max.na(conc),
  region=get.region(site[1]),season=get.quarter(date[1]))
pm25.spec <- ddply(pm25.spec.daily,c("site","date"),summarize,SO4=max.na(so4),
  NO3=max.na(no3),EC=max.na(ec),OC=max.na(oc),Crustal=max.na(crustal),
  Sea_Salt=max.na(seasalt),region=get.region(site[1]),season=get.quarter(date[1]))

## Table 1: PM10, PM2.5, and PM10-2.5 stats by quarter
pm.table1 <- rbind(
  get.stats(df=pm10.mean,metric="DA24",region="all",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="1st quarter",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="2nd quarter",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="3rd quarter",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="all",season="4th quarter",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="1st quarter",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="2nd quarter",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="3rd quarter",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="4th quarter",digits=0),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="1st quarter",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="2nd quarter",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="3rd quarter",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="4th quarter",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="1st quarter",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="2nd quarter",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="3rd quarter",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="4th quarter",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="1st quarter",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="2nd quarter",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="3rd quarter",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="4th quarter",digits=1))
pm.table1$region <- rep(c("PM10","PM10","PM2.5","PM2.5","PM10-2.5"),each=5)
colnames(pm.table1)[2:3] <- c("pollutant","quarter")
pm.table1 <- pm.table1[,c(2,1,3:ncol(pm.table1))]

## Table 2: PM10, PM2.5, and PM10-2.5 stats by NOAA climate region
pm.table2 <- rbind(
  get.stats(df=pm10.mean,metric="DA24",region="all",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="Central",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="East North Central",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="Northeast",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="Northwest",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="South",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="Southeast",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="Southwest",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="West",season="all",digits=0),
  get.stats(df=pm10.mean,metric="DA24",region="West North Central",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="all",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="Central",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="East North Central",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="Northeast",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="Northwest",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="South",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="Southeast",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="Southwest",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="West",season="all",digits=0),
  get.stats(df=pm10.mda1,metric="MDA1",region="West North Central",season="all",digits=0),
  get.stats(df=pm25.mean,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="Central",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="East North Central",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="Northeast",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="Northwest",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="South",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="Southeast",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="Southwest",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="West",season="all",digits=1),
  get.stats(df=pm25.mean,metric="DA24",region="West North Central",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="Central",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="East North Central",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="Northeast",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="Northwest",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="South",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="Southeast",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="Southwest",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="West",season="all",digits=1),
  get.stats(df=pm25.mda1,metric="MDA1",region="West North Central",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="Central",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="East North Central",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="Northeast",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="Northwest",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="South",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="Southeast",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="Southwest",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="West",season="all",digits=1),
  get.stats(df=pm10_25,metric="DA24",region="West North Central",season="all",digits=1))
pm.table2$season <- rep(c("PM10","PM10","PM2.5","PM2.5","PM10-2.5"),each=10)
colnames(pm.table2)[2:3] <- c("region","pollutant")
pm.table2 <- pm.table2[,c(3,1,2,4:ncol(pm.table2))]

## Table 3: PM2.5 species stats by quarter
pm.table3 <- rbind(
  get.stats(df=pm25.spec,metric="SO4",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="all",season="4th quarter",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="4th quarter",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="4th quarter",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="4th quarter",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="4th quarter",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="1st quarter",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="2nd quarter",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="3rd quarter",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="4th quarter",digits=2))
colnames(pm.table3)[1:3] <- c("species","region","quarter")

## Table 4: PM2.5 species stats by NOAA climate region
pm.table4 <- rbind(
  get.stats(df=pm25.spec,metric="SO4",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="SO4",region="West North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="NO3",region="West North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="EC",region="West North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="OC",region="West North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Crustal",region="West North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="all",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="East North Central",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Northeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Northwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="South",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Southeast",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="Southwest",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="West",season="all",digits=2),
  get.stats(df=pm25.spec,metric="Sea_Salt",region="West North Central",season="all",digits=2))
colnames(pm.table4)[1] <- "species"

###################################################################
## SO2: Calculate summary statistics based on MDA1 and DA24 metrics
###################################################################
load(paste("data/",curr.year,"/SO2daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
temp <- ddply(so2.daily,c("site","date"),summarize,MDA1=max.na(conc.max),DA24=max.na(conc.mean))
temp$season <- sapply(temp$date,get.season)
pct <- ddply(temp,"site",summarize,pct=round(100*sum(!is.na(MDA1))/length(MDA1)))
daily <- subset(temp,site %in% pct$site[which(pct$pct >= 75)])
so2.table1 <- rbind(
  get.stats(df=daily,metric="MDA1",region="all",season="all",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="MDA1",region="all",season="autumn",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="all",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="winter",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="spring",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="summer",digits=1),
  get.stats(df=daily,metric="DA24",region="all",season="autumn",digits=1))
save(list=ls(pattern="table"),
  file=paste("data/",curr.year,"/conctables",curr.year-2,"_",curr.year,".Rdata",sep=""))

