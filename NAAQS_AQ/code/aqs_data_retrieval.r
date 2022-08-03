#########################################################
## Retrieve AQS Data for NAAQS AQ graphics - run on atmos
#########################################################
if (dir.exists("/home/")) { source("/home/bwells01/R/get_monitors.r") }
if (dir.exists("C:/")) { source("C:/Users/bwells01/Documents/R/get_monitors.r") }
options(stringsAsFactors=FALSE); library(plyr);
aqs.tables <- unlist(get.aqs.data("SELECT table_name FROM all_tables WHERE owner = 'AIRSRAQS'"))
aqs.views <- unlist(get.aqs.data("SELECT view_name FROM all_views WHERE owner IN ('AIRSRAQS','AQSPUB')"))
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
if (!dir.exists(paste("NAAQS_AQ/data",curr.year,sep="/"))) { dir.create(paste("NAAQS_AQ/data",curr.year,sep="/")) }
pmax.na <- function(x,y) { ifelse(is.na(x) & is.na(y),NA,pmax(x,y,na.rm=TRUE)) }
merge.all <- function(x,y) { merge(x,y,all=TRUE) }

## Retrieve monitor metadata for most recent 3-year period for monitor maps
co.monitors <- get.monitors(par=42101,yr1=curr.year-2,yr2=curr.year)
no2.monitors <- get.monitors(par=42602,yr1=curr.year-2,yr2=curr.year)
o3.monitors <- get.monitors(par=44201,yr1=curr.year-2,yr2=curr.year)
so2.monitors <- get.monitors(par=42401,yr1=curr.year-2,yr2=curr.year)
pm10.monitors <- get.monitors(par=81102,yr1=curr.year-2,yr2=curr.year)
pm25.monitors <- get.monitors(par=88101,yr1=curr.year-2,yr2=curr.year)
t1 <- get.monitors(par=12128,yr1=curr.year-2,yr2=curr.year); t1$parameter <- 12128;
t2 <- get.monitors(par=14129,yr1=curr.year-2,yr2=curr.year); t2$parameter <- 14129;
t3 <- get.monitors(par=85129,yr1=curr.year-2,yr2=curr.year); t3$parameter <- 85129;
pb.monitors <- rbind(t1,t2,t3)
save(list=paste(c("co","no2","o3","so2","pm10","pm25","pb"),"monitors",sep="."),
  file=paste("NAAQS_AQ/data/",curr.year,"/monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))

## Get non-regulatory (e.g. CSN, IMPROVE) monitors operating during most recent 3-year period
pb.pm25.monitors <- get.monitors(par=88128,yr1=curr.year-2,yr2=curr.year)
pm10_25.monitors <- get.monitors(par=86101,yr1=curr.year-2,yr2=curr.year)
no3 <- get.monitors(par=88306,yr1=curr.year-2,yr2=curr.year); no3$parameter <- 88306;
oc <- get.monitors(par=88320,yr1=curr.year-2,yr2=curr.year); oc$parameter <- 88320;
ec <- get.monitors(par=88321,yr1=curr.year-2,yr2=curr.year); ec$parameter <- 88321;
so4 <- get.monitors(par=88403,yr1=curr.year-2,yr2=curr.year); so4$parameter <- 88403;
pm25.spec.monitors <- rbind(no3,oc,ec,so4)
save(list=paste(c("pb.pm25","pm10_25","pm25.spec"),"monitors",sep="."),
  file=paste("NAAQS_AQ/data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))

##################################################
## Carbon Monoxide (CO) AQS design value retrieval
##################################################
co.views <- c(aqs.tables[grep("CO_",aqs.tables)],aqs.views[grep("CO_",aqs.views)])
co.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_CO_DVS'"))
co.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         poc AS poc, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         co_1hr_2nd_max_value AS dv_1hr,
         co_8hr_2nd_max_value AS dv_8hr
    FROM EUV_CO_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42101'
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
save(co.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/COdvs2000_",curr.year,".Rdata",sep=""))

#######################################
## Lead (Pb) AQS design value retrieval
#######################################
pb.views <- c(aqs.tables[grep("LEAD_",aqs.tables)],aqs.views[grep("LEAD_",aqs.views)])
pb.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_LEAD_DVS'"))
pb.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv,
         dv_validity_ind AS valid_dv,
         dv_year_max_value AS max_ann,
         dv_year_valid_months AS valid_ann
    FROM EUV_LEAD_DVS
   WHERE dv_year >= 2010 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '14129'
     AND pollutant_standard_id = 2
     AND dv_year_max_value IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(pb.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PBdvs2010_",curr.year,".Rdata",sep=""))

####################################################
## Nitrogen Dioxide (NO2) AQS design value retrieval
####################################################
no2.views <- c(aqs.tables[grep("NO2",aqs.tables)],aqs.views[grep("NO2",aqs.views)])
no2.ann.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_NO2_ANNUAL_DVS'"))
no2.1hr.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_NO2_1HOUR_DVS'"))
no2.ann.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv_ann,
         observation_percent AS pct_ann,
         dv_validity_indicator AS valid_ann
    FROM EUV_NO2_ANNUAL_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42602'
     AND pollutant_standard_id = 8
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
no2.1hr.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         dv_year AS year,
         design_value AS dv_1hr,
         year_0_98th_percentile AS p98_1hr,
         dv_validity_indicator AS valid_1hr
    FROM EUV_NO2_1HOUR_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42602'
     AND year_0_98th_percentile IS NOT NULL
     AND pollutant_standard_id = 20
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
no2.dvs <- merge(no2.ann.dvs,no2.1hr.dvs,by=c("site","year"),all=TRUE)
save(no2.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/NO2dvs2000_",curr.year,".Rdata",sep=""))

########################################
## Ozone (O3) AQS design value retrieval
########################################
o3.views <- c(aqs.tables[grep("OZONE",aqs.tables)],aqs.views[grep("OZONE",aqs.views)])
o3.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_OZONE_DVS'"))
o3.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         FLOOR(design_value*1000) AS dv_8hr,
         dv_3_yr_percent_complete AS pct_3yr_8hr,
         dv_validity_indicator AS valid_8hr,
         FLOOR(dv_year_4th_max) AS max4_8hr,
         dv_year_percent_complete AS pct_ann_8hr
    FROM EUV_OZONE_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '44201'
     AND standard_id = 23
     AND dv_year_4th_max IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(o3.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/O3dvs2000_",curr.year,".Rdata",sep=""))

#################################################
## Particulates (PM10) AQS design value retrieval
#################################################
pm10.views <- c(aqs.tables[grep("PM10",aqs.tables)],aqs.views[grep("PM10",aqs.views)])
pm10.conc.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'PM10_MON_DESIGN_CONCENTRATIONS'"))
pm10.exc.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_PM10_DVS'"))
pm10.dv.conc <- get.aqs.data(paste("
  SELECT state_code || county_code || site_id AS site,
         poc AS poc,
         dv_year AS year,
         design_concentration AS dv_conc
    FROM PM10_MON_DESIGN_CONCENTRATIONS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND design_concentration IS NOT NULL
     AND parameter_code = '81102'
     AND pollutant_standard_id = 12
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
pm10.dv.exc <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site,
         poc AS poc,
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         dv_estimated_exceedances AS dv_exc,
         dv_validity_indicator AS dv_valid,
         dv_year_complete_quarters AS comp_qtrs
    FROM EUV_PM10_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '81102'
     AND pollutant_standard_id = 12
     AND dv_year_exceedance_count IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, poc, year"))
pm10.dvs <- merge(pm10.dv.exc,pm10.dv.conc,by=c("site","poc","year"))[,c(1,2,4:10,3,11:14)]
save(pm10.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PM10dvs2000_",curr.year,".Rdata",sep=""))

#######################################################
## Fine Particulates (PM2.5) AQS design value retrieval
#######################################################
pm25.views <- c(aqs.tables[grep("PM25",aqs.tables)],aqs.views[grep("PM25",aqs.views)])
pm25.ann.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_PM25_ANNUAL_DVS'"))
pm25.24h.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_PM25_24HR_DVS'"))
pm25.ann.dv <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         dv_year AS year,
         design_value AS dv_ann,
         dv_validity_ind AS dv_ann_valid,
         dv_year_arith_mean AS ann_mean,
         dv_year_mean_validity_ind AS ann_mean_valid
    FROM EUV_PM25_ANNUAL_DVS
   WHERE dv_year >= 2002 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '88101'
     AND pollutant_standard_id = 22
     AND dv_year_arith_mean IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
pm25.24h.dv <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site,
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         daily_design_value AS dv_24h,
         dv_validity_ind AS dv_24h_valid,
         dv_year_98th_percentile AS ann_p98,
         dv_year_98th_validity_ind AS ann_p98_valid
    FROM EUV_PM25_24HR_DVS
   WHERE dv_year >= 2002 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '88101'
     AND pollutant_standard_id = 21
     AND dv_year_98th_percentile IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
pm25.dvs <- merge(pm25.24h.dv,pm25.ann.dv,by=c("site","year"),all=TRUE)[,c(1,3:9,2,10:17)]
save(pm25.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/PM25dvs2002_",curr.year,".Rdata",sep=""))

##################################################
## Sulfur Dioxide (SO2) AQS design value retrieval
##################################################
so2.views <- c(aqs.tables[grep("SO2",aqs.tables)],aqs.views[grep("SO2",aqs.views)])
so2.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'EUV_SO2_DVS'"))
so2.dvs <- get.aqs.data(paste("
  SELECT state_code || county_code || site_number AS site, 
         local_site_name AS site_name,
         address AS address,
         latitude AS latitude,
         longitude AS longitude,
         state_name AS state_name,
         county_name AS county_name,
         cbsa_name AS cbsa_name,
         dv_year AS year,
         design_value AS dv,
         dv_validity_indicator AS dv_valid,
         year_0_99th_percentile AS p99,
         year_0_complete_ind AS p99_valid
    FROM EUV_SO2_DVS
   WHERE dv_year >= 2000 AND dv_year <=",curr.year,"
     AND edt_id IN (0,5)
     AND latitude IS NOT NULL
     AND parameter_code = '42401'
     AND pollutant_standard_id = 19
     AND year_0_99th_percentile IS NOT NULL
     AND state_code NOT IN ('80','CC')
ORDER BY site, year"))
save(so2.dvs,file=paste("NAAQS_AQ/data/",curr.year,"/SO2dvs2000_",curr.year,".Rdata",sep=""))

###################################################
## AQS hourly data retrieval for gaseous pollutants
###################################################
hourly.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'RAW_DATA_CONCURRENCES'"))
get.hourly.data <- function(par,year) {
  poll.name <- switch(paste("p",par,sep=""),p14129="pb",p42101="co",
    p42401="so2",p42602="no2",p44201="o3",p81102="pm10",p88101="pm25")
  dt.begin <- paste(year,"01-01 00:00:00",sep="-")
  dt.end <- paste(year,"12-31 23:00:00",sep="-")
  aqs.data <- subset(get.aqs.data(paste("SELECT DISTINCT
          rd.state_code || rd.county_code || rd.site_id || rd.poc AS id,
          TO_CHAR(rd.sampling_begin_datetime,'YYYY-MM-DD HH24:MI:SS') AS dt,
          GREATEST(rd.standard_sample_value,0) AS ",poll.name,",
          rd.method_code AS method,
          COALESCE(rd.event_code || rd.null_data_code,' ') AS flag,
          COALESCE(rd.event_concurence_indicator || rd.null_code_concurrence,' ') AS concur
     FROM raw_data_concurrences rd
    WHERE rd.duration_code = '1'
      AND rd.parameter_code = '",par,"'
      AND rd.sampling_begin_datetime >= TO_DATE('",dt.begin,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.sampling_begin_datetime <= TO_DATE('",dt.end,"','YYYY-MM-DD HH24:MI:SS')
      AND rd.state_code NOT IN ('80','CC')
    ORDER BY 1,2",sep="")),substr(dt,15,19) == "00:00")
  if (par == 14129) { aqs.data$pb <- floor(100*aqs.data$pb)/100 }
  if (par == 42101) { aqs.data$co <- floor(10*aqs.data$co)/10 }
  if (par == 42401) { aqs.data$so2 <- floor(10*aqs.data$so2)/10 }
  if (par == 42602) { aqs.data$no2 <- floor(10*aqs.data$no2)/10 }
  if (par == 44201) { aqs.data$o3 <- floor(aqs.data$o3*1000) }
  if (par == 81102) { aqs.data$pm10 <- floor(10*aqs.data$pm10)/10 }
  if (par == 88101) { aqs.data$pm25 <- floor(10*aqs.data$pm25)/10 }
  all.ids <- as.character(unique(aqs.data$id))
  all.dts <- unique(aqs.data$dt)[order(unique(aqs.data$dt))]
  all.hrs <- data.frame(id=rep(all.ids,each=length(all.dts)),
                        dt=rep(all.dts,times=length(all.ids)))
  out <- merge(all.hrs,aqs.data,all.x=TRUE,all.y=FALSE)
  out$method <- replace(out$method,which(is.na(out$method))," ")
  out$flag <- replace(out$flag,which(is.na(out$flag))," ")
  out$concur <- replace(out$concur,which(is.na(out$concur))," ")
  assign(poll.name,out)
  file.out <- paste("NAAQS_AQ/data/",curr.year,"/",toupper(poll.name),"hourly",year,".Rdata",sep="")
  save(list=poll.name,file=file.out)
}
for (p in c('42101','42401','42602','44201')) {
  for (y in c((curr.year-2):curr.year)) {
    get.hourly.data(par=p,year=y)
    cat(p,y,as.character(Sys.time()),"\n")
  }
}

###############################################################
## AQS daily summary data retrieval for all criteria pollutants
###############################################################
daily.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'DAILY_SUMMARIES'"))
get.daily.data <- function(par,psid,dur,stat,years) {
  ds.stat <- switch(stat,max="ds.daily_max_sample_value",mean="ds.daily_arith_mean")
  ny <- length(years)
  begin.date <- paste(years[1],"01-01",sep="-")
  end.date <- paste(years[ny],"12-31",sep="-")
  vals <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id || mo.poc AS id,
         ds.daily_coll_date AS dt,
         ",ds.stat," AS conc
    FROM daily_summaries ds,
         monitors mo,
         site_basic si
   WHERE ds.daily_coll_date >= TO_DATE('",begin.date,"','YYYY-MM-DD')
     AND ds.daily_coll_date <= TO_DATE('",end.date,"','YYYY-MM-DD')
     AND ds.edt_edt_id IN (0,2)
     AND ds.mo_mo_id = mo.mo_id
     AND ds.parameter_code = '",par,"'
     AND ds.pollutant_standard_id = ",psid,"
     AND ds.sd_duration_code = '",dur,"'
     AND mo.pa_parameter_code = '",par,"'
     AND mo.si_si_id = si.si_id
     AND si.state_code NOT IN ('80','CC')
   ORDER BY 1,2",sep=""))
   all <- data.frame(id=rep(unique(vals$id),each=length(unique(vals$dt))),
     dt=rep(unique(vals$dt),times=length(unique(vals$id))))
   data <- merge(all,vals,by=c("id","dt"),all=TRUE)
   daily <- data.frame(site=substr(data$id,1,9),poc=substr(data$id,10,10),
     date=as.Date(data$dt),conc=data$conc)
   return(daily)
}
curr.3yr <- c((curr.year-2):curr.year)
file.yrs <- paste(curr.3yr[1],curr.3yr[3],sep="_")
pb.daily <- get.daily.data(par="14129",psid=2,dur="7",stat="mean",years=curr.3yr)
save(pb.daily,file=paste("NAAQS_AQ/data/",curr.year,"/PBdaily",file.yrs,".Rdata",sep=""))
co.mda1 <- get.daily.data(par="42101",psid=3,dur="1",stat="max",years=curr.3yr)
co.mda8 <- get.daily.data(par="42101",psid=4,dur="Z",stat="max",years=curr.3yr)
co.mean <- get.daily.data(par="42101",psid=3,dur="1",stat="mean",years=curr.3yr)
co.daily <- merge(merge(co.mda1,co.mda8,by=c("site","poc","date"),all=TRUE,
   suffixes=c(".mda1",".mda8")),co.mean,by=c("site","poc","date"),all=TRUE)
save(co.daily,file=paste("NAAQS_AQ/data/",curr.year,"/COdaily",file.yrs,".Rdata",sep=""))
no2.max <- get.daily.data(par="42602",psid=20,dur="1",stat="max",years=curr.3yr)
no2.mean <- get.daily.data(par="42602",psid=8,dur="1",stat="mean",years=curr.3yr)
no2.daily <- merge(no2.max,no2.mean,by=c("site","poc","date"),all=TRUE,suffixes=c(".max",".mean"))
save(no2.daily,file=paste("NAAQS_AQ/data/",curr.year,"/NO2daily",file.yrs,".Rdata",sep=""))
o3.mda1 <- get.daily.data(par="44201",psid=9,dur="1",stat="max",years=curr.3yr)
o3.mda8 <- get.daily.data(par="44201",psid=23,dur="W",stat="max",years=curr.3yr)
o3.mean <- get.daily.data(par="44201",psid=9,dur="1",stat="mean",years=curr.3yr)
o3.daily <- merge(merge(o3.mda1,o3.mda8,by=c("site","poc","date"),all=TRUE,
   suffixes=c(".mda1",".mda8")),o3.mean,by=c("site","poc","date"),all=TRUE)
colnames(o3.daily)[ncol(o3.daily)] <- "conc.mean"; o3.daily[,4:6] <- o3.daily[,4:6]*1000;
save(o3.daily,file=paste("NAAQS_AQ/data/",curr.year,"/O3daily",file.yrs,".Rdata",sep=""))
pm10.filt <- get.daily.data(par="81102",psid=12,dur="7",stat="mean",years=curr.3yr)
pm10.cont <- get.daily.data(par="81102",psid=12,dur="X",stat="mean",years=curr.3yr)
pm10.dmax <- get.daily.data(par="81102",psid=0,dur="1",stat="max",years=curr.3yr)
pm10.daily <- rbind(pm10.filt,pm10.cont)
pm10.daily <- pm10.daily[order(pm10.daily$site,pm10.daily$poc,pm10.daily$date),]
save(list=c("pm10.daily","pm10.dmax"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM10daily",file.yrs,".Rdata",sep=""))
pm25.filt <- get.daily.data(par="88101",psid=21,dur="7",stat="mean",years=curr.3yr)
pm25.cont <- get.daily.data(par="88101",psid=21,dur="X",stat="mean",years=curr.3yr)
pm25.dmax <- get.daily.data(par="88101",psid=0,dur="1",stat="max",years=curr.3yr)
pm25.daily <- rbind(pm25.filt,pm25.cont)
pm25.daily <- pm25.daily[order(pm25.daily$site,pm25.daily$poc,pm25.daily$date),]
save(list=c("pm25.daily","pm25.dmax"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM25daily",file.yrs,".Rdata",sep=""))
so2.max <- get.daily.data(par="42401",psid=19,dur="1",stat="max",years=curr.3yr)
so2.mean <- get.daily.data(par="42401",psid=19,dur="1",stat="mean",years=curr.3yr)
so2.daily <- merge(so2.max,so2.mean,by=c("site","poc","date"),all=TRUE,suffixes=c(".max",".mean"))
save(so2.daily,file=paste("NAAQS_AQ/data/",curr.year,"/SO2daily",file.yrs,".Rdata",sep=""))
pm10_25.daily <- get.daily.data(par="86101",psid=0,dur="7",stat="mean",years=curr.3yr)
save(pm10_25.daily,file=paste("NAAQS_AQ/data/",curr.year,"/PM10_25daily",file.yrs,".Rdata",sep=""))
no3 <- get.daily.data(par=88306,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(no3)[4] <- "no3";
oc <- get.daily.data(par=88320,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(oc)[4] <- "oc";
ec <- get.daily.data(par=88321,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ec)[4] <- "ec";
so4 <- get.daily.data(par=88403,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(so4)[4] <- "so4";
al <- get.daily.data(par=88104,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(al)[4] <- "al";
ca <- get.daily.data(par=88111,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ca)[4] <- "ca";
fe <- get.daily.data(par=88126,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(fe)[4] <- "fe";
si <- get.daily.data(par=88165,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(si)[4] <- "si";
ti <- get.daily.data(par=88161,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(ti)[4] <- "ti";
cl1 <- get.daily.data(par=88115,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(cl1)[4] <- "cl1";
cl2 <- get.daily.data(par=88203,psid=0,dur="7",stat="mean",years=curr.3yr); colnames(cl2)[4] <- "cl2";
pm25.spec <- Reduce(merge.all,lapply(c("no3","oc","ec","so4","al","ca","fe","si","ti","cl1","cl2"),get))
pm25.spec$crustal <- 2.2*pm25.spec$al+1.63*pm25.spec$ca+2.42*pm25.spec$fe+2.49*pm25.spec$si+1.94*pm25.spec$ti
pm25.spec$seasalt <- 1.8*mapply(pmax.na,pm25.spec$cl1,pm25.spec$cl2)
obs <- apply(pm25.spec,1,function(x) sum(!is.na(x)))
pm25.spec.daily <- subset(pm25.spec,obs > 3)
save(pm25.spec.daily,file=paste("NAAQS_AQ/data/",curr.year,"/PM25spec_daily",
  curr.year-2,"_",curr.year,".Rdata",sep=""))

############################################################
## AQS annual summary data retrieval for all PM2.5 speciaton
############################################################
annual.cols <- unlist(get.aqs.data("SELECT column_name FROM sys.all_tab_columns
  WHERE table_name = 'ANNUAL_SUMMARIES'"))
get.annual.data <- function(par,name,years) {
  ny <- length(years)
  vals <- get.aqs.data(paste("
  SELECT si.state_code || si.county_code || si.site_id AS site,
         mo.poc AS poc,
         si.standard_latitude AS latitude,
         si.standard_longitude AS longitude,
         ans.annual_summary_year AS year,
         ans.annual_arith_mean AS ",name,"
    FROM annual_summaries ans,
         monitors mo,
         site_basic si
   WHERE ans.annual_summary_year >= ",years[1],"
     AND ans.annual_summary_year <= ",years[ny],"
     AND ans.annual_obs_cnt >= 30
     AND ans.edt_edt_id IN (0,2)
     AND ans.mo_mo_id = mo.mo_id
     AND ans.pollutant_standard_id = 0
     AND ans.sd_duration_code = '7'
     AND mo.pa_parameter_code = '",par,"'
     AND mo.si_si_id = si.si_id
     AND si.state_code NOT IN ('80','CC')
   ORDER BY 1,2,3",sep=""))
 return(vals)
}
pm10_25 <- get.annual.data(par=86101,name="pm10_25",years=c(2005:curr.year))
no3 <- get.annual.data(par=88306,name="no3",years=c(2002:curr.year))
oc1 <- get.annual.data(par=88320,name="oc",years=c(2002:curr.year))
oc2 <- get.annual.data(par=88370,name="oc",years=c(2002:curr.year))
oc <- rbind(oc1,oc2); oc <- oc[order(oc$site,oc$poc,oc$year),];
ec1 <- get.annual.data(par=88321,name="ec",years=c(2002:curr.year))
ec2 <- get.annual.data(par=88380,name="ec",years=c(2002:curr.year))
ec <- rbind(ec1,ec2); ec <- ec[order(ec$site,ec$poc,ec$year),];
so4 <- get.annual.data(par=88403,name="so4",years=c(2002:curr.year))
al <- get.annual.data(par=88104,name="al",years=c(2002:curr.year))
ca <- get.annual.data(par=88111,name="ca",years=c(2002:curr.year))
fe <- get.annual.data(par=88126,name="fe",years=c(2002:curr.year))
si <- get.annual.data(par=88165,name="si",years=c(2002:curr.year))
ti <- get.annual.data(par=88161,name="ti",years=c(2002:curr.year))
cl1 <- get.annual.data(par=88115,name="cl1",years=c(2002:curr.year))
cl2 <- get.annual.data(par=88203,name="cl2",years=c(2002:curr.year))
pm25.spec <- Reduce(merge.all,lapply(c("no3","oc","ec","so4","al","ca","fe","si","ti","cl1","cl2"),get))
pm25.spec$crustal <- 2.2*pm25.spec$al+1.63*pm25.spec$ca+2.42*pm25.spec$fe+2.49*pm25.spec$si+1.94*pm25.spec$ti
pm25.spec$seasalt <- 1.8*mapply(pmax.na,pm25.spec$cl1,pm25.spec$cl2)
save(list=c("pm10_25","pm25.spec"),
  file=paste("NAAQS_AQ/data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))

t <- ddply(pm25.spec,c("site","year"),summarize,so4=max.na(so4),
  no3=max.na(no3),ec=max.na(ec),oc=max.na(oc))
test <- data.frame(so4=rep(NA,9),no3=rep(NA,9),ec=rep(NA,9),oc=rep(NA,9),row.names=c(2002:2010))
for (sy in 2002:2010) {
  t <- subset(t,year >= sy)
  nr <- ceiling(0.75*(curr.year-sy+1))
  test$so4[sy-2001] <- sum(tapply(t$so4,list(t$site),function(x) sum(!is.na(x)) >= nr))
  test$no3[sy-2001] <- sum(tapply(t$no3,list(t$site),function(x) sum(!is.na(x)) >= nr))
  test$ec[sy-2001] <- sum(tapply(t$ec,list(t$site),function(x) sum(!is.na(x)) >= nr))
  test$oc[sy-2001] <- sum(tapply(t$oc,list(t$site),function(x) sum(!is.na(x)) >= nr))
}