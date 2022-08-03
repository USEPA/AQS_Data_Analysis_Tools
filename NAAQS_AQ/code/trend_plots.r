#################################################
## Generate trends figures for NAAQS AQ documents
#################################################

## Set up working environment
setwd("C:/Users/bwells01/Documents/NAAQS_AQ/")
options(stringsAsFactors=FALSE)
library(data.table); library(plyr); library(reshape2); library(trend);
source("C:/Users/bwells01/Documents/Census/shape2020/rmapfuns.r")
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))
count <- function(x) { return(sum(!is.na(x))) }
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
pt.col <- c("#0000FFFF","#AAAAFFFF","#FFFFFFFF","#FFAAAAFF","#FF0000FF")
pt.cex <- c(3,2,1.5,2,3); pt.pch <- c(25,25,21,24,24);
if (!dir.exists(paste("trend_charts",curr.year,sep="/"))) {
  dir.create(paste("trend_charts",curr.year,sep="/")) 
  dir.create(paste("trend_maps",curr.year,sep="/")) 
}

##############################
## Carbon Monoxide (CO) Trends
##############################
years <- c(2000:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/COdvs2000_",curr.year,".Rdata",sep=""))
co.site.dvs <- ddply(co.dvs,c("site","year"),summarize,latitude=latitude[1],
  longitude=longitude[1],dv_1hr=max.na(dv_1hr),dv_8hr=max.na(dv_8hr))
co.dvs.1hr <- recast(co.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_1hr")
co.dvs.8hr <- recast(co.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_8hr")
co.site.dvs.1hr <- co.dvs.1hr[which(apply(co.dvs.1hr[,-c(1:3)],1,count) >= nr),]
co.site.dvs.8hr <- co.dvs.8hr[which(apply(co.dvs.8hr[,-c(1:3)],1,count) >= nr),]
co.trend.dvs.1hr <- apply(co.site.dvs.1hr[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
co.trend.dvs.8hr <- apply(co.site.dvs.8hr[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
co.ts.dvs.1hr <- apply(co.site.dvs.1hr[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
co.ts.dvs.8hr <- apply(co.site.dvs.8hr[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
co.site.dvs.1hr$slope <- unlist(lapply(co.ts.dvs.1hr,function(x) x$estimates))
co.site.dvs.8hr$slope <- unlist(lapply(co.ts.dvs.8hr,function(x) x$estimates))
co.site.dvs.1hr$pval <- unlist(lapply(co.ts.dvs.1hr,function(x) x$p.value))
co.site.dvs.8hr$pval <- unlist(lapply(co.ts.dvs.8hr,function(x) x$p.value))
save(list=c("co.site.dvs.1hr","co.site.dvs.8hr"),
  file=paste("data/",curr.year,"/COtrends2000_",curr.year,".Rdata",sep=""))

## National CO trends figure
file.name <- paste("trend_charts/",curr.year,"/COtrends2000_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,15),ylab="CO Design Value (ppm)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(1999.5,rep(curr.year+0.5,2),1999.5),y=c(0,0,15,15),col="gray80")
abline(h=seq(1,14,1),v=years,col="white")
lines(x=years,y=co.trend.dvs.1hr[1,],lty=2,lwd=2,col="orange3")
lines(x=years,y=co.trend.dvs.1hr[2,],lty=1,lwd=2,col="orange3")
lines(x=years,y=co.trend.dvs.1hr[3,],lty=2,lwd=2,col="orange3")
lines(x=years,y=co.trend.dvs.8hr[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=co.trend.dvs.8hr[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=co.trend.dvs.8hr[3,],lty=2,lwd=2,col="steelblue")
abline(h=9,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile 1-hour DV","Median 1-hour DV",
  "10th/90th Percentile 8-hour DV","Median 8-hour DV","CO 8-hour NAAQS Level"),
  lty=c(2,1,2,1,2),col=c(rep("orange3",2),rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of CO 1-hour site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/COtrends1hr2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.2 ppm/yr","Decreasing < 0.2 ppm/yr","No Significant Trend",
  "Increasing < 0.2 ppm/yr","Increasing > 0.2 ppm/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.2,4,5),ifelse(slope > -0.2,2,1))),
  pval=co.site.dvs.1hr$pval,slope=co.site.dvs.1hr$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(co.site.dvs.1hr$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=co.site.dvs.1hr$longitude[r],y=co.site.dvs.1hr$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of CO 8-hour site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/COtrends8hr2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.1 ppm/yr","Decreasing < 0.1 ppm/yr","No Significant Trend",
  "Increasing < 0.1 ppm/yr","Increasing > 0.1 ppm/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.1,4,5),ifelse(slope > -0.1,2,1))),
  pval=co.site.dvs.8hr$pval,slope=co.site.dvs.8hr$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(co.site.dvs.8hr$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=co.site.dvs.8hr$longitude[r],y=co.site.dvs.8hr$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

################################
## Nitrogen Dioxide (NO2) Trends
################################
years <- c(2000:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/NO2dvs2000_",curr.year,".Rdata",sep=""))
no2.dvs$dv_1hr[which(no2.dvs$valid_1hr == "N")] <- NA
no2.dvs$dv_ann[which(no2.dvs$valid_ann == "N")] <- NA
no2.site.dvs <- ddply(no2.dvs,c("site","year"),summarize,latitude=latitude[1],
  longitude=longitude[1],dv_1hr=max.na(dv_1hr),dv_ann=max.na(dv_ann))
no2.dvs.1hr <- recast(no2.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_1hr")
no2.dvs.ann <- recast(no2.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_ann")
no2.site.dvs.1hr <- no2.dvs.1hr[which(apply(no2.dvs.1hr[,-c(1:3)],1,count) >= nr),]
no2.site.dvs.ann <- no2.dvs.ann[which(apply(no2.dvs.ann[,-c(1:3)],1,count) >= nr),]
no2.trend.dvs.1hr <- apply(no2.site.dvs.1hr[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
no2.trend.dvs.ann <- apply(no2.site.dvs.ann[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
no2.ts.dvs.1hr <- apply(no2.site.dvs.1hr[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
no2.ts.dvs.ann <- apply(no2.site.dvs.ann[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
no2.site.dvs.1hr$slope <- unlist(lapply(no2.ts.dvs.1hr,function(x) x$estimates))
no2.site.dvs.ann$slope <- unlist(lapply(no2.ts.dvs.ann,function(x) x$estimates))
no2.site.dvs.1hr$pval <- unlist(lapply(no2.ts.dvs.1hr,function(x) x$p.value))
no2.site.dvs.ann$pval <- unlist(lapply(no2.ts.dvs.ann,function(x) x$p.value))
save(list=c("no2.site.dvs.1hr","no2.site.dvs.ann"),
  file=paste("data/",curr.year,"/NO2trends2000_",curr.year,".Rdata",sep=""))

## National NO2 trends figure
file.name <- paste("trend_charts/",curr.year,"/NO2trends2000_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,100),ylab="NO2 Design Value (ppb)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(1999.5,rep(curr.year+0.5,2),1999.5),y=c(0,0,100,100),col="gray80")
abline(h=seq(10,90,10),v=years,col="white")
lines(x=years,y=no2.trend.dvs.1hr[1,],lty=2,lwd=2,col="orange3")
lines(x=years,y=no2.trend.dvs.1hr[2,],lty=1,lwd=2,col="orange3")
lines(x=years,y=no2.trend.dvs.1hr[3,],lty=2,lwd=2,col="orange3")
lines(x=years,y=no2.trend.dvs.ann[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=no2.trend.dvs.ann[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=no2.trend.dvs.ann[3,],lty=2,lwd=2,col="steelblue")
abline(h=53,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile 1-hour DV","Median 1-hour DV",
  "10th/90th Percentile Annual DV","Median Annual DV","NO2 Annual NAAQS Level"),
  lty=c(2,1,2,1,2),col=c(rep("orange3",2),rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of NO2 1-hour site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/NO2trends1hr2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 1 ppb/yr","Decreasing < 1 ppb/yr","No Significant Trend",
  "Increasing < 1 ppb/yr","Increasing > 1 ppb/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 1,4,5),ifelse(slope > -1,2,1))),
  pval=no2.site.dvs.1hr$pval,slope=no2.site.dvs.1hr$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(no2.site.dvs.1hr$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=no2.site.dvs.1hr$longitude[r],y=no2.site.dvs.1hr$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of NO2 Annual site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/NO2trendsann2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.5 ppb/yr","Decreasing < 0.5 ppb/yr","No Significant Trend",
  "Increasing < 0.5 ppb/yr","Increasing > 0.5 ppb/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.5,4,5),ifelse(slope > -0.5,2,1))),
  pval=no2.site.dvs.ann$pval,slope=no2.site.dvs.ann$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(no2.site.dvs.ann$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=no2.site.dvs.ann$longitude[r],y=no2.site.dvs.ann$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

####################
## Ozone (O3) Trends
####################
years <- c(2000:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/O3dvs2000_",curr.year,".Rdata",sep=""))
o3.dvs$dv_8hr[which(o3.dvs$valid_8hr == "N")] <- NA
o3.site.dvs <- ddply(o3.dvs,c("site","year"),summarize,
  latitude=latitude[1],longitude=longitude[1],dv_8hr=max.na(dv_8hr))
o3.dvs.8hr <- recast(o3.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_8hr")
o3.site.dvs.8hr <- o3.dvs.8hr[which(apply(o3.dvs.8hr[,-c(1:3)],1,count) >= nr),]
o3.trend.dvs.8hr <- apply(o3.site.dvs.8hr[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
o3.ts.dvs.8hr <- apply(o3.site.dvs.8hr[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
o3.site.dvs.8hr$slope <- unlist(lapply(o3.ts.dvs.8hr,function(x) x$estimates))
o3.site.dvs.8hr$pval <- unlist(lapply(o3.ts.dvs.8hr,function(x) x$p.value))
save(list=c("o3.site.dvs.8hr"),
  file=paste("data/",curr.year,"/O3trends2000_",curr.year,".Rdata",sep=""))

## National O3 trends figure
file.name <- paste("trend_charts/",curr.year,"/O3trends2000_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(50,105),ylab="O3 Design Value (ppb)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(1999.5,rep(curr.year+0.5,2),1999.5),y=c(50,50,105,105),col="gray80")
abline(h=seq(55,100,5),v=years,col="white")
lines(x=years,y=o3.trend.dvs.8hr[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=o3.trend.dvs.8hr[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=o3.trend.dvs.8hr[3,],lty=2,lwd=2,col="steelblue")
abline(h=70,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile 8-hour DV","Median 8-hour DV",
  "O3 8-hour NAAQS Level"),lty=c(2,1,2),col=c(rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of O3 8-hour site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/O3trends8hr2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 1 ppb/yr","Decreasing < 1 ppb/yr","No Significant Trend",
  "Increasing < 1 ppb/yr","Increasing > 1 ppb/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 1,4,5),ifelse(slope > -1,2,1))),
  pval=o3.site.dvs.8hr$pval,slope=o3.site.dvs.8hr$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(o3.site.dvs.8hr$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=o3.site.dvs.8hr$longitude[r],y=o3.site.dvs.8hr$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

###################
## Lead (Pb) trends
###################
years <- c(2010:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/PBdvs2010_",curr.year,".Rdata",sep=""))
pb.dvs$dv[which(pb.dvs$dv_valid == "N")] <- NA
pb.site.dvs <- recast(pb.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv")
pb.site.dvs.3mo <- pb.site.dvs[which(apply(pb.site.dvs[,-c(1:3)],1,count) >= nr),]
pb.trend.dvs.3mo <- apply(pb.site.dvs.3mo[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
pb.ts.dvs.3mo <- apply(pb.site.dvs.3mo[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
pb.site.dvs.3mo$slope <- unlist(lapply(pb.ts.dvs.3mo,function(x) x$estimates))
pb.site.dvs.3mo$pval <- unlist(lapply(pb.ts.dvs.3mo,function(x) x$p.value))
save(list=c("pb.site.dvs.3mo"),
  file=paste("data/",curr.year,"/Pbtrends2010_",curr.year,".Rdata",sep=""))

## National Pb trends figure
file.name <- paste("trend_charts/",curr.year,"/Pbtrends2010_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(2009.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,0.6),ylab="Pb Design Value (ug/m^3)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(2009.5,rep(curr.year+0.5,2),2009.5),y=c(0,0,0.6,0.6),col="gray80")
abline(h=seq(0.05,0.55,0.05),v=years,col="white")
lines(x=years,y=pb.trend.dvs.3mo[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=pb.trend.dvs.3mo[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=pb.trend.dvs.3mo[3,],lty=2,lwd=2,col="steelblue")
abline(h=0.15,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile DV","Median DV","Pb NAAQS Level"),
  lty=c(2,1,2),col=c(rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of Pb site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/Pbtrends3mo2010_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.05 ug/m^3/yr","Decreasing < 0.05 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.05 ug/m^3/yr","Increasing > 0.05 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.05,4,5),ifelse(slope > -0.05,2,1))),
  pval=pb.site.dvs.3mo$pval,slope=pb.site.dvs.3mo$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(pb.site.dvs.3mo$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=pb.site.dvs.3mo$longitude[r],y=pb.site.dvs.3mo$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

###################################
## Particulate Matter (PM10) trends
###################################
years <- c(2000:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/PM10dvs2000_",curr.year,".Rdata",sep=""))
pm10.dvs$dv_conc[which(pm10.dvs$valid_dv == "N")] <- NA
pm10.site.dvs <- ddply(pm10.dvs,c("site","year"),summarize,
  latitude=latitude[1],longitude=longitude[1],dv=max.na(dv_conc))
pm10.dvs.24h <- recast(pm10.site.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv")
pm10.site.dvs.24h <- pm10.dvs.24h[which(apply(pm10.dvs.24h[,-c(1:3)],1,count) >= nr),]
pm10.trend.dvs.24h <- apply(pm10.site.dvs.24h[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
pm10.ts.dvs.24h <- apply(pm10.site.dvs.24h[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
pm10.site.dvs.24h$slope <- unlist(lapply(pm10.ts.dvs.24h,function(x) x$estimates))
pm10.site.dvs.24h$pval <- unlist(lapply(pm10.ts.dvs.24h,function(x) x$p.value))
save(list=c("pm10.site.dvs.24h"),
  file=paste("data/",curr.year,"/PM10trends2000_",curr.year,".Rdata",sep=""))

## National PM10 trends figure
file.name <- paste("trend_charts/",curr.year,"/PM10trends2000_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,250),ylab="PM10 Design Concentration (ug/m^3)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(1999.5,rep(curr.year+0.5,2),1999.5),y=c(0,0,250,250),col="gray80")
abline(h=seq(20,240,20),v=years,col="white")
lines(x=years,y=pm10.trend.dvs.24h[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=pm10.trend.dvs.24h[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=pm10.trend.dvs.24h[3,],lty=2,lwd=2,col="steelblue")
abline(h=150,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile PM10 DV","Median PM10 DV","PM10 NAAQS Level"),
  lty=c(2,1,2),col=c(rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of PM10 site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/PM10trends24h2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 3 ug/m^3/yr","Decreasing < 3 ug/m^3/yr",
  "No Significant Trend","Increasing < 3 ug/m^3/yr","Increasing > 3 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 3,4,5),ifelse(slope > -3,2,1))),
  pval=pm10.site.dvs.24h$pval,slope=pm10.site.dvs.24h$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(pm10.site.dvs.24h$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=pm10.site.dvs.24h$longitude[r],y=pm10.site.dvs.24h$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

#########################################
## Fine Particulate Matter (PM2.5) trends
#########################################
years <- c(2002:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/PM25dvs2002_",curr.year,".Rdata",sep=""))
pm25.dvs$dv_ann[which(pm25.dvs$dv_ann_valid == "N")] <- NA
pm25.dvs$dv_24h[which(pm25.dvs$dv_24h_valid == "N")] <- NA
pm25.dvs.ann <- recast(pm25.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_ann")
pm25.dvs.24h <- recast(pm25.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv_24h")
pm25.site.dvs.ann <- pm25.dvs.ann[which(apply(pm25.dvs.ann[,-c(1:3)],1,count) >= nr),]
pm25.site.dvs.24h <- pm25.dvs.24h[which(apply(pm25.dvs.24h[,-c(1:3)],1,count) >= nr),]
pm25.trend.dvs.ann <- apply(pm25.site.dvs.ann[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
pm25.trend.dvs.24h <- apply(pm25.site.dvs.24h[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
pm25.ts.dvs.ann <- apply(pm25.site.dvs.ann[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
pm25.ts.dvs.24h <- apply(pm25.site.dvs.24h[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
pm25.site.dvs.ann$slope <- unlist(lapply(pm25.ts.dvs.ann,function(x) x$estimates))
pm25.site.dvs.ann$pval <- unlist(lapply(pm25.ts.dvs.ann,function(x) x$p.value))
pm25.site.dvs.24h$slope <- unlist(lapply(pm25.ts.dvs.24h,function(x) x$estimates))
pm25.site.dvs.24h$pval <- unlist(lapply(pm25.ts.dvs.24h,function(x) x$p.value))
save(list=c("pm25.site.dvs.ann","pm25.site.dvs.24h"),
  file=paste("data/",curr.year,"/PM25trends2002_",curr.year,".Rdata",sep=""))

## National PM2.5 trends figure
file.name <- paste("trend_charts/",curr.year,"/PM25trends2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(2001.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,50),ylab="PM2.5 Design Value (ug/m^3)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(2001.5,rep(curr.year+0.5,2),2001.5),y=c(0,0,50,50),col="gray80")
abline(h=seq(5,45,5),v=years,col="white")
lines(x=years,y=pm25.trend.dvs.ann[1,],lty=2,lwd=2,col="orange3")
lines(x=years,y=pm25.trend.dvs.ann[2,],lty=1,lwd=2,col="orange3")
lines(x=years,y=pm25.trend.dvs.ann[3,],lty=2,lwd=2,col="orange3")
lines(x=years,y=pm25.trend.dvs.24h[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=pm25.trend.dvs.24h[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=pm25.trend.dvs.24h[3,],lty=2,lwd=2,col="steelblue")
abline(h=c(12,35),lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile Annual DV","Median Annual DV",
  "10th/90th Percentile 24-hour DV","Median 24-hour DV","PM2.5 NAAQS Levels"),
  lty=c(2,1,2,1,2),col=c(rep("orange3",2),rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of PM2.5 Annual site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/PM25trendsann2002_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.5 ug/m^3/yr","Decreasing < 0.5 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.5 ug/m^3/yr","Increasing > 0.5 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.5,4,5),ifelse(slope > -0.5,2,1))),
  pval=pm25.site.dvs.ann$pval,slope=pm25.site.dvs.ann$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(pm25.site.dvs.ann$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=pm25.site.dvs.ann$longitude[r],y=pm25.site.dvs.ann$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of PM2.5 24-hour site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/PM25trends24h2002_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 1 ug/m^3/yr","Decreasing < 1 ug/m^3/yr",
  "No Significant Trend","Increasing < 1 ug/m^3/yr","Increasing > 1 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 1,4,5),ifelse(slope > -1,2,1))),
  pval=pm25.site.dvs.24h$pval,slope=pm25.site.dvs.24h$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(pm25.site.dvs.24h$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=pm25.site.dvs.24h$longitude[r],y=pm25.site.dvs.24h$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

##############################
## Sulfur Dioxide (SO2) Trends
##############################
years <- c(2000:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/SO2dvs2000_",curr.year,".Rdata",sep=""))
so2.dvs$dv[which(so2.dvs$dv_valid == "N")] <- NA
so2.dvs$p99[which(so2.dvs$p99_valid == "N")] <- NA
so2.site.dvs <- recast(so2.dvs,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="dv")
so2.site.dvs.1hr <- so2.site.dvs[which(apply(so2.site.dvs[,-c(1:3)],1,count) >= nr),]
so2.trend.dvs.1hr <- apply(so2.site.dvs.1hr[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
so2.ts.dvs.1hr <- apply(so2.site.dvs.1hr[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
so2.site.dvs.1hr$slope <- unlist(lapply(so2.ts.dvs.1hr,function(x) x$estimates))
so2.site.dvs.1hr$pval <- unlist(lapply(so2.ts.dvs.1hr,function(x) x$p.value))
save(list=c("so2.site.dvs.1hr"),
  file=paste("data/",curr.year,"/SO2trends2000_",curr.year,".Rdata",sep=""))

## National SO2 trends figure
file.name <- paste("trend_charts/",curr.year,"/SO2trends2000_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(4,4,1,1),mgp=c(2.5,1,0),cex.axis=1.5,cex.lab=1.5)
plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,curr.year+0.5),
  xlab="",yaxs='i',ylim=c(0,200),ylab="SO2 Design Value (ppb)",main="")
axis(side=1,at=years,labels=years,las=2)
polygon(x=c(1999.5,rep(curr.year+0.5,2),1999.5),y=c(0,0,200,200),col="gray80")
abline(h=seq(20,180,20),v=years,col="white")
lines(x=years,y=so2.trend.dvs.1hr[1,],lty=2,lwd=2,col="steelblue")
lines(x=years,y=so2.trend.dvs.1hr[2,],lty=1,lwd=2,col="steelblue")
lines(x=years,y=so2.trend.dvs.1hr[3,],lty=2,lwd=2,col="steelblue")
abline(h=75,lty=2,lwd=2,col="black")
legend("topright",legend=c("10th/90th Percentile DV","Median DV","SO2 NAAQS Level"),
  lty=c(2,1,2),col=c(rep("steelblue",2),"black"),bty='n',lwd=2,cex=1.5)
box(); dev.off();

## Map of SO2 site-level DV trends
file.name <- paste("trend_maps/",curr.year,"/SO2trends1hr2000_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 3 ppb/yr","Decreasing < 3 ppb/yr","No Significant Trend",
  "Increasing < 3 ppb/yr","Increasing > 3 ppb/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 3,4,5),ifelse(slope > -3,2,1))),
  pval=so2.site.dvs.1hr$pval,slope=so2.site.dvs.1hr$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(so2.site.dvs.1hr$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=so2.site.dvs.1hr$longitude[r],y=so2.site.dvs.1hr$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

##############################################
## Coarse Particulate Matter (PM10-2.5) trends
##############################################
years <- c(2005:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))
pm10_25.site.avg <- ddply(pm10_25,c("site","year"),summarize,
  latitude=latitude[1],longitude=longitude[1],pm10_25=max.na(pm10_25))
pm10_25.site.avg <- recast(pm10_25.site.avg,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="pm10_25")
pm10_25.site.avg <- pm10_25.site.avg[which(apply(pm10_25.site.avg[,-c(1:3)],1,count) >= nr),]
pm10_25.trend <- apply(pm10_25.site.avg[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
pm10_25.ts <- apply(pm10_25.site.avg[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
pm10_25.site.avg$slope <- unlist(lapply(pm10_25.ts,function(x) x$estimates))
pm10_25.site.avg$pval <- unlist(lapply(pm10_25.ts,function(x) x$p.value))
save(list=c("pm10_25.site.avg"),
  file=paste("data/",curr.year,"/PM10_25trends2005_",curr.year,".Rdata",sep=""))

## Map of PM10-2.5 site-level annual average trends
file.name <- paste("trend_maps/",curr.year,"/PM10_25trends2005_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.2 ug/m^3/yr","Decreasing < 0.2 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.2 ug/m^3/yr","Increasing > 0.2 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.2,4,5),ifelse(slope > -0.2,2,1))),
  pval=pm10_25.site.avg$pval,slope=pm10_25.site.avg$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(pm10_25.site.avg$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=pm10_25.site.avg$longitude[r],y=pm10_25.site.avg$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

############################################
## Speciated PM2.5 trends (SO4, NO3, EC, OC)
############################################
years <- c(2006:curr.year); ny <- length(years); nr <- ceiling(0.75*ny);
load(paste("data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))
if (years[1] > 2002) { pm25.spec <- subset(pm25.spec,year >= years[1]) }
pm25.spec.avg <- ddply(pm25.spec,c("site","year"),summarize,latitude=latitude[1],
  longitude=longitude[1],so4=max.na(so4),no3=max.na(no3),ec=max.na(ec),oc=max.na(oc))
so4.site.avg <- recast(pm25.spec.avg,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="so4")
so4.site.avg <- so4.site.avg[which(apply(so4.site.avg[,-c(1:3)],1,count) >= nr),]
no3.site.avg <- recast(pm25.spec.avg,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="no3")
no3.site.avg <- no3.site.avg[which(apply(no3.site.avg[,-c(1:3)],1,count) >= nr),]
ec.site.avg <- recast(pm25.spec.avg,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="ec")
ec.site.avg <- ec.site.avg[which(apply(ec.site.avg[,-c(1:3)],1,count) >= nr),]
oc.site.avg <- recast(pm25.spec.avg,site + latitude + longitude ~ year,
  id.var=c("site","latitude","longitude","year"),measure.var="oc")
oc.site.avg <- oc.site.avg[which(apply(oc.site.avg[,-c(1:3)],1,count) >= nr),]
so4.trend <- apply(so4.site.avg[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
no3.trend <- apply(no3.site.avg[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
ec.trend <- apply(ec.site.avg[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
oc.trend <- apply(oc.site.avg[,-c(1:3)],2,quantile,probs=c(0.1,0.5,0.9),na.rm=TRUE)
so4.ts <- apply(so4.site.avg[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
no3.ts <- apply(no3.site.avg[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
ec.ts <- apply(ec.site.avg[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
oc.ts <- apply(oc.site.avg[,-c(1:3)],1,function(x) sens.slope(x[which(!is.na(x))]))
so4.site.avg$slope <- unlist(lapply(so4.ts,function(x) x$estimates))
so4.site.avg$pval <- unlist(lapply(so4.ts,function(x) x$p.value))
no3.site.avg$slope <- unlist(lapply(no3.ts,function(x) x$estimates))
no3.site.avg$pval <- unlist(lapply(no3.ts,function(x) x$p.value))
ec.site.avg$slope <- unlist(lapply(ec.ts,function(x) x$estimates))
ec.site.avg$pval <- unlist(lapply(ec.ts,function(x) x$p.value))
oc.site.avg$slope <- unlist(lapply(oc.ts,function(x) x$estimates))
oc.site.avg$pval <- unlist(lapply(oc.ts,function(x) x$p.value))
save(list=c("so4.site.avg","no3.site.avg","ec.site.avg","oc.site.avg"),
  file=paste("data/",curr.year,"/PM25spec_trends",years[1],"_",curr.year,".Rdata",sep=""))

## Map of SO4 site-level annual average trends
file.name <- paste("trend_maps/",curr.year,"/PM25_SO4trends",years[1],"_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.1 ug/m^3/yr","Decreasing < 0.1 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.1 ug/m^3/yr","Increasing > 0.1 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.1,4,5),ifelse(slope > -0.1,2,1))),
  pval=so4.site.avg$pval,slope=so4.site.avg$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(so4.site.avg$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=so4.site.avg$longitude[r],y=so4.site.avg$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of NO3 site-level annual average trends
file.name <- paste("trend_maps/",curr.year,"/PM25_NO3trends",years[1],"_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.1 ug/m^3/yr","Decreasing < 0.1 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.1 ug/m^3/yr","Increasing > 0.1 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.1,4,5),ifelse(slope > -0.1,2,1))),
  pval=no3.site.avg$pval,slope=no3.site.avg$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(no3.site.avg$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=no3.site.avg$longitude[r],y=no3.site.avg$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of EC site-level annual average trends
file.name <- paste("trend_maps/",curr.year,"/PM25_ECtrends",years[1],"_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.1 ug/m^3/yr","Decreasing < 0.1 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.1 ug/m^3/yr","Increasing > 0.1 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.1,4,5),ifelse(slope > -0.1,2,1))),
  pval=ec.site.avg$pval,slope=ec.site.avg$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(ec.site.avg$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=ec.site.avg$longitude[r],y=ec.site.avg$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()

## Map of OC site-level annual average trends
file.name <- paste("trend_maps/",curr.year,"/PM25_OCtrends",years[1],"_",curr.year,".jpeg",sep="")
txt <- c("Decreasing > 0.1 ug/m^3/yr","Decreasing < 0.1 ug/m^3/yr",
  "No Significant Trend","Increasing < 0.1 ug/m^3/yr","Increasing > 0.1 ug/m^3/yr")
cat <- mapply(function(pval,slope) ifelse(pval > 0.05,3,
  ifelse(slope > 0,ifelse(slope < 0.1,4,5),ifelse(slope > -0.1,2,1))),
  pval=oc.site.avg$pval,slope=oc.site.avg$slope)
ind <- as.integer(names(table(cat))); r <- order(abs(oc.site.avg$slope));
legend.txt <- paste(txt[ind]," (",table(cat)," sites)",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.15))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=oc.site.avg$longitude[r],y=oc.site.avg$latitude[r],
  proj.args=pa,pch=pt.pch[cat][r],col="black",bg=pt.col[cat][r],cex=pt.cex[cat][r])
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.txt,bty='n',cex=2,col="black",
  pch=pt.pch[ind],pt.bg=pt.col[ind],pt.cex=pt.cex[ind],ncol=2)
dev.off()