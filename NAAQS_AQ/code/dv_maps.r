####################################################
## Generate design value maps for NAAQS AQ documents
####################################################

## Set up working environment
setwd("C:/Users/bwells01/Documents/NAAQS_AQ/")
options(stringsAsFactors=FALSE); library(plyr);
source("C:/Users/bwells01/Documents/Census/shape2014/rmapfuns.r")
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
if (!dir.exists(paste("dv_maps",curr.year,sep="/"))) { 
  dir.create(paste("dv_maps",curr.year,sep="/")) 
}

## CO design value maps
load(paste("data/",curr.year,"/COdvs2000_",curr.year,".Rdata",sep=""))
dvs.co.1hr <- ddply(subset(co.dvs,year == curr.year & !is.na(dv_1hr)),c("site"),summarize,
  latitude=latitude[1],longitude=longitude[1],year=year[1],dv_1hr=max(dv_1hr))
vals <- dvs.co.1hr$dv_1hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(5.1,10.1,20.1,35.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(sprintf("%4.1f",bin.min)," - ",bin.max," ppm (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/COdv1hr",curr.year-1,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.co.1hr$longitude[o],y=dvs.co.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

dvs.co.8hr <- ddply(subset(co.dvs,year == curr.year & !is.na(dv_8hr)),c("site"),summarize,
  latitude=latitude[1],longitude=longitude[1],year=year[1],dv_8hr=max(dv_8hr))
vals <- dvs.co.8hr$dv_8hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(3.1,6.1,9.1,15.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(sprintf("%4.1f",bin.min)," - ",bin.max," ppm (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/COdv8hr",curr.year-1,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.co.8hr$longitude[o],y=dvs.co.8hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## NO2 design value maps
load(paste("data/",curr.year,"/NO2dvs2000_",curr.year,".Rdata",sep=""))
dvs.no2.1hr <- subset(no2.dvs,year == curr.year & valid_1hr == "Y",
  c("site","latitude","longitude","year","dv_1hr"))
vals <- dvs.no2.1hr$dv_1hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(26,51,76,101); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/NO2dv1hr",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.no2.1hr$longitude[o],y=dvs.no2.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=4)
dev.off()

dvs.no2.ann <- subset(no2.dvs,year == curr.year & !is.na(dv_ann) & valid_ann == "Y",
  c("site","latitude","longitude","year","dv_ann"))
vals <- round(dvs.no2.ann$dv_ann); o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(11,21,31,54); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/NO2dvAnn",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.no2.ann$longitude[o],y=dvs.no2.ann$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## O3 design value map
load(paste("data/",curr.year,"/O3dvs2000_",curr.year,".Rdata",sep=""))
dvs.o3.8hr <- subset(o3.dvs,year == curr.year & valid_8hr == "Y",
  c("site","latitude","longitude","year","dv_8hr"))
vals <- dvs.o3.8hr$dv_8hr; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(61,66,71,76,85); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/O3dv8hr",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.o3.8hr$longitude[o],y=dvs.o3.8hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Pb design value map
load(paste("data/",curr.year,"/PBdvs2010_",curr.year,".Rdata",sep=""))
dvs.pb.3mo <- subset(pb.dvs,year == curr.year & valid_dv == "Y",
  c("site","latitude","longitude","year","dv"))
vals <- dvs.pb.3mo$dv; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.06,0.11,0.16,0.21); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.01,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-0.01,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(sprintf("%5.2f",bin.min)," - ",sprintf("%5.2f",bin.max),
  " ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/Pbdv3mo",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pb.3mo$longitude[o],y=dvs.pb.3mo$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## PM10 design value map
load(paste("data/",curr.year,"/PM10dvs2000_",curr.year,".Rdata",sep=""))
dvs.pm10.24hr <- subset(pm10.dvs,year == curr.year & dv_valid == "Y",
  c("site","latitude","longitude","year","dv_conc"))
vals <- dvs.pm10.24hr$dv_conc; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(51,101,151,201,501); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM10dv24h",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm10.24hr$longitude[o],y=dvs.pm10.24hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## PM2.5 design value maps
load(paste("data/",curr.year,"/PM25dvs2002_",curr.year,".Rdata",sep=""))
dvs.pm25.24hr <- subset(pm25.dvs,year == curr.year & dv_24h_valid == "Y",
  c("site","latitude","longitude","year","dv_24h"))
vals <- dvs.pm25.24hr$dv_24h; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(16,26,36,51,101); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25dv24h",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm25.24hr$longitude[o],y=dvs.pm25.24hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

dvs.pm25.ann <- subset(pm25.dvs,year == curr.year & dv_ann_valid == "Y",
  c("site","latitude","longitude","year","dv_ann"))
vals <- dvs.pm25.ann$dv_ann; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(6.1,9.1,12.1,15.1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-0.1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-0.1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",sprintf("%4.1f",bin.max)," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25dvAnn",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.pm25.ann$longitude[o],y=dvs.pm25.ann$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## SO2 design value map
load(paste("data/",curr.year,"/SO2dvs2000_",curr.year,".Rdata",sep=""))
dvs.so2.1hr <- subset(so2.dvs,year == curr.year & dv_valid == "Y",
  c("site","latitude","longitude","year","dv"))
vals <- dvs.so2.1hr$dv; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3","purple3"); bins <- c(26,51,76,101,251); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(min(vals),bins); bin.max <- c(bins-1,max(vals));
n.sites <- table(cut(vals,breaks=c(0,bins-1,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ppb (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/SO2dv1hr",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=dvs.so2.1hr$longitude[o],y=dvs.so2.1hr$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

###############################################################################
## Annual average concentration maps for coarse PM (PM10-2.5) and PM2.5 species
###############################################################################
load(paste("data/",curr.year,"/PM25spec_annual2002_",curr.year,".Rdata",sep=""))
load(paste("data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
pm10_25.monitors$site <- substr(pm10_25.monitors$id,1,9)
pm10_25.sites <- subset(pm10_25.monitors,!duplicated(site),c("site","latitude","longitude"))
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.sites <- subset(pm25.spec.monitors,!duplicated(site),c("site","latitude","longitude"))

## Annual average PM coarse concentration map
t <- subset(ddply(pm10_25,c("site","year"),summarize,pm10_25=max.na(pm10_25)),year >= curr.year-2)
map.vals <- merge(pm10_25.sites,ddply(t,c("site"),summarize,pm10_25=mean(pm10_25)))
vals <- map.vals$pm10_25; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(2,4,6,10); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.1,round(max(vals)));
n.sites <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM10_25avg",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average sulfate (SO4) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,so4=max.na(so4)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,so4=mean(so4))))
vals <- map.vals$so4; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.5,1,1.5,2); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
n.sites <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25_SO4avg",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average nitrate (NO3) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,no3=max.na(no3)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,no3=mean(no3))))
vals <- map.vals$no3; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.5,1,1.5,2); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
n.sites <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25_NO3avg",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average elemental carbon (EC) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,ec=max.na(ec)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,ec=mean(ec))))
vals <- map.vals$ec; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(0.25,0.5,0.75,1); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
n.sites <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25_ECavg",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

## Annual average organic carbon (OC) concentration map
t <- subset(ddply(pm25.spec,c("site","year"),summarize,oc=max.na(oc)),year >= curr.year-2)
map.vals <- na.omit(merge(pm25.spec.sites,ddply(t,c("site"),summarize,oc=mean(oc))))
vals <- map.vals$oc; o <- order(vals);
colors <- c("blue3","cyan3","yellow3","orange3","red3"); bins <- c(1,2,3,4); 
pt.col <- assign.colors(vals,discrete=TRUE,breaks=bins,palette=colors)
bin.min <- c(0,bins); bin.max <- c(bins-0.01,round(max(vals),2));
n.sites <- table(cut(vals,breaks=c(0,bins,Inf))); keep <- which(n.sites > 0);
legend.lab <- paste(bin.min," - ",bin.max," ug/m^3 (",n.sites," sites)",sep="")
file.name <- paste("dv_maps/",curr.year,"/PM25_OCavg",curr.year-2,"_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1200,height=800,quality=100)
layout(mat=matrix(c(1,2),2,1),width=1,height=c(1,0.1))
draw.map("state",proj.args=pa,hires=TRUE,col="gray95")
add.layer(type="points",x=map.vals$longitude[o],y=map.vals$latitude[o],
  proj.args=pa,pch=21,col="black",bg=pt.col[o],cex=3)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab[keep],bty='n',cex=2,
  pch=21,col="black",pt.bg=colors[keep],pt.cex=3,ncol=3)
dev.off()

