setwd("C:/Users/bwells01/Documents/NAAQS_AQ/")
options(stringsAsFactors=FALSE); library(fields); library(plyr);
source("C:/Users/bwells01/Documents/R/piechart.r")
source("C:/Users/bwells01/Documents/Census/shape2020/rmapfuns.r")
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))
curr.year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
pie.colors <- c("#FFFF00","#FF0000","#00FFFF","#000000","#A52A2A","#0000FF")

## Load daily speciation data for most recent 3 years, calculate average percent values
load(paste("data/",curr.year,"/nonreg_monitors_",curr.year-2,"_",curr.year,".Rdata",sep=""))
load(paste("data/",curr.year,"/PM25spec_daily",curr.year-2,"_",curr.year,".Rdata",sep=""))
daily <- na.omit(ddply(pm25.spec.daily,c("site","date"),summarize,no3=max.na(no3),oc=max.na(oc),
  ec=max.na(ec),so4=max.na(so4),crustal=max.na(crustal),seasalt=max.na(seasalt)))
daily$total <- apply(daily[-c(1:2)],1,sum)
vals <- subset(ddply(daily,"site",summarize,obs=length(date),total=mean(total),no3=mean(no3),
  oc=mean(oc),ec=mean(ec),so4=mean(so4),crustal=mean(crustal),seasalt=mean(seasalt)),obs >= 200)
pcts <- data.frame(site=vals$site,t(apply(vals[,-c(1:3)],1,function(x) round(100*x/sum(x),1))))
pm25.spec.monitors$site <- substr(pm25.spec.monitors$id,1,9)
pm25.spec.monitors$type <- sapply(pm25.spec.monitors$network,function(x) ifelse(x == "NCORE","NCORE",
  ifelse(grepl("CSN",x),"CSN",ifelse(x == "IMPROVE","IMPROVE","OTHER"))))
pm25.spec.monitors$rank <- sapply(pm25.spec.monitors$type,switch,NCORE=1,CSN=2,IMPROVE=3,OTHER=4)
pm25.spec.monitors <- pm25.spec.monitors[order(pm25.spec.monitors$site,pm25.spec.monitors$rank),]
sites <- subset(pm25.spec.monitors,!duplicated(site),c("site","site_name","state_name","county_name",
  "cbsa_name","type","latitude","longitude"))
map.vals <- merge(sites,merge(vals[,c("site","total")],pcts,by="site"),by="site")

## Generate map with piecharts for each location - urban sites only
urban <- subset(map.vals,type %in% c("CSN","NCORE") & cbsa_name != " ")
keep <- ddply(urban,"cbsa_name",summarize,site=site[which.max(total)])
urban <- subset(urban,site %in% keep$site)
urban <- urban[order(urban$total),]
keep <- urban; rural <- subset(map.vals,type == "IMPROVE"); max.dist <- 100;
while(max.dist > 1) {
  dist <- rdist(rural[,c("longitude","latitude")],keep[,c("longitude","latitude")])
  min.dist <- apply(dist,1,min); site.add <- which.max(min.dist);
  keep <- rbind(keep,rural[site.add,]); rural <- rural[-c(site.add),];
  max.dist <- max(min.dist);
}
keep <- keep[order(keep$total),]
file.name <- paste("spec_piecharts/speciation_piecharts_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=1800,height=800,quality=100)
draw.map("state",col="gray95")
for (i in 1:nrow(keep)) {
  if (keep$state_name[i] == "Alaska") {
    keep$longitude[i] <- scale(keep$longitude[i],186,3)
    keep$latitude[i] <- scale(keep$latitude[i],-21,3)
  }
  if (keep$state_name[i] == "Hawaii") {
    keep$longitude[i] <- scale(keep$longitude[i],-65,1)
    keep$latitude[i] <- scale(keep$latitude[i],-6,1)
  }
  pie.chart(x=unlist(keep[i,c("so4","no3","oc","ec","crustal","seasalt")]),labels="",
    radius=0.5,add=TRUE,x.ctr=keep$longitude[i],y.ctr=keep$latitude[i],col=pie.colors)
}
pie.chart(x=rep(1,6),labels=c("Sulfates","Nitrates","OC","EC","Crustal","Sea Salt"),
  radius=2,add=TRUE,x.ctr=-73,y.ctr=30,col=pie.colors,cex=1.5)
dev.off()
