#########################################################################
## Generate emissions pie charts and county-level maps from 2017 NEI data 
## Create emissions trends stacked line charts from emissions trends file
## NEI data from https://gaftp.epa.gov/air/nei/2017/data_summaries/2017v1
## Trends file from https://www.epa.gov/air-emissions-inventories/
#########################################################################
setwd("C:/Users/bwells01/Documents/NAAQS_AQ/")
options(stringsAsFactors=FALSE,scipen=10,warn=-1)
library(plyr); library(reshape2); library(xlsx);
source("C:/Users/bwells01/Documents/Census/shape2014/rmapfuns.r")
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))
map.data <- map.data.20m$county.info[,c("fips","population","area_km")]
map.colors <- c("#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C") 
pie.colors <- c("lightgreen","lightpink","lightblue","wheat")
trend.border <- c("chocolate","gray50","red","green4","blue")
trend.colors <- c("wheat","gray90","lightpink","lightgreen","lightblue")
nei.year <- 2017; curr.year <- 2021; 
trend.years <- c(2002:curr.year); ny <- length(trend.years);
trend.file <- paste("data/",curr.year,"/national_tier1_caps.xlsx",sep="")
file.Rdata <- paste("data/",curr.year,"/NEI",nei.year,"_allpoll_bycounty_sector.Rdata",sep="")
trends.Rdata <- paste("data/",curr.year,"/emissions_trends_2002_",curr.year,".Rdata",sep="")

## Generate sector and county-level total dataset from 2017 NEI data for each pollutant
poll.list <- c("Carbon Monoxide","Lead","Nitrogen Oxides","PM10 Primary (Filt + Cond)",
  "PM2.5 Primary (Filt + Cond)","Sulfur Dioxide","Volatile Organic Compounds")
t <- subset(read.csv("data/",curr.year,"/esg_cty_sector_15468.csv"),pollutant.desc %in% poll.list,
  c("fips.code","state","county","pollutant.desc","sector","total.emissions","emissions.uom"))
t$fips <- sapply(t$fips.code,function(x) ifelse(nchar(x) == 5,x,paste("0",x,sep="")))
t$fips[which(t$fips == "02158")] <- "02270" ## Fix county FIPS mismatch in AK
t$fips[which(t$fips == "46102")] <- "46113" ## Fix county FIPS mismatch in SD
t$poll <- sapply(t$pollutant.desc,function(x) switch(substr(x,1,3),
  Car="CO",Lea="Pb",Nit="NOx",PM1="PM10",PM2="PM25",Sul="SO2",Vol="VOC"))
vals <- ddply(t,c("poll","fips"),summarize,emissions=sum(total.emissions))
vals <- recast(vals,fips ~ poll,id.var=c("poll","fips"),measure.var="emissions")
county.data <- merge(map.data,vals,by="fips")
vals <- ddply(t,c("poll","sector"),summarize,emissions=sum(total.emissions))
sector.data <- recast(vals,sector ~ poll,id.var=c("poll","sector"),measure.var="emissions")
save(list=c("sector.data","county.data"),file=file.Rdata)

## Retrieve emissions trends dataset from Excel spreadsheet
trends.data <- NULL
cols <- c(1,match(trend.years,c(NA,seq(1970,1990,5),1991:curr.year)))
pm25.cols <- c(1,match(trend.years,c(NA,1990:curr.year)))
trends.data$CO <- read.xlsx(trend.file,sheetName="CO",rowIndex=c(6:19,25:28,31:35),
  colIndex=cols,colClasses=c("character",rep("numeric",ny)))
trends.data$NOx <- read.xlsx(trend.file,sheetName="NOX",rowIndex=c(6:19,25:28,31:35),
  colIndex=cols,colClasses=c("character",rep("numeric",ny)))
trends.data$PM10 <- read.xlsx(trend.file,sheetName="PM10Primary",rowIndex=c(6:19,26:31),
  colIndex=cols,colClasses=c("character",rep("numeric",ny)))
trends.data$PM25 <- read.xlsx(trend.file,sheetName="PM25Primary",rowIndex=c(6:19,26:31),
  colIndex=pm25.cols,colClasses=c("character",rep("numeric",ny)))
trends.data$SO2 <- read.xlsx(trend.file,sheetName="SO2",rowIndex=c(6:19,25:28,31:35),
  colIndex=cols,colClasses=c("character",rep("numeric",ny)))
trends.data$VOC <- read.xlsx(trend.file,sheetName="VOC",rowIndex=c(6:19,24:27,30:34),
  colIndex=cols,colClasses=c("character",rep("numeric",ny)))
colnames(trends.data$CO) <- colnames(trends.data$NOx) <- colnames(trends.data$PM10) <-
colnames(trends.data$PM25) <- colnames(trends.data$SO2) <- colnames(trends.data$VOC) <-
  c("source",as.character(trend.years))
save(trends.data,file=trends.Rdata)

## CO county-level emissions density map
load(file.Rdata)
breaks <- c(10,20,50,100)
density <- county.data$CO/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.1),round(max(density)))," (",N,")",sep="")
map.title <- "Carbon Monoxide Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/CO_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## NOx county-level emissions density map
breaks <- c(2,5,10,20)
density <- county.data$NOx/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.1),round(max(density)))," (",N,")",sep="")
map.title <- "Nitrogen Oxides Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/NOx_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## Pb county-level emissions density map
breaks <- c(0.1,0.4,1,4)
density <- county.data$Pb/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.01),round(max(density)))," (",N,")",sep="")
map.title <- "Lead Emissions Density (lbs/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/Pb_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## PM10 county-level emissions density map
breaks <- c(2,5,10,20)
density <- county.data$PM10/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.1),round(max(density)))," (",N,")",sep="")
map.title <- "Primary PM10 Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/PM10_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## PM2.5 county-level emissions density map
breaks <- c(1,2,5,10)
density <- county.data$PM25/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.1),round(max(density)))," (",N,")",sep="")
map.title <- "Primary PM2.5 Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/PM25_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## SO2 county-level emissions density map
breaks <- c(0.05,0.2,1,5)
density <- county.data$SO2/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.01),round(max(density)))," (",N,")",sep="")
map.title <- "Sulfur Dioxide Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/SO2_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## VOC county-level emissions density map
breaks <- c(5,10,20,50)
density <- county.data$VOC/county.data$area_km*2.59
county.data$color <- assign.colors(density,discrete=TRUE,breaks=breaks,palette=map.colors)
N <- table(cut(density,breaks=c(0,breaks,Inf),include.lowest=TRUE))
map.legend <- paste(c(0,breaks),"-",c((breaks-0.1),round(max(density)))," (",N,")",sep="")
map.title <- "Volatile Organic Compounds Emissions Density (tons/year/mi^2)"
file.name <- paste("emissions_maps/",nei.year,"/VOC_county_density_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=1500,height=900,quality=100)
layout(matrix(c(1,2),2,1),widths=1,heights=c(0.9,0.1))
draw.map("state",proj.args=pa)
for (i in 1:nrow(county.data)) {
  draw.map("county",county.data$fips[i],add=TRUE,proj.args=pa,col=county.data$color[i])
}
draw.map("state",proj.args=pa,add=TRUE,lwd=2)
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("top",legend=map.legend,ncol=5,fill=map.colors,title=map.title,cex=2,bty='n')
dev.off()

## CO emissions pie chart
load(file.Rdata)
df <- data.frame(sector=c("Biogenics","Wildfires","Other Fires","Fuel Comb","Industrial",
  "Onroad","Non-Road","Other"),emis=NA)
df$emis[1] <- sector.data$CO[grep("Biogenics",sector.data$sector)]
df$emis[2] <- sector.data$CO[grep("Wildfires",sector.data$sector)]
df$emis[3] <- sum(sector.data$CO[c(grep("Fires - Ag",sector.data$sector),
  grep("Fires - Pre",sector.data$sector))])
df$emis[4] <- sum(sector.data$CO[grep("Fuel Comb -",sector.data$sector)])
df$emis[5] <- sum(sector.data$CO[grep("Industrial Processes -",sector.data$sector)])
df$emis[6] <- sum(sector.data$CO[grep("Mobile - On-Road",sector.data$sector)])
df$emis[7] <- sum(sector.data$CO[setdiff(grep("Mobile -",sector.data$sector),
  grep("On-Road",sector.data$sector))])
df$emis[8] <- sum(sector.data$CO,na.rm=TRUE)-sum(df$emis[1:7])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Biogenics","Wildfires","Agricultural &\nPrescribed Fires",
  "Stationary Fuel\n Combustion","Industrial\nProcesses","Highway Vehicles",
  "Non-Road Mobile","Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("CO Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/CO_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## NOx emissions pie chart
df <- data.frame(sector=c("Biogenics","Fires","Fuel Comb","Industrial",
  "Onroad","Non-Road","Other"),emis=NA)
df$emis[1] <- sector.data$NOx[grep("Biogenics -",sector.data$sector)]
df$emis[2] <- sum(sector.data$NOx[grep("Fires -",sector.data$sector)])
df$emis[3] <- sum(sector.data$NOx[grep("Fuel Comb",sector.data$sector)])
df$emis[4] <- sum(sector.data$NOx[grep("Industrial Processes -",sector.data$sector)])
df$emis[5] <- sum(sector.data$NOx[grep("Mobile - On-Road",sector.data$sector)])
df$emis[6] <- sum(sector.data$NOx[setdiff(grep("Mobile -",sector.data$sector),
  grep("On-Road",sector.data$sector))])
df$emis[7] <- sum(sector.data$NOx,na.rm=TRUE)-sum(df$emis[1:6])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Biogenics","All Fires","Stationary Fuel Combustion","Industrial\nProcesses",
  "Highway Vehicles","Non-Road Mobile","Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("NOx Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/NOx_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## Pb emissions pie chart
load(file.Rdata)
df <- data.frame(sector=c("Aircraft","Industrial","Fuel Comb","Other"),emis=NA)
df$emis[1] <- sector.data$Pb[grep("Mobile - Aircraft",sector.data$sector)]
df$emis[2] <- sum(sector.data$Pb[grep("Industrial Processes -",sector.data$sector)])
df$emis[3] <- sum(sector.data$Pb[grep("Fuel Comb -",sector.data$sector)])
df$emis[4] <- sum(sector.data$Pb,na.rm=TRUE)-sum(df$emis[1:3])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Mobile - Aircraft","Industrial Processes","Stationary Fuel\nCombustion",
  "Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/2000)),big.mark=",")
pie.title <- paste("Pb Emissions (",pie.total," Tons/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/Pb_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## PM10 emissions pie chart
df <- data.frame(sector=c("Wildfires","Other Fires","Unpaved Roads","Paved Roads","Construction",
  "Agriculture","Industrial","Fuel Comb","Mobile","Other"),emis=NA)
df$emis[1] <- sector.data$PM10[grep("Wildfires",sector.data$sector)]
df$emis[2] <- sum(sector.data$PM10[c(grep("Fires - Ag",sector.data$sector),
  grep("Fires - Pre",sector.data$sector))])
df$emis[3] <- sector.data$PM10[grep("Unpaved Road Dust",sector.data$sector)]
df$emis[4] <- sector.data$PM10[grep("Paved Road Dust",sector.data$sector)]
df$emis[5] <- sector.data$PM10[grep("Construction Dust",sector.data$sector)]
df$emis[6] <- sector.data$PM10[grep("Agriculture - Crops",sector.data$sector)]
df$emis[7] <- sum(sector.data$PM10[grep("Industrial Processes -",sector.data$sector)])
df$emis[8] <- sum(sector.data$PM10[grep("Fuel Comb -",sector.data$sector)])
df$emis[9] <- sum(sector.data$PM10[grep("Mobile -",sector.data$sector)])
df$emis[10] <- sum(sector.data$PM10,na.rm=TRUE)-sum(df$emis[1:9])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Wildfires","Agricultural &\n Prescribed Fires","Unpaved Road Dust",
  "Paved Road\nDust","Construction Dust","Crops & Livestock Dust","Industrial\nProcesses",
  "Stationary Fuel\nCombustion","Mobile Sources","Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("Primary PM10 Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/PM10_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## PM2.5 emissions pie chart
df <- data.frame(sector=c("Wildfires","Other Fires","Unpaved Roads","Paved Roads","Construction",
  "Agriculture","Fuel Comb","Industrial","Mobile","Waste Disposal","Other"),emis=NA)
df$emis[1] <- sector.data$PM25[grep("Wildfires",sector.data$sector)]
df$emis[2] <- sum(sector.data$PM25[c(grep("Fires - Ag",sector.data$sector),
  grep("Fires - Pre",sector.data$sector))])
df$emis[3] <- sector.data$PM25[grep("Unpaved Road Dust",sector.data$sector)]
df$emis[4] <- sector.data$PM25[grep("Paved Road Dust",sector.data$sector)]
df$emis[5] <- sector.data$PM25[grep("Construction Dust",sector.data$sector)]
df$emis[6] <- sector.data$PM25[grep("Agriculture - Crops",sector.data$sector)]
df$emis[7] <- sum(sector.data$PM25[grep("Fuel Comb -",sector.data$sector)])
df$emis[8] <- sum(sector.data$PM25[grep("Industrial Processes -",sector.data$sector)])
df$emis[9] <- sum(sector.data$PM25[grep("Mobile -",sector.data$sector)])
df$emis[10] <- sector.data$PM25[grep("Waste Disposal",sector.data$sector)]
df$emis[11] <- sum(sector.data$PM25,na.rm=TRUE)-sum(df$emis[1:10])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Wildfires","Agricultural &\n Prescribed Fires","Unpaved Road\nDust",
  "Paved Road Dust","Construction Dust","Crops & Livestock Dust","Stationary Fuel Combustion",
  "Industrial Processes","Mobile Sources","Waste Disposal","Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("Primary PM2.5 Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/PM25_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## SO2 emissions pie chart
df <- data.frame(sector=c("Wildfires","Other Fires","Fuel Comb Coal","Fuel Comb Other",
  "Industrial","Mobile","Other"),emis=NA)
df$emis[1] <- sector.data$SO2[grep("Wildfires",sector.data$sector)]
df$emis[2] <- sum(sector.data$SO2[c(grep("Fires - Ag",sector.data$sector),
  grep("Fires - Pre",sector.data$sector))])
df$emis[3] <- sum(sector.data$SO2[intersect(grep("Fuel Comb",sector.data$sector),
  grep("Coal",sector.data$sector))])
df$emis[4] <- sum(sector.data$SO2[setdiff(grep("Fuel Comb",sector.data$sector),
  grep("Coal",sector.data$sector))])
df$emis[5] <- sum(sector.data$SO2[grep("Industrial Processes -",sector.data$sector)])
df$emis[6] <- sum(sector.data$SO2[grep("Mobile -",sector.data$sector)])
df$emis[7] <- sum(sector.data$SO2,na.rm=TRUE)-sum(df$emis[1:6])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Wildfires","Agricultural &\nPrescribed Fires",
  "Stationary Fuel\nCombustion: Coal","Stationary Fuel \nCombustion: Other",
  "Industrial Processes","Mobile Sources","Other")," ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("SO2 Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/SO2_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## VOC emissions piechart
df <- data.frame(sector=c("Biogenics","Wildfires","Other Fires","Solvents","Industrial",
  "Mobile","Other"),emis=NA)
df$emis[1] <- sector.data$VOC[grep("Biogenics",sector.data$sector)]
df$emis[2] <- sector.data$VOC[grep("Wildfires",sector.data$sector)]
df$emis[3] <- sum(sector.data$VOC[c(grep("Fires - Ag",sector.data$sector),
  grep("Fires - Pre",sector.data$sector))])
df$emis[4] <- sum(sector.data$VOC[grep("Solvent -",sector.data$sector)])
df$emis[5] <- sum(sector.data$VOC[grep("Industrial Processes",sector.data$sector)])
df$emis[6] <- sum(sector.data$VOC[grep("Mobile - ",sector.data$sector)])
df$emis[7] <- sum(sector.data$VOC,na.rm=TRUE)-sum(df$emis[1:6])
df$pct <- 100*df$emis/sum(df$emis)
pie.labels <- paste(c("Biogenics","Wildfires","Agricultural & Prescribed Fires",
  "Solvent Utilization","Industrial\nProcesses","Mobile\nSources","Other"),
  " ",round(df$pct),"%",sep="")
pie.total <- format(round(sum(df$emis/1000)),big.mark=",")
pie.title <- paste("VOC Emissions (",pie.total," kTon/year)",sep="")
file.name <- paste("emissions_piecharts/",nei.year,"/VOC_piechart_",nei.year,".jpeg",sep="")
jpeg(file=file.name,width=600,height=400,quality=100)
par(mar=c(0,5,2,5),cex=1.25,cex.main=1.5,bg="gray95")
pie(x=df$emis,labels=pie.labels,main=pie.title,col=pie.colors,radius=0.9)
dev.off()

## CO emissions trends figure
load(trends.Rdata)
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$CO[match(source.cat,trends.data$CO$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/CO_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,90000),ylab="CO Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,90000,10000),labels=seq(0,90000,10000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,90000,90000),col="gray80")
abline(h=seq(5000,85000,5000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

## NOx emissions trends figure
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$NOx[match(source.cat,trends.data$NOx$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/NOx_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,24000),ylab="NOx Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,24000,4000),labels=seq(0,24000,4000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,24000,24000),col="gray80")
abline(h=seq(2000,22000,2000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

## Pb emissions trends figure - data pulled from Air Trends report
trend.year <- ((curr.year - 1) %/% 3) * 3 + 1;
df <- read.csv(paste(base.dir,"data/",curr.year,"/Lead_emissions_1990_",trend.year,".csv",sep=""))
source.cat <- c("Highway.Vehicles","Non.Road.Mobile","Stationary.Fuel.Combustion",
                "Industrial.and.Other.Processes")
df2 <- apply(df[,source.cat],1,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes")
file.name <- paste(base.dir,"emissions_trends/",curr.year,"/Pb_trends_1990_",trend.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(1990,trend.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,5),ylab="Pb Emissions (kTons/year)")
axis(side=1,at=seq(1990,trend.year,3),labels=seq(1990,trend.year,3))
axis(side=2,at=seq(0,5,1),labels=seq(0,5,1))
polygon(x=c(1990,rep(trend.year,2),1990),y=c(0,0,5,5),col="gray80")
abline(h=seq(0.5,4.5,0.5),v=seq(1990,trend.year,3),col="white")
polygon(x=c(seq(1990,trend.year,3),seq(trend.year,1990,-3)),y=c(df2[1,],rep(0,((trend.year-1990)/3+1))),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df2)) {
  polygon(x=c(seq(1990,trend.year,3),seq(trend.year,1990,-3)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df2)) { lines(x=seq(1990,trend.year,3),y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=rev(source.labels),fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();

## PM10 emissions trends figure
t <- trends.data$PM10
t <- rbind(t,c(NA,apply(t[1:3,-1],2,sum)),c(NA,apply(t[4:10,-1],2,sum)),
  c(NA,apply(t[11:12,-1],2,sum)))
t$source[20:22] <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation")
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- t[match(source.cat,t$source),c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/PM10_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,24000),ylab="Primary PM10 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,24000,4000),labels=seq(0,24000,4000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,24000,24000),col="gray80")
abline(h=seq(2000,22000,2000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

## PM2.5 emissions trends figure
t <- trends.data$PM25
t <- rbind(t,c(NA,apply(t[1:3,-1],2,sum)),c(NA,apply(t[4:10,-1],2,sum)),
  c(NA,apply(t[11:12,-1],2,sum)))
t$source[20:22] <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation")
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- t[match(source.cat,t$source),c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/PM25_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,7000),ylab="PM2.5 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,7000,1000),labels=seq(0,7000,1000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,7000,7000),col="gray80")
abline(h=seq(500,7000,500),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

## SO2 emissions trends figure
source.cat <- c("Stationary fuel combustion","Industrial and other processes",
  "Transportation","Miscellaneous without wildfires")
df <- trends.data$SO2[match(source.cat,trends.data$SO2$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Stationary Fuel Combustion","Industrial and Other Processes",
  "Transportation","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/SO2_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,16000),ylab="SO2 Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,16000,3000),labels=seq(0,16000,3000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,16000,16000),col="gray80")
abline(h=seq(1500,16000,1500),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[2],border=trend.colors[2])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i+1],border=trend.colors[i+1])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i+1],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors[2:5]),
  border=rev(trend.border[2:5]),bty='n')
box(); dev.off();

## VOC emissions trends figure
source.cat <- c("HIGHWAY VEHICLES","OFF-HIGHWAY","Stationary fuel combustion",
  "Industrial and other processes","Miscellaneous without wildfires")
df <- trends.data$VOC[match(source.cat,trends.data$VOC$source),
  c("source",as.character(2002:curr.year))]
df2 <- apply(df[c(nrow(df):1),-1],2,cumsum)
source.labels <- c("Highway Vehicles","Non-Road Mobile","Stationary Fuel Combustion",
  "Industrial and Other Processes","Other Anthropogenic Sources")
file.name <- paste("emissions_trends/",curr.year,"/VOC_trends_2002_",curr.year,".jpeg",sep="")
jpeg(file=file.name,width=800,height=600,quality=100)
par(mar=c(5,5,1,1),mgp=c(3.5,0.7,0),cex=1.5,las=2)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(2002,curr.year),
  xlab="Inventory Year",yaxs='i',ylim=c(0,24000),ylab="VOC Emissions (kTons/year)")
axis(side=1,at=seq(2002,curr.year,3),labels=seq(2002,curr.year,3))
axis(side=2,at=seq(0,24000,4000),labels=seq(0,24000,4000))
polygon(x=c(2002,rep(curr.year,2),2002),y=c(0,0,24000,24000),col="gray80")
abline(h=seq(2000,22000,2000),v=trend.years,col="white")
polygon(x=c(trend.years,rev(trend.years)),y=c(df2[1,],rep(0,ny)),
  col=trend.colors[1],border=trend.colors[1])
for (i in 2:nrow(df)) {
  polygon(x=c(trend.years,rev(trend.years)),y=c(df2[i,],rev(df2[(i-1),])),
    col=trend.colors[i],border=trend.colors[i])
}
for (i in 1:nrow(df)) { lines(x=trend.years,y=df2[i,],col=trend.border[i],lwd=2) }
legend("topright",legend=source.labels,fill=rev(trend.colors),border=rev(trend.border),bty='n')
box(); dev.off();
