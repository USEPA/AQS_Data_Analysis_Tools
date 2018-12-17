## Source functions, set 'proj.args' argument
source("rmapfuns.r")
pa <- list(projection='albers',parameters=c(33,45),orientation=c(90,0,-100))

## Examples using 'draw.map' function

## National map - Standard lat/lon projection
draw.map("state")

## National map - Albers equal-area projection
draw.map("state",proj.args=pa)

## North America - use 'maps' and 'mapdata' packages for international borders
pa2 <- list(projection='lambert',parameters=c(30,60),orientation=c(90,0,-100))
draw.map("state",rescale=FALSE,proj.args=pa2)
map("worldHires",add=TRUE,projection=pa2[[1]],parameters=pa2[[2]],orientation=pa2[[3]])

## National map without Alaska, Hawaii, and Puerto Rico
x <- state.abb[-which(state.abb %in% c("AK","HI","PR"))]
draw.map("state",x,id="code")
draw.map("state",x,id="code",proj.args=pa)

## Add layers to an existing map using 'add=TRUE'
draw.map("state",proj.args=pa)
draw.map("cbsa",add=TRUE,proj.args=pa,col="cyan",lwd=2)
draw.map("csa",add=TRUE,proj.args=pa,col="gray75",lwd=2)
draw.map("county",add=TRUE,proj.args=pa)
draw.map("state",add=TRUE,proj.args=pa,border="blue",lwd=2)

## Add CBSAs/CSAs to state-level maps
draw.map("state",c("NC","SC"),id="code",hires=TRUE)
draw.map("cbsa",c("NC","SC"),id="code",add=TRUE,hires=TRUE,col="cyan",lwd=2)
draw.map("csa",c("NC","SC"),id="code",add=TRUE,hires=TRUE,col="gray75",lwd=2)
draw.map("county",c("NC","SC"),id="code",hires=TRUE,add=TRUE)
draw.map("state",add=TRUE,hires=TRUE,border="blue",lwd=2)

## State/local maps - use 'hires' option for more detail
draw.map("state","NC",id="code",border="gray50",lwd=2)  ## North Carolina - standard resolution
draw.map("state","NC",id="code",add=TRUE,hires=TRUE)  ## North Carolina - high resolution

draw.map("county","Wake",id="name",border="gray50",lwd=2)  ## Wake County, NC - standard resolution
draw.map("county","Wake",id="name",add=TRUE,hires=TRUE)  ## Wake County, NC - high resolution

## Be careful using id="name"!!!
draw.map("state",proj.args=pa)
draw.map("county","Lincoln",id="name",add=TRUE,proj.args=pa,col="blue")

## Set 'plot=FALSE' to resolve potential mis-matches
draw.map("county","Lincoln",id="name",plot=FALSE)
draw.map("state",proj.args=pa)
draw.map("county","Lincoln County, North Carolina",id="name",add=TRUE,proj.args=pa,col="green3")

## Better method - use id="fips"
draw.map("state",proj.args=pa)
draw.map("county","37109",id="fips",add=TRUE,proj.args=pa,col="green3")

## Examples using 'add.layer' function

## Type="points" example: State map with points showing monitor locations
load("ozone_monitors_1980_2017.Rdata")
draw.map("state",proj.args=pa)
add.layer(type="points",x=monitors$longitude,y=monitors$latitude,
  proj.args=pa,pch=16,col="blue",cex=0.5)

## Type="polygons" example: Map of PM2.5 nonattainment areas using GreenBook shapefiles
draw.map("state",proj.args=pa)
add.layer(shapefile="PM25_2012Std_naa.shp",proj.args=pa,col="gray50")
add.layer(shapefile="PM25_2006Std_naa.shp",proj.args=pa,angle=45,density=30)

## Using the 'subset' argument: 2008 ozone nonattainment areas in California
dbf.data <- read.dbf("Ozone_8hr_2008std_naa.dbf")[[1]]
naa.index <- grep("CA",dbf.data$AREA_NAME)
monitor.index <- which(monitors$state_name == "California")

draw.map("county","CA",id="code",add=TRUE,hires=TRUE,border="gray75")
add.layer(shapefile="Ozone_8hr_2008std_naa.shp",subset=naa.index,col="pink")
draw.map("state","CA",id="code",add=TRUE,hires=TRUE,border="blue",lwd=2)
add.layer(type="points",x=monitors$longitude,y=monitors$latitude,
  subset=monitor.index,pch=16,col="blue")

## Examples using 'assign.colors' function

## County map colored by population density
data <- draw.map("county",plot=FALSE)
data$density <- data$population/data$area_km
data$color <- assign.colors(log(data$density,base=10),range=c(-1,4))
draw.map("state",proj.args=pa)
for (i in 1:nrow(data)) {
  draw.map("county",data$fips[i],add=TRUE,proj.args=pa,
    col=data$color[i],border="gray75")
}
draw.map("state",add=TRUE,lwd=2,proj.args=pa)

## Map of 2015-2017 ozone design values by monitoring site
load("ozone_8hrdv_2006_2017.Rdata")
dvs <- subset(out,code.2015.2017 != "I",c("site","latitude","longitude","dv.2015.2017"))
dvs$color <- assign.colors(dvs$dv.2015.2017,discrete=TRUE,breaks=c(61,66,71,76),
  palette=c("blue","green","yellow","orange","red"))
dvs <- dvs[order(dvs$dv.2015.2017),]  ## Sort to place highest DVs on top
base.txt <- c("<= 60 ppb","61 - 65 ppb","66 - 70 ppb","71 - 75 ppb"," > 75 ppb")
n.sites <- summary(as.factor(dvs$color))[c(1,2,5,3,4)]
legend.lab <- paste(base.txt," (",n.sites," sites)",sep="")

## Use 'layout' to save space at the bottom for the legend
layout(mat=matrix(c(1,2),2,1),widths=1,heights=c(0.88,0.12))
draw.map("state",proj.args=pa)
add.layer(type="points",x=dvs$longitude,y=dvs$latitude,proj.args=pa,pch=21,bg=dvs$color)
plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=c(0,1),ylim=c(0,1))
legend("bottom",legend=legend.lab,pt.bg=c("blue","green","yellow","orange","red"),
  ncol=3,title="2015 - 2017 Ozone DVs",pch=21,pt.cex=1.5,bty='n')
