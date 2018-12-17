############################################################################
## draw.map - Plot maps of various geographical areas within the USA
##
## Arguments:
##
## type - type of geographic areas to be drawn.
##   Possible values:
##     "state" - draw state boundaries
##     "county" - draw county and equivalent boundaries
##     "cbsa" - draw Core Based Statistical Area boundaries
##     "csa" - draw Combined Statistical Area boundaries
##
## x - character vector describing the geographic areas to be plotted.
##   If NULL (default), all geographic areas of 'type' are drawn.
##
## id - id variable used to match 'x' with geographic areas.
##   Possible values:
##     "fips" - match based on FIPS codes (default)
##     "code" - match based on state postal abbreviations
##     "name" - match based on geographical names
##
## add - logical, should the map be added to an existing plot window?
##   If FALSE (default), the plot window is established internally.
##
## hires - logical, if TRUE high resolution maps (1:500,000) are drawn
##   instead of the standard lower resolution maps (1:20,000,000).
##   Recommendation is to use high resolution when drawing state or local
##   maps and low resolution when drawing regional or national maps.
##
## plot - logical, if FALSE plotting is suppressed, and a data.frame with
##   information on the geographic areas matched with 'x' is returned.
##
## rescale - logical, if TRUE areas in Alaska, Hawaii, and Puerto Rico are
##   re-scaled so that they fit nicely on a map with the lower 48 states.
##
## proj.args - list of arguments be passed to the 'mapproject' function in
##   the 'mapproj' package.
##
## ... - additional arguments passed to 'polygon'.
##
## Return value: If plot=TRUE, a map of the geographical areas in 'x' (or a
##   layer added to an existing map, if add=TRUE). If plot=FALSE, a
##   data.frame containing information on the geographic areas in 'x'.
############################################################################
if (!exists("map.data.20m")) { load("map_data_20m.Rdata") }
if (!exists("map.data.500k")) { load("map_data_500k.Rdata")}

draw.map <- function(type="state",x,id="fips",add=FALSE,hires=FALSE,
  plot=TRUE,rescale=TRUE,proj.args,...) {
  if (!(tolower(type) %in% c("state","county","cbsa","csa"))) {
    stop("'type' must be one of 'state', 'county', 'cbsa', or 'csa'")
  }
  if (!(tolower(id) %in% c("fips","code","name"))) {
    stop("'id' must be one of 'fips', 'code', or 'name'")
  }
  if (hires) {
    map.data <- map.data.500k
  }
  if (!hires) {
    map.data <- map.data.20m
  }
  info <- map.data[[paste(tolower(type),"info",sep=".")]]
  shape <- map.data[[paste(tolower(type),"shape",sep=".")]]
  if (missing(x)) {
    info.x <- info
    shape.x <- shape
  }
  if (!missing(x)) {
    x <- as.character(unique(x[order(x)]))
    info.x <- info[unique(unlist(sapply(tolower(x),grep,tolower(info[,id])))),]
    if (nrow(info.x) == 0) { stop("no matches found with 'x'") }
    info.x <- info.x[order(info.x$fips),]
    shape.x <- shape[which(shape$fips %in% info.x$fips),]
  }
  if (!plot) {
    return(info.x[,1:5])
  }
  if (rescale) {
    t1 <- which(info.x$center.x != 0 | info.x$center.y != 0 |
      info.x$scale.y != 1 | info.x$scale.y != 1)
    n <- length(t1)
    if (n > 0) {
      t2 <- info.x[t1,]
      for (i in 1:n) {
        t3 <- which(shape.x$fips == t2$fips[i])
        shape.x$lon[t3] <- scale(shape.x$lon[t3],t2$center.x[i],t2$scale.x[i])
        shape.x$lat[t3] <- scale(shape.x$lat[t3],t2$center.y[i],t2$scale.y[i])
      }
    }
  }
  if (!missing(proj.args)) {
    require(mapproj)
    temp <- mapproject(x=shape.x$lon,y=shape.x$lat,projection=proj.args$projection,
      parameters=proj.args$parameters,orientation=proj.args$orientation)
    shape.x$lon <- temp$x
    shape.x$lat <- temp$y
  }
  if (!add) {
    xlim <- c(min(shape.x$lon)+diff(range(shape.x$lon))*0.03,
      (max(shape.x$lon)-diff(range(shape.x$lon))*0.03))
    ylim <- c((min(shape.x$lat)+diff(range(shape.x$lat))*0.03),
      (max(shape.x$lat)-diff(range(shape.x$lat))*0.03))
    par(mar=c(0,0,0,0))
    plot(x=NULL,y=NULL,type='n',axes=FALSE,xlim=xlim,ylim=ylim)
  }
  poly.list <- unique(data.frame(fips=shape.x$fips,id=shape.x$id))
  for (i in 1:nrow(poly.list)) {
    t <- which(shape.x$fips == poly.list$fips[i] & shape.x$id == poly.list$id[i])
    polygon(x=shape.x$lon[t],y=shape.x$lat[t],...)
  }
  invisible(NULL)
}

############################################################################
## add.layer - add points, lines, or polygons to an existing plot or map
##
## Arguments:
##
## shapefile - name of file ("path/shapefile.shp") containing points, lines,
##   or polygons to be plotted or output. If specified, 'type', 'x', 'y',
##   and 'id' are determined internally.
##
## type - type of layer to be added ("points", "lines", or "polygon").
##
## x,y - coordinates (e.g. lon,lat) of the points, lines, or polygons.
##
## id - optional vector of ID variables for matching pairs of coordinates to
##   individual lines or polygons. Ignored if type="points". Note 'x', 'y',
##   and 'id' must all have the same length.
##
## subset - numeric vector specifying a subset of points, lines, or polygons
##   to plot or output. If type="points", only those elements of 'x' and 'y'
##   in 'subset' are plotted or output.  If type="lines" or type="polygon",
##   only those objects with 'id' values in 'subset' are plotted. Generally,
##   'id' values are determined from the database file (.dbf) accompanying
##   the shapefile (.shp).
##
## plot - logical, if FALSE plotting is suppressed and a data.frame with
##   'id', 'x', and 'y' is returned.
##
## rescale - logical, if TRUE points in Alaska, Hawaii, and Puerto Rico are
##   re-scaled so that they fit nicely on a map with the lower 48 states.
##
## proj.args - list of arguments be passed to the 'mapproject' function in
##   the 'mapproj' package.
##
## ... - additional arguments passed to 'points', 'lines', or 'polygon'
##
## Return value: A layer of points, lines, or polygons added to an existing
##   plot, if plot=TRUE.  If plot=FALSE, a data.frame object with columns
##   'id', 'x', and 'y'.
############################################################################

add.layer <- function(shapefile,type,x,y,id,subset,
  plot=TRUE,rescale=TRUE,proj.args,...) {
  if (!missing(shapefile)) {
    require(shapefiles)
    require(data.table)
    if (!file.exists(shapefile)) {
      stop("Shapefile not found")
    }
    shape.in <- read.shp(shapefile)
    st <- shape.in$header$shape.type
    type <- ifelse(st %in% c(1,11),"points",ifelse(st %in% c(3,13),"lines",
      ifelse(st %in% c(5,15),"polygon",stop("Invalid shapefile type"))))
    shape <- convert.to.simple(shape.in)
    if (type == "points") {
      if (missing(subset)) {
        subset <- c(1:nrow(shape))
      }
      x <- shape$X[subset]
      y <- shape$Y[subset]
    }
    if (type != "points") {
      if (missing(subset)) {
        subset <- c(1:max(shape$Id))
      }
      shapes <- unique(shape$Id)[subset]
      temp <- vector("list",max(shape$Id))
      for (i in shapes) {
        t <- shape[which(shape$Id == i),]
        n <- shape.in[[1]][[i]]$num.parts
        p <- shape.in[[1]][[i]]$parts
        t$Part <- 1
        if (n > 1) {
          for (j in 2:n) {
            pmin <- p[j] + 1
            pmax <- ifelse(j < n,p[(j+1)],nrow(t))
            t$Part[pmin:pmax] <- j
          }
        }
        temp[[i]] <- t[,c("Id","Part","X","Y")]
      }
      shape <- data.frame(rbindlist(temp))
      id <- paste(shape$Id,shape$Part,sep="_")
      x <- shape$X
      y <- shape$Y
    }
  }
  if (!(tolower(type) %in% c("points","lines","polygon"))) {
    stop("'type' must be one of 'points', 'lines', or 'polygon'")
  }
  if (missing(x) | missing(y)) {
    stop("'x' and 'y' are required arguments")
  }
  if (missing(shapefile) & !missing(subset)) {
    if (missing(id)) {
      x <- x[subset]
      y <- y[subset]
    }
    if (!missing(id)) {
      id.list <- unique(id)[subset]
      t <- which(id %in% id.list)
      id <- id[t]
      x <- x[t]
      y <- y[t]
    }
  }
  if (rescale) {
    ak <- which(x < -125 & y > 50)
    hi <- which(x < -125 & y < 30)
    pr <- which(x > -70 & y < 20)
    if (length(ak) > 0) {
      x[ak] <- scale(x[ak],186,3)
      y[ak] <- scale(y[ak],-21,3)
    }
    if (length(hi) > 0) {
      x[hi] <- scale(x[hi],-65,1)
      y[hi] <- scale(y[hi],-6,1)
    }
    if (length(pr) > 0) {
      x[pr] <- scale(x[pr],-23.5,0.5)
      y[pr] <- scale(y[pr],5,0.5)
    }
  }
  if (!missing(proj.args)) {
    require(mapproj)
    temp <- mapproject(x,y,projection=proj.args$projection,
      parameters=proj.args$parameters,orientation=proj.args$orientation)
    x <- temp$x
    y <- temp$y
  }
  if (!plot) {
    if (missing(id)) {
      out <- data.frame(x,y)
    }
    if (!missing(id)) {
      out <- data.frame(id,x,y)
    }
    return(out)
  }
  if (missing(id)) {
    do.call(type,list(x=x,y=y,...))
  }
  if (!missing(id)) {
    id.list <- unique(id)
    for (i in 1:length(id.list)) {
      t <- which(id == id.list[i])
      do.call(type,list(x=x[t],y=y[t],...))
    }
  }
  invisible(NULL)
}

############################################################################
## assign.colors - assign discrete or semi-continuous color values based on
##   numeric input values. Intended to assign color values to spatial points
##   (x,y) based on values of variable z for plotting purposes.
##
## Arguments:
##
## z - numeric vector, matrix, or array with at least one finite value
##
## discrete - logical, if FALSE, semi-continuous color values are assigned
##   over equally-spaced intervals of z based on 'range' and 'n.colors'.
##   If TRUE, discrete color values are assigned based on 'breaks'.
##
## range - vector of the form c(min,max), where 'min' is the lowest value
##   assigned to the first color in the palette, and 'max' is the highest
##   value assigned to the last color in the palette. If 'min' < 'max', 
##   then colors will be assigned in reverse order. Values in 'z' falling
##   outside of 'range' are set to 'min' or 'max'.  Defaults to the range
##   of finite values in 'z', used only if 'discrete=FALSE'.
##
## breaks - vector of breakpoints used for assigning discrete intervals to
##   colors.  Note that the lowest interval includes all values less than
##   the first element, and the highest interval inclues all values greater
##   than the highest element.  Defaults to equally-spaced intervals over
##   the range of 'z', used only if 'discrete=TRUE'.
##
## palette - either a string specifying the color palette to be used, or
##   a character vector specifying specific colors to be used. Default is
##   the 'tim.colors' palette in the 'fields' package.
##
## na.color - color value to be assigned to NA values (if any) in 'x',
##   default is "#FFFFFF" (white).
##
## n.colors - number of colors in the palette.  Overridden if 'breaks' is
##   set, or if 'palette' is a vector.  Default is 256 colors.
##
## Return value: a vector, matrix, or array of color values with the same
##   length or dimensions as 'x'.
############################################################################

assign.colors <- function(z,discrete=FALSE,range=NULL,breaks=NULL,
  palette="tim.colors",na.color="#FFFFFF",n.colors=256) {
  require(fields)
  colors <- rep(na.color,length(z))
  d <- dim(z)
  n.colors <- floor(pmin(pmax(as.numeric(n.colors),2),256))
  if (length(palette) == 1) {
    cols <- do.call(palette,list(n.colors))
  }
  if (length(palette) > 1) {
    cols <- palette
  }
  if (n.colors != length(cols)) {
    n.colors <- length(cols)
  }
  if (!discrete) {
    if (is.null(range)) {
      range <- range(z,finite=TRUE)
    }
    scope <- diff(range[1:2])
    z <- as.numeric(pmin(pmax(as.numeric(z),range[1]),range[2]))
    for (i in 1:n.colors) {
      t <- which((z >= range[1] + (i-1)*scope/n.colors) &
        (z <= range[1] + i*scope/n.colors))
      if (length(t) > 0) {
        colors[t] <- cols[i]
      }
    }
  }
  if (discrete) {
    if (is.null(breaks)) {
      breaks <- seq(range(z,finite=TRUE),length.out=(n.colors-1))
    }
    r <- length(breaks)+1
    if (r != n.colors) {
      if (r < n.colors) {
        cols <- cols[1:r]
      }
      if (r > n.colors) {
        k <- ceiling(r/n.colors)
        cols <- rep(cols,each=k)[1:r]
      }
      n.colors <- r
    }
    t <- which(z <= breaks[1])
    if (length(t) > 0) {
      colors[t] <- cols[1]
    }
    for (i in 2:(n.colors-1)) {
      t <- which((z >= breaks[(i-1)]) & (z <= breaks[i]))
      if (length(t) > 0) {
        colors[t] <- cols[i]
      }
    }
    t <- which(z >= breaks[(n.colors-1)])
    if (length(t) > 0) {
      colors[t] <- cols[n.colors]
    }
  }
  if (!is.null(d)) { dim(colors) <- d }
  return(colors)
}
