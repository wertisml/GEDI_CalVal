## calculate the grid convergence (first order only)
get_grid_convergence <- function(latitude,longitude,utmzone){
  centralmeridian <- -183 + utmzone * 6
  sin(latitude*pi/180) * ((longitude*pi/180) - (centralmeridian*pi/180))
}

## determine UTM/WGS84 epsg code to use for a plot location
get_utmwgs84_epsg <- function(longitude,latitude) {
  #browser()
  utmzone <- ( floor( (longitude + 180) / 6 ) %% 60 ) + 1
  epsg_base <- ifelse(latitude < 0, 32700, 32600)
  epsg <- epsg_base + utmzone
  epsg
}

convert_projection <- function(x,y,src_epsg,tar_epsg) {
  #browser()
  d <- data.frame(x=x, y=y)
  d <- vect(d, geom = c("x", "y"), crs = paste0("EPSG:",src_epsg))
  d <- project(d, paste0("EPSG:",tar_epsg))
  crds(d)

}

geo2utm <- function(lon,lat) {
  #browser()
  d <- data.frame(lon=lon, lat=lat)
  d <- vect(d, geom = c("lon", "lat"), crs = "+init=epsg:4326")
  utmepsg <- get_utmwgs84_epsg(lon,lat)
  d <- project(d, paste("+init=epsg",utmepsg[1],sep=":"))
  crds(d)
}

## convenience function to convert UTM to geographic coordinates
utm2geo <- function(x,y,utmzone,south) {
  #browser()
  d <- data.frame(x=x, y=y)
  epsg <- ifelse(south, 32500, 32600)
  d <- vect(d, geom = c("x", "y"), crs = paste0("+init=epsg:",epsg+utmzone))
  d <- project(d, paste0("EPSG:",4326))
  crds(d)
}

epsg2geo <- function(x,y,epsg) {
  #browser()
  d <- data.frame(x=x, y=y)
  d <- vect(d, geom = c("x", "y"), crs = paste0("EPSG:",epsg))  
  d <- project(d, "+init=epsg:4326")
  crds(d)
}

## calculate the area of a polygon from a geom string
get_parea <- function(x) {
  #browser()
  if ( !is.na(x) ) {
    p <- vect(x)
    expanse(p)
  } else {
    NA
  }
}

## Rotates x,y points by the given angle in degrees
## Translation to origin assumed
rotate2D <- function(x, y, phi, xt=0, yt=0) {
  #browser()
  phi <- phi*pi/180
  xr <- ( x * cos(phi) + y * sin(phi) ) + xt
  yr <- ( y * cos(phi) - x * sin(phi) ) + yt
  list(x=xr, y=yr)
}

## create a geometry for a circular plot
create_circular_plot_geom <- function(easting,northing,radius,quadsegs=10) {
  #browser()
  p.geom <- data.frame(x=easting, y=northing)
  p.geom <- vect(p.geom, geom = c("x", "y"), crs = "")
  p.geom <- terra::buffer(p.geom, width=radius, quadsegs=quadsegs)
  geom(p.geom, wkt=T, list=T)
}

## Need to work on this
reproject_wkt <- function(wkt,src_epsg,tar_epsg) {
  #browser()
  geom <- vect(wkt, crs = paste0("EPSG:",src_epsg))
  geom <- project(geom, paste0("+init=epsg:",tar_epsg))
  geom(geom, wkt=T, list=T)
}

## reproject a set of point coordinates
reproject_pts <- function(x,y,src_epsg,tar_epsg) {
  #browser()
  reproject_fun <- function(i) {
    i <- vect(i, geom = c("x", "y"), crs = paste0("+init=epsg:",i$src_epsg[1]))
    it <- project(i, paste("+init=epsg",i$tar_epsg[1],sep=":"))
    data.frame(crds(it), id=1:length(x))
  }
  xy <- data.frame(x=x, y=y, src_epsg=src_epsg, tar_epsg=tar_epsg)
  xyt <- plyr::ddply(xy, c("src_epsg","tar_epsg"), reproject_fun)
  xyt <- plyr::arrange(xyt, id)
  data.frame(x=xyt$x,y=xyt$y)
}

## Retrieve an estimate of the magnetic declination.
get_magnetic_declination <- function(latitude,longitude,year,month,day) {
  #browser()
  
  # query the magnetic declination
  urlstr <- paste("http://www.ngdc.noaa.gov/geomag-web/calculators/calculateDeclination?",
                  "lat1=",latitude,"&",
                  "lon1=",longitude,"&",
                  "model=","IGRF","&",
                  "startYear=",year,"&",
                  "startMonth=",month,"&",
                  "startDay=",day,"&",
                  "resultFormat=csv",sep="")
  
  qry <- getURLContent(urlstr, .opts=curlOptions(followlocation=TRUE))
  testraw1= readBin(qry,what='characer', n=length(qry)/4); new.dataframe = data.frame(read.csv(textConnection(testraw1)))
  
  md_str <-as.numeric(as.character(new.dataframe[17,]))
}

## Cache queried estimates of magnetic declination
## Only returns unique records
read_magnetic_declination <- function(cachefile,raw,plot.col,date.col,latitude.col,longitude.col) {
  #browser()
  if ( file.exists(cachefile) ) {
    md.data <- read_csv(cachefile)
    
    
    md.exists <- paste(md.data$plot,md.data$date,sep=".") %in% paste(raw[,plot.col],raw[,date.col],sep=".")
    if ( any(!md.exists) ) {
      
      ii <- which(!md.exists & !duplicated( paste(raw[,plot.col],raw[,date.col],sep=".") ) )
      year <- as.numeric( substr(raw[,date.col][ii],1,4) )
      month <- as.numeric( substr(raw[,date.col][ii],6,7) )
      day <- as.numeric( substr(raw[,date.col][ii],9,10) )
      md.results <- mapply(get_magnetic_declination, raw[,latitude.col][ii], raw[,longitude.col][ii], year, month, day)
      md.data.tmp <- data.frame(plot=raw[,plot.col][ii],date=raw[,date.col][ii],
                                longitude=raw[,longitude.col][ii],latitude=raw[,latitude.col][ii],
                                md=md.results)
      md.data <- rbind(md.data,md.data.tmp)
      write_csv(md.data, cachefile)
      
    }
    
  } else {
    
    ii <- !duplicated( paste(raw[,plot.col],raw[,date.col],sep=".") )
    year <- as.numeric( substr(raw[,date.col][ii],1,4) )
    month <- as.numeric( substr(raw[,date.col][ii],6,7) )
    day <- as.numeric( substr(raw[,date.col][ii],9,10) )
    md.results <- mapply(get_magnetic_declination, raw[,latitude.col][ii], raw[,longitude.col][ii], year, month, day)
    md.data <- data.frame(plot=raw[,plot.col][ii],date=raw[,date.col][ii],
                          longitude=raw[,longitude.col][ii],latitude=raw[,latitude.col][ii],
                          md=md.results)
    write_csv(md.data, cachefile)
    
  }
  
  colnames(md.data) <- c(plot.col,date.col,longitude.col,latitude.col,"md")
  plyr::join(raw, md.data, by=c(plot.col,date.col), type="left", match="all")
}

## Create a regular grid of GEDI footprint center coordinates
create_gedi_footprint_grid <- function(data, fp.spacing, fp.radius, tk.spacing) {
  #browser()
  # read the subplot polygon, or polygon if does not exist
  p.proj4str <- crs(paste0("+init=epsg:",data$p.epsg[1]))
  
  p.geom <- ifelse(is.na(data$sp.geom), data$p.geom[1], data$sp.geom[1])
  
  geom.obj <- vect(p.geom[1], crs=p.proj4str)
  
  unique( crds(geom.obj))
  # clean polygon, work out plot top left corner and orientation
  if (data$p.shape[1] == "R") {
    v <- unique( crds(geom.obj))
    iv <- unique(c(which.min(v[,"x"]),which.max(v[,"y"]),which.max(v[,"x"]),which.min(v[,"y"])))
    if (length(iv) >= 4) v <- v[iv,]
    v <- v[chull(v[,1], v[,2]),]
    ix <- sort(v[,"x"], index.return=TRUE)$ix
    v1 <- v[ix[which.min( c(v[ix[1],"y"], v[ix[2],"y"]) )],]
    v2 <- v[ix[which.max( c(v[ix[1],"y"], v[ix[2],"y"]) )],]
    theta <- pi/2 + atan2(v1[2]-v2[2],v1[1]-v2[1])
  } else {
    if ( geomtype(geom.obj) == "points" ) { # this might need to be changed
      p.radius <- sqrt( data$p.area[1] / pi )
      geom.obj <- buffer(geom.obj, width=p.radius, quadsegs=10)
    }
    v2 <- c(crds(centroids(geom.obj)))
    theta <- 0
  }
  
  # calculate plot dimensions
  xdim <- ext(geom.obj)[2] - ext(geom.obj)[1]
  ydim <- ext(geom.obj)[4] - ext(geom.obj)[3]
  maxdim <- pmax(xdim,ydim)
  
  # generate gedi tracks defined by footprint spacing (along track)
  # tracks are orientated along the longest axis of the plot
  if (data$p.shape[1] == "R") {
    if (xdim > ydim) {
      d <- expand.grid(x=seq(0,maxdim,fp.spacing),y=-seq(0,maxdim,tk.spacing),KEEP.OUT.ATTRS=TRUE)
    } else {
      d <- expand.grid(x=seq(0,maxdim,tk.spacing),y=-seq(0,maxdim,fp.spacing),KEEP.OUT.ATTRS=TRUE)
    }
  } else {
    if ( data$p.sample[1] == 3 ) {
      d <- expand.grid(x=0,y=0,KEEP.OUT.ATTRS=TRUE)
    } else {
      offset <- ceiling(maxdim / 2 / fp.spacing) * fp.spacing
      if (xdim > ydim) {
        d <- expand.grid(x=seq(-offset,maxdim,fp.spacing),y=-seq(-offset,maxdim,tk.spacing),KEEP.OUT.ATTRS=TRUE)
      } else {
        d <- expand.grid(x=seq(-offset,maxdim,tk.spacing),y=-seq(-offset,maxdim,fp.spacing),KEEP.OUT.ATTRS=TRUE)
      }
    }
  }
  
  # snap the footprint grid to the upper left corner
  if (data$p.shape[1] == "R") {
    d$x <- d$x + fp.radius
    d$y <- d$y - fp.radius
  }
  
  # work out the subplot majoraxis if required
  if ( !is.na(data$sp.geom[1]) ) {
    if (data$p.shape[1] == "R") {
      sp.majoraxis <- sqrt(data$sp.area[1])
    } else {
      sp.majoraxis <- sqrt(data$sp.area[1] / pi) * 2
    }
  } else {
    sp.majoraxis <- data$p.majoraxis[1]
  }
  
  # define track and along track indices
  ix <- rep(seq(attributes(d)$out.attrs$dim["x"][[1]]), attributes(d)$out.attrs$dim["y"][[1]])
  iy <- rep(seq(attributes(d)$out.attrs$dim["y"][[1]]), 1, each=attributes(d)$out.attrs$dim["x"][[1]])
  
  # Account for small subplots/plots that should only have one footprint in the centre
  nfootprints <- sp.majoraxis / (fp.radius * 2)
  if ( (nfootprints < 1.75) & (data$p.shape[1] == "R") ) {
    offset <- sp.majoraxis / 2 - fp.radius
    d$x <- d$x + offset
    d$y <- d$y - offset
    d <- d[1,]
    if ( !is.na(data$sp.ix) & !is.na(data$sp.iy) ) {
      ix <- data$sp.ix[1]
      iy <- data$sp.iy[1]
    } else {
      ix <- ix[1]
      iy <- iy[1]
    }
  }
  
  # rotate the gedi tracks to be aligned with the plot
  xt <- d$x * cos(theta) - d$y * sin(theta) + v2[1]
  yt <- d$x * sin(theta) + d$y * cos(theta) + v2[2]
  
  # define the output data frame
  fp.grid <- data.frame(x=xt, y=yt, edge=NA, ix=ix, iy=iy)
  fp.grid <- vect(fp.grid, geom = c("x","y"), crs = p.proj4str)
  
  # intersect the footprint grid with the plot extent
  fp.grid.buff <- buffer(fp.grid, quadsegs=10, width=fp.radius)
  # This needs to be tested to make sure that this is set up to acheive the same thing as %over%
  ii <- as.vector( relate(fp.grid.buff, geom.obj, "intersects") & !(relate(fp.grid.buff, geom.obj, "touches")) )
  if ( !all(is.na(ii)) ) {
    
    # only retain footprints that intersect
    fp.grid <- fp.grid[!is.na(ii) & ii,]
    fp.grid.buff <- fp.grid.buff[!is.na(ii) & ii,]
    
    # flag those that are on the edge of the plot
    fp.grid.intersection <- crop(fp.grid.buff, geom.obj)
    theAreas <- expanse(fp.grid.intersection)
    fp.grid$edge <- as.numeric( theAreas > floor( max(theAreas) ) )
    fp.grid$edge.frac <- theAreas / max(theAreas)
    
    # reproject footprint coordinates to the same projection as the ALS data
    fp.grid <- project(fp.grid, paste("+init=epsg",data$l.epsg[1],sep=":"))
    # This results in a slightly different value, where ""_row":["y"]" is missing from the end
    data$g.fp <- jsonlite::toJSON(data.frame(values(fp.grid),crds(fp.grid)), dataframe="columns", digits=2, flatten=TRUE)
  }
  
  # Debugging code
  # #browser()
  # p <- ggplot()
  # p <- p + geom_polygon(data=fp.grid.buff, aes(x=long, y=lat, group=group), fill="blue", alpha=0.25)
  # p <- p + geom_polygon(data=geom.obj, aes(x=long, y=lat), fill=NA, color="red")
  # print(p)
  
  data
}

## Create circular ring of GEDI footprint center coordinates
get_footprint_ring <- function(r, fp.radius, fp.overlap, r.id) {
  #browser()
  if (fp.radius < r) {
    np <- floor( pi / ( asin(fp.radius / r) ) + 0.5 )
    theta.step <- 2*pi/np * (1 - fp.overlap)
    theta <- seq(0, 2*pi, theta.step)
    xt <- r * cos(theta)
    yt <- r * sin(theta)
    ix <- r.id
    iy <- seq( length(theta) )
  } else {
    xt <- 0
    yt <- 0
    ix <- 1
    iy <- 1
  }
  
  list(x=xt,y=yt,ix=ix,iy=iy)
}

## Create circular rings of GEDI footprint center coordinates
create_gedi_footprint_ring <- function(data, fp.radius, fp.overlap) {
  #browser()
  p.proj4str <- crs(paste0("+init=epsg:",data$p.epsg[1]))
  p.geom <- ifelse(is.na(data$sp.geom), data$p.geom, data$sp.geom)
  geom.obj <- vect(p.geom[1], crs=p.proj4str)
  if ( geomtype(geom.obj) == "points" ) {
    p.radius <- sqrt( data$p.area[1] / pi )
    geom.obj <- buffer(geom.obj, width=p.radius, quadsegs=10)
  }
  v0 <- c(crds(centroids(geom.obj)))
  
  p.radius <- pmax((ext(geom.obj)[2] - ext(geom.obj)[1]) / 2,
                   (ext(geom.obj)[4] - ext(geom.obj)[3]) / 2)
  
  r.step <- (fp.radius * 2) * (1 - fp.overlap)
  r <- seq(0, p.radius, r.step)
  pts <- mapply(get_footprint_ring, r=r, fp.radius=fp.radius, fp.overlap=fp.overlap, r.id=seq(length(r)))
  
  fp.ring <- data.frame(x=unlist(pts["x",])+v0[1], y=unlist(pts["y",])+v0[2],
                        edge=NA, ix=unlist(pts["ix",]), iy=unlist(pts["iy",]))
  fp.ring <- vect(fp.ring, geom = c("x","y"), crs = p.proj4str)
  
  # relate(fp.ring, geom.obj, "intersects", pairs=T)
  ii <- relate(fp.ring, geom.obj, "intersects") 
  if ( !all(is.na(ii)) ) {
    fp.ring <- fp.ring[!is.na(ii),]
    fp.ring.buff <- buffer(fp.ring, quadsegs=10, width=fp.radius)
    #fp.ring$edge <- as.numeric( !gCovers(geom.obj, fp.ring.buff, byid=TRUE) )
    
    fp.ring.intersection <- crop(fp.ring.buff, geom.obj)
    theAreas <- expanse(fp.ring.intersection)
    fp.ring$edge <- as.numeric( theAreas > floor( max(theAreas) ) )
    fp.grid$edge.frac <- theAreas / max(theAreas)
    
    fp.ring <- project(fp.ring, paste("+init=epsg",data$l.epsg[1],sep=":"))
    data$g.fp <- toJSON(data.frame(values(fp.ring),crds(fp.ring)), dataframe="columns", digits=2, flatten=TRUE)
  }
  
  data
}

simulateGEDIFootprintsOnLine <- function(data,alongTrack=30) {
  #browser()
  p.proj4str <- crs(paste0("+init=epsg:",data$p.epsg[1]))
  p.geom <- ifelse(is.na(data$sp.geom), data$p.geom, data$sp.geom)
  geom.obj <- vect(p.geom[1], crs=p.proj4str)
  theWindow <- owin(poly=list(x=crds(geom.obj)[c(5,4,3,2,1),1],
                              y=crds(geom.obj)[c(5,4,3,2,1),2]))
  
  # step 1: create a matrix with a pair of coordinates that will define the end points of a line that intersects the plot
  # these coordinates are initially set at 0, -1500 and 1, 1500, which ensures that the line (a) begins and ends outside the plot
  # and intersects the plot
  gc(verbose=FALSE)
  theCoords <- matrix(cbind(0,seq(-1500,1500,alongTrack)),ncol=2)
  theIDs <- seq(1,dim(theCoords)[1],1)
  
  # step 2: draw a random rotation angle for the line and build a rotation matrix
  angle <- runif(1,0,2*pi)
  rotationMat <- matrix(NA, nrow=2, ncol=2)
  rotationMat[1,1] <- cos(angle) 
  rotationMat[2,1] <- -sin(angle)
  rotationMat[1,2] <- sin(angle)
  rotationMat[2,2] <- cos(angle)
  
  # step 3: rotate the line and give it a random origin
  theCoordsRotated<- t(apply(theCoords,1,function(x) x%*%rotationMat))
  theCoordsRotated[,1] <- theCoordsRotated[,1] + runifpoint(1,theWindow)$x
  theCoordsRotated[,2] <- theCoordsRotated[,2] + runifpoint(1,theWindow)$y
  
  coords=data.frame(x=theCoordsRotated[,1],
                    y=theCoordsRotated[,2])
  
  footprintLocationsSample <- vect(coords, geom = c("x","y"), crs=crs(geom.obj))
  
  # step 4: compute the order of the distances from the eastern most footprint, and sort the IDs by this order
  theIDs <- theIDs[which(relate(footprintLocationsSample, geom.obj, "intersects")==1)]
  fp.data <- footprintLocationsSample[theIDs,]
  
  # step 5: convert to projection of the lidar and store as a JSON string
  fp.data <- project(fp.data, paste("+init=epsg",data$l.epsg[1],sep=":"))
  data$g.fp <- toJSON(data.frame(crds(fp.data)), dataframe="columns", digits=2, flatten=TRUE)  
  
  data
}

polygonChopper <- function(polygon, ID, PlotStatus, dimension, start_side=NULL, origin, asGrid=FALSE, proj4string) {
  #browser()
  # Reorganize the coordinates
  plot_coordinate <- crds(polygon) %>% as.data.frame()
  
  if(as.character(proj4string) == ("+proj=longlat +datum=WGS84 +no_defs")){
    utmzone <- get_utmwgs84_epsg(plot_coordinate[1,1],plot_coordinate[1,2])
    polygon <- project(polygon, paste("+init=epsg:", utmzone, sep=""))
    
    plot_coordinate <- crds(polygon)
    proj4string <- crs(polygon)
  }
  
  #Find the corners of the plot
  plot_corners <- plot_coordinate[chull(round(plot_coordinate[,1:2])),1:2]
  
  rect_area <- function(rect_coords){
    abs((rect_coords[1,1]*rect_coords[2,2] - rect_coords[1,2]*rect_coords[2,1] +
           rect_coords[2,1]*rect_coords[3,2] - rect_coords[2,2]*rect_coords[3,1] +
           rect_coords[3,1]*rect_coords[4,2] - rect_coords[3,2]*rect_coords[4,1] +
           rect_coords[4,1]*rect_coords[1,2] - rect_coords[4,2]*rect_coords[1,1])/2)
  }
  
  if(nrow(plot_corners)>4){
    
    cent <- crds(centroids(polygon))
    colnames(cent) <- colnames(plot_corners)
    dist_to_cent <- as.matrix(dist(rbind(cent,plot_corners)))[-1,1]
    plot_corners <- plot_corners[ round(dist_to_cent, -1)== max(unique(round(dist_to_cent,-1))) | round(dist_to_cent, -1)== max(unique(round(dist_to_cent,-1)))-10,]    
    
    combinations <- combn(nrow(plot_corners), 4)
    areas <- apply(combinations, 2, FUN = function(x){rect_area(plot_corners[x,])})
    corner_index <- combinations[,which(areas==max(areas))]
    plot_corners <- plot_corners[corner_index,]
  }
  
  plot_corners <- sort_points(data.frame(plot_corners), y = "y", x = "x", clockwise = TRUE)
  
  plot_polygon <- vect(x = as.matrix(plot_corners),
                       type = "polygons", 
                       crs = proj4string)
  
  #Use either 'origin' or 'start_side' to determine which side of the polygon to start from
  # Cuts in the polygon will be parallel to this side (and perpendicular if asGrid==TRUE)
  if(is.null(start_side)){
    origin <- matrix(data=origin, nrow=1, ncol=2)
    colnames(origin) <- colnames(plot_corners)
    dist_to_origin <- as.matrix(dist(rbind(origin,plot_corners)))[-1,1]
    start_corners <- order(dist_to_origin, decreasing=FALSE)[1:2]
  }else {
    if(start_side=='topright'){
      start_corners <- 2:3
    }else if(start_side=='bottomright'){
      start_corners <- 3:4
    }else if(start_side=='bottomleft'){
      start_corners <- c(4,1)
    }else if(start_side=='topleft'){
      start_corners <- 1:2
    }
  }
  
  
  # Use the first two points to calculate the slope
  slope <- atan((plot_corners[start_corners[1],2] - plot_corners[start_corners[2],2]) / (plot_corners[start_corners[1],1] - plot_corners[start_corners[2],1]))
  
  
  # Transfer the line according to an angle and distance
  newpos <- function(input, bearing, distance) {
    x <- as.numeric(input[1]) + distance * cos(bearing)
    y <- as.numeric(input[2]) + distance * sin(bearing)
    matrix(c(x, y), ncol = 2)
  }
  
  # Get the sdistance of all points to the first two points in the polygon
  l <- vect(x = as.matrix(plot_corners[start_corners,]),type = "lines", crs = proj4string)
  p <- vect(x = as.matrix(plot_corners),type = "points", crs = proj4string)
  dis <- terra::distance(p) %>% as.matrix()
  maxdis <- round(max(dis)/dimension) * dimension
  
  # Get the offset intervals 
  intervals <- seq(from = 0, to = maxdis, by = dimension)
  intervals <- c(rev(-intervals[-1]), intervals)
  
  # Extend the first point to make sure intersection
  p1 <- newpos(plot_corners[start_corners[1],], slope, 9000000)
  p2 <- newpos(plot_corners[start_corners[1],], slope, -9000000)
  
  # Get the new coordinates for two points according the intervals
  new_p1 <- newpos(p1, slope + pi / 2, intervals)
  new_p2 <- newpos(p2, slope + pi / 2, intervals)
  
  # Make the spatial lines
  ply_crs <- proj4string
  
  new_lines <- list()
  for (i in seq(along = intervals)) {
    ter <- vect(rbind(new_p1[i,], new_p2[i,]), type="lines")
    ter$ID <- i
    new_lines[[i]] <- ter
  }
  new_lines <- vect(new_lines, crs = ply_crs, type = "lines")
  
  #perpendicular lines
  p1 <- newpos(plot_corners[start_corners[1],], slope, dimension*2)
  p2 <- newpos(plot_corners[start_corners[1],], slope, -dimension*2)
  new_p1 <- newpos(p1, slope + pi / 2, c(-maxdis,maxdis)*2)
  new_p2 <- newpos(p2, slope + pi / 2, c(-maxdis,maxdis)*2)
  perp_lines <- list()
  ter_1 <- vect(rbind(new_p1[1,], new_p1[nrow(new_p1),]),type="lines")
  ter_1$ID <- 1
  perp_lines[[1]] <- ter_1
  ter_2 <- vect(rbind(new_p2[1,], new_p2[nrow(new_p2),]),type="lines")
  ter_2$ID <- 2
  perp_lines[[2]] <- ter_2
  if(asGrid){
    p1 <- newpos(plot_corners[start_corners[1],], slope + pi / 2, 9000000)
    p2 <- newpos(plot_corners[start_corners[1],], slope + pi / 2, -9000000)
    new_p1 <- newpos(p1, slope,  intervals)
    new_p2 <- newpos(p2, slope,  intervals)
    for (i in seq(along = intervals)) {
      Line <- vect(rbind(new_p1[i,], new_p2[i,]),type="lines")
      Line$ID <- i
      perp_lines[[i]] <- Line
    }
  }
  perp_lines <- vect(perp_lines, crs = ply_crs, type="lines")
  
  # This is another function that I was unable to test side by side 
  outside_polygon <- list()
  id <- 1
  for(i in 1:(length(perp_lines)-1)){
    for(j in 1:(length(new_lines)-1)){
      outside <- crop(new_lines[j:(j+1)], perp_lines[i:(i+1)]) %>% crds()
      outside <- sort_points(data.frame(outside), y = "y", x = "x", clockwise = TRUE)
      outside <- vect(x = as.matrix(outside),
                      type = "polygons", 
                      crs = proj4string)
      outside$ID <- id
      outside_polygon[[id]] <- outside
      id <- id+1
    }
  }
  #order the new polygons starting from the start_corners
  if(asGrid){
    outside_polygon <- outside_polygon[unlist(lapply(outside_polygon, function(x){round(x %>% expanse(transform = F)) <= dimension^2}))]
    outside_coords <- matrix(unlist(lapply(outside_polygon, function(x){x %>% centroids() %>% crds()})),ncol=2,byrow=TRUE)
    outside_coords <- vect(outside_coords, crs = proj4string, type="points")
    start_line <- vect(as.matrix(plot_corners[start_corners,]), crs = proj4string, type="lines")
    start_point <- vect(as.matrix(plot_corners[start_corners[1],]), crs = proj4string, type="points")
    
    outside_polygon <- outside_polygon[order(as.numeric(round(distance(outside_coords, start_line), 2)),
                                             as.numeric(round(distance(outside_coords, start_point), 2)), decreasing=F)]
  }
  
  outside_polygon <- compact(outside_polygon)
  
  # Find the intersection
  inter <- list()
  for(i in 1:length(outside_polygon)){
    crop <- terra::intersect(outside_polygon[[i]], polygon) 
    crop$id <- paste0(ID, '_np', i)
    inter[[i]] <- crop
  }
  inter <- inter[!unlist(lapply(inter, rlang::is_empty))]
  inter <- inter[unlist(lapply(inter, function(x){class(x)=="SpatVector"}))]
  
  
  if(dplyr::coalesce(sd(unlist(lapply(inter, expanse))) / mean(unlist(lapply(inter, expanse))),0) >= 0.1){
    area <- unlist(lapply(inter, expanse))
    inter <- inter[unlist(lapply(inter, function(x){x %>% expanse(transform = F) > mean(area)- 0.5*sd(area)}))]
  }
  inter <- do.call(rbind, inter)
  
  #print("inter test")
  #print(inter)
  
  newID <- paste0(ID, '_np', seq_len(length(inter)))
  for (i in 1:nrow(inter)) {
    inter$ID[i] <- newID[i]
  }
  
  # Create SPDF
  data <- data.frame(newID=newID, oldID=ID, PlotStatus=PlotStatus, row.names=newID)
  
  
  # Combine into a list
  polys.df <- cbind(inter, data)
  
  return(polys.df)
}

## Sort coordinates either clockwise or counter-clockwise around their centroid.
sort_points <- function(df, y = "latitude", x = "longitude", clockwise = TRUE) {
  #browser()
  # NA check, if NAs drop them
  if (any(is.na(c(df[, y], df[, x])))) {
    
    # Remove NAs
    df <- df[!(is.na(df[, y]) & is.na(df[, x])), ]
    
    # Raise warning
    warning("Missing coordinates were detected and have been removed.", 
            call. = FALSE)
    
    # Check 
    if (nrow(df) == 0) stop("There are no valid coordinates.", call. = FALSE)
    
  }
  
  # Get centre (-oid) point of points
  x_centre <- mean(df[, x])
  y_centre <- mean(df[, y])
  
  # Calculate deltas
  df$x_delta <- df[, x] - x_centre
  df$y_delta <- df[, y] - y_centre
  
  # Resolve angle, in radians
  df$angle <- atan2(df$y_delta, df$x_delta)
  # d$angle_degrees <- d$angle * 180 / pi
  
  # Arrange by angle
  if (clockwise) {
    
    df <- df[order(df$angle, decreasing = TRUE), ]
    
  } else {
    
    df <- df[order(df$angle, decreasing = FALSE), ]
    
  }
  
  # Drop intermediate variables
  df[, c("x_delta", "y_delta", "angle")] <- NULL
  
  # Return
  df
  
}

