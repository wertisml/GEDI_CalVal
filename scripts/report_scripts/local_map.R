	## Map of project area
	
	# Subset plot data for project
	project.data <- subset(gedicalval$plotdata, project == reportproject)
	
	# Set extent of mapped area.
	# If the project has multiple plots, map location is set outside of range of plot location.
	# If the project has one plot, map location is set to 0.1 degrees around the plot
	if(length(unique(project.data$plot))>1){
		long.range <- diff(range(project.data$longitude))
		lat.range <- diff(range(project.data$latitude))
		max.range <- max(c(long.range, lat.range))
		location <- c(mean(range(project.data$longitude))-max.range*1.2,
			mean(range(project.data$latitude))-max.range*1.2,
			mean(range(project.data$longitude))+max.range*1.2,
			mean(range(project.data$latitude))+max.range*1.2)
	} else{
		location <- c(min(project.data$longitude)-0.1,
			min(project.data$latitude)-0.1,
			max(project.data$longitude)+0.1,
			max(project.data$latitude)+0.1)
	}
	# Get map of project area.
	gmap <- get_map(location=location, maptype = "terrain", source = "osm")
	if(!exists("gmap")){
		gmap <- get_map(location=location, maptype = "toner", source = "stamen")
	}
	gg <- ggplot()
	gg <- ggmap(gmap)
	# Add points for plot locations
	gg <- gg + geom_point(data=gedicalval$plotdata[gedicalval$plotdata$project == reportproject,], 
					aes(x=longitude, y=latitude), 
					size=2, alpha=1, color="red")
	
		### For mapping plot geometries instead of points	
	    # for (ii in seq( nrow(project.data) )) {
        #  p.proj4str <- CRS(paste("+init=epsg",project.data$p.epsg[ii],sep=":"))
        #  if ( is.na(project.data$sp.geom[ii]) ) {
        #    polygon.data <- readWKT(project.data$p.geom[ii], p4s=p.proj4str)
        #    if ( class(polygon.data) == "SpatialPoints" ) {
        #      p.radius <- sqrt( plotdata()$p.area[1] / pi )
        #      polygon.data <- gBuffer(polygon.data, width=p.radius, quadsegs=10)
        #    }
        #    polygon.data <- spTransform(polygon.data, CRS("+init=epsg:4326"))
        #    popup.html <- project.data$plot[ii]
        #  } else {
        #    polygon.data <- readWKT(project.data$sp.geom[ii], p4s=p.proj4str)
        #    if ( class(polygon.data) == "SpatialPoints" ) {
        #      p.radius <- sqrt( plotdata()$p.area[1] / pi )
        #      polygon.data <- gBuffer(polygon.data, width=p.radius, quadsegs=10)
        #    }
        #    polygon.data <- spTransform(polygon.data, CRS("+init=epsg:4326"))
        #  }
		#  gg <- gg + geom_polygon(data=polygon.data, aes(x=long, y=lat, group=group), color="blue", alpha=0)
		#  }
	gg <- gg + theme(legend.position="none",panel.border=element_rect(colour="black",fill=NA,size=0.5)) +  
				labs(x="Longitude",y="Latitude") + theme_bw()