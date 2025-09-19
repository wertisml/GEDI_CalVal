	# Figure showing GEDI footprint and simulated ALS data
	# One panel shows plot geometry and the placement of GEDI footprints within the plot
	# One panel shows plot geometry and the stemmap. Stemmap panel is only shown if stemmap exists
	# One panel shows profile of RH metrics

	project.data <- subset(gedicalval$plotdata, project == reportproject)
	footprint.data <- subset(gedicalval$fpdata, project == reportproject)
	
	if(any(footprint.data$p.stemmap == 1)){
		ii <- footprint.data$p.stemmap == 1 & footprint.data$g.edge.frac >= 0.9 & 
			(footprint.data$g.agb.valid == 1 | is.na(footprint.data$g.agb.valid) | is.infinite(footprint.data$g.agb.valid) )
		plot.id <- unique(footprint.data$plot[ii][which(footprint.data$rhReal50[ii] == max(footprint.data$rhReal50[ii], na.rm=TRUE))])[1]
	} else {
		ii <- footprint.data$g.edge.frac >= 0.9 & 
			(footprint.data$g.agb.valid == 1 | is.na(footprint.data$g.agb.valid) | is.infinite(footprint.data$g.agb.valid) )
		plot.id <- unique(footprint.data$plot[which(footprint.data$rhReal50 == max(footprint.data$rhReal50, na.rm=TRUE))])[1]
	}
	if(nrow(footprint.data)==0){
		plot.id <- unique(project.data$plot)[1]
	}	
	plot.data <- subset(gedicalval$plotdata, (project == reportproject) & (plot == plot.id))
	gedi.data <- subset(gedicalval$gedidata, (project == reportproject) & (plot == plot.id))
	tree.data <- subset(gedicalval$treedata, (project == reportproject) & (plot == plot.id))
	fp.data <- subset(gedicalval$fpdata, (project == reportproject) & (plot == plot.id)) #& (obstime=='Night') & (power==9.0))


	# p.geom <- readWKT(plot.data$p.geom[1])
	# if ( !is.na(plot.data$l.epsg[1]) ) {
	# 	p.proj4str <- CRS(paste("+init=epsg",plot.data$l.epsg[1],sep=":"))
	# }
        
	p.geom <- vect(plot.data$p.geom[1])
	if (!is.na(plot.data$l.epsg[1])) {
	  p.crs <- paste("EPSG:", plot.data$l.epsg[1], sep = "")
	  crs(p.geom) <- p.crs
	}
	
	#Start plot showing footprint placement
	fp.plot <- ggplot()
	if(length(unique(gedi.data$g.fp))>1){
		fp.geom <-  jsonlite::fromJSON(paste("[",paste(unique(gedi.data$g.fp),collapse=','),"]",sep="") )
		#if(is.list(fp.geom[[1]])){
		#	fp.geom <- lapply(fp.geom, as.data.frame)
		#}
		#fp.geom <- as.data.frame(do.call(rbind, fp.geom))
		jsonstr <- jsonlite::toJSON(fp.geom)
	} else {
		jsonstr <- unique(gedi.data$g.fp)
	}


	if ( !is.na(jsonstr) ) {
		# Join footprint geometry with footprint data
		fp.geom <- jsonlite::fromJSON(jsonstr) %>%
		  as.data.frame() %>%
		  tidyr::unnest(cols = everything())
		fp.geom <- left_join(fp.geom, fp.data, by = c("x" = "g.x", "y" = "g.y"))
		fp.geom.coords <- fp.geom[,c('x','y')]
		fp.geom <- terra::vect(fp.geom, geom = c('x', 'y'), p.crs)
		#coordinates(fp.geom) <- ~x+y
		#proj4string(fp.geom) <- p.proj4str
            
		fp.geom <- terra::buffer(fp.geom, width=gedicalval$fp_radius, quadsegs=10)
		fp.geom <- terra::project(fp.geom, paste("EPSG",plot.data$p.epsg[1],sep=":"))
		#fp.geom <- spTransform(fp.geom, CRS(paste("+init=epsg",plot.data$p.epsg[1],sep=":")))
            
		## Plot footprints on the edge of plots
		#ii <- fp.geom$edge.frac < 0.9
		#if ( any(ii) ) {
		#  fp.plot <- fp.plot + geom_polygon(data=fp.geom[ii,], aes(x=long, y=lat, group=group), fill="blue", alpha=0.25)
		#}
		
		## Plot invalid footprints that are within the plot
		## using an open grey circle
		ii <- fp.geom$edge.frac >= 0.9 & (fp.geom$g.agb.valid != 1 & !is.infinite(fp.geom$g.agb.valid) & !is.na(fp.geom$g.agb.valid))
		if ( any(ii) ) {
		  
		  fp.sf <- sf::st_as_sf(fp.geom[ii,])
		  fp.coords <- sf::st_coordinates(fp.sf)
		  
		  fp.df <- as.data.frame(fp.coords) %>%
		    dplyr::rename(long = X, lat = Y) %>%
		    mutate(group = L2)  # L2 is the polygon ID from st_coordinates
		  
		  fp.plot <- fp.plot + geom_polygon(data=fp.df, aes(x=long, y=lat, group = group), fill=NA, color = "grey80", alpha=1)
		}

		## Plot valid footprints that are within the plot
		## using a filled grey circle
		ii <- fp.geom$edge.frac >= 0.9 & (fp.geom$g.agb.valid == 1 | is.na(fp.geom$g.agb.valid) | is.infinite(fp.geom$g.agb.valid) )
		if ( any(ii) ) {
		  
		  fp.sf <- sf::st_as_sf(fp.geom[ii,])
		  fp.coords <- sf::st_coordinates(fp.sf)
		  
		  fp.df <- as.data.frame(fp.coords) %>%
		    dplyr::rename(long = X, lat = Y) %>%
		    mutate(group = L2)  # L2 is the polygon ID from st_coordinates
		  
			fp.plot <- fp.plot + geom_polygon(data=fp.df, aes(x=long, y=lat, group = group), fill="grey80", alpha=1)

			## Select one focal footprint to display RH metrics
			if(sum(ii)==1 & any(!is.na(fp.geom$rhReal50[ii]))){
				focal_footprint <- which(ii)
			} else{
				jj <- which(fp.geom$rhReal50[ii] == max(fp.geom$rhReal50[ii], na.rm=TRUE))
				focal_footprint <- which(ii)[jj]
			}
			if(exists("focal_footprint") & length(focal_footprint) > 0){
				RHmetric <- t(as.data.frame(fp.geom[focal_footprint,paste("rhReal", seq(0,100), sep="")]))
				## Focal footprint plotted in cyan.
				
				fp.sf <- sf::st_as_sf(fp.geom[focal_footprint,])
				fp.coords <- sf::st_coordinates(fp.sf)
				
				fp.df <- as.data.frame(fp.coords) %>%
				  dplyr::rename(long = X, lat = Y) %>%
				  mutate(group = L2) 
				
				fp.plot <- fp.plot + geom_polygon(data=fp.df, aes(x=long, y=lat, group=group), fill="cyan", alpha=1)
			}				
			
		}

             
		#centroids <- plyr::ldply(fp.geom@polygons, function(p) data.frame(x=p@labpt[1],y=p@labpt[2]))
    centroids <- terra::centroids(fp.geom) %>% terra::crds() %>% as.data.frame()         
	}

		
	# Add plot outlines. Plots in red and subplots in black.
	if ( class(p.geom) == "SpatVector" ) {
		p.radius <- sqrt( plot.data$p.area[1] / pi )
		p.geom <- terra::buffer(p.geom, width=p.radius, quadsegs=10)
		
		fp.sf <- sf::st_as_sf(p.geom)
		fp.coords <- sf::st_coordinates(fp.sf)
		
		p.df <- as.data.frame(fp.coords) %>%
		  dplyr::rename(long = X, lat = Y) %>%
		  mutate(group = L2)
		
		fp.plot <- fp.plot + geom_polygon(data=p.df, aes(x=long, y=lat), fill=NA, color="red")
		
	} else {
	  fp.sf <- sf::st_as_sf(p.geom)
	  fp.coords <- sf::st_coordinates(fp.sf)
	  
	  p.df <- as.data.frame(fp.coords) %>%
	    dplyr::rename(long = X, lat = Y) %>%
	    mutate(group = L2)
	  
		fp.plot <- fp.plot + geom_polygon(data=p.df, aes(x=long, y=lat), fill=NA, color="red")
	} 
	fp.plot <- fp.plot + coord_equal() + labs(x="Easting (m)",y="Northing (m)") + 
			#ggtitle(paste(reportproject,plot.id,sep=": ")) + 
			theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 2))
       
	if(length(plot.data$sp.geom)<500){

		for (wkt in plot.data$sp.geom) {
			if ( !is.na(wkt) ) {
				#sp.geom <- readWKT(wkt)
			  sp.geom <- terra::vect(wkt)
			  
			  sp.sf <- sf::st_as_sf(sp.geom)
			  sp.coords <- sf::st_coordinates(sp.sf)
			  
			  sp.df <- as.data.frame(sp.coords) %>%
			    dplyr::rename(long = X, lat = Y) %>%
			    mutate(group = L2)

			  fp.plot <- fp.plot + geom_polygon(data=sp.df, aes(x=long, y=lat), fill=NA, color="black", linetype="dashed")
			}
		}
	}
		
#	## Add text for aboveground biomass density and plot area.
#	agbd.ha.str <- sprintf("AGBD = %.2f Mg/ha", mean(plot.data$agbd.ha, na.rm=TRUE))
#	p.area.str <- sprintf("Plot area = %.2f ha", plot.data$p.area[1]/10000)
#	annotations <- data.frame(xpos=c(-Inf,-Inf), ypos=c(Inf,Inf), annotateText=c(agbd.ha.str,p.area.str), 
#					hjustvar=c(-0.05,-0.075), vjustvar=c(1.05,2.75))
#	fp.plot <- fp.plot + geom_text(data=annotations, aes(x=xpos, y=ypos, hjust=hjustvar, vjust=vjustvar, label=annotateText), size=4)
		
	filter_down <- function(data){
	  
	  matches_x <- regexpr("x([0-9]+)", data)  # Match the x component
	  matches_y <- regexpr("y([0-9]+)", data)  # Match the y component
	  
	  # Extract the parts for x and y as characters
	  x_value <- substr(data, matches_x[1] + 1, matches_x[1] + attr(matches_x, "match.length")[1] - 1) %>%
	    as.numeric() %>%
	    as.character()
	  y_value <- substr(data, matches_y[1] + 1, matches_y[1] + attr(matches_y, "match.length")[1] - 1) %>%
	    as.numeric() %>%
	    as.character()
	  
	  # Pad with trailing zeros (ensure specific lengths as required)
	  x_padded <- paste0(x_value, strrep("0", 12 - nchar(x_value)))  # Pad to 6 digits for x
	  y_padded <- paste0(y_value, strrep("0", 13 - nchar(y_value)))  # Pad to 7 digits for y
	  
	  x_padded <- paste0(substr(x_padded, 1, 6), ".", substr(x_padded, 7, nchar(x_padded)))
	  y_padded <- paste0(substr(y_padded, 1, 7), ".", substr(y_padded, 8, nchar(y_padded)))
	  
	  
	  # Combine x and y with a "." in between
	  formatted_id <- paste(x_padded, y_padded, sep=".")
	  return(formatted_id)
	}
	
	## Plot height vs RH metrics for focal footprint.
	if(exists("RHmetric")){
		RHmetric <- data.frame(rh = RHmetric[,1], ind=seq(0:100))
		RHcurve <- ggplot(data = RHmetric, aes(x=ind, y=rh, group=1))
		RHcurve <- RHcurve + geom_path(size=1.0)
		RHcurve <- RHcurve + geom_segment(data = as.data.frame(RHmetric[c(26,51,76,99),'rh']), aes(y = RHmetric[c(26,51,76,99),'rh'], yend=RHmetric[c(26,51,76,99),'rh'], x = 0, xend = c(25,50,75,98)), color = "grey70", linetype="dashed")
		RHcurve <- RHcurve + annotate("text", 12, RHmetric[c(26,51,76,99),'rh'], vjust = -0.5, label=c("RH25","RH50","RH75","RH98"), size = 3)
		RHcurve <- RHcurve + labs(x='% energy',y='Height above ground (m)') + theme_bw()

		
		#Import waveform data from hdf5 file
		#simWave <- paste('../../../../data/gedi/simulations/database/waveforms/', fp.geom$l.project[focal_footprint], '/gediWave.', fp.geom$l.project[focal_footprint], '.0.h5', sep="")
		#simMet <- paste('../../../../data/gedi/simulations/database/metrics/noisedMetric.', fp.geom$l.project[focal_footprint], '.pow.9.Night.1.metric.txt', sep ="")

	  #file <- list.files(path = paste0("gedi_database_sims/waveforms/", fp.geom$l.project[focal_footprint], "/tmp"),
	  #                   pattern = ".h5")[1]
	  #simWave <- paste0("gedi_database_sims/waveforms/", fp.geom$l.project[focal_footprint], "/tmp/",file)
	  #simMet <- paste0("gedi_database_sims/metrics/gedi_metric_csv/unnoisedMetric.", fp.geom$l.project[focal_footprint],".metric.csv")
		
		if(reportproject == "sweden_nfi"){
			simWave <- paste('../gedi/waveforms/', fp.geom$l.project[focal_footprint], '/gediWave.', fp.geom$l.project[focal_footprint], '.3.h5', sep="")
			simMet <- paste('../gedi/metrics/noisedMetric.', fp.geom$l.project[focal_footprint], '.pow.9.Night.4.metric.txt', sep ="")
		}
		if(reportproject == "usa_cafi" | reportproject == "usa_murphydome"){
			simWave <- paste('../gedi/waveforms/', fp.geom$l.project[focal_footprint], '/gediWave.', fp.geom$l.project[focal_footprint], '.1.h5', sep="")
			simMet <- paste('../gedi/metrics/noisedMetric.', fp.geom$l.project[focal_footprint], '.pow.9.Night.2.metric.txt', sep ="")
		}

		#print(simWave)
    for(h5.file in list.files(paste0("gedi_database_sims/waveforms/", fp.geom$l.project[focal_footprint], "/tmp"))){
      simWave <- paste0(path = paste0("gedi_database_sims/waveforms/", fp.geom$l.project[focal_footprint], "/tmp/", h5.file))
      #file_num <- sub(".*\\.([^.]+)\\.h5$", "\\1", h5.file)
	    #simMet <- paste0(paste0("gedi_database_sims/metrics/gedi_metric_csv/unnoisedMetric.", fp.geom$l.project[focal_footprint],".metric.csv")
      simMet <- paste0("gedi_database_sims/metrics/gedi_metric_csv/unnoisedMetric.", fp.geom$l.project[focal_footprint],".metric.csv")
                       
	  #print(h5.file)
		#print(simWave)
		#print(simMet)

		if(length(simWave) > 1){
			simWave <- simWave[1]
		}
		if(length(simMet) > 1){
			simMet <- simMet[1]
		}


		sWave <- rhdf5::h5read(simWave, 'RXWAVECOUNT')
		grWave <- rhdf5::h5read(simWave, 'GRWAVECOUNT')
		sID <- rhdf5::h5read(simWave, 'WAVEID')
		sZN <- rhdf5::h5read(simWave, 'ZN')
		sZ0 <- rhdf5::h5read(simWave, 'Z0')
		nBins <- rhdf5::h5read(simWave, 'NBINS')[1]
		
#		print('sWave')
#		str(sWave)		
#		print('grWave')
#		str(grWave)
#		print('sID')
#		str(sID)
#		print('sZN')
#		str(sZN)
#		print('sZ0')
#		str(sZ0)
#		print('nBins')
#		str(nBins)
		
		true_ground <- read.csv(simMet, stringsAsFactors=FALSE)[,2:3]
		#str(true_ground)

		# Match focal footprint using unique ID
		gedi.id <- paste(fp.geom$project, fp.geom$plot, fp.geom$survey, fp.geom$subplot,
                     sprintf("x%09iy%09i", round(fp.geom.coords$x*100), round(fp.geom.coords$y*100)),
                     sep=":")
		#print(gedi.id[focal_footprint])
		
		
		if(any(apply(sID, 2, paste, collapse="")==filter_down(gedi.id[focal_footprint]))){
		  break()
		}
		
    }

		waveform_id <- which(apply(sID, 2, paste, collapse="")==filter_down(gedi.id[focal_footprint]))
		metric_id <- which(true_ground[,1]==filter_down(gedi.id[focal_footprint]))


		# set resolution of waveform
		sRes=(sZ0[waveform_id]-sZN[waveform_id])/nBins

		# align waves
		alignS<-rep(0, 1024)
		alignG<-rep(0, 1024)
		z=rep(0, nrow(sWave))
		for(j in 1:nrow(sWave)){
			z[j]<-sZ0[waveform_id]-j*sRes
			sBin<-trunc((sZ0[waveform_id]-z[j])/sRes)
			if ((sBin>0)&(sBin<nBins)){
				alignS[j]<-sWave[sBin,waveform_id]
				alignG[j]<-grWave[sBin,waveform_id]
			}
		}
		z <- z-true_ground[metric_id, 2]

		# Trim waveform to match range of RH metrics
		alignS <- alignS[z>RHmetric[1,'rh'] & z<RHmetric[101,'rh']]
		alignG <- alignG[z>RHmetric[1,'rh'] & z<RHmetric[101,'rh']]
		z <- z[z>RHmetric[1,'rh'] & z<RHmetric[101,'rh']]


		waveform <- data.frame(sim = c(alignS, alignG) ,z = rep(z,2), group=rep(c("Canopy","Ground"), each=length(z)))

		waveform.curve <- ggplot(data = waveform, aes(x=sim, y=z, group=group,color=group, linetype=group)) + scale_colour_grey()
		waveform.curve <- waveform.curve + geom_path(size=1.0,)
		waveform.curve<- waveform.curve + labs(x='Intensity',y='Height above ground (m)') + theme_bw()
		waveform.curve<- waveform.curve + theme(legend.justification=c(0.5,0), legend.position=c(0.5,0.01), 
			legend.direction="horizontal", legend.title=element_blank(), legend.text=element_text(size=8),
			legend.background=element_rect(fill='white', color='gray70'), 
			legend.margin=margin(t=0.05, r=0.05, l=0.05, b=0.05, unit='cm'))

	}
	## For projects with a stem map, create stemmap plot.
	if ( any(plot.data$p.stemmap==1) ) {
		stemmap <- ggplot()
		# Stems are colored according to their biomass
		ii <- tree.data[,'status']==1 | is.na(tree.data$status)
		tree.order <- tree.data[ii,][order(tree.data$m.agb[ii]),]
		stemmap <- stemmap + geom_point(data=tree.order, aes(x=x, y=y, colour=m.agb), size=1/(plot.data$p.area[1]/10000)/8, alpha=1) + 
			scale_colour_gradient(low="blue",high="red",name="Tree Biomass (kg)",guide = guide_colorbar(direction = "horizontal"))
			
		# Add plot outlines. Plots in red and subplots in black.
		if ( class(p.geom) == "SpatVector" ) {
			# p.radius <- sqrt( plot.data$p.area[1] / pi )
			# p.geom <- gBuffer(p.geom, width=p.radius, quadsegs=10)
		  p.sf <- sf::st_as_sf(p.geom)
		  p.coords <- sf::st_coordinates(fp.sf)
		  p.df <- as.data.frame(p.coords) %>%
		    dplyr::rename(long = X, lat = Y) %>%
		    mutate(group = L2)  
		  
			stemmap <- stemmap + geom_polygon(data=p.df, aes(x=long, y=lat), fill=NA, color="red")
		} else {
		  p.sf <- sf::st_as_sf(p.geom)
		  p.coords <- sf::st_coordinates(fp.sf)
		  p.df <- as.data.frame(p.coords) %>%
		    dplyr::rename(long = X, lat = Y) %>%
		    mutate(group = L2)
		  
			stemmap <- stemmap + geom_polygon(data=p.geom, aes(x=long, y=lat), fill=NA, color="red")
		} 
		stemmap <- stemmap + coord_equal() + labs(x="Easting (m)",y="Northing (m)") + 
			#ggtitle(paste(reportproject,plot.id,sep=": ")) + 
			theme_bw()+
			theme(legend.position="bottom",
				legend.box="horizontal",
				axis.text.x = element_text(angle = 90, hjust = 2),
				legend.key.width= unit(1.0, 'cm')) +
				guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5) )
		
		if(length(plot.data$sp.geom)<500){
			for (wkt in plot.data$sp.geom) {
				if ( !is.na(wkt) ) {
					sp.geom <- vect(wkt)
					sp.sf <- sf::st_as_sf(sp.geom)
					
					# Extract polygon vertices
					sp.coords <- sf::st_coordinates(sp.sf)
					
					# Combine coordinates with feature ID (for grouping)
					sp.df <- as.data.frame(sp.coords) %>%
					  dplyr::rename(long = X, lat = Y) %>%
					  mutate(group = L2)  
					
					stemmap <- stemmap + geom_polygon(data=sp.df, aes(x=long, y=lat), fill=NA, color="black", linetype="dashed" )
				}
			}
		}
		
		## Arrange footprint panel and stemmap on left, RH metric profile on right.
		g1 <- ggplotGrob(fp.plot)
		g2 <- ggplotGrob(stemmap)
		g <- rbind(g1, g2, size = "first")
		g$widths <- grid::unit.pmax(g1$widths, g2$widths)

		plot.id2 <- gsub("_", "\\_", plot.id, fixed = TRUE)

		if(exists("RHmetric")){
		  grid.arrange(g, waveform.curve, RHcurve, layout_matrix=matrix(c(1,1,1,2,2,3,3),ncol=7))
		  # Figure caption for when a stemmap and canopy profile are included
		  caption <- paste('GEDI footprints within plot', plot.id2, 'in the', gedi.projectname, 'project (top left). 
					The plot is outlined in red, derived from GPS measurements of the plot corners,
					and subplots are outlined in black, showing the idealized subplot arrangement.
					GEDI footprints within the plot are colored grey. 
					Any footprints that are being excluded from analysis are shown as an open circle.
					The vertical canopy profile for one footprint, in cyan, is shown on the right.
					The canopy profile shows the lidar waveform (center) and canopy height vs. relative height metrics (0--98th percentile; right). 
					In the plot stem map (bottom left), points indicate the location of trees, with color indicating the biomass of each tree.')
		} else{
		  grid.arrange(g, ncol=2)
		  # Figure caption for when a stemmap is included and canopy profile is not included
		  caption <- paste('GEDI footprints within plot', plot.id2, 'in the', gedi.projectname, 'project (top). 
					The plot is outlined in red, derived from GPS measurements of the plot corners,
					and subplots are outlined in black, showing the idealized subplot arrangement.
					GEDI footprints within the plot are colored grey. 
					Any footprints that are being excluded from analysis are shown as an open circle.
					In the plot stem map (bottom), points indicate the location of trees, with color indicating the biomass of each tree.')
		}
		
		
	}else{
	  if(exists("RHmetric")){
	    grid.arrange(fp.plot, waveform.curve, RHcurve, layout_matrix=matrix(c(1,1,1,2,2,3,3),ncol=7))
	    # Figure caption for when a stemmap is not included and canopy profile is included
	    plot.id2 <- gsub("_", "\\_", plot.id, fixed = TRUE)
	    caption <- paste('GEDI footprints within plot', plot.id2, 'in the', gedi.projectname, 'project (left). 
					The plot is outlined in red, derived from GPS measurements of the plot corners,
					and subplots are outlined in black, showing the idealized subplot arrangement.
					GEDI footprints within the plot are colored grey. 
					Any footprints that are being excluded from analysis are shown as an open circle.
					The vertical canopy profile for one footprint, in cyan, is shown on the right.
					The canopy profile shows the lidar waveform (center) and canopy height vs. relative height metrics (0--98th percentile; right).')
	  } else{
		  show(fp.plot)
		  # Figure caption for when a stemmap is not included and canopy profile is not included
		  plot.id2 <- gsub("_", "\\_", plot.id, fixed = TRUE)
		  caption <- paste('GEDI footprints within plot', plot.id2, 'in the', gedi.projectname, 'project (left). 
					The plot is outlined in red, derived from GPS measurements of the plot corners,
					and subplots are outlined in black, showing the idealized subplot arrangement.
					GEDI footprints within the plot are colored grey. 
					Any footprints that are being excluded from analysis are shown as an open circle.')
		}

	}
	