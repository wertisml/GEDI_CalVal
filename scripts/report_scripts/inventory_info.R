plot.data <- subset(gedicalval$plotdata, project == reportproject)
	tree.data <- subset(gedicalval$treedata, project == reportproject)
	
	n_plots <- length(unique(plot.data$plot))
	if(any(!is.na(tree.data$tree))){
		n_trees <- as.integer(dplyr::count(tree.data[,c('plot','tree')]))
	} else{
		n_trees <- 0
	}
	n_subplot <- as.integer(dplyr::count(plot.data[,c('plot','subplot')]))

	allom.key <- unique(tree.data$allom.key)
	start <- c(1,3,13,21,25,26,27)
	end<- c(2,12,20,24,25,26,50)
	refs <- c(
		"Chave et al. (2014)",
		"Jenkins et al. (2003)",
		"Paul et al. (2016)",
		"Muukkonen (2007)",
		"Moore (2010)",
		"Beets et al. (2011)",
		"Forrester et al. (2017)"
	)

	refs <- as.data.frame(cbind(start, end, as.data.frame(refs, stringsAsFactors=F)))
	ii <- rep(FALSE, nrow(refs))

	for(i in 1:nrow(refs))
		ii[i] <- any(allom.key%in%as.numeric(refs$start[i]):as.numeric(refs$end[i]))
	
	reference <- reference <- paste(refs$refs[ii], collapse=', ')
	
	ii <- !is.na(tree.data[,'h.t']) & !is.na(tree.data[,'d.stem']) & (tree.data[,'status']==1 | is.na(tree.data[,'status']))
	
	if(is.na(Projectname)){
		source_project <- gedi.projectname
	} else{
		source_project <- Projectname
	}
	
	if(biomass.source=="Calculated by data provider"){
		inventorytext <- paste("Metadata on the sampling protocol and methods for in situ inventory data from the ", source_project," project are shown in Table \\ref{tab:samplingtab}
			and the locations of the ", n_plots, " available plots are shown in Figure \\ref{fig:localmap}. Estimates of aboveground biomass were calculated by the data provider.
			These were aggregated at the subplot level to estimate aboveground biomass density at a scale that provided the best possible spatial match to GEDI lidar waveform footprints. 
			The distribution of aboveground biomass density estimates at the native and GEDI footprint scales are shown in Figure \\ref{fig:AGBDhist}. 
			At the time of this report, ", n_trees, " trees from ", n_subplot, " subplot(s) across ", n_plots, " plot(s) have been imported to the GEDI FSBD 
			and translated to a common set of geolocated tree and plot level observations.", sep = "")
	} else if(any(ii)){
		inventorytext <- paste("Metadata on the sampling protocol and methods for in situ inventory data from the ", source_project," project are shown in Table \\ref{tab:samplingtab}
			and the locations of the ", n_plots, " available plots are shown in Figure \\ref{fig:localmap}. Tree level estimates of aboveground biomass were derived 
			using the allometry of ", reference, " and the tree height-diameter model shown in Figure \\ref{fig:HDplot}. These were aggregated at the subplot 
			level to estimate aboveground biomass density at a scale that provided the best possible spatial match to GEDI lidar waveform footprints. 
			The distribution of aboveground biomass density estimates at the native and GEDI footprint scales are shown in Figure \\ref{fig:AGBDhist}. 
			At the time of this report, ", n_trees, " trees from ", n_subplot, " subplot(s) across ", n_plots, " plot(s) have been imported to the GEDI FSBD 
			and translated to a common set of geolocated tree and plot level observations.", sep = "")
	} else{
		inventorytext <- paste("Metadata on the sampling protocol and methods for in situ inventory data from the ", source_project," project are shown in Table \\ref{tab:samplingtab}
			and the locations of the ", n_plots, " available plots are shown in Figure \\ref{fig:localmap}. Tree level estimates of aboveground biomass were derived 
			using the allometry of ", reference, ". These were aggregated at the subplot 
			level to estimate aboveground biomass density at a scale that provided the best possible spatial match to GEDI lidar waveform footprints. 
			The distribution of aboveground biomass density estimates at the native and GEDI footprint scales are shown in Figure \\ref{fig:AGBDhist}. 
			At the time of this report, ", n_trees, " trees from ", n_subplot, " subplot(s) across ", n_plots, " plot(s) have been imported to the GEDI FSBD 
			and translated to a common set of geolocated tree and plot level observations.", sep = "")
	}
	