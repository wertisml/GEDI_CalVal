project.data <- subset(gedicalval$plotdata, project == reportproject)
	footprint.data <- subset(gedicalval$fpdata, (project == reportproject) & !is.na(l.project))# & (obstime=='Night') & (power==9.0))

	## Plot histograms of aboveground biomass
	## First plot is hisogram of aboveground biomass at native subplot scale
	p1 <- ggplot(data=project.data, aes(agbd.ha))
	ii <- !is.na(gedicalval$plotdata[,'agbd.ha'])
	p1 <-  p1 + 
	  geom_histogram(binwidth = 20, color = "black", fill = "black") + 
	  xlim(0, 2000) + 
	  labs(x = bquote('Aboveground biomass (Mg' ~ ha^-1 * ")"),
	    y = "Number of plots") + 
	  theme_bw() + 
	  theme(plot.title = element_text(size = 0.25),
	    axis.title.y = element_text(size = 8))


	# Caption without error histogram
	caption <- paste('Distribution of aboveground biomass density at the native subplot scale (top), 
		and at the 25-m scale of the GEDI footprint (bottom) for the ', gedi.projectname, ' project (red)
		relative to all contributed sites (gray).', sep='')
	
	## Second plot is histogram at GEDI footprint scale.
	## All projects are shown in background in grey, with focal project in red.
	if(any(footprint.data[,'p.stemmap']==1)){
		p2 <- ggplot(data=footprint.data, aes(g.agbd.ha))
		ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) #  & gedicalval$fpdata[,'obstime']=='Night' & gedicalval$fpdata[,'power']==9.0
		p2 <- p2 + geom_histogram(data=gedicalval.full$fpdata[ii,],aes(g.agbd.ha), 
				color="grey50",fill="grey50",binwidth=20) + 
				geom_histogram(binwidth=20, color="red", fill="red") + xlim(0,2000) + 
				scale_y_log10() + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y="Number of GEDI footprints") + 
				theme_bw() + theme(plot.title=element_text(size=0.25), axis.title.y=element_text(size=8))
		if(any(!is.na(footprint.data[,'g.agbd.ha.sd']))){
			p3 <- ggplot(data=footprint.data, aes(g.agbd.ha.sd))
			p3 <- p3 + geom_histogram(binwidth=20,pad=TRUE, color="red", fill="red") + xlim(0,2000) + 
				scale_y_log10() + labs(x=bquote('Standard deviation Aboveground biomass (Mg' ~ha^-1*")"),y="Number of GEDI footprints") + 
				theme_bw() + theme(plot.title=element_text(size=0.25), axis.title.y=element_text(size=8))
			# Caption without error histogram
			caption <- paste('Distribution of aboveground biomass density at the native subplot scale (top), 
				and at the 25-m scale of the GEDI footprint (center) for the ', gedi.projectname, ' project (red)
				relative to all contributed sites (gray). The bottom pannel shows the distribution of error in aboveground biomass density estimates
				for the ', gedi.projectname, ' project at the GEDI footprint scale', sep='')
		}
	}else{
		p2 <- ggplot(data=footprint.data, aes(agbd.ha))
		ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha'])   #& gedicalval$fpdata[,'obstime']=='Night' & gedicalval$fpdata[,'power']==9.0
		
		p2 <- p2 +
		    geom_histogram(data=gedicalval.full$fpdata[ii,],aes(g.agbd.ha), 
				color="grey50",fill="grey50",binwidth=20) + 
				geom_histogram(binwidth=20, color="red", fill="red") + 
		    xlim(0,2000) + 
				scale_y_log10() + 
		    labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y="Number of GEDI footprints") + 
				theme_bw() + 
		    theme(plot.title=element_text(size=0.25), axis.title.y=element_text(size=8))
		
		if(any(!is.na(footprint.data[,'agbd.ha.lower']))){
			p3 <- ggplot(data=footprint.data, aes((agbd.ha.upper - agbd.ha.lower)/2))
			p3 <- p3 + geom_histogram(binwidth=20,pad=TRUE, fill="red", alpha=0.5) + xlim(0,2000) + 
				scale_y_log10() + labs(x=bquote('Uncertainty in aboveground biomass (Mg' ~ha^-1*")"),y="Number of GEDI footprints") + 
				theme_bw() + theme(plot.title=element_text(size=0.25), axis.title.y=element_text(size=8))
			caption <- paste('Distribution of aboveground biomass density at the native subplot scale (top), 
				and at the 25-m scale of the GEDI footprint (center) for the ', gedi.projectname, ' project (red)
				relative to all contributed sites (gray). The bottom pannel shows the distribution of uncertainty in aboveground biomass density estimates
				for the ', gedi.projectname, ' project at the GEDI footprint scale', sep='')
		}
	}
	if(exists("p3")){
		multiplot(p1,p2,p3)
	} else {
		multiplot(p1,p2)
	}
	
	