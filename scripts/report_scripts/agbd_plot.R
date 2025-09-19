	## Figure of relative height metric versus aboveground biomass


	project.data <- subset(gedicalval$fpdata, (project == reportproject) & !is.na(l.project)) # & (obstime=='Night') & (power==9.0))
	#plot.data <- subset(gedicalval$fpdata, (project == reportproject) & (plot == input$plot) & !is.na(l.project))
	filters <- temp[,c('g.x','g.y','minFootprintFilter','beamDenseFilter','coverFilter','AGBDFilter','edgeFilter','heightDiscrepFilter')] #temp from footprint_filter_table.R
	project.data <- dplyr::left_join(project.data, filters) #temp from footprint_filter_table.R
	filter.data <- project.data[,c('minFootprintFilter','beamDenseFilter','coverFilter','AGBDFilter','edgeFilter','heightDiscrepFilter')] 
	filtered <- apply(filter.data,1,any, na.rm=T)
	
	# Stemmapped plots use biomass from g.agbd.ha, un-mapped plots use agbd.ha
	combined.agbd.ha <- rep(NA, nrow(project.data))
	combined.agbd.ha[project.data$p.stemmap ==1] <- project.data$g.agbd.ha[project.data$p.stemmap ==1]
	combined.agbd.ha[project.data$p.stemmap ==0] <- project.data$agbd.ha[project.data$p.stemmap ==0]



	# Plot RH metric 98th percentile vs biomass for all stemmapped projects. Points in black.
	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'rhReal98']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
	gg98 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='rhReal98'))
	gg98 <- gg98 + geom_point(size=1, alpha=0.05, color="black")
	# Plot RH metric 98th percentile vs biomass for all non-stemmapped projects. Points in black.
	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'rhReal98']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
	gg98 <- gg98 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='rhReal98'),size=1, alpha=0.05, color="black")
	
	# Plot RH metric 98th percentile vs biomass for focal project. Points in red.
	ii <- !is.na(combined.agbd.ha) & !is.na(project.data[,'rhReal98'])
	# Footprints that will be filtered out
	gg98 <- gg98 + geom_point(data=project.data[ii & filtered,], 
							aes_string(x=combined.agbd.ha[ii & filtered], y='rhReal98'), 
							size=1, alpha=1, pch=1, color="red") 
	# Footprints that will be included						
	gg98 <- gg98 + geom_point(data=project.data[ii & !filtered,], 
							aes_string(x=combined.agbd.ha[ii & !filtered], y='rhReal98'), 
							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
	gg98 <- gg98 + geom_smooth(data=project.data[ii & !filtered,],   #Trendline of included points
				aes_string(x=combined.agbd.ha[ii & !filtered], y='rhReal98'), 
				size=0.5, alpha=0.5, linetype="dashed", color="red", se = FALSE, method = 'lm',formula = y ~ log(x))						
#	# Add standard deviation to biomass if available
#	if(any(project.data[,'p.stemmap']==1) & any(!is.na(project.data[,'g.agbd.ha.sd']))){
#		gg98 <- gg98 + geom_errorbarh(data=project.data[ii,], 
#							aes(xmin=g.agbd.ha-g.agbd.ha.sd, xmax=g.agbd.ha+g.agbd.ha.sd, y=rhReal98), height=.2,
#							color = 'red', alpha = 0.2)
#	}
	gg98 <- gg98 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='GEDI RH98 (m)') + theme_bw()
		
	# Repeat above for RH metric 50th percentile  
	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'rhReal50']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
	gg50 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='rhReal50'))
	gg50 <- gg50 + geom_point(size=1, alpha=0.05, color="black")
	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'rhReal50']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
	gg50 <- gg50 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='rhReal98'),size=1, alpha=0.05, color="black")
	ii <- !is.na(combined.agbd.ha) & !is.na(project.data[,'rhReal50'])
	# Footprints that will be filtered out
	gg50 <- gg50 + geom_point(data=project.data[ii & filtered,], 
							aes_string(x=combined.agbd.ha[ii & filtered], y='rhReal50'), 
							size=1, alpha=1, pch=1, color="red") 
	# Footprints that will be included	
	gg50 <- gg50 + geom_point(data=project.data[ii & !filtered,], 
							aes_string(x=combined.agbd.ha[ii & !filtered], y='rhReal50'), 
							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
	gg50 <- gg50 + geom_smooth(data=project.data[ii & !filtered,], 
				aes_string(x=combined.agbd.ha[ii & !filtered], y='rhReal50'), 
				size=0.5, alpha=0.5, linetype="dashed", color="red", se = FALSE, method = 'lm',formula = y ~ log(x))	
	gg50 <- gg50 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='GEDI RH50 (m)') + theme_bw()

#	# Add to the figure caption if error bars are presented.						
#	caption <- ""
#	if(any(project.data[,'p.stemmap']==1) & any(!is.na(project.data[,'g.agbd.ha.sd']))){
#		gg50 <- gg50 + geom_errorbarh(data=project.data[ii,], 
#				aes(xmin=g.agbd.ha-g.agbd.ha.sd, xmax=g.agbd.ha+g.agbd.ha.sd, y=rhReal50), height=.2,
#				color = 'red', alpha = 0.2)
#		caption <- 'Error bars show standard deviation of aboveground biomass.'
#	}


#ICESat-2 simulations
#	# Plot RH metric 98th percentile vs biomass for all stemmapped projects. Points in black.
#	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.ground.rhReal98']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
#	i2ground98 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='I2.ground.rhReal98'))
#	i2ground98 <- i2ground98 + geom_point(size=1, alpha=0.05, color="black")
#	# Plot RH metric 98th percentile vs biomass for all non-stemmapped projects. Points in black.
#	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.ground.rhReal98']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
#	i2ground98 <- i2ground98 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='I2.ground.rhReal98'),size=1, alpha=0.05, color="black")
#	
#	# Plot RH metric 98th percentile vs biomass for focal project. Points in red.
#	ii <- !is.na(project.data[,x.var]) & !is.na(project.data[,'I2.ground.rhReal98'])
#	i2ground98 <- i2ground98 + geom_point(data=project.data[ii,], 
#							aes_string(x=x.var, y='I2.ground.rhReal98'), 
#							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
#	i2ground98 <- i2ground98 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='I2 w/ ground RH98 (m)') + theme_bw()
#
#	# Repeat above for RH metric 50th percentile  
#	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.ground.rhReal50']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
#	i2ground50 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='rhReal50'))
#	i2ground50 <- i2ground50 + geom_point(size=1, alpha=0.05, color="black")
#	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.ground.rhReal50']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
#	i2ground50 <- i2ground50 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='rhReal98'),size=1, alpha=0.05, color="black")
#	ii <- !is.na(project.data[,x.var]) & !is.na(project.data[,'I2.ground.rhReal50'])
#	i2ground50 <- i2ground50 + geom_point(data=project.data[ii,], 
#							aes_string(x=x.var, y='I2.ground.rhReal50'), 
#							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
#	i2ground50 <- i2ground50 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='I2 w/ ground RH50 (m)') + theme_bw()
#	
#		# Plot RH metric 98th percentile vs biomass for all stemmapped projects. Points in black.
#	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.noground.rhReal98']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
#	i2noground98 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='I2.noground.rhReal98'))
#	i2noground98 <- i2noground98 + geom_point(size=1, alpha=0.05, color="black")
#	# Plot RH metric 98th percentile vs biomass for all non-stemmapped projects. Points in black.
#	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.noground.rhReal98']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
#	i2noground98 <- i2noground98 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='I2.noground.rhReal98'),size=1, alpha=0.05, color="black")
#	
#	# Plot RH metric 98th percentile vs biomass for focal project. Points in red.
#	ii <- !is.na(project.data[,x.var]) & !is.na(project.data[,'I2.noground.rhReal98'])
#	i2noground98 <- i2noground98 + geom_point(data=project.data[ii,], 
#							aes_string(x=x.var, y='I2.noground.rhReal98'), 
#							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
#	i2noground98 <- i2noground98 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='I2 w/o ground RH98 (m)') + theme_bw()
#
#	# Repeat above for RH metric 50th percentile  
#	ii <- !is.na(gedicalval.full$fpdata[,'g.agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.noground.rhReal50']) &  gedicalval.full$fpdata[,'g.agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==1
#	i2noground50 <- ggplot(data=gedicalval.full$fpdata[ii,],aes_string(x='g.agbd.ha', y='rhReal50'))
#	i2noground50 <- i2noground50 + geom_point(size=1, alpha=0.05, color="black")
#	ii <- !is.na(gedicalval.full$fpdata[,'agbd.ha']) & !is.na(gedicalval.full$fpdata[,'I2.noground.rhReal50']) &  gedicalval.full$fpdata[,'agb.valid']==1 &
#				gedicalval.full$fpdata[,'obstime']=='Night' & gedicalval.full$fpdata[,'power']==9.0 & gedicalval.full$fpdata[,'p.stemmap']==0
#	i2noground50 <- i2noground50 + geom_point(data=gedicalval.full$fpdata[ii,],aes_string(x='agbd.ha', y='rhReal98'),size=1, alpha=0.05, color="black")
#	ii <- !is.na(project.data[,x.var]) & !is.na(project.data[,'I2.noground.rhReal50'])
#	i2noground50 <- i2noground50 + geom_point(data=project.data[ii,], 
#							aes_string(x=x.var, y='I2.noground.rhReal50'), 
#							size=1, alpha=1, color="red") + xlim(0,2000) + ylim(0,70)
#	i2noground50 <- i2noground50 + labs(x=bquote('Aboveground biomass (Mg' ~ha^-1*")"),y='I2 w/o ground RH50 (m)') + theme_bw()

#	multiplot(gg98,gg50,i2ground98,i2ground50,i2noground98,i2noground50, cols=2, layout=matrix(c(1,2,3,4,5,6), nrow=3, byrow=TRUE))		  
	multiplot(gg98,gg50, cols=2, layout=matrix(c(1,1,2,2), nrow=1, byrow=TRUE))
	