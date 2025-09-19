	## Figure of relative height metric versus max field height

	project.data <- subset(gedicalval$fpdata, (project == reportproject) & !is.na(l.project))# & (obstime=='Night') & (power==9.0))
	
	project.data <- project.data %>%
	  filter(rhReal98 < 300)
	
	# Stemmapped plots use height from g.h.t.max, un-mapped plots use h.t.max
	combined.h.t.max <- rep(NA, nrow(project.data))
	combined.h.t.max[project.data$p.stemmap ==1] <- project.data$g.h.t.max[project.data$p.stemmap ==1]
	combined.h.t.max[project.data$p.stemmap ==0] <- project.data$h.t.max[project.data$p.stemmap ==0]

	
	# Plot RH metric 98th percentile vs max height for focal project. Open points in black.
	ii <- !is.na(combined.h.t.max) & !is.na(project.data[,'rhReal98'])
	gg.h.t <- ggplot()
	gg.h.t <- gg.h.t + geom_point(data=project.data[ii,], 
							aes_string(x=combined.h.t.max[ii], y='rhReal98'), 
							size=1, alpha=1, pch=1, color="black")




	# Plot RH metric 98th percentile vs max height that pass height filter. Solid points in black.
	
      #ii <- !(abs(project.data[,x.var] - project.data$rhReal98)/project.data$rhReal98 > .2 | abs(project.data[,x.var] - project.data$rhReal98) > 10) & 
		#!is.na(project.data[,x.var]) & !is.na(project.data[,'rhReal98'])
		
	  ii <- !(abs(combined.h.t.max - project.data$rhReal98) > 10) & 
		!is.na(combined.h.t.max) & !is.na(project.data[,'rhReal98'])


	gg.h.t <- gg.h.t + geom_point(data=project.data[ii,], 
							aes_string(x=combined.h.t.max[ii], y='rhReal98'), 
							size=1, alpha=1, color="black") 


							

	gg.h.t <- gg.h.t + labs(x='in situ max tree height (m)',y='Simulated GEDI RH98 (m)') + theme_bw() + coord_fixed(ratio = 1)
		
		  
	gg.h.t
	