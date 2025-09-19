	## Map of the world, with points for each plot
	## Focal project is in red, other projects in black
	
	world <- map_data("world")
	gg <- ggplot()
	gg <- gg + geom_map(data=world, map=world,
				aes(map_id=region),
				color="#595959", fill="#e8e8e8", size=0.05, alpha=1)
	# Non-focal projects in black
	gg <- gg + geom_point(data=gedicalval.full$plotdata, 
				aes(x=longitude, y=latitude), 
				size=0.5, alpha=1, color="black")
	# Focal project in red
	gg <- gg + geom_point(data=gedicalval$plotdata[gedicalval$plotdata$project == reportproject,], 
				aes(x=longitude, y=latitude), 
				size=2, alpha=1, color="red")
	# Dashed lines to show GEDI's latitudinal extent
	gg <- gg + geom_line(aes(x=c(-180,180),y=c(51,51)),linetype="dashed",color="blue")
	gg <- gg + geom_line(aes(x=c(-180,180),y=c(-51,-51)),linetype="dashed",color="blue")
	gg <- gg + theme(legend.position="none",panel.border=element_rect(colour="black",fill=NA,linewidth=0.5)) +  
				labs(x="Longitude",y="Latitude") + coord_equal() + theme_bw() +
				scale_x_continuous(expand = c(0, 0), limits = c(-180, 180)) + 
				scale_y_continuous(expand = c(0, 0), limits = c(-84, 84))  
	gg