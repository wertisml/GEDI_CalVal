# Load the data from most recent RDS file

#reportproject <- "brazil_hum2014"
#setwd("L:/vclgp/minord/git/gedicalval/scripts")

#	rds.files <- list.files(path="../shiny", pattern="^gedicalval_........_r..\\.rds$")
#rds.files <- list.files(path="../shiny", pattern=paste("^gedicalval_", reportproject, "_........_r..\\.rds$", sep=""))

#rds.file.full <- list.files(path="../shiny", pattern="^gedicalval_20210512_r03\\.rds$")
#reportproject <- "usa_falkowski"

#rds.file.full <- list.files(path="../shiny", pattern="^gedicalval_20220512_r03\\.rds$")
#rds.files <- list.files(path="../shiny", pattern=paste("^gedicalval_", reportproject, "_........_r03\\.rds$", sep=""))

#if ( length(rds.files) > 0 ) {
#  rds.file <- rds.files[length(rds.files)]
#} else {
#  rds.file <- "gedicalval.rds"
#}
#print(rds.file)
#print(rds.file.full)

#gedicalval <- readRDS(paste('..','shiny', rds.file, sep=.Platform$file.sep))




requireStemMap=FALSE
theProjects=reportproject
rh_type=c("rhReal")
rh_offset=100
noised=FALSE
aggregation=1
minFootprints=20
maxFootprints=200
allAGB=F
UseChaveWSG=T
requireValid=T
beamDenseFilter=T
beamDenseThresh=2
coverFilter=T
AGBDFilter=T
heightDiscrepFilter=T
heightDiscrepThresh=10
edgeFilter=T
edgeThresh=0.9
  
  #require(sp)
  #require(rgdal)
  require(terra)
  require(maps)
  


  
  temp <- subset(gedicalval$fpdata, (project == reportproject))
  
  table.out <- data.frame(n.footprints = sum(temp$noise=="unnoised" & !is.na(temp$noise)))
  table.out$noALS <- NA
  table.out$noAGB <- NA
  table.out$noAgg <- NA
  table.out$invalid <- NA
  table.out$minFootprintFilter <- NA
  table.out$beamDenseFilter <- NA
  table.out$AGBDFilter <- NA
  table.out$coverFilter <- NA
  table.out$edgeFilter <- NA
  table.out$heightDiscrepFilter <- NA
  table.out$n.remaining <- NA
  
  
  # drop surveys other than 2009 at costarica_laselva and costarica_chazdon
  
  #costarica <- temp[which(temp$project=="costarica_laselva" | temp$project=="costarica_chazdon"),]
  #costarica <- costarica[which(costarica$survey==2009),]
  #temp <- temp[-which(temp$project=="costarica_laselva" | temp$project=="costarica_chazdon"),]
  #temp <- rbind(temp,costarica)
  
  # populate pft.name at czechrepublic_zofin with Deciduous Broadleaf trees
  
  temp[which(temp$project=="czechrepublic_zofin"),which(names(temp)=="pft.name")] <- "Deciduous Broadleaf trees"
  
  

  
  # For each simulated waveform, is there a value in pft.name? If so, use it. If not, use pft.modis, then drop pft.name, and rename pft.modis as pft.name
  
  temp[which(is.na(temp$pft.name)==F),"pft.modis"] <- temp[which(is.na(temp$pft.name)==F),"pft.name"]   # if there is something in pft.name, then overwrite pft.modis with pft.name
  temp <- temp[,-which(names(temp)=="pft.name")]   # drop pft.name
  names(temp)[which(names(temp)=="pft.modis")] <- "pft.name"   # rename pft.modis as pft.name
  
  # if (noised==TRUE){
  #   
  #   temp <- temp[which(temp$noise=="noised"),]
  #   
  # }
  # 
  # if (noised==FALSE){
  #   
  #   temp <- temp[which(temp$noise=="unnoised"),]
  #   
  # }
  
    # drop records with NA in rhReal10 (as a proxy for records that do not intersect ALS data

  table.out$noALS <- sum(is.na(temp$rhReal10))
  if(any(is.na(temp$rhReal10))){
    temp <- temp[-which(is.na(temp$rhReal10)),] 
  }
    
  # compute derived RH metrics
  
  temp[,grep("rhReal", dimnames(temp)[[2]])] <- temp[,grep("rhReal", dimnames(temp)[[2]])] + rh_offset   # add offset to deal with negative RH metrics
  
  theMetrics <- c(paste("rhReal",seq(5,95,5),sep=""),"rhReal98")   # extract RH metrics in increments of 5% plus rh_98
  thePairs <- combn(theMetrics,2)   # indices for two-way interactions
  theInters <- matrix(NA, nrow=dim(temp)[1], ncol=dim(thePairs)[2])   # matrix to hold two-way interactions
  dimnames(theInters)[[2]] <- seq(1,dim(theInters)[2])
  
  for (i in 1:dim(theInters)[2]){
    
    theInters[,i] <- ((temp[,which(dimnames(temp)[[2]]==thePairs[1,i])]) * (temp[,which(dimnames(temp)[[2]]==thePairs[2,i])]))
    first <- strsplit(thePairs[1,i],"rhReal")[[1]][2]
    if (nchar(first)==1){first <- paste("0",first,sep="")}
    second <- strsplit(thePairs[2,i],"rhReal")[[1]][2]
    dimnames(theInters)[[2]][i] <- paste("RH_",first,"_RH_",second,sep="")
    
  }
  
  RH_SUM=temp$rhReal5+temp$rhReal10+temp$rhReal15+temp$rhReal20+temp$rhReal25+temp$rhReal30+temp$rhReal35+temp$rhReal40+temp$rhReal45+temp$rhReal50+temp$rhReal55+temp$rhReal60+temp$rhReal65+temp$rhReal70+temp$rhReal75+temp$rhReal80+temp$rhReal85+temp$rhReal90+temp$rhReal95
  
  # rename Grass and Shrub pft.name to Grass/Shrub, assemble variables into one file
  
  temp$pft.name <- paste(temp$pft.name)
  temp[which(temp$pft.name=="Grass" | temp$pft.name=="Shrub"),grep("pft.name",dimnames(temp)[[2]])] <- "GS"
  temp[which(temp$pft.name=="Evergreen Broadleaf trees"),grep("pft.name",dimnames(temp)[[2]])] <- "EBT"
  temp[which(temp$pft.name=="Evergreen Needleleaf trees"),grep("pft.name",dimnames(temp)[[2]])] <- "ENT"
  temp[which(temp$pft.name=="Deciduous Broadleaf trees"),grep("pft.name",dimnames(temp)[[2]])] <- "DBT"
  temp[which(temp$pft.name=="Deciduous Needleleaf trees"),grep("pft.name",dimnames(temp)[[2]])] <- "DNT"
  
  # rename SEAsia and SEAs to As 
  
  temp[which(temp$region=="SEAs"),which(dimnames(temp)[[2]]=="region")] <- "As"
  temp[which(temp$region=="SEAsia"),which(dimnames(temp)[[2]]=="region")] <- "As"
  
  # make PFTbyRegion
  
  temp <- cbind(temp,theInters,RH_SUM,paste(temp$pft.name,temp$region,sep="_"))
  dimnames(temp)[[2]][length(dimnames(temp)[[2]])] <- 'PFTbyRegion'
  temp$PFTbyRegion <- as.character(temp$PFTbyRegion)
  
  # select the RH metrics (e.g., rhReal, rhInfl, etc.)
  
  theInd <- which(dimnames(temp)[[2]] %in% paste(rh_type,c(seq(5,95,5),98),sep="")==T)
  theInd <- c(theInd,grep("inter",dimnames(temp)[[2]]))
  theInd <- c(theInd,grep("RH_",dimnames(temp)[[2]]))
  
  # include MAP and MAT
  
  theInd <- c(theInd,which(dimnames(temp)[[2]]=="mat"))
  theInd <- c(theInd,which(dimnames(temp)[[2]]=="map"))
  
  # include plot, survey and subplot
  
  theInd <- c(theInd, which(dimnames(temp)[[2]]=="plot"))
  theInd <- c(theInd, which(dimnames(temp)[[2]]=="survey"))
  theInd <- c(theInd, which(dimnames(temp)[[2]]=="subplot"))
  
  # subset stem mapped (or not) data, and identify columns that contain variables of interest
  
  if (requireStemMap==T){
    
	
    p.stemmap <- temp$p.stemmap
    theInd <- c(which(dimnames(temp)[[2]] %in% c("project","g.agbd.ha", "g.sba.ha", "g.wsg.ba","g.nfootprints","g.ntracks","pft.name","region","cover","niM2","FHD","trailingedgeextent","leadingedgeext","g.x","g.y","latitude","longitude","l.epsg","PFTbyRegion","g.agb.valid","agb.valid","beamDense","g.h.t.max","g.edge.frac")),theInd)
	temp <- temp[which(temp$p.stemmap==1),sort(theInd)]
    
    if (allAGB==T){         
      
      # identify records with NA in the AGB, BA, or WD variable, omit these
      
      theNAs <- which(apply(temp[,which(dimnames(temp)[[2]] %in% c("g.agbd.ha","g.sba.ha","g.wsg.ba"))],1,function(x) length(which(is.na(x)==T)))>0)
      theInd <- seq(1,dim(temp)[1],1)
      bad <- unique(c(theNAs))
	  
      if (length(bad)>0){theInd <- theInd[-bad]}
      temp <- temp[theInd,]
      p.stemmap <- p.stemmap[theInd]
      
      dimnames(temp)[[2]][grep("agbd",dimnames(temp)[[2]])] <- "AGBD"
      dimnames(temp)[[2]][grep("wsg",dimnames(temp)[[2]])] <- "WSG"
      dimnames(temp)[[2]][grep("ba",dimnames(temp)[[2]])] <- "BA"
      dimnames(temp)[[2]][grep("g.h.t.max",dimnames(temp)[[2]])] <- "h.max"
      
    }
    
    if (allAGB==F){
      
      # identify records with NA in AGB omit these
      
      theNAs <- which(is.na(temp[,which(dimnames(temp)[[2]] %in% c("g.agbd.ha"))])==T)
      theInd <- seq(1,dim(temp)[1],1)
      bad <- unique(c(theNAs))
	  
	  table.out$noAGB <- length(bad)

      if (length(bad)>0){theInd <- theInd[-bad]}
      temp <- temp[theInd,]
      p.stemmap <- p.stemmap[theInd]
      
      dimnames(temp)[[2]][grep("agbd",dimnames(temp)[[2]])] <- "AGBD"
      dimnames(temp)[[2]][grep("wsg",dimnames(temp)[[2]])] <- "WSG"
      dimnames(temp)[[2]][grep("ba",dimnames(temp)[[2]])] <- "BA"
      dimnames(temp)[[2]][grep("g.h.t.max",dimnames(temp)[[2]])] <- "HEIGHT"
      
    }
    
  }
  
  if (requireStemMap==F){
    
    p.stemmap <- temp$p.stemmap
    theInd <- c(which(dimnames(temp)[[2]] %in% c("project", "agbd.ha","g.agbd.ha","sba.ha","g.sba.ha","swsg.ba","g.wsg.ba","g.nfootprints","g.ntracks","pft.name","region","cover","niM2","FHD","trailingedgeextent","leadingedgeext","g.x","g.y","latitude","longitude","l.epsg","PFTbyRegion","g.agb.valid","agb.valid","beamDense","g.h.t.max","h.t.max","g.edge.frac","p.stemmap")),theInd)
    temp <- temp[,theInd]
    
    theNAs <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    theZeros <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    
    if (allAGB==T){
      
      # identify records with NA in the AGB, BA, or WD variable, omit these
      
      for (i in 1:dim(temp)[1]){
        #browser()
        #print(p.stemmap[i])
        if (p.stemmap[i]==1){
          
          if (is.na(temp$g.agbd.ha[i])==TRUE | is.na(temp$g.sba.ha[i])==TRUE | is.na(temp$g.wsg.ba[i])==TRUE){ 
            
            theNAs[i,1] <- 1
            
          }
          
        } 
        
        if (p.stemmap[i]==0){
          
          if (is.na(temp$agbd.ha[i])==TRUE | is.na(temp$sba.ha[i])==TRUE | is.na(temp$swsg.ba[i])==TRUE){ 
            
            theNAs[i,1] <- 1
            
          }
          
        }
        
      } 
      
      theNAs <- which(theNAs==1)
      theInd <- seq(1,dim(temp)[1],1)
      bad <- unique(c(theNAs))
	  
	  	  table.out$noAGB <- length(bad)

	  
      if (length(bad)>0){theInd <- theInd[-bad]}
      temp <- temp[theInd,]
      p.stemmap <- p.stemmap[theInd]
      
    }
    
    if (allAGB==F){
      
      for (i in 1:dim(temp)[1]){
        #print(p.stemmap[i])
        if (p.stemmap[i]==1){
          
          if (is.na(temp$g.agbd.ha[i])==TRUE){ 
            
            theNAs[i,1] <- 1
            
          }
          
        } 
        
        if (p.stemmap[i]==0){
          
          if (is.na(temp$agbd.ha[i])==TRUE){ 
            
            theNAs[i,1] <- 1
            
          }
          
        }
        
      }
      
      theNAs <- which(theNAs==1)
      theInd <- seq(1,dim(temp)[1],1)
      bad <- unique(c(theNAs))
	  
	  	  	  table.out$noAGB <- length(bad)

	  
      if (length(bad)>0){theInd <- theInd[-bad]}
      temp <- temp[theInd,]
      p.stemmap <- p.stemmap[theInd]
      
    }
    
    # combine g. and non-g. variables into single variables
    # if the plot is stem mapped, take g., if not, take non-g.
    
    AGBD <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    AGBD[which(p.stemmap==1)] <- temp$g.agbd.ha[which(p.stemmap==1)]
    AGBD[which(p.stemmap==0)] <- temp$agbd.ha[which(p.stemmap==0)]
    AGBD[which(AGBD==0),1] <- 0.001
    
    BA <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    BA[which(p.stemmap==1)] <- temp$g.sba.ha[which(p.stemmap==1)]
    BA[which(p.stemmap==0)] <- temp$sba.ha[which(p.stemmap==0)]
    BA[which(BA==0),1] <- 0.001
    
    WSG <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    WSG[which(p.stemmap==1)] <- temp$g.wsg.ba[which(p.stemmap==1)]
    WSG[which(p.stemmap==0)] <- temp$swsg.ba[which(p.stemmap==0)]
    WSG[which(WSG==0),1] <- 0.001
    
    HEIGHT <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    HEIGHT[which(p.stemmap==1),1] <- temp$g.h.t.max[which(p.stemmap==1)]
    HEIGHT[which(p.stemmap==0),1] <- temp$h.t.max[which(p.stemmap==0)]
    HEIGHT <- HEIGHT + rh_offset
    
    temp <- temp[,-which(dimnames(temp)[[2]] %in% c("agbd.ha","sba.ha","swsg.ba","g.h.t.max"))]
    dimnames(temp)[[2]][grep("agbd",dimnames(temp)[[2]])] <- "AGBD"
    dimnames(temp)[[2]][grep("wsg",dimnames(temp)[[2]])] <- "WSG"
    dimnames(temp)[[2]][grep("ba",dimnames(temp)[[2]])] <- "BA"
    dimnames(temp)[[2]][grep("h.t.max",dimnames(temp)[[2]])] <- "HEIGHT"
    
    temp$AGBD <- as.numeric(AGBD)
    temp$BA <- as.numeric(BA)
    temp$WSG <- as.numeric(WSG)
    temp$HEIGHT <- as.numeric(HEIGHT)
    
  }
  
  if (aggregation==1){
    
	table.out$noAgg <- sum(!(temp$g.ntracks==1 & temp$g.nfootprints==1))

    temp <- temp[which(temp$g.ntracks==1 & temp$g.nfootprints==1),]
    
  }
  
  projects <- names(which(table(temp$project)>minFootprints))
  table.out$minFootprintFilter <- nrow(temp[which(!(temp$project %in% projects)),])
  temp$minFootprintFilter <- !(temp$project %in% projects)
  #temp <- temp[which(temp$project %in% projects),]
  
  names(temp)[grep("latitude", names(temp))] <- "Lat"
  names(temp)[grep("longitude", names(temp))] <- "Long"
  
  if (UseChaveWSG==T){
    
    theCountries = map.where(database="world", x=temp$Long, y=temp$Lat)
    ChaveWSGValues <- matrix(NA, nrow=dim(temp)[1], ncol=1)
    
    ChaveWSGValues[which(temp$region=="Af" & abs(temp$Lat) < 23.43696),1] <- 0.598
    ChaveWSGValues[which(temp$region=="Af" & abs(temp$Lat) >= 23.43696),1] <- 0.648
    ChaveWSGValues[which(temp$region=="Au" & abs(temp$Lat) < 23.43696),1] <- 0.636
    ChaveWSGValues[which(temp$region=="Au" & abs(temp$Lat) >= 23.43696),1] <- 0.725
    ChaveWSGValues[which(theCountries=="Belize" | theCountries=="Costa Rica" | theCountries=="El Salvador" | theCountries=="Guatemala" | theCountries=="Honduras" | theCountries=="Nicaragua" | theCountries=="Panama"),1] <- 0.56
    ChaveWSGValues[which(theCountries=="China" | theCountries=="Japan:Honshu"),1] <- 0.541
    ChaveWSGValues[which(temp$region=="Eu"),1] <- 0.525
    ChaveWSGValues[which(theCountries=="India"),1] <- 0.652
    ChaveWSGValues[which(theCountries=="Madagascar"),1] <- 0.662
    ChaveWSGValues[which(theCountries=="Mexico"),1] <- 0.676
    ChaveWSGValues[which(theCountries=="Canada" | theCountries=="USA"),1] <- 0.54
    ChaveWSGValues[which(temp$region=="SA" & abs(temp$Lat) < 23.43696),1] <- 0.632
    ChaveWSGValues[which(temp$region=="SA" & abs(temp$Lat) >= 23.43696),1] <- 0.715
    ChaveWSGValues[which(temp$region=="As" & abs(temp$Lat) < 23.43696),1] <- 0.574
    ChaveWSGValues[which(temp$region=="As" & abs(temp$Lat) >= 23.43696),1] <- 0.559
    ChaveWSGValues[which(temp$project=="usa_falkowski"),1] <- 0.54   # not sure why this project maps to Canada:27 in theCountries
    ChaveWSG <- ChaveWSGValues
    temp <- cbind(temp, ChaveWSG)
    
  }
  
  # subset the projects of interest
  
  unfiltered <- temp
  filtered <- matrix(0, dim(temp)[1], ncol=13)
  dimnames(filtered)[[2]] <- c("project","requireValid","beamDense","coverFilter","AGBDFilter1","AGBDFilter2","edgeFilter","heightDiscrepFilter","cereal","project_id","pft","region","pft_region")
  filtered <- data.frame(filtered)
  filtered$project_id <- unfiltered$project
  filtered$pft <- unfiltered$pft.name
  filtered$region <- unfiltered$region
  filtered$pft_region <- unfiltered$PFTbyRegion
  
  if (is.null(theProjects)==FALSE){
    
    filtered[-which(unfiltered$project %in% theProjects),1] <- 1
    temp <- temp[which(temp$project %in% theProjects),]
    
  }
  
  if (is.null(projects)==TRUE){
    
    projects <- sort(unique(temp$project))
    
  }
  
  # omit invalid footprints
  
  if (requireValid==T){
    
    valid <- matrix(0, nrow=dim(temp)[1], ncol=1)
    valid[which(temp$p.stemmap==1 & temp$g.agb.valid==1),1] <- 1
    valid[which(temp$p.stemmap==0 & temp$agb.valid==1),1] <- 1
    valid[which(temp$p.stemmap==0 & is.na(temp$agb.valid)),1] <- 1
    filtered[which(valid==1),2] <- 1
	
	table.out$invalid <- sum(!(valid==1))
	
    #temp <- temp[which(valid==1),]
    
  }
  
  # apply beam density filter
  
  if (beamDenseFilter==T){
    
    if (length(which(temp$beamDense<beamDenseThresh))>0){
      
      filtered[-which(unfiltered$beamDense>beamDenseThresh),3] <- 1
	  
	  	table.out$beamDenseFilter <- sum(!(temp$beamDense>beamDenseThresh) & !is.na(temp$beamDense))
	  	
	  	temp$beamDenseFilter <- !(temp$beamDense>beamDenseThresh) & !is.na(temp$beamDense)
	  
      #temp <- temp[which(temp$beamDense>beamDenseThresh),]
      
    }
    
    if(is.na(table.out$beamDenseFilter)){
      table.out$beamDenseFilter <- 0
      temp$beamDenseFilter <- FALSE
    }
    
  }
  
  # apply cover filter
  
  if (coverFilter==T){
    
    if (length(which(temp$cover==0 & (temp$rhReal98-rh_offset)>5))>0){
      
      filtered[which(unfiltered$cover==0 & (unfiltered$rhReal98-rh_offset)>5),4] <- 1
	  
	  table.out$coverFilter <- sum((temp$cover==0 & (temp$rhReal98-rh_offset)>5) & !is.na(temp$cover))
	   
	  temp$coverFilter <- (temp$cover==0 & (temp$rhReal98-rh_offset)>5 & !is.na(temp$cover))
	  
      #temp <- temp[-which(temp$cover==0 & (temp$rhReal98-rh_offset)>5),]
      
    }
    
    
    if(is.na(table.out$coverFilter)){
      table.out$coverFilter <- 0
      temp$coverFilter <- FALSE
    }
    
  }
  
  # apply AGBD filter
  
  if (AGBDFilter==T){
    
    if (length(which(temp$AGBD<1 & (temp$rhReal98-rh_offset)>5))>0){
      
      filtered[which(unfiltered$AGBD<1 & (unfiltered$rhReal98-rh_offset)>5),5] <- 1
	  
	  	  table.out$AGBDFilter <- sum(temp$AGBD<1 & (temp$rhReal98-rh_offset)>5 & !is.na(temp$AGBD))
	  	  temp$AGBDFilter <- (temp$AGBD<1 & (temp$rhReal98-rh_offset)>5 & !is.na(temp$AGBD))
	  
      #temp <- temp[-which(temp$AGBD<1 & (temp$rhReal98-rh_offset)>5),]
      
    }
    
    if (length(which(temp$AGBD>150 & (temp$rhReal98-rh_offset)<5))>0){
      
      filtered[which(unfiltered$AGBD>150 & (unfiltered$rhReal98-rh_offset)<5),6] <- 1
	  
	  table.out$AGBDFilter <- sum(temp$AGBD>150 & (temp$rhReal98-rh_offset)<5 & !is.na(temp$AGBD))
    temp$AGBDFilter <- (temp$AGBD>150 & (temp$rhReal98-rh_offset)<5 & !is.na(temp$AGBD))
	  
      #temp <- temp[-which(temp$AGBD>150 & (temp$rhReal98-rh_offset)<5),]
      
    }
    
    if(is.na(table.out$AGBDFilter)){
      table.out$AGBDFilter <- 0
      temp$AGBDFilter <- FALSE
    }
    
  }
  
  # apply edge filter
  
  if (edgeFilter==T){
    
    if (length(which(temp$g.edge.frac>edgeThresh))>0){
      
      filtered[-which(unfiltered$g.edge.frac>edgeThresh),7] <- 1
	  
	   table.out$edgeFilter <- sum(!(temp$g.edge.frac>edgeThresh) & !is.na(temp$g.edge.frac))

	   temp$edgeFilter <- !(temp$g.edge.frac>edgeThresh & !is.na(temp$g.edge.frac))
	   
      #temp <- temp[which(temp$g.edge.frac>edgeThresh),]
      
    }
    
    if(is.na(table.out$edgeFilter)){
      table.out$edgeFilter <- 0
      temp$edgeFilter <- FALSE
    }
    
  }
  
  # apply height discrepancy filter
  
  if (heightDiscrepFilter==T){
    
    if (length(which(abs(temp$HEIGHT-temp$rhReal98)>heightDiscrepThresh))>0){
      
      filtered[which(abs(unfiltered$HEIGHT-unfiltered$rhReal98)>heightDiscrepThresh),8] <- 1
	  
	  	   table.out$heightDiscrepFilter <- sum(abs(temp$HEIGHT-temp$rhReal98)>heightDiscrepThresh & !is.na(temp$HEIGHT))


	  	temp$heightDiscrepFilter <- (abs(temp$HEIGHT-temp$rhReal98)>heightDiscrepThresh & !is.na(temp$HEIGHT))
	  	   
      #temp <- temp[-which(abs(temp$HEIGHT-temp$rhReal98)>heightDiscrepThresh),]
      
    }
    
    if(is.na(table.out$heightDiscrepFilter)){
      table.out$heightDiscrepFilter <- 0
      temp$heightDiscrepFilter <- FALSE
    }
    
  }
  
  
  table.out$n.remaining <- nrow(temp) - sum(temp$minFootprintFilter==TRUE | temp$beamDenseFilter==TRUE | temp$AGBDFilter==TRUE | temp$edgeFilter==TRUE | temp$coverFilter==TRUE | temp$heightDiscrepFilter==TRUE)
  
table.out <- t(table.out)
table.out[,1] <- as.integer(table.out[,1])

description <- c("Total number of footprints", 
	"No airborne lidar available",
	"No aboveground biomass estimate",
	"Filter out aggregated footprint simulations",
	"Tree DBH outside range of allometric equation",
	"Site has fewer than minimum required number of footprints",
	"Airborne lidar pulse density is too low",
	"Aboveground biomass value is implausible based on lidar-measured height",
	"Canopy cover value is implausible based on lidar-measured height",
	"Simulated footprint falls on the edge of a plot",
	"Field-measured height and lidar-measured height differ by more than 10m",
	"Total number of footprints after filtering")

table.out <- cbind(rownames(table.out), table.out, description)
colnames(table.out) <- c("Filter name", "N footprints", "Description")
rownames(table.out) <- NULL	
	
	
  