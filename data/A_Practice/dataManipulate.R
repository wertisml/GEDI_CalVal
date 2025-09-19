manipulate <- function(raw) {

  raw <- raw[!duplicated(raw[,c('individualID', 'eventID')]),]
  
  utmcoord <- t(mapply(geo2utm, raw$decimalLongitude, raw$decimalLatitude))
  
  raw$x <- utmcoord[,1] + raw$stemDistance * sin((raw$stemAzimuth)*pi/180)
  raw$y <- utmcoord[,2] + raw$stemDistance * cos((raw$stemAzimuth)*pi/180)
  
  # Trees were mapped from various points in the plot. 
  # Shift the tree coordinates based on the measurment points' locations within the plot.
  # Points: 21,23,25,31,33,39,41,43,49,51,57,59,61
  raw$x[raw$pointID==21 & !is.na(raw$pointID)] <- raw$x[raw$pointID==21 & !is.na(raw$pointID)] - 20
  raw$y[raw$pointID==21 & !is.na(raw$pointID)] <- raw$y[raw$pointID==21 & !is.na(raw$pointID)] - 20
  raw$x[raw$pointID==23 & !is.na(raw$pointID)] <- raw$x[raw$pointID==23 & !is.na(raw$pointID)] - 0
  raw$y[raw$pointID==23 & !is.na(raw$pointID)] <- raw$y[raw$pointID==23 & !is.na(raw$pointID)] - 20
  raw$x[raw$pointID==25 & !is.na(raw$pointID)] <- raw$x[raw$pointID==25 & !is.na(raw$pointID)] +	20
  raw$y[raw$pointID==25 & !is.na(raw$pointID)] <- raw$y[raw$pointID==25 & !is.na(raw$pointID)] - 20
  raw$x[raw$pointID==31 & !is.na(raw$pointID)] <- raw$x[raw$pointID==31 & !is.na(raw$pointID)] - 10
  raw$y[raw$pointID==31 & !is.na(raw$pointID)] <- raw$y[raw$pointID==31 & !is.na(raw$pointID)] - 10
  raw$x[raw$pointID==33 & !is.na(raw$pointID)] <- raw$x[raw$pointID==33 & !is.na(raw$pointID)] + 10
  raw$y[raw$pointID==33 & !is.na(raw$pointID)] <- raw$y[raw$pointID==33 & !is.na(raw$pointID)] - 10
  raw$x[raw$pointID==39 & !is.na(raw$pointID)] <- raw$x[raw$pointID==39 & !is.na(raw$pointID)] - 20
  raw$y[raw$pointID==39 & !is.na(raw$pointID)] <- raw$y[raw$pointID==39 & !is.na(raw$pointID)] - 0
  raw$x[raw$pointID==43 & !is.na(raw$pointID)] <- raw$x[raw$pointID==43 & !is.na(raw$pointID)] + 20
  raw$y[raw$pointID==43 & !is.na(raw$pointID)] <- raw$y[raw$pointID==43 & !is.na(raw$pointID)] - 0
  raw$x[raw$pointID==49 & !is.na(raw$pointID)] <- raw$x[raw$pointID==49 & !is.na(raw$pointID)] - 10
  raw$y[raw$pointID==49 & !is.na(raw$pointID)] <- raw$y[raw$pointID==49 & !is.na(raw$pointID)] + 10
  raw$x[raw$pointID==51 & !is.na(raw$pointID)] <- raw$x[raw$pointID==51 & !is.na(raw$pointID)] + 10
  raw$y[raw$pointID==51 & !is.na(raw$pointID)] <- raw$y[raw$pointID==51 & !is.na(raw$pointID)] + 10
  raw$x[raw$pointID==57 & !is.na(raw$pointID)] <- raw$x[raw$pointID==57 & !is.na(raw$pointID)] -	20
  raw$y[raw$pointID==57 & !is.na(raw$pointID)] <- raw$y[raw$pointID==57 & !is.na(raw$pointID)] + 20
  raw$x[raw$pointID==59 & !is.na(raw$pointID)] <- raw$x[raw$pointID==59 & !is.na(raw$pointID)] - 0
  raw$y[raw$pointID==59 & !is.na(raw$pointID)] <- raw$y[raw$pointID==59 & !is.na(raw$pointID)] + 20
  raw$x[raw$pointID==61 & !is.na(raw$pointID)] <- raw$x[raw$pointID==61 & !is.na(raw$pointID)] + 20
  raw$y[raw$pointID==61 & !is.na(raw$pointID)] <- raw$y[raw$pointID==61 & !is.na(raw$pointID)] + 20
  
  ## plot geometry function, used below
  write.p.geom <- function(coord.x, coord.y, dim){
    sw.x <- coord.x - dim/2
    sw.y <- coord.y - dim/2
    nw.x <- coord.x - dim/2
    nw.y <- coord.y + dim/2
    ne.x <- coord.x + dim/2
    ne.y <- coord.y + dim/2
    se.x <- coord.x + dim/2
    se.y <- coord.y - dim/2
    poly <- terra::vect(as.character(paste('POLYGON((', sw.x, sw.y, ',',
                                           nw.x, nw.y, ',',
                                           ne.x, ne.y, ',',
                                           se.x, se.y, ',',
                                           sw.x, sw.y, '))', sep = ' ')))
    #p.geom <- rgeos::writeWKT(poly)
    p.geom <- geom(poly, wkt=T, list=T)
  }
  
  raw$p.geom <- mapply(write.p.geom, utmcoord[,1], utmcoord[,2], MoreArgs = list(dim=40))
  
  # Within 40x40 m plot, there are 1-2 20x20m subplots. 
  # Single subplots are in the center of the plot.
  # If there are two subplots, they are in two quadrants of the plot.
  raw$subplot <- raw$subplotID
  raw$subplot[raw$totalSampledAreaTrees==400] <- 'center'
  raw$subplot[raw$subplot%in%c(21,31)] <- 'sw'
  raw$subplot[raw$subplot%in%c(23,32)] <- 'se'
  raw$subplot[raw$subplot%in%c(39,40)] <- 'nw'
  raw$subplot[raw$subplot==41] <- 'ne'
  
  #Create subplot geometries, offset from the plot coordinate.
  raw$sp.geom[raw$subplot=='sw' & !is.na(raw$subplot)] <- mapply(write.p.geom, utmcoord[,1][raw$subplot=='sw' & !is.na(raw$subplot)]-10, 
                                                                 utmcoord[,2][raw$subplot=='sw' & !is.na(raw$subplot)]-10, MoreArgs = list(dim=20))
  raw$sp.geom[raw$subplot=='se' & !is.na(raw$subplot)] <- mapply(write.p.geom, utmcoord[,1][raw$subplot=='se' & !is.na(raw$subplot)]+10,
                                                                 utmcoord[,2][raw$subplot=='se' & !is.na(raw$subplot)]-10, MoreArgs = list(dim=20))
  raw$sp.geom[raw$subplot=='nw' & !is.na(raw$subplot)] <- mapply(write.p.geom, utmcoord[,1][raw$subplot=='nw' & !is.na(raw$subplot)]-10, 
                                                                 utmcoord[,2][raw$subplot=='nw' & !is.na(raw$subplot)]+10, MoreArgs = list(dim=20))
  raw$sp.geom[raw$subplot=='ne' & !is.na(raw$subplot)] <- mapply(write.p.geom, utmcoord[,1][raw$subplot=='ne' & !is.na(raw$subplot)]+10, 
                                                                 utmcoord[,2][raw$subplot=='ne' & !is.na(raw$subplot)]+10, MoreArgs = list(dim=20))
  raw$sp.geom[raw$subplot=='center' & !is.na(raw$subplot)] <- mapply(write.p.geom, utmcoord[,1][raw$subplot=='center' & !is.na(raw$subplot)], 
                                                                     utmcoord[,2][raw$subplot=='center' & !is.na(raw$subplot)], MoreArgs = list(dim=20))
  
  #Remove some extraneous data entries
  raw <- raw[!is.na(raw$uid),]
  #Remove plots that are empty in some surveys but not others. Only remove them for empty surveys
  for(i in unique(raw$plotID)){
    for(j in unique(raw$key[raw$plotID==i])){
      if(all(is.na(raw$status[raw$key==j])) & !all(is.na(raw$status[raw$plotID==i]))){
        raw <- raw[raw$key!=j,]
      }
    }
  }
  
raw
}

manipulate_plot <- function(raw) {
  opts <- read_data_raw_import_options("data/usa_neonabby/plotDataImportOptions.csv")
  
  raw<- read_csv("data/usa_neonabby/plotData2020-08.csv", header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  
  # Format date
  raw$date <- as.Date(raw$date, format='%Y-%m-%d')
  
  # Assign lidar date and project
  raw$l.date[raw$eventID=='vst_ABBY_2020'] <- 2021
  
  raw$l.project[raw$eventID=='vst_ABBY_2020'] <- 'neon_abby2021'
  
  # Combine plot and survey for key to combine with tree data
  raw$key <- paste(raw$plotID, raw$eventID, sep='_')
  
raw
}

manipulate_tree <- function(raw) {

  # Load the file that explains how to read in our tree data
  opts <- read_data_raw_import_options("data/A_Practice/treeDataImportOptions.csv")
  
  # Load in the tree data
  treeData<- read_csv("data/A_Practice/treeData2020-09.csv", header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  
  # Make sure there are no duplicated columns 
  treeData <- treeData[!duplicated(treeData[,c('individualID', 'eventID')]),]
  
  treeLocation <- read_csv("data/usa_neonabby/treeLocation.csv", header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  treeLocation <- treeLocation[!duplicated(treeLocation[,'individualID'], fromLast=TRUE),]
  
  raw <- dplyr::left_join(treeData, treeLocation, by='individualID', suffix = c("", ".y"))
  
  # Remove stems under 10cm. There are multiple sub-subplots with smaller stems that we are excluding
  raw <- raw[raw$stemDiameter >= 10,]
  
  # Correct an DBH measurement. DBH and height of DBH measurement switched
  raw$stemDiameter[raw$individualID=='NEON.PLA.D16.ABBY.00862' & raw$eventID=='vst_ABBY_2016' & !is.na(raw$individualID)]<-raw$measurementHeight[raw$individualID=='NEON.PLA.D16.ABBY.00862' & raw$eventID=='vst_ABBY_2016' & !is.na(raw$individualID)]
  raw$measurementHeight[raw$individualID=='NEON.PLA.D16.ABBY.00862' & raw$eventID=='vst_ABBY_2016' & !is.na(raw$individualID)] <- 130
  
  # Set live-dead status
  raw$status <- 0
  raw$status[grep('Live', raw$plantStatus)] <- 1
  
  # Crown diameter was measured for a subset of trees.
  raw$c.w <- apply(cbind(raw$maxCrownDiameter, raw$ninetyCrownDiameter), 1, mean, na.rm=T)
  
  # Format date
  raw$tree <- as.Date(raw$date, format='%Y-%m-%d')
  
  # Remove authority from species names, and set unknown species to NA
  raw$species <- sapply(raw$scientificName, function(x){paste(unlist(strsplit(x, " "))[1:2], collapse=" ")})
  raw$species[raw$taxonRank=="genus" & !is.na(raw$taxonRank)] <- sapply(raw$species[raw$taxonRank=="genus" & !is.na(raw$taxonRank)], function(x){unlist(strsplit(x, " "))[1]})
  raw$species[raw$species=="NA NA" & !is.na(raw$species)] <- NA
  raw$species[raw$species=="Unknown plant" & !is.na(raw$species)] <- NA
  
  # Add species data to the tree data. Includes allom.key
  opts <- read_data_raw_import_options("data/usa_neonabby/speciesDataImportOptions.csv")
  species.raw <- read_csv("data/usa_neonabby/speciesData.csv",
                          header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  raw <- merge(raw, species.raw, by.x='taxonID', by.y=opts$key, all.x=TRUE, all.y=FALSE)
  
  raw$species <- raw$ScientificName
  raw$species[raw$species=="NA NA" & !is.na(raw$species)] <- NA
  
  # mode function
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # For trees with missing allom.key because species is missing, replace with the most common allom.key for the project
  raw$allom.key[is.na(raw$allom.key) & is.na(raw$species)] <- getmode(raw$allom.key[!is.na(raw$allom.key)])
  
  
  # Combine plot and survey for key to combine with plot data
  raw$key <- paste(raw$plotID, raw$eventID, sep='_')
                                       
raw
}
