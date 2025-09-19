post_process <- function(data, allometric_definitions, wsg_database, metadata, project) {
  browser()
  ## Standardize plot, subplot and tree geometry to UTM/WGS84
  ## Plot point location coordinates are latitude/longitude
  data <- standardize_geometry(data)
  
  ## Fill missing derived variables
  data <- fill_derived_variables(data)
  
  ## Fill wood specific gravity estimates
  #data <- fill_wsg(data, wsg_database)
  data <- fill_wsg_biomass(data, project)
  
  ## Fill bioclimatic variables
  data <- fill_bioclim(data)
  
  ## Fill plant functional type classification
  data <- fill_pft_mcd12q1(data, layer="PFT")
  #data <- fill_pft_mcd12q1(data, layer="LC_Type1")

  ## Fill WWF ecoregions
  data <- fill_wwf(data)
  
  ## Fill height using local/regional DBH-Height relationship
  result <- fill_height(data, metadata)
  data <- result$data
  HtModelRSE <- result$HtModelRSE

  ## Estimate above-ground biomass
  data <- predict_biomass(data, allometric_definitions)
  
  list(data=data,HtModelRSE=HtModelRSE,project=project)
}


fill_derived_variables <- function(data) {
  browser()
  ## Plot data
  ii <- !is.na(data$p.majoraxis) & !is.na(data$p.minoraxis) & !is.na(data$p.shape) & (data$p.shape == "E") & is.na(data$p.area)
  data$p.area[ii] <- pi * (data$p.majoraxis[ii]/2) * (data$p.minoraxis[ii]/2)
  ii <- !is.na(data$p.majoraxis) & !is.na(data$p.minoraxis) & !is.na(data$p.shape) & (data$p.shape == "R") & is.na(data$p.area)
  data$p.area[ii] <- data$p.majoraxis[ii] * data$p.minoraxis[ii]
  ii <- is.na(data$p.area)
  data$p.area[ii] <- unlist( sapply(data$p.geom[ii], get_parea) )
  ii <- is.na(data$p.sample)
  data$p.sample[ii] <- 0
  ii <- is.na(data$p.majoraxis) & !is.na(data$p.area) & (data$p.shape == "R")
  data$p.majoraxis[ii] <- sqrt(data$p.area[ii])
  ii <- is.na(data$p.majoraxis) & !is.na(data$p.area) & (data$p.shape == "E")
  data$p.majoraxis[ii] <- sqrt(data$p.area[ii] / pi) * 2
  ii <- !is.na(data$p.majoraxis) & is.na(data$p.minoraxis) 
  data$p.minoraxis[ii] <- data$p.majoraxis[ii]
  ii <- is.na(data$p.stemmap)
  data$p.stemmap[ii] <- 0
  ii <- is.na(data$private)
  data$private[ii] <- 1
  ii <- is.na(data$survey)
  data$survey[ii] <- substr(data$date[ii],1,4)
  ii <- is.na(data$date) & !is.na(data$tree.date)
  if ( any(ii, na.rm=TRUE) ) {
    newdata <- plyr::ddply(data[ii,], c("plot","survey"), summarize, date=as.character(min(tree.date,na.rm=TRUE)))
    newdata <- plyr::join(data[ii,], newdata, by=c("plot","survey"), type="left", match="all")
    data$date[ii] <- newdata$date
  }
  
  ## Subplots must have geometry to be treated independently
  ii <- is.na(data$sp.geom) & !(data$p.sample %in% c(1,2,3))
  data$subplot[ii] <- NA
  data$sp.shape[ii] <- NA
  data$sp.mindiam[ii] <- NA
  data$sp.area[ii] <- NA
  data$sp.majoraxis[ii] <- NA
  data$sp.minoraxis[ii] <- NA
  ii <- is.na(data$sp.area) & !is.na(data$sp.geom)
  data$sp.area[ii] <- unlist( sapply(data$sp.geom[ii], get_parea) )
  
  ## Tree data
  ii <- is.na(data$a.stem) & !is.na(data$d.stem)
  data$a.stem[ii] <- (pi/4) * data$d.stem[ii]^2
  ii <- !is.na(data$a.stem) & is.na(data$d.stem)
  data$d.stem[ii] <- sqrt(data$a.stem[ii] / (pi/4))
  ii <- is.na(data$stem)
  data$stem[ii] <- 1
  ii <- is.na(data$tree.date) & !is.na(data$date)
  data$tree.date[ii] <- data$date[ii]
  
  data
}


fill_wsg_biomass <- function(data, project) {
 
  if ( any(!is.na(data$species)) ) {
    
    cachefile <- file.path("data", project, "biomassTaxonData.csv")
    #data <- read_taxon_data_biomass(cachefile, data)
    
    species <- unlist( lapply(data$species, function(x) strsplit(x," ")[[1]][2]) )
    genus <- unlist( lapply(data$species, function(x) strsplit(x," ")[[1]][1]) )
    
    invisible(capture.output( dataWD <- BIOMASS::getWoodDensity(genus = stringr::str_to_title(genus),
                                      species = stringr::str_to_title(species))))#,
                                      #family = data$family) ))
    
    data$wsg <- dataWD$meanWD
    data$wsg.sd <- dataWD$sdWD
    
  }
  
  data
}


fill_wsg <- function(data, wsg_database) {

  # extract genus level information from the species information
  ii <- is.na(data$wsg)
  if ( any(ii) ) {
  
    genus <- plyr::adply(strsplit(data$species[ii]," "), 1, function(x) x[1])$V1
    newdata <- data.frame(family=data$family[ii],genus=genus,species=data$species[ii],wsg=NA,wsg.sd=NA)
    
    # Calculate the mean wsg from all species names, all genus and all families
    species.wsg <- plyr::ddply(wsg_database, "species", summarize, 
                               wsg = mean(OrigValueStr,na.rm=TRUE),
                               wsg.sd = sd(OrigValueStr,na.rm=TRUE))
    genus.wsg <- plyr::ddply(wsg_database, "genus", summarize, 
                             wsg = mean(OrigValueStr,na.rm=TRUE),
                             wsg.sd = sd(OrigValueStr,na.rm=TRUE))
    family.wsg <- plyr::ddply(wsg_database, "family", summarize, 
                              wsg = mean(OrigValueStr,na.rm=TRUE),
                              wsg.sd = sd(OrigValueStr,na.rm=TRUE))
    
    # Add the species level information
    newdata <- plyr::join(newdata, species.wsg, by="species")
    newdata <- plyr::join(newdata, genus.wsg, by="genus")
    newdata <- plyr::join(newdata, family.wsg, by="family")
    colnames(newdata)[5:10] <- c("wsg.species","wsg.sd.species",
                                 "wsg.genus","wsg.sd.genus",
                                 "wsg.family","wsg.sd.family")
    
    # Progressively gap-fill missing WSG in order of species, genus, and family level
    # Use global average values for unknown species
    jj <- is.na(newdata$wsg)
    newdata$wsg[jj] <- newdata$wsg.species[jj]
    newdata$wsg.sd[jj] <- newdata$wsg.sd.species[jj]
    jj <- is.na(newdata$wsg)
    newdata$wsg[jj] <- newdata$wsg.genus[jj]
    newdata$wsg.sd[jj] <- newdata$wsg.sd.genus[jj]
    jj <- is.na(newdata$wsg)
    newdata$wsg[jj] <- newdata$wsg.family[jj]
    newdata$wsg.sd[jj] <- newdata$wsg.sd.family[jj]
    newdata$wsg[is.na(newdata$wsg)] <- mean(wsg_database$OrigValueStr,na.rm=TRUE)
    newdata$wsg.sd[is.na(newdata$wsg)] <- sd(wsg_database$OrigValueStr,na.rm=TRUE)
    
    # Then fill the wsg data
    data$wsg[ii] <- newdata$wsg
    data$wsg.sd[ii] <- newdata$wsg.sd
  
  }
  
  data
}


## Fill bioclimatic variables
fill_bioclim <- function(data, highres=FALSE) {
  
  if ( highres ) {
  
    rs <- rast(nrows=5, ncols=12, xmin=-180, xmax=180, ymin=-60, ymax=90)
    row <- rowFromY(rs, pmin(90, pmax(-60, data$latitude))) - 1
    col <- colFromX(rs, pmin(180, pmax(-180, data$longitude))) - 1
    rc <- paste(row, col, sep='')
    
    for ( i in unique(rc) ) {
      ii <- rc == i
      newdata <- query_worldclim(data[ii,], highres=TRUE)
      data$map[ii] <- newdata$map
      data$mat[ii] <- newdata$mat
    }
  
  } else {
    
    newdata <- query_worldclim(data, highres=FALSE)
    data$map <- newdata$map
    data$mat <- newdata$mat
  
  }
  
  data
}

## Extract the MCD12Q1 v006 record
extract_pft_mcd12q1 <- function(data, layer='PFT') {
  
  mcd12q1.path <- "/workspace/GEDI_CalVal/extra/MODIS_pft/"
  
  nc_filepath <- list.files(
    path = mcd12q1.path,
    pattern = "\\.nc$",     # use regex to match only files ending in .nc
    full.names = TRUE
  )
  
  # Load the NetCDF file
  pft_raster <- terra::rast(nc_filepath)
  
  # Create coordinates vector (keep in WGS84 - no need to reproject)
  coords <- cbind(data$longitude, data$latitude)
  coords <- terra::vect(coords, crs = terra::crs("+init=epsg:4326"), type="points") %>%
    project(pft_raster)
  
  # Extract PFT values directly
  MCD12Q1_006_Land_Cover_Type <- terra::extract(pft_raster, coords)
  
  # Convert to data frame
  MCD12Q1_006_Land_Cover_Type <- as.data.frame(MCD12Q1_006_Land_Cover_Type)
  
  return(MCD12Q1_006_Land_Cover_Type)
}

## Fill the dominant plant functional type name from the MCD12Q1 product
fill_pft_mcd12q1 <- function(data, layer='PFT') {
browser()
  if(layer=="LC_Type1"){
    pft.lut <- data.frame(
      MCD12Q1_006_Land_Cover_Type = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,254,255),
      pft.modis = c("Evergreen Needleleaf trees","Evergreen Broadleaf trees",
                  "Deciduous Needleleaf trees","Deciduous Broadleaf trees",
                  "Mixed forests", "Closed Shrubland","Open Shrublands",
		  		  "Woody savannas", "Savannas", "Grasslands","Permanent wetlands",
				  "Croplands","Urban and built-up","Cropland Natural vegetation mosaic",
                  "Snow and ice","Barren","Water",NA,NA)
    )

    pft.type <- "MCD12Q1_006_Land_Cover_Type_1"

  }

  if(layer=="PFT"){
    pft.lut <- data.frame(
    MCD12Q1_006_Land_Cover_Type = c(0,1,2,3,4,5,6,7,8,9,10,11,254,255),
    pft.modis = c("Water","Evergreen Needleleaf trees","Evergreen Broadleaf trees",
                "Deciduous Needleleaf trees","Deciduous Broadleaf trees",
                "Shrub","Grass","Cereal crops","Broad-leaf crops","Urban and built-up",
                "Snow and ice","Barren or sparse vegetation",NA,NA)
    )

    pft.type <- "MCD12Q1_006_Land_Cover_Type_5"

  }


  data$survey <- factor(data$survey, levels=unique(data$survey), ordered=TRUE)
  mcd12q1.data <- plyr::ddply(data, ~ survey, extract_pft_mcd12q1)
  colnames(mcd12q1.data)[grepl('PFT',colnames(mcd12q1.data))] <- 'MCD12Q1_006_Land_Cover_Type'
  newdata <- plyr::join(mcd12q1.data, pft.lut, by=c("MCD12Q1_006_Land_Cover_Type"))

  if ( nrow(newdata) == nrow(data) ) {
	if(layer=="LC_Type1"){
      data$pft.type1 <- newdata$pft.modis
	}
	if(layer=="PFT"){
      data$pft.modis <- newdata$pft.modis
	}
  }
  data$survey <- as.character(data$survey)

  return(data)
}

### Fill WWF Ecoregions
fill_wwf <- function(data) {
  browser()
  wwfshpfile <- file.path("extra","official_teow","wwf_terr_ecos.shp")
  if ( !file.exists(wwfshpfile) ) {
    wwfzipfile <- file.path("extra","official_teow.zip")
    if ( !file.exists(wwfzipfile) ) {
      remotefile <- "https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip"
      download.file(remotefile, destfile=file.path("extra","official_teow.zip"))
    }
    unzip(wwfzipfile, exdir=file.path("extra","official_teow"), junkpaths=TRUE)
  }  
  wwfmapname <- terra::vector_layers(wwfshpfile)[1]
  wwfmap <- vect(wwfshpfile, layer=wwfmapname)
  
  pts <- data.frame(lon=data$longitude,lat=data$latitude)
  pts <- vect(pts, geom=c("lon","lat"), crs = crs(wwfmap))
  
  wwfptnames <- terra::relate(pts, wwfmap[, 'ECO_NAME'], relation ="intersects", pairs=T)
  
  if(length(data$wwf.ecoregion) == length(wwfmap$ECO_NAME[wwfptnames[,2]])){
    data$wwf.ecoregion <- wwfmap$ECO_NAME[wwfptnames[,2]]
    data
  } else{
    # pts <- data.frame(lon=data$longitude,lat=data$latitude)
    # pts <- vect(pts, geom=c("lon","lat"), crs = crs(wwfmap)) %>%
    #   buffer(width = 5)
    # 
    # wwfptnames <- terra::relate(pts, wwfmap[, 'ECO_NAME'], relation ="intersects", pairs=T)
    # data$wwf.ecoregion <- wwfmap$ECO_NAME[wwfptnames[,2]]
    # data
    pts$row <- seq(1:nrow(pts))
    points <- dplyr::left_join(as.data.frame(pts), as.data.frame(wwfptnames), by = c("row" = "id.x"))
    NA_points <- points %>% dplyr::filter(dplyr::if_any(c(id.y), is.na))
    NA_points <- terra::merge(pts, NA_points %>% dplyr::select(row), by = "row")
    
    wwfptnames_NA <- terra::nearest(NA_points, wwfmap[, 'ECO_NAME'], pairs=T) %>%
      as.data.frame() %>%
      dplyr::rename(id.x = from_id,
             id.y = to_id) %>%
      dplyr::select(id.x, id.y)
    
    wwfptnames_NA$id.x <- NA_points$row[wwfptnames_NA$id.x]
    
    wwfptnames <- rbind(wwfptnames, wwfptnames_NA)
    
    data$wwf.ecoregion <- wwfmap$ECO_NAME[wwfptnames[,2]]
    data
  }
}

## Extract a centre latitude/longitude from a polygon
set_plot_center_latlong <- function(data) {
  browser()
  sp_obj <- vect(data$p.geom)    
  if ( class(sp_obj) == "SpatVector" ) {
    p <- centroids(sp_obj)%>%crds()
  } else {
    p <- c(crds(centroids(sp_obj)))
  }
  coords <- epsg2geo(p[,1],p[,2],data$p.epsg)
  data$longitude <- coords[,1]
  data$latitude <- coords[,2]
  data
}


## Reproject a set of tree coordinates with same source and target epsg
do_tree_coordinate_reprojection <- function(data) {
  browser()
  xy <- data.frame(x=data$x, y=data$y)
  xy <- vect(xy, geom = c("x","y"), crs = paste("+init=epsg",data$p.epsg[1],sep=":") )
  xyt <- project(xy, paste0("EPSG:",data$tar_epsg[1]) )
  data$x <- crds(xyt)[,1]#xyt$x
  data$y <- crds(xyt)[,2]#xyt$y
  data
}


## Reproject a tree coordinate
reproject_tree_coordinates <- function(data) {
  browser()
  standard.fields <- names(data)
  data$id <- 1:length(data$tree)
  data$tar_epsg <- get_utmwgs84_epsg(data$longitude,data$latitude)
  data <- plyr::ddply(data, c("p.epsg","tar_epsg"), do_tree_coordinate_reprojection)
  data <- plyr::arrange(data, id)
  #print(standard.fields)
  #print(names(data))
  data[,standard.fields]
}


## Reproject a plot/subplot polygon
reproject_plot_polygon <- function(data) {
  browser()
  in_crs <- crs( paste("+init=epsg",data$p.epsg,sep=":") )
  geom <- vect(data$p.geom, crs=in_crs)
  out_epsg <- get_utmwgs84_epsg(data$longitude,data$latitude)
  out_crs <- crs( paste("+init=epsg",out_epsg,sep=":") )
  geom <- project(geom, out_crs)
  data$p.geom <- geom(geom, wkt=T)
  if ( !is.na(data$sp.geom) ) {
    geom <- vect(data$sp.geom, crs=in_crs)
    geom <- project(geom, out_crs)
    data$sp.geom <- geom(geom, wkt=T)
  }
  data$p.epsg <- out_epsg
  data
}


## Convert all geometry WKT to UTM / WGS84
standardize_geometry <- function(data) {
  browser()
  # derive plot centre latitude and longitude
  keyvar <- c("plot","survey")
  geom.data <- data[!duplicated(data[,keyvar]),]
  ii <- ( is.na(geom.data$longitude) | is.na(geom.data$latitude) ) &
        ( !is.na(geom.data$p.epsg) & !is.na(geom.data$p.geom) )
  if ( any(ii) ) {
    geom.data[ii,] <- plyr::adply(geom.data[ii,], 1, set_plot_center_latlong)
    geom.data$p.origin[ii] <- "C"
    data <- data[, !( names(data) %in% c("longitude","latitude","p.origin") )]
    data <- plyr::join(data, geom.data[,c(keyvar,"longitude","latitude","p.origin")], 
                       by=keyvar, type="left", match="all")
  }
  
  # tree level coordinate data
  ii <- !( (data$p.epsg >= 32600) & (data$p.epsg < 32800) ) & 
    (!is.na(data$x) & !is.na(data$y) )
  if ( any(ii) ) {
    data[ii,] <- reproject_tree_coordinates(data[ii,])
  }
  
  # now the plot and subplot polygon coordinate data
  keyvar <- c("plot","survey","subplot")
  geom.data <- data[!duplicated(data[,keyvar]),]
  ii <- !( (geom.data$p.epsg >= 32600) & (geom.data$p.epsg < 32800) ) & 
    (!is.na(geom.data$p.epsg) & !is.na(geom.data$p.geom) )
  if ( any(ii) ) {
    geom.data[ii,] <- plyr::adply(geom.data[ii,], 1, reproject_plot_polygon)
    data <- data[, !( names(data) %in% c("p.geom","sp.geom","p.epsg") )]
    data <- plyr::join(data, geom.data[,c(keyvar,"p.geom","sp.geom","p.epsg")], 
                       by=keyvar, type="left", match="all")
  }
  
  data  
}


## Fit local H-D relationship using the BIOMASS package
fill_height <- function(data, metadata) {
  browser()
  ii <- is.na(data$h.t) & !is.na(data$d.stem) & (data$d.stem > 0)
  if ( any(ii, na.rm=TRUE) ) {
    jj <- !is.na(data$d.stem) & !is.na(data$h.t) & (data$d.stem > 0) & (data$h.t > 0)
    if ( any(jj, na.rm=TRUE) ) {
      if ( "Height model" %in% metadata$Item ) {
        HDmodel <- BIOMASS::modelHD(D=data$d.stem[jj]*100, H=data$h.t[jj], useWeight=TRUE, 
                                    method=metadata$Value[metadata$Item == "Height model"])
        HDest <- BIOMASS::retrieveH(D=data$d.stem*100, model=HDmodel)
        data$h.t.mod <- HDest$H
        errH <- HDest$RSE
      } else {
        errH <- NA
      }
    } else {
      errH <- NA
    }
  } else {
    errH <- NA
  }
  
  list(data=data,HtModelRSE=errH) 
}


## Placeholder until error propagation approach implemented
predict_biomass <- function(data, allometricDefinitions) {
  browser()
  #data$allom.key[data$allom.key %in% c(7,8,9,10,11,12)] <- 69

  #Toggle for tropical allometry testing:
  # For Disney TLS allometry, use Disney allometry if measured or modeled height available and PFT is EBT, otherwise use Chave 2014
#  pft <- data$pft.modis
#  pft[!is.na(data$pft.name)] <- data$pft.name[!is.na(data$pft.name)]
#  ii <- (data$allom.key %in% c(1,2,68)) & ( !is.na(data$h.t) | !is.na(data$h.t.mod) & pft=="Evergreen Broadleaf trees")
#  data$allom.key[ii] <- 67
#  ii <- (data$allom.key %in% c(67,68)) & is.na(data$h.t) & is.na(data$h.t.mod)
#  data$allom.key[ii] <- 2
  
  # For Chave allometry, change all Disney to Chave allom.key
  ii <- is.na(data$m.agb) & (data$allom.key %in% c(67,68)) & ( !is.na(data$h.t) | !is.na(data$h.t.mod) )
  data$allom.key[ii] <- 1
  ii <- is.na(data$m.agb) & (data$allom.key %in% c(67,68)) & is.na(data$h.t) & is.na(data$h.t.mod)
  data$allom.key[ii] <- 2
  
  
  
  # Only process valid measurements
  a <- join(data[!is.na(data$d.stem),], allometricDefinitions, by="allom.key", type = "left")  
  

  ## Chave et al. (2014). Order of allometric selection.
  ## 1. All height measurements present
  ## 2. Local D-H model heights
  ## 3. Height modelled using E
  ii <- is.na(a$m.agb) & (a$allom.name %in% c("chave2014a","chave2014b")) & ( !is.na(a$h.t) | !is.na(a$h.t.mod) )
  if ( all(ii, na.rm=TRUE) ) {
    h.t.tmp <- a$h.t[ii]
    jj <- is.na(h.t.tmp) | (h.t.tmp <= 0)
    h.t.tmp[jj] <- a$h.t.mod[ii][jj]
    a$m.agb[ii] <- BIOMASS::computeAGB(a$d.stem[ii]*100, a$wsg[ii], H=h.t.tmp) * 1e3
    a$allom.key[ii] <- 2
  }
  ii <- is.na(a$m.agb) & (a$allom.name %in% c("chave2014a","chave2014b")) & is.na(a$h.t) & is.na(a$h.t.mod)
  if ( all(ii, na.rm=TRUE) ) {
    geo.coords <- cbind(a$longitude[ii],a$latitude[ii])
    a$m.agb[ii] <- BIOMASS::computeAGB(a$d.stem[ii]*100, a$wsg[ii], coord=geo.coords) * 1e3
    a$allom.key[ii] <- 1
  }
  
  ## Muukkonen 2007
  ii <- is.na(a$m.agb) & (a$allom.name == "muukkonen2007") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- a$p1[ii] * (a$d.stem[ii]*100)^a$p2[ii]

  ## Forrester 2017
  ii <- is.na(a$m.agb) & (a$allom.name == "forrester2017") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- a$p3[ii] * exp(a$p1[ii] + (a$p2[ii]*log(a$d.stem[ii]*100)))
  
  ## Moore 2010
  ii <- is.na(a$m.agb) & (a$allom.name == "moore2010") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- exp(a$p1[ii] + (a$p2[ii]*log(a$d.stem[ii]*100)) + 
                                           (a$p3[ii]*log(a$d.stem[ii]*100)^2)) * exp((a$p4[ii]^2)/2) 
  ## Beets 2011
  ii <- is.na(a$m.agb) & (a$allom.name == "beets2011") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- (a$p1[ii] * ((a$d.stem[ii]*100)^2 * a$h.t[ii])^0.978) * a$wsg[ii]
  
  ## Ung 2008
  ii <- is.na(a$m.agb) & (a$allom.name == "ung2008") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) {
	h.t.tmp <- a$h.t[ii]
    jj <- is.na(h.t.tmp) | (h.t.tmp <= 0)
    h.t.tmp[jj] <- a$h.t.mod[ii][jj]
	a$m.agb[ii] <- (a$p1[ii] * ((a$d.stem[ii]*100)^a$p2[ii] * (h.t.tmp)^a$p3[ii])) +
								(a$p4[ii] * ((a$d.stem[ii]*100)^a$p5[ii] * (h.t.tmp)^a$p6[ii])) +
								(a$p7[ii] * ((a$d.stem[ii]*100)^a$p8[ii] * (h.t.tmp)^a$p9[ii])) +
								(a$p10[ii] * ((a$d.stem[ii]*100)^a$p11[ii] * (h.t.tmp)^a$p12[ii]))
  }
  
  ## Disney 2021
  ## 1. All height measurements present
  ## 2. Local D-H model heights

  
  ii <- is.na(a$m.agb) & (a$allom.name %in% c("disney2021a","disney2021b")) & ( !is.na(a$h.t) | !is.na(a$h.t.mod) )
  if ( all(ii, na.rm=TRUE) ) {
    a[ii,c("p1","p2","p3")] <- allometricDefinitions[allometricDefinitions$allom.key==67,c("p1","p2","p3")]
    h.t.tmp <- a$h.t[ii]
    jj <- is.na(h.t.tmp) | (h.t.tmp <= 0)
    h.t.tmp[jj] <- a$h.t.mod[ii][jj]
    a$m.agb[ii] <- (a$p1[ii] * ((a$d.stem[ii]*100)^2 * h.t.tmp * a$wsg[ii])^a$p2[ii] + a$p3[ii]) / 10
    a$allom.key[ii] <- 67
  }
  ii <- is.na(a$m.agb) & (a$allom.name %in% c("disney2021a","disney2021b")) & is.na(a$h.t) & is.na(a$h.t.mod)
  if ( all(ii, na.rm=TRUE) ) {
    a[ii,c("p1","p2","p3")] <- allometricDefinitions[allometricDefinitions$allom.key==68,c("p1","p2","p3")]
    a$m.agb[ii] <- a$p1[ii] * ((a$d.stem[ii]*100)^a$p2[ii]) / 10
    a$allom.key[ii] <- 68
  }
  
  ## Disney Sequoia allometry
  ii <- is.na(a$m.agb) & (a$allom.name == "disney2020") & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- (a$p1[ii] * (a$d.stem[ii]*100)^a$p2[ii] + a$p3[ii]*a$h.t[ii]^a$p4[ii]) * a$wsg[ii]

  
  ## Linear models
  ii <- is.na(a$m.agb) & (a$allom.model == 1) & !is.na(a$allom.key)
  if ( any(ii,na.rm=TRUE) ) a$m.agb[ii] <- exp(a$p1[ii] + (a$p2[ii]*log(a$d.stem[ii]*100)))*exp((a$p3[ii]^2)/2)
  
  ## Flag trees with invalid allometric prediction
  d.stem.max <- ifelse(is.na(a$d.stem.max), Inf, a$d.stem.max)
  ii <- (a$d.stem <= d.stem.max)
  if ( any(ii,na.rm=TRUE) ) a$d.stem.valid[ii] <- 1
  if ( any(!ii,na.rm=TRUE) ) a$d.stem.valid[!ii] <- 0
  
  ## Combine results
  data$m.agb[!is.na(data$d.stem)] <- a$m.agb
  data$allom.key[!is.na(data$d.stem)] <- a$allom.key
  data$d.stem.valid[!is.na(data$d.stem)] <- a$d.stem.valid
  
  data
}




## Read the Environmental Stress (E) raster file at specified coordinates
query_e <- function(longitude,latitude) {
  browser()
  localfile <- file.path("extra","E.nc.zip")
  if ( !file.exists(localfile) ) {
    remotefile <- "http://chave.ups-tlse.fr/pantropical_allometry/E.nc.zip"
    download.file(remotefile, destfile="extra/E.nc.zip")
  }
  unzip(localfile, exdir="extra")
  e.data <- terra::rast("extra/E.nc")
  coords <- cbind(longitude,latitude)
  pixeldata <- terra::extract(e.data,coords,method="bilinear")
  file.remove("extra/E.nc")
  pixeldata
}


## Read bioclimatic variables at specified coordinates
query_worldclim <- function(data,highres=FALSE) {
  browser()
  if ( highres ) {
    bioclim <- geodata::worldclim_tile(var="bio", res=0.5, lon=mean(data$longitude), lat=mean(data$latitude), path="extra", version = "2.1")
  } else {
    bioclim <- geodata::worldclim_global(var="bio", res=2.5, path="extra", version = "2.1")
  }
  coords <- cbind(data$longitude, data$latitude)
  pixeldata <- terra::extract(bioclim, coords, method="bilinear")
  data.frame(mat=pixeldata[,1], map=pixeldata[,12])
}
