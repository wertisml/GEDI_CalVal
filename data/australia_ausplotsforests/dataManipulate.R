manipulate <- function(raw){

  # Add 25 x 25 m subplots 
  plotnames <- unique(raw$plot)
  noplots <- length(unique(raw$plot))
  raw_subplots <- raw[!duplicated(raw$plot),c("plot","p.geom","p.epsg","easting","northing","orientation")]
  raw_subplots <- raw_subplots[rep(c(1:nrow(raw_subplots)),each=16),]
  raw_subplots$sp.geom <- NA
  raw_subplots$subplot <- paste(raw_subplots$plot,c(1:16),sep="-")
  
  counter <- 1
  for (k in 1:noplots){
    subplot_corners <- as.data.frame(matrix(NA, ncol=8,nrow=16))
    colnames(subplot_corners) <- c("X1","X2","X3","X4","Y1","Y2","Y3","Y4")
    xplots <- seq(from=0,to=(25 * 4),by=25)
    yplots <- seq(from=0,to=(25 * 4),by=25)
    
    count <- 1
    for(i in 1:(length(xplots)-1)){
      for(j in 1:(length(yplots)-1)){
        subplot_corners$X1[count] <- xplots[i]
        subplot_corners$X2[count] <- xplots[i]
        subplot_corners$X3[count] <- xplots[i + 1]
        subplot_corners$X4[count] <- xplots[i + 1]
        subplot_corners$Y1[count] <- yplots[j]
        subplot_corners$Y2[count] <- yplots[j + 1]
        subplot_corners$Y3[count] <- yplots[j + 1]
        subplot_corners$Y4[count] <- yplots[j]
        count <- count + 1
      }
    }
    
    # Rotate plot based on orientation
    points <- rotate2D(subplot_corners[,1:4],subplot_corners[,5:8],raw_subplots$orientation[counter])
    
    # Move subplot based on lowerleft corner
    points$x <- points$x + raw_subplots$easting[counter]
    points$y <- points$y + raw_subplots$northing[counter]
    
    for(l in 1:16){
      lcount <- counter + l - 1 
      sp.geom <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))", 
                         points$x[l,1], points$y[l,1],
                         points$x[l,2], points$y[l,2],
                         points$x[l,3], points$y[l,3],
                         points$x[l,4], points$y[l,4],
                         points$x[l,1], points$y[l,1])
      raw_subplots[lcount,"sp.geom"] <- sp.geom
    }
    counter <- counter + 16
  }
  
  rotate2D <- function(x, y, phi, xt=0, yt=0) {
    xr <- ( x * cos(phi) + y * sin(phi) ) + xt
    yr <- ( y * cos(phi) - x * sin(phi) ) + yt
    list(x=xr, y=yr)
  }

  # Reproject points to same coordinate system as the plot
  ii <- !( is.na(raw$x) | is.na(raw$y) | is.na(raw$easting) | is.na(raw$northing) | is.na(raw$orientation) )
  phi <- (raw$orientation[ii]) * pi / 180
  coords <- rotate2D(raw$x[ii], raw$y[ii], phi, xt=raw$easting[ii], yt=raw$northing[ii])
  raw$st_x[ii] <- coords$x
  raw$st_y[ii] <- coords$y

  raw$sp.geom <- NA
  ## Assign subplot names and geometry to each tree 
  for (k in 1:noplots){
    thesetrees <- raw[raw$plot == plotnames[k],]
    #thesetrees <- thesetrees %>% drop_na(plot)
    thesesubplots <- raw_subplots[raw_subplots$plot == plotnames[k],]
    #thesesubplots <- thesesubplots%>%filter(rowSums(is.na(.)) != ncol(.))
    create_spatialvector <- function(row) {
      geom <- row["sp.geom"]
      data <- row[ , -which(names(row) == "sp.geom"), drop = FALSE]
      sp_vector <- vect(geom$sp.geom, crs = paste("EPSG:", data$p.epsg, sep=""))
      values(sp_vector) <- data
      return(sp_vector)
    }
    spatial_vectors <- lapply(1:nrow(thesesubplots), function(i) create_spatialvector(thesesubplots[i, ]))
    shape_subplots <- do.call(rbind, spatial_vectors)
    
    tree_loc <- vect(thesetrees,
                     geom = c("st_x", "st_y"), 
                     crs = paste("EPSG:", thesetrees$p.epsg[1], sep=""))

    plot_tree_intersect <- terra::relate(shape_subplots, tree_loc, relation = "intersects", pairs = T)
    for(l in 1:length(unique(plot_tree_intersect[,1]))){
    raw$subplot[which(raw$plot == plotnames[k])][plot_tree_intersect[,1] == l] <- thesesubplots$subplot[l]
    raw$sp.geom[which(raw$plot == plotnames[k])][plot_tree_intersect[,1] == l] <- thesesubplots[thesesubplots$plot == plotnames[k], "sp.geom"][l]
    }
  }
  
  raw <- raw[raw$subplot != 1,] # some trees did not fit in any of the subplot boundaries 
  
  raw <- raw[raw$plot %in% c('NSFNNC004','NSFNNC005','NSFNNC007','NSFNNC008','TCFTSR002','TCFTSR001','TCFTSR005'),]

  
raw
}

manipulate_plot <- function(raw){

  # Join the biomass data with the plot data
  # opts <- read_data_raw_import_options("data/australia_ausplotsforests/plotBiomassDataImportOptions.csv")
  # biomass.raw <- read_csv("data/australia_ausplotsforests/plotBiomassData.csv",
  #                         header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  # raw <- join(raw, biomass.raw, by=opts$key, type="left", match="all")
  # raw$agbd.ha.upper <- raw[,"DM (t/ha)"] + 1.96 * raw[,"s.e. (t/ha)"]
  # raw$agbd.ha.lower <- raw[,"DM (t/ha)"] - 1.96 * raw[,"s.e. (t/ha)"]
  # raw$agb.upper <- raw[,"DM (t)"] + 1.96 * raw[,"s.e. (t)"]
  # raw$agb.lower <- raw[,"DM (t)"] - 1.96 * raw[,"s.e. (t)"]

  # Lidar EPSG codes (always MGA94)
  zone <- ( floor( (raw$longitude + 180) / 6 ) %% 60 ) + 1
  raw$l.epsg <- 28300 + zone
  
  # Plot geometry (TERN database export has errors so reconstruct here)
  raw$p.epsg <- get_utmwgs84_epsg(raw$longitude,raw$latitude)
  coords <- mapply(convert_projection,raw$longitude,raw$latitude,raw$st_srid,raw$p.epsg)
  raw$easting <- coords[1,]
  raw$northing <- coords[2,]
  
  #We adjust the location of the NSFNNC008, NSFNNC007, NSFNNC005 and NSFNNC004 after manual location optimization
  changedata <- data.frame(plotnames_change = c("NSFNNC008", "NSFNNC007", "NSFNNC005", "NSFNNC004"), 
                           x_change = c(-10, -5, 4, 3), y_change = c(-5, 5, 4, 2), 
                           orientation_change = c(-1, -1, 182, -2))
  
  raw[raw$plot == "NSFNNC008", "easting"] <- raw[raw$plot == "NSFNNC008", "easting"] - 10
  raw[raw$plot == "NSFNNC008", "northing"] <- raw[raw$plot == "NSFNNC008", "northing"] - 5
  raw[raw$plot == "NSFNNC008", "orientation"] <- raw[raw$plot == "NSFNNC008", "orientation"] -1
  
  raw[raw$plot == "NSFNNC007", "easting"] <- raw[raw$plot == "NSFNNC007", "easting"] -5
  raw[raw$plot == "NSFNNC007", "northing"] <- raw[raw$plot == "NSFNNC007", "northing"] + 5
  raw[raw$plot == "NSFNNC007", "orientation"] <- raw[raw$plot == "NSFNNC007", "orientation"] -1
  
  raw[raw$plot == "NSFNNC005", "easting"] <- raw[raw$plot == "NSFNNC005", "easting"] + 3
  raw[raw$plot == "NSFNNC005", "northing"] <- raw[raw$plot == "NSFNNC005", "northing"] + 2 
  raw[raw$plot == "NSFNNC005", "orientation"] <- raw[raw$plot == "NSFNNC005", "orientation"] - 2
  
  raw[raw$plot == "NSFNNC004", "easting"] <- raw[raw$plot == "NSFNNC004", "easting"] + 4 
  raw[raw$plot == "NSFNNC004", "northing"] <- raw[raw$plot == "NSFNNC004", "northing"] + 4
  raw[raw$plot == "NSFNNC004", "orientation"] <- raw[raw$plot == "NSFNNC004", "orientation"] + 182
  
  phi <- (raw$orientation) * pi / 180
  d0 <- 0.0
  d1 <- 100.0
  
  geom.ulx.r <- d0 * cos(phi) + d1 * sin(phi) + raw$easting
  geom.uly.r <- d1 * cos(phi) - d0 * sin(phi) + raw$northing
  geom.urx.r <- d1 * cos(phi) + d1 * sin(phi) + raw$easting
  geom.ury.r <- d1 * cos(phi) - d1 * sin(phi) + raw$northing
  geom.lrx.r <- d1 * cos(phi) + d0 * sin(phi) + raw$easting
  geom.lry.r <- d0 * cos(phi) - d1 * sin(phi) + raw$northing
  geom.llx.r <- d0 * cos(phi) + d0 * sin(phi) + raw$easting
  geom.lly.r <- d0 * cos(phi) - d0 * sin(phi) + raw$northing
  
  raw$p.geom <- sprintf("POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))", 
                         geom.llx.r, geom.lly.r,
                         geom.ulx.r, geom.uly.r,
                         geom.urx.r, geom.ury.r,
                         geom.lrx.r, geom.lry.r,
                         geom.llx.r, geom.lly.r)
  
raw
}

manipulate_tree <- function(raw){

  # Join the biomass data with the tree data
  #opts <- read_data_raw_import_options("data/australia_ausplotsforests/treeBiomassDataImportOptions.csv")
  #biomass.raw <- read_csv("data/australia_ausplotsforests/treeBiomassData.csv",
  #                        header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  #raw <- join(raw, biomass.raw, by=opts$key, type="left", match="all")
  #raw$m.agb.upper <- raw[,"AGB Dry mass (Kg)"] + 1.96 * raw[,"AGB Dry mass (s.d.)"]
  #raw$m.agb.lower <- raw[,"AGB Dry mass (Kg)"] - 1.96 * raw[,"AGB Dry mass (s.d.)"]
  
  # Tree attributes
  raw$status <- as.numeric(raw$condition == 0)
  raw$status[raw$tree == "D"] <- 0
  
  # Unique tree coordinates columns
  colnames(raw)[which(colnames(raw) == "st_srid")] <- c("st_srid_tree")
  
  # Join the allometric data with the tree data
  opts <- read_data_raw_import_options("data/australia_ausplotsforests/allomDataImportOptions.csv")
  allom.raw <- read_csv("data/australia_ausplotsforests/allomData.csv",
                          header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  raw <- plyr::join(raw, allom.raw, by=opts$key, type="left", match="all")
  
  allomDef <- read_csv("config/allometricDefinitions.csv", header=opts$header, skip=opts$skip, na.strings=opts$na.strings)
  
  
  # Species codes
  raw$species[startsWith(raw$species, "UNN")] <- NA
  raw$species[startsWith(raw$species, "UWE")] <- NA
  raw$species[startsWith(raw$species, "Unk")] <- NA
  raw$species[startsWith(raw$species, "USE")] <- NA
  raw$species[startsWith(raw$species, "UTS")] <- NA
  raw$species[startsWith(raw$species, "UVC")] <- NA
  raw$species[startsWith(raw$species, "UWA")] <- NA
    
  # Allometric codes
  allom.col <- which(names(raw) == "allom.name")
  raw$allom.key <- NA
  ii <- raw$d1 <= 200
  raw$allom.key[ii] <- 19
  ii <- raw$d1 > 200
  raw$allom.key[ii] <- 20
  ii <- raw[allom.col] == "Other trees - high wood density"
  raw$allom.key[ii] <- 16
  ii <- raw[allom.col] == "Eucalypt trees"
  raw$allom.key[ii] <- 13
  ii <- raw[allom.col] == "Single stemmed acacia trees"
  raw$allom.key[ii] <- 15
  ii <- raw[allom.col] == "Shrubs"
  raw$allom.key[ii] <- 18
  ii <- raw[allom.col] == "Multi-stemmed acacias and mallees"
  raw$allom.key[ii] <- 14
  ii <- raw[allom.col] == "@UniversalLarge"
  raw$allom.key[ii] <- 20
  ii <- raw[allom.col] == "@UniversalSmall"
  raw$allom.key[ii] <- 19
  
  ## If a tree is larger than the max DBH for the species-specific allometric key,
  ##	replace it with a more general model.

  ## All species
  for(i in c(13, 14, 15, 16, 17, 18)){
    d.stem.max <- allomDef$d.stem.max[allomDef$allom.key == i]
    raw$allom.key[raw$allom.key == i & raw$d1 > d.stem.max & raw$d1 <= 200] <- 19
    raw$allom.key[raw$allom.key == i & raw$d1 > d.stem.max & raw$d1 > 200] <- 20
    
  }
  
  # Stick with 1 ha plots (remove all trees outside 1 ha)
  ii <- (raw$x > 100) | (raw$y > 100) | (raw$x < 0) | (raw$y < 0)
  raw <- raw[!ii,]
  
  # Subplot ID
  #x.id <- clamp(as.integer(raw$x / 25) + 1, lower=1, upper=4)
  #y.id <- clamp(as.integer(raw$y / 25) + 1, lower=1, upper=4)
  #raw$subplot.id <- sprintf("%i.%i", x.id, y.id)
  
raw
}
