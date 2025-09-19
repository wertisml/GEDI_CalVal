load_study <- function(filename_data_opts_plot,
                       filename_data_opts_tree,
                       filename_manipulate,
                       filename_columns,
                       filename_new_data,
                       filename_metadata,
                       variable_definitions,
                       allometric_definitions,
                       lidar_definitions,
                       wsg_database,
                       conversions,
                       fp_radius,
                       fp_spacing,
                       tk_spacing,
                       montecarlo,
                       nruns) {
  browser()
  metadata <- read_csv(filename_metadata)
  
  result <- read_data_study(filename_data_opts_plot,
                          filename_data_opts_tree,
                          filename_manipulate,
                          filename_columns,
                          filename_new_data,
                          variable_definitions,
                          allometric_definitions,
                          lidar_definitions,
                          wsg_database,
                          conversions,
                          metadata)
  
  plotdata <- summarize_plots_study(result$data,variable_definitions,result$HtModelRSE,montecarlo=montecarlo,nruns=nruns)
  gedidata <- summarize_gedi_study(plotdata,variable_definitions,fp_radius,fp_spacing,tk_spacing)
  
  nTreesWithMeasurements <- sum( !is.na(result$data$a.stem) )
  if (nTreesWithMeasurements == 0) {
    data <- NULL
  }
  
  list(key       = result$project,
       data      = result$data,
       plotdata  = plotdata,
       gedidata  = gedidata,
       metadata  = metadata)
  
}

## These are the cleaning steps:
read_data_study <- function(filename_data_opts_plot,
                            filename_data_opts_tree,
                            filename_manipulate,
                            filename_columns,
                            filename_new_data,
                            variable_definitions,
                            allometric_definitions,
                            lidar_definitions,
                            wsg_database,
                            conversions,
                            metadata) {
  browser()
  project <- basename( dirname(filename_data_opts_plot) )
  data <- read_data_raw(filename_data_opts_plot, filename_data_opts_tree)

  ## Here we clean up up the tree level data
  # 1. Manipulate the data to convert projections, etc.
  # 2. Convert data to desired format, changing units, variable names
  # 3. Add/remove columns to match standard template
  # 4. Modify data by adding new values from table studyName/dataNew.csv
  # 5. Ensures variables have correct data types
  # 6. Derive missing variables from existing variables (including AGB)
  data <- manipulate_data_levels(data, filename_data_opts_tree, filename_manipulate)
  data <- convert_data(data, filename_columns, variable_definitions, conversions)
  data <- add_all_columns(data, variable_definitions)
  data <- add_new_data(data, filename_new_data)  
  data <- fix_types(data, variable_definitions)
  result <- post_process(data, allometric_definitions, wsg_database, metadata, project)
  
  result
}

read_data_raw <- function(filename_opts_plot, filename_opts_tree) {
  browser()
  opts_plot <- read_data_raw_import_options(filename_opts_plot)
  filename_plot_raw <- file.path(dirname(filename_opts_plot),opts_plot$name)
  plot_data <- read_csv(filename_plot_raw, sep=opts_plot$separator,
               header=opts_plot$header, skip=opts_plot$skip, na.strings=opts_plot$na.strings)
  
  opts_tree <- read_data_raw_import_options(filename_opts_tree)
  if (!is.na(opts_tree$name)) {
    filename_tree_raw <- file.path(dirname(filename_opts_tree),opts_tree$name)
    tree_data <- read_csv(filename_tree_raw, sep=opts_tree$separator, 
                          header=opts_tree$header, skip=opts_tree$skip, na.strings=opts_tree$na.strings)
    keyname <- strsplit(opts_tree$key, ",")[[1]]
  } else {
    tree_data <- NULL
    keyname <- NULL
  }
  
  list(key  = keyname,
       tree = tree_data,
       plot = plot_data)
}

## Apply data manipulation to each level of raw data and join
manipulate_data_levels <- function(data, filename_data_opts_tree, filename_manipulate) {
  browser()
  opts_tree <- read_data_raw_import_options(filename_data_opts_tree)
  if ( !is.na(opts_tree$name) ) {
    data$plot <- manipulate_data(data$plot, "manipulate_plot", filename_manipulate)
    data$tree <- manipulate_data(data$tree, "manipulate_tree", filename_manipulate)
    data <- plyr::join(data$plot, data$tree, by=data$key, type="left", match="all")
    data <- manipulate_data(data, "manipulate", filename_manipulate)
  } else {
    data <- manipulate_data(data$plot, "manipulate", filename_manipulate)
  }
  data
}

## If the `dataManipulate.R` file is present within a study's data
## directory, it must contain the function `manipulate_<level>`. Otherwise
## we return the identity function to indicate no manipulations will be
## done.  The function must take a data.frame as an argument and
## return one as the return value, but this is not checked at present.
##
## TODO: This needs modifying to deal with scoping issues more
## carefully.
manipulate_data <- function(data, fun, filename_manipulate) {
  browser()
  manipulate <- get_function_from_source(fun, filename_manipulate)
  manipulate(data)
}

## Convert data to desired format, changing units, variable names
convert_data <- function(data, filename_columns, variable_definitions, conversions) {
  browser()
  var_match <- read_match_columns(filename_columns)
  data <- data[,names(data)[names(data) %in% var_match$var_in]]
  data <- rename_columns(data, var_match$var_in, var_match$var_out)


  info <- column_info(variable_definitions)

  ## Change units
  to_check <- intersect(names(data), var_match$var_out)
  to_check <- to_check[(info$type[to_check] == "numeric") & !is.na(info$units[to_check])]
  for (col in to_check) {
    unit_from <- var_match$unit_in[match(col, var_match$var_out)[[1]]]
    unit_to <- info$units[[col]]
    if (unit_from != unit_to) {
      ## TODO: This is absolutely horrible and should change.
      x <- data[[col]]
      i <- (conversions$unit_in == unit_from &
            conversions$unit_out == unit_to)
      print(str(data))
      data[[col]] <- eval(parse(text=conversions$conversion[i]))
    }
  }
  
  data
}

## Standardise data columns to match standard template.
##
## May add or remove columns of data as needed so that all sets have
## the same columns.
add_all_columns <- function(data, variable_definitions) {
  browser()
  na_vector <- function(type, n) {
    rep(list(character=NA_character_, numeric=NA_real_)[[type]], n)
  }

  info <- column_info(variable_definitions)
  missing <- setdiff(info$variable, names(data))
  if (length(missing) != 0) {
    extra <- as.data.frame(lapply(info$type[missing], na_vector, nrow(data)),
                           stringsAsFactors = FALSE)
    data <- cbind(data[names(data) %in% info$variable], extra)
  } else {
    data <- data[names(data) %in% info$variable]
  }
  data[info$variable]
}

## Modifies data by adding new values from table studyName/dataNew.csv
##
## Within the column given by `newVariable`, replace values that match
## `lookupValue` within column `lookupVariable` with the value
## `newValue`.  If `lookupVariable` is `NA`, then replace all elements
## of `newVariable` with the value `newValue`. Note that
## lookupVariable can be the same as newVariable.
add_new_data <- function(data, filename) {
  browser()
  import <- read_csv(filename)
  if (nrow(import) > 0) {
    import$lookupVariable[import$lookupVariable == ""] <- NA
  }

  if (!is.null(import)) {
    for (i in seq_len(nrow(import))) {
      col_to <- import$newVariable[i]
      col_from <- import$lookupVariable[i]
      if (is.na(col_from)) {
        # apply to whole column
        data[col_to] <- import$newValue[i]
      } else {
        ## apply to subset
        rows <- data[[col_from]] == import$lookupValue[i]
        data[rows, col_to] <- import$newValue[i]
      }
    }
  }

  data
}

## Ensures variables have correct type
fix_types <- function(data, variable_definitions) {
  browser()
  var_def <- variable_definitions
  for (i in seq_along(var_def$variable)) {
    v <- var_def$variable[i]
    data[[v]] <- suppressWarnings( match.fun(paste0("as.", var_def$type[i]))(data[[v]]) )
  }
  data
}

## Error propagation for Chave et al. (2014)
plotAGB_chave2014 <- function(data, q=0.95, errH=NA, nruns=1000) {
  browser()
  ii <- !is.na(data$d.stem)
  if ( sum(ii) > 1 ) {
    
    data <- data[ii,]
    
    h.t <- ifelse(is.na(data$h.t), data$h.t.mod, data$h.t)
    h.t.err <- ifelse(is.na(data$h.t), errH, 1.0)
    
    if ( all(!is.na(h.t)) ) {
      resultMC <- BIOMASS::AGBmonteCarlo(D=data$d.stem*100, WD=data$wsg, H=h.t, 
                                         errWD=data$wsg.sd, errH=h.t.err, Dpropag="chave2004", 
                                         n=nruns, Dlim=5.0)
    } else {
      geo.coords <- c(data$longitude[1],data$latitude[1])
      resultMC <- BIOMASS::AGBmonteCarlo(D=data$d.stem*100, WD=data$wsg, coord=geo.coords, 
                                         errWD=data$wsg.sd, Dpropag="chave2004", n=nruns, 
                                         Dlim=5.0)
    }
    
    AGB_simu <- (resultMC$AGB_simu * 1e3) / replicate( ncol(resultMC$AGB_simu), data$sp.area / data$p.area )
    agb.mc <- plyr::adply(AGB_simu, 2, sum, na.rm=TRUE, .id=NULL)$V1
    agb <- mean(agb.mc)
    agb.cred <- quantile(agb.mc, probs=c(1-q,q), .id=NULL, names=FALSE, na.rm=TRUE)
    agb.lower <- agb.cred[1]
    agb.upper <- agb.cred[2]
    
    agbd.ha <-  (agb / 1e3) / (data$p.area[1] / 1e4)
    agbd.ha.lower <- (agb.lower / 1e3) / (data$p.area[1] / 1e4)
    agbd.ha.upper <- (agb.upper / 1e3) / (data$p.area[1] / 1e4)
    
    sn <- sum( as.numeric(data$a.stem > 0) * (data$p.area / data$sp.area), na.rm=TRUE)
    snd.ha <- sum( as.numeric(data$a.stem > 0) * (data$p.area / data$sp.area) / (data$p.area / 1e4), na.rm=TRUE)
    sba <- sum( data$a.stem / (data$sp.area / data$p.area), na.rm=TRUE)
    sba.ha <- sum( ( data$a.stem / (data$sp.area / data$p.area) ) / (data$p.area / 1e4), na.rm=TRUE)
    swsg.ba <- sum(data$wsg * data$a.stem * (data$sp.area / data$p.area), na.rm=TRUE)
    if ( any(!is.na(h.t)) ) {
      h.t.max <- max(h.t, na.rm=TRUE)
    } else {
      h.t.max <- NA
    }
    
    if ( all(is.na(data$d.stem.valid)) ) {
      agb.valid <- 1
    } else {
      min.d.stem.valid <- min(data$d.stem.valid, na.rm=TRUE)
      if ( is.infinite(min.d.stem.valid) ) {
        agb.valid <- 1
      } else {
        if ( min.d.stem.valid > 0 ) {
          agb.valid <- 1
        } else {
          agb.valid <- 0
        }
      }
    }
    
    data.frame(plot=data$plot[1], survey=data$survey[1], subplot=data$subplot[1],
               agb=agb, agb.lower=agb.lower, agb.upper=agb.upper, 
               agbd.ha=agbd.ha, agbd.ha.lower=agbd.ha.lower, agbd.ha.upper=agbd.ha.upper,
               sn=sn, snd.ha=snd.ha, sba=sba, sba.ha=sba.ha, swsg.ba=swsg.ba, h.t.max=h.t.max,
               agb.valid=agb.valid)
    
  } else {
    
    plotAGB_generic(data)
      
  }
}


## Error propagation for other allometrics
plotAGB_generic <- function(data) {
  browser()
  agb <- sum(data$m.agb, na.rm=TRUE)
  agb <- ifelse(is.na(agb), 0, agb)

  agbd.ha <- sum( ( (data$m.agb / 1e3) / (data$sp.area / data$p.area) ) / (data$p.area / 1e4), na.rm=TRUE)
  
  sn <- sum( as.numeric(data$a.stem > 0) * (data$p.area / data$sp.area), na.rm=TRUE)
  snd.ha <- sum( as.numeric(data$a.stem > 0) * (data$p.area / data$sp.area) / (data$p.area / 1e4), na.rm=TRUE)
  sba <- sum( data$a.stem / (data$sp.area / data$p.area), na.rm=TRUE)
  sba.ha <- sum( ( data$a.stem / (data$sp.area / data$p.area) ) / (data$p.area / 1e4), na.rm=TRUE)
  swsg.ba <- sum(data$wsg * data$a.stem * (data$sp.area / data$p.area), na.rm=TRUE)
  h.t <- ifelse(is.na(data$h.t), data$h.t.mod, data$h.t)
  if ( any(!is.na(h.t)) ) {
    h.t.max <- max(h.t, na.rm=TRUE)
  } else {
    h.t.max <- NA
  }
  
  if ( all(is.na(data$d.stem.valid)) ) {
    agb.valid <- 1
  } else {
    min.d.stem.valid <- min(data$d.stem.valid, na.rm=TRUE)
    if ( is.infinite(min.d.stem.valid) ) {
      agb.valid <- 1
    } else {
      if ( min.d.stem.valid > 0 ) {
        agb.valid <- 1
      } else {
        agb.valid <- 0
      }
    }
  }
  
  data.frame(plot=data$plot[1], survey=data$survey[1], subplot=data$subplot[1],
             agb=agb, agb.lower=NA, agb.upper=NA, 
             agbd.ha=agbd.ha, agbd.ha.lower=NA, agbd.ha.upper=NA,
             sn=sn, snd.ha=snd.ha, sba=sba, sba.ha=sba.ha, swsg.ba=swsg.ba, h.t.max=h.t.max, 
             agb.valid=agb.valid)
}


## Summarize the tree data at the plot level
summarize_plots_study <- function(data,variable_definitions,HtModelRSE,subplot_agg=NA,montecarlo=FALSE,nruns=1000) {
  browser()
  # Aggregate subplots if requried
  if ( !is.na(subplot_agg) ) {
      data$sp.ix <- as.integer((data$sp.ix - 1) / 2) + 1
      data$sp.iy <- as.integer((data$sp.iy - 1) / 2) + 1
      data$subplot <- sprintf("%i.%i", data$sp.ix, data$sp.iy)
  }
  
  plotvar <- variable_definitions$variable[which(variable_definitions$group %in% c("key","plot","subplot","lidar"))]
  keysvar <- variable_definitions$variable[which(variable_definitions$group == "key")]
  plotdata <- plyr::arrange(data[!duplicated(data[,keysvar]),plotvar], plot, survey, subplot)

  # Only derive biomass if we can
  nTreesWithMeasurements <- sum( !is.na(data$a.stem) )
  if (nTreesWithMeasurements > 0) {
    
    # Calculate scaled values for each subplot
    ii <- is.na(data$sp.area)
    data$sp.area[ii] <- data$p.area[ii]
    ii <- data$p.sample == 3 # variable radius plots
    data$p.area[ii] <- 10000 # force the output for variable radius plots to be 1 ha
    ii <- (data$p.sample == 0) & !is.na(data$sp.geom) & !is.na(data$sp.area)
    data$p.area[ii] <- data$sp.area[ii]
    
    # Calcuate biomass and structure metrics
    if ( montecarlo ) {
        newdata.exists <- FALSE
        ii <- ( data$allom.key %in% c(1,2) ) & !is.na(data$allom.key)
        if ( any(ii) ) {
          newdata <- plyr::ddply(data[ii,], keysvar, plotAGB_chave2014, errH=HtModelRSE, nruns=nruns)
          newdata.exists <- TRUE
        }
        ii <- !( data$allom.key %in% c(1,2) )
        if ( any(ii) ) {
          if ( newdata.exists ) {
            newdata.generic <- plyr::ddply(data[ii,], keysvar, plotAGB_generic)
            newdata <- rbind(newdata,newdata.generic)  
          } else {
            newdata <- plyr::ddply(data[ii,], keysvar, plotAGB_generic)
          }
        }
    } else {
        newdata <- plyr::ddply(data, keysvar, plotAGB_generic)
    }
    newdata <- plyr::arrange(newdata, plot, survey, subplot)
    
    # Check if there are plots without any trees
    if ( length(plotdata$agb) > length(newdata$agb) ) {
      newdata <- plyr::join(plotdata[,keysvar], newdata, by=keysvar)
      ii <- is.na(newdata$agb)
      newdata$agb[ii] <- 0
      newdata$agbd.ha[ii] <- 0
      newdata$sn[ii] <- 0
      newdata$snd.ha[ii] <- 0
      newdata$sba[ii] <- 0
      newdata$sba.ha[ii] <- 0
      newdata$agb.valid[ii] <- 1
    }
    
    # only use calculated data estimates that do not already exist
    ii <- is.na(plotdata$agb) & !is.na(newdata$agb)
    plotdata$agb[ii] <- newdata$agb[ii]
    plotdata$agb.valid[ii] <- newdata$agb.valid[ii]
    plotdata$agb.lower[ii] <- newdata$agb.lower[ii]
    plotdata$agb.upper[ii] <- newdata$agb.upper[ii]
    ii <- is.na(plotdata$agbd.ha) & !is.na(newdata$agbd.ha)
    plotdata$agbd.ha[ii] <- newdata$agbd.ha[ii]
    plotdata$agbd.ha.lower[ii] <- newdata$agbd.ha.lower[ii]
    plotdata$agbd.ha.upper[ii] <- newdata$agbd.ha.upper[ii]
    ii <- is.na(plotdata$sn) & !is.na(newdata$sn)
    plotdata$sn[ii] <- newdata$sn[ii]
    ii <- is.na(plotdata$snd.ha) & !is.na(newdata$snd.ha)
    plotdata$snd.ha[ii] <- newdata$snd.ha[ii]
    ii <- is.na(plotdata$sba) & !is.na(newdata$sba)
    plotdata$sba[ii] <- newdata$sba[ii]    
    ii <- is.na(plotdata$sba.ha) & !is.na(newdata$sba.ha)
    plotdata$sba.ha[ii] <- newdata$sba.ha[ii]
    ii <- is.na(plotdata$swsg.ba) & !is.na(newdata$swsg.ba) & (newdata$sba > 0)
    plotdata$swsg.ba[ii] <- newdata$swsg.ba[ii]
    ii <- is.na(plotdata$h.t.max) & !is.na(newdata$h.t.max) & (newdata$h.t > 0)
    plotdata$h.t.max[ii] <- newdata$h.t.max[ii]
    
    # Aggregate subplots to each plot where they are not independent
    aggregate_subplots <- function(plotdata) {
      newdata <- plotdata[1,]
      newdata$agb <- ifelse(newdata$p.sample == 1, sum(plotdata$agb), mean(plotdata$agb))
      newdata$agb.valid <- min(plotdata$agb.valid, na.rm=TRUE)
      newdata$agb.lower <- ifelse(newdata$p.sample == 1, sum(plotdata$agb.lower), mean(plotdata$agb.lower))
      newdata$agb.upper <- ifelse(newdata$p.sample == 1, sum(plotdata$agb.upper), mean(plotdata$agb.upper))
      newdata$agbd.ha <- ifelse(newdata$p.sample == 1, sum(plotdata$agbd.ha), mean(plotdata$agbd.ha))
      newdata$agbd.ha.lower <- ifelse(newdata$p.sample == 1, sum(plotdata$agbd.ha.lower), mean(plotdata$agbd.ha.lower))
      newdata$agbd.ha.upper <- ifelse(newdata$p.sample == 1, sum(plotdata$agbd.ha.upper), mean(plotdata$agbd.ha.upper))
      newdata$sn <- ifelse(newdata$p.sample == 1, sum(plotdata$sn), mean(plotdata$sn))
      newdata$snd.ha <- ifelse(newdata$p.sample == 1, sum(plotdata$snd.ha), mean(plotdata$snd.ha))
      newdata$sba <- ifelse(newdata$p.sample == 1, sum(plotdata$sba), mean(plotdata$sba))
      newdata$sba.ha <- ifelse(newdata$p.sample == 1, sum(plotdata$sba.ha), mean(plotdata$sba.ha))
      newdata$swsg.ba <- ifelse(newdata$p.sample == 1, sum(plotdata$swsg.ba), mean(plotdata$swsg.ba))
      if ( any(!is.na(plotdata$h.t.max)) ) {
        newdata$h.t.max <- max(plotdata$h.t.max, na.rm=TRUE)
      } else {
        newdata$h.t.max <- NA
      }
      newdata
    }
    
    # Combine the aggregated data for each sampling protocol
    keysvar <- keysvar[!(keysvar %in% "subplot")]
    ii <- (plotdata$p.sample == 1) | (plotdata$p.sample == 2)
    newdata <- plyr::ddply(plotdata[ii,], keysvar, aggregate_subplots)   
    plotdata <- plyr::arrange(rbind(plotdata[!ii,], newdata), plot, survey, subplot)
    
    # Normalize the basal area weighted WSG sum by SBA so we have the weighted average
    ii <- !is.na(plotdata$swsg.ba)
    plotdata$swsg.ba[ii] <- plotdata$swsg.ba[ii] / plotdata$sba[ii]
    
  }
  
  plotdata
}

## Generate collection of footprint geometries
summarize_gedi_study <- function(plotdata,variable_definitions,fp.radius,fp.spacing,tk.spacing,circlepacking=FALSE) {
  browser()
  # Add the empty GEDI footprint field
  plotdata$g.fp <- NA
  
  # Determine GEDI footprints
  if ( circlepacking ) {
    
    # Rectangular plots
    ii <- (plotdata$p.shape == "R") & !is.na(plotdata$l.epsg)
    fp.spacing <- fp.radius * 2 * (1 - fp.overlap)
    plotdata[ii,] <- plyr::adply(plotdata[ii,], 1, create_gedi_footprint_grid, fp.spacing, fp.radius, tk.spacing)
    
    # Circular plots
    ii <- (plotdata$p.shape == "E") & !is.na(plotdata$l.epsg)
    fp.overlap <- 1 - ( fp.spacing / (fp.radius * 2) )
    plotdata[ii,] <- plyr::adply(plotdata[ii,], 1, create_gedi_footprint_ring, fp.radius, fp.overlap, tk.spacing)  
  
  } else {
  
    # Determine GEDI footprints for all plots
    ii <- !is.na(plotdata$l.epsg)
    if ( any(ii, na.rm=TRUE) ) {
      plotdata[ii,] <- plyr::adply(plotdata[ii,], 1, create_gedi_footprint_grid, fp.spacing, fp.radius, tk.spacing)  
    }
    
  }
  
  # Return the selected data
  outvar <- variable_definitions$variable[which(variable_definitions$group %in% c("key","lidar","gedi"))]
  plotdata[,outvar]
  
}
