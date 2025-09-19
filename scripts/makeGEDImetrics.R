#.libPaths("/gpfs/data1/vclgp/wertisl/R_Code/R_Notebook/lib/R/library")
library(lidR)
library(terra)
library(tidyverse)
#library(optparse)

## Function to parse command line arguments
cmds_fun <- function() {
  option_list = list(
    make_option(c("-p", "--project"), action="store", default=NA, type='character',
                help="Input project name [default %default].")
  )
  opt <- parse_args(OptionParser(option_list=option_list))
  return(opt)
}

retile <- function(project){
  
  WorkDir <- ""
  CoordDir <- paste0(WorkDir, "gedi_database_sims/coords/")
  calval <- "gedicalval_gedirat_"
  Data <- "data/"
  las_folder <- "/las/"
  proj <- sub("_[0-9]+$", "", project)
  path <- paste0(WorkDir,Data,proj,las_folder)
  #locations <- dir(path)
  
  coords <- read_csv(paste0(CoordDir,calval,project,".csv"), col_names = FALSE)
  locations <- unique(coords$X3)
  
  plot.dir <- data.frame()
  for(i in locations){
    
    files <- list.files(path = paste0(path,i), pattern = "\\.las$")
    if (length(files) == 0) next
    
    # for(file in files){
    #   if(file.info(paste0(path,i,"/",file))$size / (1024 * 1024) > 700) {
    # 
    #     n <- ceiling(file.info(paste0(path,i,"/",file))$size / (1024 * 1024) / 700)
    #     ctg = readLAScatalog(paste0(path,i,"/",file))
    # 
    #     file_name <- gsub("\\.las$", "", file)
    # 
    #     #opt_chunk_size(ctg) <- sqrt((expanse(vect(ext(ctg), crs = crs(paste0("EPSG:",ctg@data$CRS)))) / n))
    #     opt_chunk_size(ctg) <- sqrt((expanse(vect(ext(ctg))) / n))
    #     opt_output_files(ctg) <- paste0(path,i,"/", file_name,".{ID}")
    #     opt_filter(ctg) <- "-drop_point_count_below 1"
    #     retiled <- catalog_retile(ctg)
    #   }
    # }
    
    epsg <- coords %>% 
      filter(X3 == i) %>% 
      select(X4) %>%
      head(n=1)
    
    new_row <- data.frame(
      proj = proj,
      location = i,
      path = paste0(path,i),
      epsg = epsg[[1]],
      stringsAsFactors = FALSE
    )
    plot.dir <- rbind(plot.dir, new_row)
  }
  
  file_path <- "gedi_als_processing/scripts/lists/plot_dir.txt"
  
  append_unique <- function(data, file_path) {
    # Check if the file exists and read existing data if it does
    if (file.exists(file_path)) {
      existing_data <- read.table(file_path, header = FALSE, sep = " ", stringsAsFactors = FALSE)
      colnames(existing_data) <- colnames(data) # Ensure column names match
    } else {
      # Create the file and write the data if it doesn't exist
      write.table(data, file = file_path, 
                  sep = " ", 
                  row.names = FALSE, 
                  col.names = FALSE, # Write column names for the first time
                  quote = FALSE)
      return() # Exit the function after writing the data
    }
    
    # Identify unique rows in `data` that are not duplicates of existing data
    new_data <- data[!duplicated(rbind(existing_data, data))[-seq_len(nrow(existing_data))], ]
    
    # Write to the file: either create it if necessary or append unique rows
    write.table(new_data, file = file_path, 
                sep = " ",         
                row.names = FALSE, 
                col.names = FALSE,  
                append = file.exists(file_path),  # Only append if file exists
                quote = FALSE)
  }
  
  append_unique(plot.dir, file_path)
}

gediRatListPreProcess <- function(project){
  
  WorkDir <- ""
  Processing <- paste0(WorkDir, "gedi_als_processing/scripts")
  CoordDir <- paste0(WorkDir, "gedi_database_sims/coords/")
  calval <- "gedicalval_gedirat_"
  Simulate <- paste0(Processing,"/simulate/simulate.csh")
  proj <- sub("_[^_]*$", "", project)
  
  coords <- read_csv(paste0(CoordDir,calval,project,".csv"), col_names = FALSE)
  
  locations <- unique(coords$X3)
  
  for(i in locations){
    
    epsg <- coords %>% 
      filter(X3 == i) %>% 
      select(X4) %>%
      head(n=1)
    
    roi <- coords %>%
      filter(X3 == i) %>%
      rename(x = X1,
             y = X2) %>%
      select(x, y)
    
    write.table(roi, paste0(CoordDir,"plots/gediCalCoord.",i,".txt"), col.names = FALSE, row.names = FALSE)
    
    las_folder <- read_delim("gedi_als_processing/scripts/lists/plot_dir.txt", 
                             delim = " ", col_names = FALSE) %>%
      filter(X2 == i)
    
    las_files <- list.files(path = paste0(las_folder$X3), pattern = "*.las", full.names = TRUE)
    
    # small_files <- list()
    # for (file in las_files) {
    #   # Get file size in MB
    #   file_size_mb <- file.info(file)$size / (1024 * 1024)
    # 
    #   # If the file is less than or equal to 700 MB, add it to the list
    #   if (file_size_mb <= 700) {
    #     small_files <- c(small_files, file)
    #   }
    # }

    results <- data.frame(FileName = character(),
                          Min_X = numeric(),
                          Min_Y = numeric(),
                          Min_Z = numeric(),
                          Max_X = numeric(),
                          Max_Y = numeric(),
                          Max_Z = numeric(),
                          stringsAsFactors = FALSE)
    # 
    #for (file in small_files) {
    for(file in las_files) {
      # Read the LAS file
      las <- readLAScatalog(file)
      
      # Get min and max values for X, Y, and Z
      min_x <- round(las@data$Min.X)
      max_x <- round(las@data$Max.X)
      
      min_y <- round(las@data$Min.Y)
      max_y <- round(las@data$Max.Y)
      
      min_z <- round(las@data$Min.Z)
      max_z <- round(las@data$Max.Z)
      
      # Append results to the data frame
      results <- rbind(results, data.frame(FileName = file,
                                           Min_X = min_x,
                                           Min_Y = min_y,
                                           Min_Z = min_z,
                                           Max_X = max_x,
                                           Max_Y = max_y,
                                           Max_Z = max_z,
                                           stringsAsFactors = FALSE))
    }
    pts <- list()
    fp <- vect(roi, c("x","y"), crs = paste0("EPSG:", epsg[[1]]))
    for(j in 1:nrow(results)){
      
      site <- results[j,]
      site_name <- gsub("\\.las$", "", basename(site[,1]))
      
      #bbox <- terra::ext(site[,2], site[,5], site[,3], site[,6])
      bbox <- terra::ext(site[,2], site[,5], site[,3], site[,6])
      bbox <- as.polygons(bbox, crs = paste0("EPSG:", epsg[[1]]))
      
      p <- fp[bbox]
      p$las_file <- site_name
      
      pts[[j]] <- p
    }
    
    non_empty_pts <- Filter(function(v) !terra::is.empty(v), pts)
    combined_pts <- do.call(rbind, non_empty_pts)
    
    files_needed <- c(unique(combined_pts$las_file))
    
    # Filter full paths that contain any of the shorter versions
    filtered_paths <- results[,1][sapply(results[,1], function(full_path) {
      any(sapply(files_needed, function(short) grepl(short, full_path)))
    })]
    
    
    results <- results %>% filter(FileName %in% filtered_paths)
    
    append_unique_results <- function(results, file_path) {
      # Check if the file already exists and read it if it does
      if (file.exists(file_path)) {
        existing_data <- read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
        colnames(existing_data) <- colnames(results) # Ensure matching column names for comparison
        
        # Identify rows in `results` that are not duplicates of existing data
        new_data <- results[!duplicated(rbind(existing_data, results))[-seq_len(nrow(existing_data))], ]
        
        # Append only new rows if they exist
        if (nrow(existing_data) == 0 || nrow(new_data) > 0) {
          write.table(new_data, file = file_path, 
                      sep = "\t", 
                      row.names = FALSE, 
                      col.names = FALSE, 
                      quote = FALSE, 
                      append = file.exists(file_path))
        }
      } else {
        write.table(results, 
                    file = paste0(WorkDir,"gedi_database_sims/LAS/boundGround.",i ,".txt"),
                    sep = "\t",
                    row.names = FALSE, 
                    quote = FALSE,
                    col.names = FALSE) # Initialize empty if file doesn't exist
      }
    }
    
    # Use the function
    file_path <- paste0(WorkDir, "gedi_database_sims/LAS/boundGround.", i, ".txt")
    append_unique_results(results, file_path)
    
  }
}

gediRatIterate <- function(project){
  
  WorkDir <- ""
  Database <- paste0(WorkDir, "gedi_database_sims")
  CoordDir <- paste0(Database, "/coords/")
  point_files <- paste0(CoordDir,"plots")
  LAS_files <- paste0(Database, "/LAS/")
  waveDir <- paste0(Database,"/waveforms/")
  calval <- "gedicalval_gedirat_"
  
  coords <- read_csv(paste0(CoordDir,calval,project,".csv"), col_names = FALSE)
  
  epsg <- coords[4,]
  locations <- unique(coords$X3)
  
  
  for(location in locations){
    
    if (!dir.exists(paste0(waveDir,location))) {
      # If it doesn't exist, create the folder
      dir.create(paste0(waveDir,location), recursive = TRUE)
    } 
    
    if (!dir.exists(paste0(waveDir,location,"/tmp"))) {
      # If it doesn't exist, create the folder
      dir.create(paste0(waveDir,location,"/tmp"), recursive = TRUE)
    } 
    
    pts <- paste0(point_files,"/gediCalCoord.",location,".txt")
    
    las_folder <- read_table(paste0(LAS_files, "boundGround.", location, ".txt"), col_names = FALSE) %>%
      select(X1)

    # Setting up the text for the individual .sh scripts
    script_path <- paste0(Database,"/scratch/",location,".sh")
    
    header <- "#!/bin/bash\n\n# Conversion of Point Cloud to GEDI simulated footprint\n"
    
    script_content <- c(
      'cd;',                       
       ''
    )
    
    rat <- list()
    metric <- list()
    for(n in 1:nrow(las_folder)){
      
      las_file <- las_folder[n,]
      
      # Run gediRat
      gediRat <- paste0("gediRat -input /workspace/GEDI_CalVal/",las_file," -output /workspace/GEDI_CalVal/",waveDir, location,"/tmp/gedWave.", location,".", n,".h5 -listCoord /workspace/GEDI_CalVal/", pts, " -pBuff 3 -pFWHM 15.6 -fSigma 5.5 -ground -hdf -pulseAfter -aEPSG ", coords[1,4][[1]])
      rat <- append(rat, list(gediRat))
      
      # Run gediMetric
      gediMetric <-paste0("gediMetric -input /workspace/GEDI_CalVal/",waveDir, location, "/tmp/gedWave.", location,".", n, ".h5 -outRoot /workspace/GEDI_CalVal/", Database, "/metrics/unnoisedMetric.", location ,".", n," -rhRes 1 -ground -minWidth 3 -sWidth 0.5 -readHDFgedi -laiRes 1 -laiH 60")
      metric <- append(metric, list(gediMetric))
    }
    
    writeLines(c(header, script_content, unlist(rat), unlist(metric)), script_path)
    
    system(paste0("source /workspace/GEDI_CalVal/", script_path))
  }
}

makeGEDImetrics <- function(project){
  
  system(command = paste0("Rscript scripts/exportGEDICalValFootprints.R -p ", project))
  retile(project)
  gediRatListPreProcess(project)
  gediRatIterate(project)
  
  source("scripts/makeCCPAVDmetrics.R")
  combine_metrics(project)
  source("scripts/combineGEDICalValFootprintData.R")
  footprint.data(project)
  }

#opt <- cmds_fun()
#makeGEDImetrics(opt$project)