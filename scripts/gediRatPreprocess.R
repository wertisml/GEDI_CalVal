#.libPaths("/gpfs/data1/vclgp/wertisl/R_Code/R_Notebook/lib/R/library")
library(lidR)
library(tidyverse)

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
    
    small_files <- list()
    for (file in las_files) {
      # Get file size in MB
      file_size_mb <- file.info(file)$size / (1024 * 1024)
      
      # If the file is less than or equal to 700 MB, add it to the list
      if (file_size_mb <= 700) {
        small_files <- c(small_files, file)
      }
    }
    
    results <- data.frame(FileName = character(),
                          Min_X = numeric(),
                          Min_Y = numeric(),
                          Min_Z = numeric(),
                          Max_X = numeric(),
                          Max_Y = numeric(),
                          Max_Z = numeric(),
                          stringsAsFactors = FALSE)
    
    for (file in small_files) {
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
    for(j in 1:nrow(results)){
      
      site <- results[j,]
      site_name <- gsub("\\.las$", "", basename(site[,1]))
      
      bbox <- ext(site[,2], site[,3], site[,5], site[,6])
      bbox <- as.polygons(bbox, crs = paste0("EPSG:", epsg[[1]]))
      
      fp <- vect(roi, c("x","y"), crs = paste0("EPSG:", epsg[[1]]))
      
      p <- fp[bbox,]
      p$las_file <- site_name
      
      pts[[j]] <- p
    }
    
    non_empty_pts <- Filter(function(v) !is.empty(v), pts)
    combined_pts <- do.call(rbind, non_empty_pts)
    
    files_needed <- c(unique(combined_pts$las_file))
    
    # Filter full paths that contain any of the shorter versions
    filtered_paths <- results[,1][sapply(results[,1], function(full_path) {
      any(sapply(files_needed, function(short) grepl(short, full_path)))
    })]
    
    
    results <- results %>% filter(FileName %in% filtered_paths)
    
    write.table(results, file = paste0(WorkDir,"gedi_database_sims/LAS/boundGround.",i ,".txt"), sep = "\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
    
  }
}





