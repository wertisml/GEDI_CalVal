library(lidR)
library(terra)

retile <- function(project){
  
  WorkDir <- ""
  CoordDir <- paste0(WorkDir, "gedi_database_sims/coords/")
  calval <- "gedicalval_gedirat_"
  Data <- "data/"
  las_folder <- "/las/"
  proj <- sub("_[^_]*$", "", project)
  path <- paste0(WorkDir,Data,proj,las_folder)
  locations <- dir(path)
  
  coords <- read_csv(paste0(CoordDir,calval,project,".csv"), col_names = FALSE)
  
  plot.dir <- data.frame()
  for(i in locations){
    
    files <- list.files(path = paste0(path,i), pattern = ".las")
    
    for(file in files){
    if(file.info(paste0(path,i,"/",file))$size / (1024 * 1024) > 700) {
      
      n <- round(file.info(paste0(path,i,"/",file))$size / (1024 * 1024) / 700)
      ctg = readLAScatalog(paste0(path,i,"/",file))
      
      opt_chunk_size(ctg) <- sqrt((expanse(vect(ext(ctg))) / n))
      opt_output_files(ctg) <- paste0(path,i,"/", file,"_{ID}")
      opt_filter(ctg) <- "-drop_point_count_below 1"
      retiled <- catalog_retile(ctg)
      }
    }
    
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
  
  write.table(plot.dir, file = file_path, 
              sep = " ",         
              row.names = FALSE, 
              col.names = FALSE,  
              append = TRUE,      
              quote = FALSE) 
}

retile("australia_ausplotsforests_20240711")

