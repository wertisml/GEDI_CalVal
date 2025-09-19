library(hdf5r)
library(rhdf5)
library(tidyverse)

read_sim_h5 <- function(fn, i) {
  
  rx <- fn[["RXWAVEFRAC"]][,i]
  rg <- fn[["GRWAVEFRAC"]][,i]
  z0 <- fn[["Z0"]][i]
  zn <- fn[["ZN"]][i]
  nbins <- fn[["NBINS"]][1]
  zg <- fn[["ZG"]][i]
  zgdem <- fn[["ZGDEM"]][i]
  
  # Calculate intermediate values
  v <- (z0 - zn) / (nbins - 1)
  elev <- z0 - seq(0, nbins - 1) * v
  height <- elev - zg
  rv <- rx - rg
  rv[elev <= zgdem] <- 0
  
  # Process waveid
  waveid_temp <- fn[["WAVEID"]][,i]
  waveid_temp2 <- paste0(waveid_temp, collapse = "")
  waveid <- gsub("b|'", "", waveid_temp2)
  
  # Close the HDF5 file
  #fn$close()
  
  return(list(rx = rx, rg = rg, rv = rv, elev = elev, height = height, waveid = waveid))
}

extract_dz_vals <- function(profiles, height, dz_vals, max_val, null_value) {
  # Initialize profile_vals with the null_value
  profile_vals <- rep(null_value, length(dz_vals))
  
  # Loop through each element in dz_vals
  for (i in seq_along(dz_vals)) {
    tmp <- height - dz_vals[i]
    
    # Find the index where tmp first becomes greater than 0
    idx <- which.min(tmp > 0)
    val <- height[idx]
    
    # Check the condition and assign values accordingly
    if (!is.na(val) && val >= max_val) {
      profile_vals[i] <- null_value
    } else {
      profile_vals[i] <- profiles[idx]
    }
  }
  
  return(profile_vals)
}

cover_pavd <- function(file, dz = 5){
  
  sim_psigma0_fn <- file
  
  sim <- H5File$new(sim_psigma0_fn, "r")
  
  waveid_list <- list()
  cover_list <- list()
  PAVD_list <- list() 
  
  waveid_data <- h5read(sim_psigma0_fn, "/WAVEID")
  
  # Determine the length of the '/WAVEID' dataset
  sim_length <- ncol(waveid_data)
  
  for (i in 0:sim_length) {
    tryCatch({
      # Read the simulated data
      sim_data <- read_sim_h5(sim, i)
      rx_sim <- sim_data$rx
      rg_sim <- sim_data$rg
      rv_sim <- sim_data$rv
      elev_sim <- sim_data$elev
      height_sim <- sim_data$height
      waveid <- sim_data$waveid
      
      rv_cum_sim <- cumsum(rv_sim)
      rv_tot_sim <- sum(rv_sim)
      rg_tot_sim <- sum(rg_sim)
      
      rhov_rhog <- 1
      pgap_theta_z_sim <- 1.0 - (rv_cum_sim / (rg_tot_sim * rhov_rhog + rv_tot_sim))
      pgap_theta_sim <- min(pgap_theta_z_sim, na.rm = TRUE)
      
      # L2B metrics (cover, PAI, PAVD)
      dz <- dz 
      maxheight <- 50
      dz_vals <- seq(0, maxheight, by = dz)
      max_dz_val <- tail(dz_vals, 1) + dz
      local_beam_elevation <- 1.5
      cos_zenith <- abs(sin(local_beam_elevation))
      
      cover_z_sim <- cos_zenith * (1.0 - pgap_theta_z_sim)
      pai_z_sim <- -(1.0 / (0.5 * 1.0)) * log(pgap_theta_z_sim) * cos_zenith
      pai_z_r_sim <- extract_dz_vals(pai_z_sim, height_sim, dz_vals, max_dz_val, null_value = -9999)
      
      pavd_z_gr_sim <- -diff(pai_z_r_sim) / dz
      
      waveid_list <- append(waveid_list, list(waveid))
      cover_list <- append(cover_list, list(cover_z_sim[length(cover_z_sim)]))
      PAVD_list <- append(PAVD_list, list(pavd_z_gr_sim))
      
      df_cover_w <- data.frame(waveid = unlist(waveid_list), canopy_cover = unlist(cover_list))
      df_pavd <- as.data.frame(do.call(rbind, PAVD_list))
      
      df_l2b_sim <- cbind(df_cover_w, df_pavd)
    }, error = function(e) {})
  }
  sim$close_all()
  
  # Check if df_l2b_sim is empty and set it to 1 if it is
  if (!exists("df_l2b_sim")) {
    df_l2b_sim <- 1
  }
  return(df_l2b_sim)
}


combine_metrics <- function(project){
  
  WorkDir <- ""
  gedi_metric_txt <- paste0("gedi_database_sims/metrics/")
  gedi_metric_csv <- paste0(gedi_metric_txt, "gedi_metric_csv/")
  unnoised <- "unnoisedMetric."
  CoordDir <- paste0("gedi_database_sims/coords/")
  calval <- "gedicalval_gedirat_"
  
  coords <- readr::read_csv(paste0(CoordDir,calval,project,".csv"), col_names = FALSE)
  
  locations <- unique(coords$X3)
  
  for(location in locations){
    tryCatch({
  files <- list.files(path = paste0("gedi_database_sims/waveforms/",location,"/tmp"),
                      pattern = ".h5",
                      full.names = TRUE)
  
    }, error = function(e) {})
  Full_Data <- list()
  for(j in 1:length(files)){
    
      metrics <- cover_pavd(files[j], dz = 5)
      
      if(is.null(nrow(metrics))){
        next
      }
      
      metrics <- metrics[-1, ]
      metrics <- as.data.frame(lapply(metrics, function(x) gsub("\"", "", x)))

      pattern <- paste0(".*", location, "\\.(\\d+)\\.h5$")
      k <- sub(pattern, "\\1", files[j])
      
      # Step 1: Read the data, skipping the first line with column names
      # Define the file path
      file_path <- paste0(gedi_metric_txt, unnoised, location, ".", k, ".metric.txt")
      
      # Check if the file exists, if not, skip to the next iteration
      if (!file.exists(file_path)) {
        next
      }
      
      # Step 1: Read the data, skipping the first line with column names
      tryCatch({
        data <- read.table(file_path, header = FALSE, skip = 1)
      }, error = function(e) {})
  
      # Step 2: Extract the first line (with column names) and clean it
      column_names <- readLines(paste0(gedi_metric_txt, unnoised,location,".",k,".metric.txt"), n = 1)  # Read just the first line
 
      # Step 3: Clean the column names by removing the leading "#" and numbers
      #cleaned_column_names <- gsub("^#\\s*", "", column_names)  # Remove the leading '#' and any spaces
      cleaned_column_names <- gsub("^#\\s*|\\s+(\\d+)", "\\1", column_names)
      cleaned_column_names <- gsub("\\b\\d+\\s", "", cleaned_column_names)
      cleaned_column_names <- unlist(strsplit(cleaned_column_names, "\\s*,\\s*"))  # Split by commas and remove extra spaces
      colnames(data) <- cleaned_column_names
      
      metrics <- left_join(data, metrics, join_by("wave ID" == "waveid"))
  
      Full_Data[[j]] <- metrics
      #print(paste0(location,j))
  }
  combined_data <- bind_rows(Full_Data) %>%
    distinct(`wave ID`, .keep_all = TRUE)
  
  combined_data <- combined_data %>%
    left_join(coords %>% filter(X1 %in% combined_data$lon) %>% select(X1, X2, X5),
              by = join_by(lon == X1,
                           lat == X2)) %>%
    dplyr::rename(waveID = X5)
  
  if (!dir.exists(gedi_metric_csv)) {
    dir.create(folder_path, recursive = TRUE)
  } 
  
  write.csv(combined_data, paste0(gedi_metric_csv,unnoised,location,".metric.csv"))
  }
}

#combine_metrics("australia_ausplotsforests_20240711")

  