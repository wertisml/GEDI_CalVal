# Load renv environment
#renv::load("/root/renv")

#!/usr/bin/env Rscript
library(whisker)
library(remake)
library(optparse)

# Function to separate multiple values into a list
parse_project_list <- function(val) {
  if ( is.na(val) ) {
    return( NA )
  } else {
    option_value.list <- strsplit(val,",",fixed=TRUE)
    return( unlist(option_value.list) )
  }
}

# Function to parse command line arguments
cmds_fun <- function() {
    yyyymmdd <- format(Sys.time(), "%Y%m%d")
    label <- sprintf("%s_r01", yyyymmdd)
    option_list = list(
      make_option(c("-i", "--include"), action="store", default=NA, type='character',
                  help="Project names to include [default %default]."),
      make_option(c("-e", "--exclude"), action="store", default=NA, type='character',
                  help="Project names to exclude [default %default]."),
      make_option(c("-b", "--build"), action="store_true", default=FALSE, type='logical',
                  help="Build as well as compile so outputs are generated [default %default]."),
      make_option(c("-l", "--label"), action="store", default=label, type='character',
                  help="Label for the output filenames [default %default]."),
      make_option(c("-r","--radius"), action="store", default=12.5, type='double',
                  help="GEDI footprint radius [default %default]"),
      make_option(c("-f","--footprint_spacing"), action="store", default=25.0, type='double',
                  help="GEDI footprint spacing [default %default]"),
      make_option(c("-t","--track_spacing"), action="store", default=25.0, type='double',
                  help="GEDI track spacing [default %default]"),
      make_option(c("-m", "--montecarlo"), action="store_true", default=FALSE, type='logical',
                  help="Propagate errors using Monte Carlo in AGB estimation [default %default]."),
      make_option(c("-n","--nruns"), action="store", default=1000, type='integer',
                  help="Number of Monte Carlo runs in AGB estimation [default %default]")
    )
    opt <- parse_args(OptionParser(option_list=option_list))
    return(opt)
}

# Configure the GEDICalVal build
configGEDICalVal <- function(study_names, opt) {
  vals <- list(study_names=iteratelist(study_names, value="study_name"),
               label=opt$label,
               fp_radius=opt$radius,
               fp_spacing=opt$footprint_spacing,
               tk_spacing=opt$track_spacing,
               montecarlo=opt$montecarlo,
               nruns=opt$nruns)
  
  remake_str <- whisker.render(readLines("config/remake.yml.whisker"), vals)
  remake_file <- sprintf("remake_%s.yml", opt$label)
  writeLines(remake_str, remake_file)
  
  remake_data_str <- whisker.render(readLines("config/remake_data.yml.whisker"), vals)
  remake_data_file <- sprintf("remake_data_%s.yml", opt$label)
  writeLines(remake_data_str, remake_data_file)
  
  remake::install_missing_packages(remake_file=remake_file)
}


opt <- cmds_fun()

if ( !is.na(opt$include) ) {
  
  include.study_names <- parse_project_list(opt$include)
  valid.study_names <- include.study_names %in% dir("data")
  if ( !all(valid.study_names) ) {
    stop("Invalid project names provided")
  }
  
} else if ( !is.na(opt$exclude) ) {
  
  exclude.study_names <- parse_project_list(opt$exclude)
  valid.study_names <- exclude.study_names %in% dir("data")
  if ( !all(valid.study_names) ) {
    stop("Invalid project names provided")
  }
  include.study_names <- dir("data")[!(dir("data") %in% exclude.study_names)]
  
} else {
  
  include.study_names <- dir("data")
  
}

configGEDICalVal(include.study_names, opt)
remake_file <- sprintf("remake_%s.yml", opt$label)
if ( opt$build ) {
  remake::make("export",remake_file=remake_file)
} else {
  remake::make(include.study_names,remake_file=remake_file)
}
