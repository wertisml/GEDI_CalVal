library(optparse)

## Function to parse command line arguments
cmds_fun <- function() {
  option_list = list(
    make_option(c("-p", "--project"), action="store", default=NA, type='character',
                help="Input project name [default %default].")
  )
  opt <- parse_args(OptionParser(option_list=option_list))
  return(opt)
}

combineFootprintmetrics <- function(project){
  
  source("scripts/makeCCPAVDmetrics.R")
  
  combine_metrics(project)
  
  system(command = paste0("Rscript scripts/combineGEDICalValFootprintData.R -p ", project ," -i r01 -o r03"))
}

opt <- cmds_fun()
combineFootprintmetrics(opt$project)

