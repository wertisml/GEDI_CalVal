#!/usr/bin/env Rscript
library(plyr)
library(jsonlite)
library(optparse)

## Function to parse command line arguments
cmds_fun <- function() {
    option_list = list(
      make_option(c("-p", "--project"), action="store", default=NA, type='character',
                  help="Project name if processing a single project [default %default].")
    )
    opt <- parse_args(OptionParser(option_list=option_list))
    return(opt)
}

## Export GEDI footprint center coordinates
export_gedi_footprints <- function(data) {
  if ( !is.na(data$g.fp) ) {
    fp.grid <- as.data.frame( fromJSON(data$g.fp) )
    max.id <- nrow(fp.grid)
    gedi.id <- paste(data$project, data$plot, data$survey, data$subplot,
                     sprintf("x%09iy%09i", round(fp.grid$x*100), round(fp.grid$y*100)),
                     sep=":")
    data.frame(fp.grid[,c("x","y")],
               lidar_project=data$l.project,lidar_epsg=data$l.epsg,gedi_id=gedi.id)
  }
}

## Generate collection of footprint geometries
summarize_gedi_study <- function(data) {
  
  plotvar <- c("project","plot","subplot","survey","p.shape","p.geom","sp.geom","p.epsg","l.project","l.epsg","g.fp")
  plotdata <- data[, plotvar]
  
  plotdata.fp <- plyr::adply(plotdata, 1, export_gedi_footprints)
  plotdata.fp <- plotdata.fp[,!(names(plotdata.fp) %in% plotvar)]
  
  plyr::arrange(plotdata.fp, gedi_id)
}

## Run export
opt <- cmds_fun()

if ( !is.na(opt$project) ) {
    rdata.files <- file.path("shiny", sprintf("gedicalval_%s_r01.rds", opt$project))
    outfile <- file.path("export", sprintf("gedicalval_gedirat_%s.csv", opt$project))
    outfile2 <- file.path("gedi_database_sims/coords", sprintf("gedicalval_gedirat_%s.csv", opt$project))
} else {
    rdata.files <- list.files(path="shiny", pattern="^gedicalval_.*?_r02\\.rds", full.names=TRUE)
    outfile <- file.path("export", "gedicalval_gedirat.csv")
    outfile2 <- file.path("gedi_database_sims/coords", "gedicalval_gedirat.csv")
}

if ( file.exists(outfile) ) {
    file.remove(outfile)
}

for (rdata.file in rdata.files) {

    # Read the data and join the gedi footprints with the plotdata
    gedicalval <- readRDS(rdata.file)
    thedata <- cbind(gedicalval$plotdata, gedicalval$gedidata)
    remove(gedicalval)

    # Query and export the valid data
    newdata <- subset(thedata, !is.na(p.epsg) & !is.na(l.epsg) & !is.na(l.project))
    if ( nrow(newdata) > 0 ) {
        outdata <- summarize_gedi_study(newdata)
        outdata$gedi_id <- gsub("N/A", "NA", outdata$gedi_id, ignore.case=TRUE)
        write.table(outdata, file=outfile, row.names=FALSE, quote=FALSE, append=TRUE, sep=",", col.names=FALSE) 
        write.table(outdata, file=outfile2, row.names=FALSE, quote=FALSE, append=TRUE, sep=",", col.names=FALSE)
    } else {
        print( sprintf("No valid data in %s", rdata.file) )
    }

}

