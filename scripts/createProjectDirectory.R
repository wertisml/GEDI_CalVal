#!/usr/bin/env Rscript

## Create a template directory for a new data project
create_project_directory <- function(project_name) {
  
  project_template <- list(
    plotDataImportOptions.csv = c("name,plotData.csv", "header,TRUE", "skip,0", "separator,\",\""),
    treeDataImportOptions.csv = c("name,treeData.csv", "header,TRUE", "skip,0", "separator,\",\"", "key,NULL"),
    studyMetadata.csv = c("Item,Value", "Source,NULL", "Project,NULL", "Contact,NULL", "Email,NULL", 
                          "Region,NULL", "Sampling strategy,NULL", "Stem measurement,NULL", 
                          "Height measurement,NULL", "Location measurement,NULL", "Status,NULL"),
    dataMatchColumns.csv = "var_in,method,unit_in,var_out,notes",
    dataNew.csv = c("lookupVariable,lookupValue,newVariable,newValue,source"),
    dataManipulate.R = c("manipulate <- function(raw) {\n\nraw\n}\n",
                         "manipulate_plot <- function(raw) {\n\nraw\n}\n",
                         "manipulate_tree <- function(raw) {\n\nraw\n}")
  )
  
  if ( dir.exists("data") ) {
    dirname <- paste("data", project_name, sep=.Platform$file.sep)
    if ( !dir.exists(dirname) ) {
      dir.create(dirname)
      for ( name in names(project_template) ) {
        ii <- which(names(project_template) == name)
        outfile <- file.path(dirname, name)
        lapply(project_template[[ii]], write.table, outfile, append=TRUE,
               quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
      }
    }    
  }
}

args <- commandArgs(TRUE)
project_name <- args[1]
create_project_directory(project_name)
