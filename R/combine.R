load_study_helper <- function(project_name,
                              variable_definitions,
                              allometric_definitions,
                              lidar_definitions,
                              footprint_definitions,
                              wsg_database,
                              conversions,
                              fp_radius,
                              fp_spacing,
                              tk_spacing,
                              montecarlo,
                              nruns) {
  message(project_name)
  path <- function(x) file.path("data", project_name, x)
  load_study(path("plotDataImportOptions.csv"),
             path("treeDataImportOptions.csv"),
             path("dataManipulate.R"),
             path("dataMatchColumns.csv"),
             path("dataNew.csv"),
             path("studyMetadata.csv"),
             variable_definitions,
             allometric_definitions,
             lidar_definitions,
             footprint_definitions,
             wsg_database,
             conversions,
             fp_radius,
             fp_spacing,
             tk_spacing,
             montecarlo,
             nruns)
}

combine_gedicalval <- function(..., d=list(...), variable_definitions, allometric_definitions, 
                               lidar_definitions, footprint_definitions, fp_radius, fp_spacing, tk_spacing,
                               montecarlo, nruns) {
  combine <- function(name, d) {
    ret <- plyr::ldply(d, function(x) if(!is.null(x[[name]])) x[[name]]) 
    rename_columns(ret, ".id", "project")
  }
  names(d) <- sapply(d, "[[", "key")
  ret <- list(treedata=combine("data", d),
              plotdata=combine("plotdata", d),
              gedidata=combine("gedidata", d),
              metadata=combine("metadata", d))
  ret$dictionary <- variable_definitions
  ret$allometry <- allometric_definitions
  ret$lidar <- lidar_definitions
  ret$footprint <- footprint_definitions
  ret$fp_radius <- fp_radius
  ret$fp_spacing <- fp_spacing
  ret$tk_spacing <- tk_spacing
  ret$montecarlo <- montecarlo
  ret$nruns <- nruns
  ret
}

## Functions for extracting bits from gedicalval. Works around some of the
## limitations in how remake was written for GEDICalVal.
extract_gedicalval_treedata <- function(gedicalval, variable_definitions) {
  if ( nrow(gedicalval$treedata) == 0 ) {
    gedicalval$treedata <- add_all_columns(gedicalval$treedata, variable_definitions)
  }
  columns <- gedicalval$dictionary$variable[which( gedicalval$dictionary$group %in% c("key","tree") )]
  columns <- c("project", columns)
  gedicalval$treedata[columns]
}
extract_gedicalval_plotdata <- function(gedicalval) {
  columns <- gedicalval$dictionary$variable[which( gedicalval$dictionary$group %in% c("key","plot","subplot") )]
  columns <- c("project", columns)
  gedicalval$plotdata[columns]
}
extract_gedicalval_gedidata <- function(gedicalval) {
  columns <- gedicalval$dictionary$variable[which( gedicalval$dictionary$group %in% c("key","lidar","gedi") )]
  columns <- c("project", columns[!(columns %in% "subplot")])
  gedicalval$gedidata[columns]
}
extract_gedicalval_metadata <- function(gedicalval) {
  names(gedicalval$metadata) <- tolower(names(gedicalval$metadata))
  gedicalval$metadata
}
extract_gedicalval_dictionary <- function(gedicalval) {
  gedicalval$dictionary
}
extract_gedicalval_allometry <- function(gedicalval) {
  gedicalval$allometry
}
extract_gedicalval_lidar <- function(gedicalval) {
  gedicalval$lidar
}
extract_gedicalval_footprint <- function(gedicalval) {
  gedicalval$footprint
}
