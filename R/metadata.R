read_data_raw_import_options <- function(filename) {
  tmp <- read_csv(filename, header=FALSE, row.names=1)
  opts <- structure(as.list(tmp[[1]]), names=rownames(tmp))
  
  import <- modifyList(list(na.strings="NA",
                            separator=","), opts)

  ## Then some processing:
  import$header <- as.logical(import$header)
  import$skip <- as.integer(import$skip)
  import$na.strings <- union("NA", import$na.strings)

  import  
}

read_match_columns <- function(filename) {
  ret <- read_csv(filename, na.strings = c("NA", ""))
  ret[!is.na(ret$var_out), ]
}

column_info <- function(variable_definitions) {
  ret <- as.list(variable_definitions[c("variable", "type", "units", "group")])
  names(ret$type) <- names(ret$units) <- names(ret$group) <- ret$variable
  ret
}
