get_function_from_source <- function(name, filename, default=identity) {
  e <- new.env()
  if (file.exists(filename)) {
    source(filename, local=e)
  }
  if (exists(name, envir=e)) {
    get(name, mode="function", envir=e)
  } else {
    default
  }
}

rename_columns <- function(obj, from, to) {
  names(obj)[match(from, names(obj))] <- to
  obj
}

read_csv <- function(...) {
  read.csv(..., stringsAsFactors=FALSE, check.names=FALSE,
           strip.white=TRUE)
}

last <- function(x) {
  x[[length(x)]]
}

capitalize <- function (string) {
  capped <- grep("^[^A-Z]*$", string, perl = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  string
}

write_csv <- function(data, filename) {
  write.csv(data, filename, row.names=FALSE)
}

# Paste together list of var_names and their values, used for
# aggregating varnames into 'grouping' variable NOTE: Used in
# dataManipulate.R
makeGroups <- function(data, var_names) {
  apply(cbind(data[, var_names]), 1, function(x)
        ## jsonlite::toJSON(as.list(x), auto_unbox=TRUE)
        paste(var_names, "=", x, collapse = "; "))
}

## Check species name and retrieve family
lookup_taxon <- function(species) {
  
  results <- data.frame(species=species, family=NA, new.species=NA)
  txz <- gnr_resolve(species, stripauthority=TRUE, http='post', best_match_only=TRUE, fields="all")
  
  class.path <- strsplit(txz$classification_path, "|", fixed=TRUE)
  
  class.rank <- strsplit(txz$classification_path_ranks, "|", fixed=TRUE)
  class.family <- mapply(function(x,y) x[which(y == "family")], x=class.path, y=class.rank)
  txz$family <- unlist( lapply(class.family, function(x) ifelse(length(x) == 1L, x[[1]], NA)) )
  txz <- join(data.frame(user_supplied_name=species), txz, by=c("user_supplied_name"), 
              type="left", match="all")
  
  family.matched <- !is.na(txz$family)
  results$family[family.matched] <- txz$family[family.matched]
  
  species.matched <- txz$score > 0.9 & nzchar(txz$matched_name) > 0 & !is.na(txz$matched_name)
  results$new.species[species.matched] <- txz$matched_name[species.matched]
  
  ii <- is.na(results$family)
  genus <- unlist( lapply(strsplit(txz$matched_name, " "), function(xx) xx[1]) )
  genus.results <- tax_name(query=genus[ii], get="family", db="ncbi", verbose=FALSE, 
                            http="post", ask=FALSE)
  results$family[ii] <- genus.results$family
  
  ii <- !is.na(results$family) & !species.matched
  results$new.species[ii] <- paste(genus[ii], "sp.")
  
  results
}

## Cache queried taxon data for a project
## Only returns unique records
read_taxon_data <- function(cachefile, raw, species.col) {
  
  if ( file.exists(cachefile) ) {
    taxon.data <- read_csv(cachefile)
    
    taxon.exists <- raw[,species.col] %in% taxon.data$species
    if ( any(!taxon.exists & !is.na(raw[,species.col])) ) {
      
      ii <- which( !taxon.exists & !is.na(raw[,species.col]) & !duplicated(raw[,species.col]) )
      taxon.data.tmp <- lookup_taxon(raw[ii,species.col])
      taxon.data <- rbind(taxon.data,taxon.data.tmp)
      write_csv(taxon.data, cachefile)
      
    }
    
  } else {
    
    ii <- !duplicated( raw[,species.col] ) & !is.na(raw[,species.col])
    taxon.data <- lookup_taxon(raw[ii,species.col])
    write_csv(taxon.data, cachefile)
    
  }
  
  colnames(taxon.data) <- c(species.col,"family","new.species")
  join(raw, taxon.data, by=c(species.col), type="left", match="all")
}

## Cache taxon data queried using the BIOMASS package for a project
## Only returns unique records
read_taxon_data_biomass <- function(cachefile, raw) {
  
  if ( file.exists(cachefile) ) {
    taxon.data <- read_csv(cachefile)
    
    taxon.exists <- raw[,"species"] %in% taxon.data$species
    if ( any(!taxon.exists & !is.na(raw[,"species"])) ) {
      
      ii <- which( !taxon.exists & !is.na(raw[,"species"]) & !duplicated(raw[,"species"]) )
      
      species <- unlist( lapply(raw$species[ii], function(x) strsplit(x," ")[[1]][2]) )
      genus <- unlist( lapply(raw$species[ii], function(x) strsplit(x," ")[[1]][1]) )
      invisible(capture.output(taxo <- BIOMASS::correctTaxo(genus=genus, species=species) ))
      invisible(capture.output(apg <- BIOMASS::getTaxonomy(taxo$genusCorrected, findOrder=FALSE) ))
      
      taxon.data.tmp <- data.frame(species=raw$species[ii], new.family=unlist(apg$family),
                               new.genus=unlist(taxo$genusCorrected), new.species=unlist(taxo$speciesCorrected))
      taxon.data <- rbind(taxon.data, taxon.data.tmp)
      
      write_csv(taxon.data, cachefile)
      
    }
    
  } else {
    
    ii <- !duplicated( raw[,"species"] ) & !is.na(raw[,"species"])
    species <- unlist( lapply(raw$species[ii], function(x) strsplit(x," ")[[1]][2]) )
    genus <- unlist( lapply(raw$species[ii], function(x) strsplit(x," ")[[1]][1]) )
    
    invisible(capture.output(taxo <- BIOMASS::correctTaxo(genus=genus, species=species) ))
    invisible(capture.output(apg <- BIOMASS::getTaxonomy(taxo$genusCorrected, findOrder=FALSE) ))
    taxon.data <- data.frame(species=raw$species[ii], new.family=unlist(apg$family),
                             new.genus=unlist(taxo$genusCorrected), new.species=unlist(taxo$speciesCorrected))
    write_csv(taxon.data, cachefile)
    
  }
  
  newdata <- plyr::join(raw, taxon.data, by=c("species"), type="left", match="all")
  raw$species <- sprintf("%s %s", newdata$new.genus, newdata$new.species)
  raw$family <- newdata$new.family
  
  raw
}

# Read metadata xml file and return list with two data frames:
# GEDI compiled metadata and data provider submitted metadata.
metadata_xml_to_dataframe <- function(file){

  doc <- xml2::read_xml(file)
  
 
  
  gedi.metadata <- as.data.frame(t(matrix(xml2::xml_text(xml2::xml_children(xml2::xml_children(doc))[1:9]))))
  colnames(gedi.metadata) <- xml2::xml_name(xml2::xml_children(xml2::xml_children(doc))[1:9])
  
  submitted.key <- as.data.frame(t(matrix(xml2::xml_text(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/key"))))))
  colnames(submitted.key) <- xml2::xml_name(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/key")))
  
  submitted.field <- as.data.frame(t(matrix(xml2::xml_text(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/field"))))))
  colnames(submitted.field) <- xml2::xml_name(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/field")))
  
  submitted.lidar <- as.data.frame(t(matrix(xml2::xml_text(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/lidar"))))))
  colnames(submitted.lidar) <- xml2::xml_name(xml2::xml_children(xml2::xml_child(doc, search="submitted.metadata/lidar")))
  
  submitted.metadata <- cbind(submitted.key, submitted.field, submitted.lidar)
  

#	doc <- xmlParse(file)

#	gedi.metadata <- XML::xmlToDataFrame(doc, nodes=getNodeSet(doc, '//project//gedi.metadata'), stringsAsFactors = FALSE)
	
#	submitted.metadata <- cbind(XML::xmlToDataFrame(doc, nodes=getNodeSet(doc, '//project//submitted.metadata//key'), stringsAsFactors = FALSE),
#		XML::xmlToDataFrame(doc, nodes=getNodeSet(doc, '//project//submitted.metadata//field'), stringsAsFactors = FALSE),
#		XML::xmlToDataFrame(doc, nodes=getNodeSet(doc, '//project//submitted.metadata//lidar'), stringsAsFactors = FALSE))
	
	gedi.metadata[gedi.metadata=='NA'] <- NA	
	
	
	if(ncol(submitted.metadata)>0){
		submitted.metadata[submitted.metadata=='NA'] <- NA
		return(list(gedi.metadata = gedi.metadata, submitted.metadata = submitted.metadata))
	} else{
		return(list(gedi.metadata = gedi.metadata))
	}
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
