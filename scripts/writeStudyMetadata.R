library(XML)


metadata <- read.csv(file = "./extra/gedicalval_metadata_allresponses.csv", stringsAsFactors = FALSE)
metadata <- as.data.frame(apply(metadata, 2, iconv, from="UTF-8", to="UTF-8",sub=''))
definitions <- read.csv(file = "./config/metadataDefinitions.csv", stringsAsFactors = FALSE)
key <- read.csv(file = "./config/metadataProjectKey.csv", stringsAsFactors = FALSE)

write_metadata_xml <- function(project){
	project_metadata <- read.csv(file = paste("./data", project, "studyMetadata.csv", sep=.Platform$file.sep), stringsAsFactors = FALSE)
	project_metadata$Item <- gsub(pattern = " ", replacement = ".", project_metadata$Item)
	project_metadata$Item <- tolower(project_metadata$Item)
	project_metadata <- as.data.frame(apply(project_metadata, 2, iconv, from="UTF-8", to="UTF-8",sub=''), stringsAsFactors = FALSE)

	ii <- as.character(metadata$study.id) %in% as.character(key$study.id[key$gedi.project == project])

	xml <- suppressWarnings(xmlTree("project", attrs = c(ID = as.character(project))))

	xml$addNode("gedi.metadata", close = FALSE)
		for(j in 1:nrow(project_metadata)) {
			xml$addNode(project_metadata$Item[j], project_metadata$Value[j])
		}
	xml$closeNode()

	if( any(ii)){
		for(i in which(ii)){
		xml$addNode("submitted.metadata", metadata[i, 'study.id'], close = FALSE)
			xml$addNode("key", close = FALSE)
				for (j in definitions$variable[definitions$group=="key"]) {
					xml$addNode(j, metadata[i, j])
				}
			xml$closeNode()
	
			xml$addNode("field", close = FALSE)
				for (j in definitions$variable[definitions$group=="field"]) {
					xml$addNode(j, metadata[i, j])
				}
			xml$closeNode()
	
			xml$addNode("lidar", close = FALSE)
				for (j in definitions$variable[definitions$group=="lidar"]) {
					xml$addNode(j, metadata[i, j])
				}
			xml$closeNode()
		xml$closeNode()
		}
	}

saveXML(xml$doc(), paste("./data", project, "studyMetadata.xml", sep=.Platform$file.sep), encoding = "UTF-8")
}

## Below to be adapted to run xml writing above
args <- commandArgs(TRUE)
if ( length(args) > 0 ) {
	valid.study_names <- args %in% dir("data")
	if ( !all(valid.study_names) ) {
		stop("Invalid project names provided")
	}
	sapply(args, write_metadata_xml)
} else {
	sapply(dir("data"), write_metadata_xml)
}