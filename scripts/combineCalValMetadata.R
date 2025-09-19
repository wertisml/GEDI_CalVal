## Combining GEDI CalVal metadata responses into a single file.
library(plyr)
library(data.table)

# Files downloaded from google sheet
googleSheet <- c("GEDI field ALS data form (Responses) - Form Responses 1.csv",
	"GEDI field ALS data form (Responses) - Form Responses 2.csv",
	"GEDI field ALS data form (Responses) - Form Responses 3.csv",
	"GEDI field ALS data form (Responses) - Form Responses4.csv",
	"GEDI field ALS data form (Responses) - Form Responses 4.csv")

## Load individual responses downloaded from Dropbox

# Pull all survey response files from directory
filenames <- list.files("./extra/gedicalval_metadata_responses/", pattern = ".csv")

#Remove "GEDI field ALS data form (Responses) - Form Responses4.csv" - it is duplicated in 
# "gedi_provider_template_Fekety.csv" and "Ruben Valbuena - gedi_provider_template_Valbuena.csv"
filenames <- filenames[filenames != "GEDI field ALS data form (Responses) - Form Responses4.csv"]

#Put individual responses into list
indiv <- lapply(paste("./extra/gedicalval_metadata_responses/", filenames, sep=.Platform$file.sep), 
         read.csv, stringsAsFactors = FALSE, na.strings = c(""," ","NA"), sep = ",")
names(indiv) <- filenames

# Clean up responses from surveys that added columns to the template
names(indiv$gedi_provider_template_Fekety.csv)[names(indiv$gedi_provider_template_Fekety.csv)=='X'] <- 'notes'
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,'Study.ID'] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,'X.1']
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,colnames(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']])!='X.1']
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,colnames(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']])!='X.2']
valbuena_citations <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,colnames(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']])=='X'|
	colnames(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']])=='If.applicable..please.provide.citations.or.URLs.that.describe.your.study.and.or.data.']
valbuena_citations <- valbuena_citations[!is.na(valbuena_citations[,2]),]
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,'If.applicable..please.provide.citations.or.URLs.that.describe.your.study.and.or.data.'] <-
	as.character(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][1 ,'If.applicable..please.provide.citations.or.URLs.that.describe.your.study.and.or.data.'])
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][1 ,'If.applicable..please.provide.citations.or.URLs.that.describe.your.study.and.or.data.'] <- 
	paste(as.character(valbuena_citations[valbuena_citations[,'X']=='25 circular plots' | 
		valbuena_citations[,'X']=='RS data' |
		valbuena_citations[,'X']=='GPS',2]), collapse='; ')
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][1 ,'If.applicable..please.provide.citations.or.URLs.that.describe.your.study.and.or.data.'] <- 
	paste(as.character(valbuena_citations[valbuena_citations[,'X']=='6 rectangular plots' | 
		valbuena_citations[,'X']=='6 rectangular plots + GPS' | 
		valbuena_citations[,'X']=='RS data' |
		valbuena_citations[,'X']=='GPS',2]), collapse='; ')
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][,colnames(indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']])!='X']
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][1:2,]
indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][2,2:3] <- indiv[['Ruben Valbuena - gedi_provider_template_Valbuena.csv']][1,2:3]
indiv[['Copia de GEDI_provider_template_FASA.csv']] <- indiv[['Copia de GEDI_provider_template_FASA.csv']][-1,]


# Convert the survey questions from various versions of the google form
# into a single set of column names
convert_queries <- function(convert.file, response){
	conversion <- read.csv(convert.file, stringsAsFactors = FALSE)

	old_names <- as.data.frame(names(response), stringsAsFactors = FALSE)
	names(old_names) <- "from"
	old_names$from <- iconv(old_names$from, to='ASCII//TRANSLIT')
	new_names <- join(old_names, conversion, by = 'from', type = "left", match = "all")

	return(new_names$to)
}


#new_names <- lapply(indiv, convert_queries, convert.file = "./config/metadataConversion.csv")
mapply(setnames, indiv, new_names)

# Combine all responses into a single dataframe
allResponse <- do.call(rbind.fill, indiv)

# Remove columns for survey template options
allResponse <- allResponse[,colnames(allResponse)!='remove']

# Remove rows that only have a timestamp, email, and institution - no real information to match to a project
allResponse <- allResponse[rowSums(is.na(allResponse[,!(colnames(allResponse)%in%c('submit.timestamp', 'email.submit', 'institution'))])) 
	!= ncol(allResponse[,!(colnames(allResponse)%in%c('submit.timestamp', 'email.submit', 'institution'))]),]

# Remove test entries from JPL people and test response by David Minor
allResponse <- allResponse[allResponse$submit.timestamp != "3/13/2018 9:53:16" | is.na(allResponse$submit.timestamp),]
allResponse <- allResponse[allResponse$submit.timestamp != "3/14/2018 18:23:37" | is.na(allResponse$submit.timestamp),]
allResponse <- allResponse[allResponse$submit.timestamp != "1/22/2019 13:13:15" | is.na(allResponse$submit.timestamp),]

# If the study does not have a name, use the local study area, or institution
allResponse[is.na(allResponse[,'study.id']),'study.id'] <- allResponse[is.na(allResponse[,'study.id']),'study.area']
allResponse[is.na(allResponse[,'study.id']),'study.id'] <- allResponse[is.na(allResponse[,'study.id']),'institution']
allResponse[allResponse[,'study.id']=='Lägern','study.id'] <- 'Laegern'

# Customize study.id for projects that have a duplicate name of another project
allResponse[allResponse[,'name.contact']=='Doreen Boyd'&!is.na(allResponse[,'name.contact']),'study.id'] <- 'Danum Valley Boyd'
allResponse[allResponse[,'name.contact']=='Patrick Fekety' & allResponse[,'study.id']=='ClearCreek'&!is.na(allResponse[,'name.contact']),'study.id'] <- 'ClearCreek Fekety'

# If a contact email was not provided, use the submitter's email
allResponse[is.na(allResponse[,'email.contact']),'email.contact'] <- allResponse[is.na(allResponse[,'email.contact']),'email.submit']

# Add corrections to metadata
key <- read.csv(file = "./config/metadataProjectKey.csv", stringsAsFactors = FALSE)
metadataNew <- read.csv(file = "./config/metadataNew.csv", stringsAsFactors = FALSE)

for (i in 1:nrow(metadataNew)) {
	lookupProject <- key$study.id[key$gedi.project==metadataNew$lookupProject[i]]
	allResponse[allResponse$study.id%in%lookupProject & !is.na(allResponse$study.id), metadataNew$newVariable[i]] <- metadataNew$newValue[i]
}


# Write the responses to a CSV file
write.csv(allResponse, file = "./extra/gedicalval_metadata_allresponses.csv", row.names = FALSE)