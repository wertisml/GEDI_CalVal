library(knitr)
library(tools)
library(terra)
library(tinytex)

#args <- commandArgs(trailingOnly = TRUE)
#if (length(args) < 1) {
#  stop("Usage GEDI_calval_project_report.r project")
#}

#reportproject <- args[1]
#print(reportproject)
#knit(input = "scripts/GEDI_calval_project_report.rnw", output = "reports/GEDI_calval_project_report.tex")
#knit2html(input = "./scripts/GEDI_calval_project_report.rnw", output = "./reports/GEDI_calval_project_report.html")
#pandoc(input = "./reports/GEDI_calval_project_report.tex", format = "html", config='configAsHTML.txt')


Create_Report <- function(Project){
  source("R/support.R")
  reportproject <- Project
  knit(input = "scripts/GEDI_calval_project_report.rnw", output = paste0("reports/GEDI_CalVal_Project_Report_",reportproject,".tex"))
  #texi2dvi("reports/GEDI_calval_project_report.tex", pdf = TRUE)
}

#Create_Report("australia_ausplotsforests")


