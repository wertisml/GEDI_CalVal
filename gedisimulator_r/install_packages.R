install.packages('renv', quiet=TRUE, repos = 'https://cloud.r-project.org')

renv::init(force = TRUE)
renv::status()

libs = c(
    "XML", 
    "data.table",
    "tidyverse",
    "plyr",
    "geodata",
    "lidR",
    "terra",
    "optparse",
    "jsonlite",
    "BIOMASS",
    "hdf5r",
    "whisker",
    "BiocManager",
    "devtools"
)

missing_packages <- libs[!(libs %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

if(!"rhdf5" %in% installed.packages()[,"Package"]) {
  BiocManager::install("rhdf5")
}

if(!"remake" %in% installed.packages()[,"Package"]) {
  devtools::install_github("richfitz/storr")
  devtools::install_github("richfitz/remake")
}
