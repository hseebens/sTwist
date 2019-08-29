#########################################################################################
## Merging databases of alien species distribution and first records
##
## 1. step: load databases and harmonise species names 
## 2. step: harmonising of country names 
## 3. step: merging all databases
## 4. step: add GBIF taxonomic information (family, class etc.)
## 5. step: add GBIF records (number of records per region/country)
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
## new databases have to be implemented in the following scripts:
## HarmoniseSpeciesNames.r, HarmoniseCountryNames.r, MergeDatabases.r
##
## Input: 
## 1. Species database files stored in sub-folder named 'Data'.
## 2. Shapefile providing region boundaries (only required for step 5).
##
## Output: A final masterfile containing all records from all databases.
## The masterfile is called AlienSpecies_MultipleDBs_Masterfile.csv and contains a 
## column for each database with an 'x' indicating that this record was in the respective
## database.
## Output will be stored in a sub-folder named 'Data'.
## Data sets for each intermediate step will be stored as well.
##
## sTwist workshop
## Hanno Seebens et al., 07.08.2019
#########################################################################################

graphics.off()
rm(list=ls())

library(rgbif) # for checking names, records and taxonomy
library(openxlsx)

## set working directory; requires a sub-folder named "Data" including all relevant data files
setwd("/home/hanno/Bioinvasion/IndicatorAliens/Workflow") # set working directory
# setwd("/scratch/home/hseebens/Bioinvasion/IndicatorAliens") # working directory

version <- "2.0" # which version of the database are you going to produce?

output <- T # shall intermediate results be stored to disk? (may overwrite existing files!)


################################################################################
### load other functions #######################################################
source("R/PrepareDatasets.r") # preparing example data sets as input files
source("R/StandardiseSpeciesNames.r") # harmonising species names, requires GBIF connection, takes some time...
source("R/StandardiseCountryNames.r") # harmonising country names
source("R/MergeDatabases.r") # combine data sets
source("R/CheckGBIFTax.r") #function to check species names using GBIF taxonomy
################################################################################


################################################################################
######## Load data set table ###################################################

FileInfo <- read.xlsx("Config/DatabaseInfo.xlsx",sheet=1)


## load databases, extract required information and harmonise species names...
cat("\n 1 Preparation of example data sets \n")
PrepareDatasets(FileInfo)

## load databases, extract required information and harmonise species names...
cat("\n 2 Harmonisation of species names \n")
StandardiseSpeciesNames(FileInfo)

## harmonise species names...
cat("\n 3 Harmonisation of country names \n")
StandardiseCountryNames(FileInfo)

## merge databases...
cat("\n 4 Merging databases \n")
MergeDatabases(FileInfo,version,output)
