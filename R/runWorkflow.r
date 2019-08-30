#########################################################################################
## Merging databases of alien species distribution and first records
##
## 1. step: prepare column names of secies databases  
## 2. step: standardisation of species names using GBIF backbone taxonomy; user-defined
##          species names and taxonomic information can be added afterwards
## 3. step: standardisation of country names
## 4. step: standardisation of first records
## 5. step: merging all databases
##
## Input: 
## Information about databases has to provided in DatabaseInfo.xlsx.
## Modification of country names, species names and rules to treat first records can be
## done in UserDefinedSpeciesNames.xlsx, AllRegionsList.xlsx, SubspecIdentifier.xlsx 
## and Guidelines_FirstRecords.xlsx
## Note that only the first sheet of the Excel file is read in. Others are ignored.
##
## Output: 
## A standardised masterfile built from all databases.
## The masterfile is called AlienSpecies_MultipleDBs_Masterfile_[version].csv and contains a 
## column for each database with an 'x' indicating the respective database.
## Intermediate output files provide information about species names not resoled at
## GBIF, a full list of species names with taxonomic information, missing country names,
## list of original and new country names and a list of unclear first records.
## Additional output from each step can be stored if 'Output' is set to TRUE. This is 
## required if step are run individually.
##
## More details provided in manual.
##
## sTwist workshop
## Hanno Seebens, 30.08.2019
#########################################################################################

graphics.off()
rm(list=ls())

library(rgbif) # for checking names, records and taxonomy; note: usage of rgbif may cause warnings like "Unknown or uninitalised column: " which is a bug. Can be ignored.
library(openxlsx)

## set working directory; requires a sub-folder named "Data" including all relevant data files
setwd("/home/hanno/Bioinvasion/IndicatorAliens/Workflow") # set working directory

version <- "2.0" # which version of the database are you going to produce?

output <- T # shall intermediate results be stored to disk? (may overwrite existing files!)


################################################################################
### load other functions #######################################################
source("R/PrepareDatasets.r") # preparing example data sets as input files
source("R/StandardiseSpeciesNames.r") # standardising species names, requires GBIF connection, takes some time...
source("R/OverwriteSpeciesNames.r") # replace species names with user-defined ones
source("R/StandardiseCountryNames.r") # standardising country names
source("R/GetFirstRecord.r") # standardising country names
source("R/MergeDatabases.r") # combine data sets
source("R/CheckGBIFTax.r") #function to check species names using GBIF taxonomy
################################################################################


################################################################################
######## Load data set table ###################################################

FileInfo <- read.xlsx("Config/DatabaseInfo.xlsx",sheet=1)
if (nrow(FileInfo)==0) stop("No database information provided. Add information to Config/DatabaseInfo.xlsx.")

## load databases, extract required information and harmonise species names...
cat("\n1 Preparation of provided data sets \n")
PrepareDatasets(FileInfo)

## load databases, extract required information and harmonise species names...
cat("\n2 Standardisation of species names \n")
StandardiseSpeciesNames(FileInfo)
OverwriteSpeciesNames(FileInfo) # user-defined species names

## harmonise species names...
cat("\n3 Standardisation of country names \n")
StandardiseCountryNames(FileInfo)

## standardise first records....
cat("\n4 Standardisation of first records \n")
GetFirstRecord(FileInfo)

## merge databases...
cat("\n5 Merging databases \n")
MergeDatabases(FileInfo,version,output)
