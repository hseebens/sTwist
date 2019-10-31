#!/usr/bin/env Rscript

#########################################################################################
## Merging databases of alien species distribution and first records
## Hanno Seebens, 31.10.2019
#########################################################################################

graphics.off()
rm(list=ls())

## required libraries
library(rgbif) # for checking names, records and taxonomy; note: usage of rgbif may cause warnings like "Unknown or uninitalised column: " which is a bug. Can be ignored.
library(openxlsx)


## option for storing the intermediate and final output
outputfilename <- "AlienSpecies_MultipleDBs_Masterfile_vs" # name of final output file

version <- "2.2" # which version of the database are you going to produce? this will be attached to the end of 'outputfilename'

output <- T # shall intermediate results be stored to disk? (may overwrite existing files!)


################################################################################
### load other functions #######################################################
source(file.path("R","PrepareDatasets.r")) # preparing example data sets as input files
source(file.path("R","StandardiseSpeciesNames.r")) # standardising species names, requires GBIF connection, takes some time...
source(file.path("R","OverwriteSpeciesNames.r")) # replace species names with user-defined ones
source(file.path("R","StandardiseCountryNames.r")) # standardising country names
source(file.path("R","GetFirstRecord.r")) # standardising country names
source(file.path("R","MergeDatabases.r")) # combine data sets
source(file.path("R","CheckGBIFTax.r")) #function to check species names using GBIF taxonomy
################################################################################


################################################################################
######## Load data set table ###################################################

FileInfo <- read.xlsx(file.path("Config","DatabaseInfo.xlsx"),sheet=1)
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
MergeDatabases(FileInfo,version,outputfilename,output)

