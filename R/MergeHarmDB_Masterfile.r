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
## Hanno Seebens et al., 28.03.2019
#########################################################################################

graphics.off()
rm(list=ls())

library(data.table) # calculate first records after combining regions efficiently
library(rgbif) # for checking names, records and taxonomy
# library(rgdal) # required to count GBIF records per country in function getGBIFrecords.r

## set working directory; requires a sub-folder named "Data" including all relevant data files
setwd("/home/hanno/Bioinvasion/IndicatorAliens") # set working directory
# setwd("/scratch/home/hseebens/Bioinvasion/IndicatorAliens") # working directory

version <- "1.3" # which version of the database are you going to produce?

output <- T # shall intermediate results be stored to disk? (may overwrite existing files!)
## Note that required data sets are stored to disk anyway.

# ## Regions/countries to aggregate GBIF records (only required for the extraction of GBIF records using getGBIFrecords.r)
# RegionShapeObj <- "RegionsShapefile_OGR_small"
# ## This should be the name of a shapefile providing information about the    ##
# ## boundaries of regions. File will be loaded as a SpatialPolygonDataFrame.  ##


#### function to check species names using GBIF taxonomy #######################
source("CheckGBIFTax.r")
## The function expects a data.frame containing a column with                 ##
## species names called 'Species' and returns the same data.frame             ##
## with an additional column 'SpeciesGBIF' containing the GIBF                ##
## names. NAs indicate no match with GBIF.                                    ##

## get GBIF taxonomy 
## function adds columns to data.frame with information of family, order, class, phylum
source("addGBIFTax.r")

################################################################################

### load other functions #######################################################
## Each function writes the final data set to disk, which is then loaded in   ##
## the next function.
source("HarmoniseSpeciesNames.r") # harmonising species names, requires GBIF connection, takes some time...
source("HarmoniseCountryNames.r") # harmonising country names
source("MergeDatabases.r") # combine data sets
source("run_addGBIFTax.r") # get GBIF taxonomy, requires GBIF connection, takes some time...
# source("getGBIFrecords.r") # get GBIF number of records, requires GBIF connection, takes some days (!!!)
################################################################################


## load databases, extract required information and harmonise species names...
cat("\n 1 Harmonisation of species names \n")
HarmoniseSpeciesNames(output)

## harmonise species names...
cat("\n 2 Harmonisation of country names \n")
HarmoniseCountryNames(output)

## merge databases...
cat("\n 3 Merging databases \n")
MergeDatabases(output,version)

## add GBIF taxonomy (familiy, class, order, phylum)
cat("\n 4 Add GBIF taxonomy \n")
run_addGBIFTax(version) # may take hours !!!

# ## add GBIF records (number of records per species and country)
# cat("\n 5 Add GBIF records \n")
# getGBIFrecords(RegionShapeObj) # may take days !!!
