#########################################################################################
## Merging databases of alien species distribution and first records
## add taxonomic information to records
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, amphibians + reptiles
##
## sTwist workshop
## Hanno Seebens, 20.03.2019
#########################################################################################


run_addGBIFTax <- function(version) {
  
  # get species data
  all_records <- read.table(paste("Data/AlienSpecies_MultipleDBs_Masterfile_vs",version,".csv",sep=""),sep=";",header=T,stringsAsFactors = F)
  
  # get taxonomy from GBIF for each species name
  all_records_withtax <- addGBIFTax(all_records)
  
  # output
  write.table(all_records_withtax,paste("Data/AlienSpecies_MultipleDBs_Masterfile_vs",version,".csv",sep=""),sep=";",row.names=F)
  
}

