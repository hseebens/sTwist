#########################################################################################
## Merging databases of alien species distribution and first records
## harmonisation of species names using the GBIF backbone taxonomy
##
## Script requires internet connection.
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
##
## sTwist workshop
## Hanno Seebens et al., 28.03.2019
#########################################################################################


HarmoniseSpeciesNames <- function (output){
  
  ################################################################################
  ######## GRIIS #################################################################
  
  # griis_raw <- read.table("Data/DATA-GRIIS-June-2017_Leipzig_trial.csv",header=T,stringsAsFactors = F,sep=";")
  # 
  # griis_all <- griis_raw[,c("country_territory","accepted_species_name","taxon_group","impact_evidence")]
  # colnames(griis_all) <- c("Country","Species","LifeForm","GRIISimpact_evidence")
  
  # griis_raw <- read.table("Data/GRIIS_sTwist_10032019+Belgium.csv",header=T,stringsAsFactors = F,sep=";")
  # griis_raw <- read.table("Data/GRIIS_March_Final.csv",header=T,stringsAsFactors = F,sep=";")
  griis_raw <- read.table("Data/GRIIS_sTwist_March_Final_DatesRes.csv",header=T,stringsAsFactors = F,sep=";")
  
  griis_all <- griis_raw[,c("country","scientificName", 
                            colnames(griis_raw)[grep("HABITAT",colnames(griis_raw))], 
                            colnames(griis_raw)[grep("establishmentMeans",colnames(griis_raw))],
                            colnames(griis_raw)[grep("Invasive",colnames(griis_raw))],
                            colnames(griis_raw)[grep("island_territory",colnames(griis_raw))],
                            colnames(griis_raw)[grep("Resolved",colnames(griis_raw))]),]  #Introduced_before
  # columnames <- c("Country","OrigSpecies","Habitat","Status","GRIISimpact_evidence","GRIISIntro_before","GRIISIntro_after")
  columnames <- c("Country","OrigSpecies","Habitat","Status","GRIISimpact_evidence","Island","GRIISFirstRecord")
  if (dim(griis_all)[2]!=length(columnames)) print("ERROR: Column names of GRIIS not matching!")
  
  colnames(griis_all) <- columnames
  
  griis_all$Region <- griis_all$Country
  griis_all$Region[griis_all$Island!=""] <- griis_all$Island[griis_all$Island!=""]
  
  
  #### check griis names using 'rgibf' GBIF taxonomy ###########
  ## can be commented out to run without harmonisation
  
  cat("\n Working on GRIIS... \n")
  griis_all <- CheckGBIFTax(griis_all)
  
  noGBIFrecords <- cbind(griis_all[is.na(griis_all$SpeciesGBIF),c("OrigSpecies","Region")],DB="GRIIS")  # save original species names if not in GBIF
  
  
  ## output ####################################################
  
  if (output) write.table(griis_all,"Data/GRIIS_GBIFharmonised.csv",sep=";",row.names=F)
  
  
  
  #####################################################################################
  ### First record databse ############################################################
  
  ## load first record database with sub-national regions
  firstrecord_raw <- read.table("Data/GlobalAlienSpeciesFirstRecordDatabase_v1.2_withcountries.csv",stringsAsFactors = F,sep=",",header=T)
  fr_country <- firstrecord_raw[,c("Region","Country","Taxon","LifeForm","FirstRecord","FirstRecord_orig")]
  colnames(fr_country) <-      c("Region","Country","OrigSpecies","LifeForm","FirstRecord","FirstRecord_orig")
  
  continents <- read.table("Data/CountriesRegions.csv",stringsAsFactors = F,sep=";",header=T)
  continents <- unique(continents[,c("Region","Continent","Continent1","Continent2")])
  
  fr_country <- merge(fr_country,continents,by="Region")
  
  #### check first record species names using 'rgibf' GBIF taxonomy ###########
  ## can be commented out to run without harmonisation
  
  cat("\n Working on first record DB... \n")
  fr_country <- CheckGBIFTax(fr_country)
  
  noGBIFrecords <- rbind(noGBIFrecords,cbind(fr_country[is.na(fr_country$SpeciesGBIF),c("OrigSpecies","Region")],DB="FirstRecords"))  # save original species names if not in GBIF
  
  
  
  ## output ####################################
  
  if (output) write.table(fr_country,"Data/FirstRecords_GBIFharmonised.csv",sep=";",row.names=F)
  
  
  ################################################################################
  ## GloNAF ######################################################################
  
  glonaf_species <- read.table("Data/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology.csv",stringsAsFactors = F,sep=";",header=T)
  # glonaf_species <- subset(glonaf_species,status=="naturalized")
  glonaf_species <- glonaf_species[,c("standardized_name","region_id")]
  
  glonaf_regions_all <- read.table("Data/GloNAF_Regions.csv",stringsAsFactors = F,sep=";",header=T)
  glonaf_regions <- glonaf_regions_all[,c("region_id","country","country_ISO","tdwg4_name","tdwg1_name")]
  
  # tab_countrytdwg <- table( unique(glonaf_regions[,c("country","tdwg1_name")])$country)
  # names(tab_countrytdwg)[tab_countrytdwg==2]
  
  glonaf_specreg <- merge(glonaf_species,glonaf_regions,by="region_id")
  colnames(glonaf_specreg) <- c("region_id","OrigSpecies","Country","Country_ISO","Region","TDWG1_name")
  glonaf <- glonaf_specreg[,c("OrigSpecies","Country","Country_ISO","Region","TDWG1_name")]
  
  glonaf <- glonaf[!duplicated(glonaf),] # remove duplicates due to coarser spatial resolution (country wise)
  
  #### check first record species names using 'rgibf' GBIF taxonomy ###########
  ## can be commented out to run without harmonisation
  
  cat("\n Working on GloNAF... \n")
  glonaf <- CheckGBIFTax(glonaf,LifeForm="Vascular plants")
  
  noGBIFrecords <- rbind(noGBIFrecords,cbind(glonaf[is.na(glonaf$SpeciesGBIF),c("OrigSpecies","Region")],DB="GloNAF"))  # save original species names if not in GBIF
  
  
  ## output ####################################
  
  if (output) write.table(glonaf,"Data/GloNAF_GBIFharmonised.csv",sep=";",row.names=F)

  
  
  ################################################################################
  ## Amphibian/reptiles ######################################################################
  
  amphrep <- read.table("Data/AmphibiansReptiles_Capinha-etal2017.csv",header=T,stringsAsFactors = F,sep=";")
  amphrep <- amphrep[, c("Species","Code","Group")]
  colnames(amphrep) <- c("OrigSpecies","Region","Group")
  amphrep <- subset(amphrep,!is.na(amphrep$OrigSpecies))
  
  #### check species names using 'rgibf' GBIF taxonomy ###########
  ## can be commented out to run without harmonisation
  
  cat("\n Working on amphibians and reptiles... \n")
  amphrep <- CheckGBIFTax(amphrep,LifeForm="Herptiles")
  
  noGBIFrecords <- cbind(amphrep[is.na(amphrep$SpeciesGBIF),c("OrigSpecies","Region")],DB="AmphRep")  # save original species names if not in GBIF
  
  
  ## output ####################################################
  
  if (output) write.table(amphrep,"Data/AmphRep_GBIFharmonised.csv",sep=";",row.names=F)

  
  
  ################################################################################
  ## GAVIA ######################################################################
  
  gavia <- read.table("Data/GAVIA_NaturalSpecCountry.csv",header=T,stringsAsFactors = F,sep=";")
  colnames(gavia) <- c("OrigSpecies","Region")
  
  #### check species names using 'rgibf' GBIF taxonomy ###########
  ## can be commented out to run without harmonisation
  
  cat("\n Working on GAVIA... \n")
  gavia <- CheckGBIFTax(gavia,LifeForm="Birds")
  
  noGBIFrecords <- rbind(noGBIFrecords,cbind(gavia[is.na(gavia$SpeciesGBIF),c("OrigSpecies","Region")],DB="GAVIA"))  # save original species names if not in GBIF
  
  
  
  ## output ####################################################
  
  if (output) write.table(gavia,"Data/GAVIA_GBIFharmonised.csv",sep=";",row.names=F)
  
  write.table(unique(noGBIFrecords),"Data/SpeciesWithoutGBIFrecord.csv",sep=";",row.names=F)
}
