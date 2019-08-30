#########################################################################################
## Merging databases of alien species distribution and first records
## standardisation of species names using the GBIF backbone taxonomy
##
## Script requires internet connection.
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
##
## sTwist workshop
## Hanno Seebens et al., 28.08.2019
#########################################################################################


OverwriteSpeciesNames <- function (FileInfo){

  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardSpecNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  
  ## overwrite taxonomic information ################################################
  
  new_names <- read.xlsx("Config/UserDefinedSpeciesNames.xlsx")
  fullspeclist <- read.table("Output/SpeciesNamesFullList.csv",stringsAsFactors = F,header=T)

  ## loop over all data sets 
  for (i in 1:length(inputfiles)){ # loop over inputfiles 
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    missing <- read.table(paste0("Output/MissingSpecNames_",FileInfo[i,"Dataset_brief_name"],".csv"),stringsAsFactors=F)[,1]
    
    if (!any(new_names$Species_names_orig%in%dat$Species_name_orig)) next # jump to next database if no match found
    
    ## replace taxonomic information for each provided new species name 
    for (j in 1:nrow(new_names)){
      
      if (length(unique(dat[dat$Species_name_orig==new_names$Species_names_orig[j],]$Family))>1 & is.na(new_names$Family[j])){
        cat(paste0("\n Warning: Species name '",new_names$Species_names_orig[j],"' found for more than one family. Add taxonomic information in UserDefinedSpeciesNames.xlsx \n"))
        next
      }
  
      ## overwrite taxononomic information in individual database files
      dat[dat$Species_name_orig==new_names$Species_names_orig[j],]$Species_name <- new_names$New_species_name[j]
      if (!is.na(new_names$New_species_name_author[j])) dat[dat$Species_name==new_names$Species_names_orig[j],]$Species_author <- new_names$New_species_name_author[j]
      
      ## overwrite taxononomic information in full species list
      fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Species_name <- new_names$New_species_name[j]
      fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$GBIFstatus <- ""
      if (!is.na(new_names$New_species_name_author[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Speces_author <- new_names$New_species_name_author[j]
      if (!is.na(new_names$Family[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Family <- new_names$Family[j]
      if (!is.na(new_names$Order[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Order <- new_names$Order[j]
      if (!is.na(new_names$Class[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Class <- new_names$Class[j]
      if (!is.na(new_names$Phylum[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Phylum <- new_names$Phylum[j]
      if (!is.na(new_names$Kingdom[j])) fullspeclist[fullspeclist$Species_name_orig==new_names$Species_names_orig[j],]$Kingdom <- new_names$Kingdom[j]
      
      ## remove species name from list of missing species names
      if (any(new_names$Species_names_orig[j]%in%missing)) missing <- missing[!missing%in%new_names$Species_names_orig[j]]
    }
    
    write.table(missing,paste0("Output/MissingSpecNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
    
    write.table(dat,paste0("Output/StandardSpecNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
  }
  
  write.table(fullspeclist,"Output/SpeciesNamesFullList.csv",row.names=F)

}