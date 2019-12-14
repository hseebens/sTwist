#########################################################################################
## Merging databases of alien taxon distribution and first records
## standardisation of taxon names using the GBIF backbone taxonomy
##
## sTwist workshop
## Hanno Seebens et al., 11.12.2019
#########################################################################################


OverwriteTaxonNames <- function (FileInfo){

  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardTaxonNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  
  ## overwrite taxonomic information ################################################
  
  new_names <- read.xlsx("Config/UserDefinedTaxonNames.xlsx")
  fullspeclist <- read.table("Output/TaxonNamesFullList.csv",stringsAsFactors = F,header=T)

  ## loop over all data sets 
  for (i in 1:length(inputfiles)){ # loop over inputfiles 
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    missing <- read.table(paste0("Output/MissingTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv"),stringsAsFactors=F)[,1]
    
    if (!any(new_names$Taxon_names_orig%in%dat$Taxon_name_orig)) next # jump to next database if no match found
    
    ## replace taxonomic information for each provided new taxon name 
    for (j in 1:nrow(new_names)){
      
      if (length(unique(dat[dat$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Family))>1 & is.na(new_names$Family[j])){
        cat(paste0("\n Warning: Taxon name '",new_names$Taxon_names_orig[j],"' found for more than one family. Add taxonomic information in UserDefinedTaxonNames.xlsx \n"))
        next
      }
  
      ## overwrite taxononomic information in individual database files
      dat[dat$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Taxon_name <- new_names$New_taxon_name[j]
      if (!is.na(new_names$New_taxon_name_author[j])) dat[dat$Taxon_name==new_names$Taxon_names_orig[j],]$Scientific_name <- new_names$New_taxon_name_author[j]
      
      ## overwrite taxononomic information in full taxon list
      fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Taxon_name <- new_names$New_taxon_name[j]
      fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$GBIFstatus <- ""
      if (!is.na(new_names$New_taxon_name_author[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Scientific_name <- new_names$New_taxon_name_author[j]
      if (!is.na(new_names$Family[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Family <- new_names$Family[j]
      if (!is.na(new_names$Order[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Order <- new_names$Order[j]
      if (!is.na(new_names$Class[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Class <- new_names$Class[j]
      if (!is.na(new_names$Phylum[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Phylum <- new_names$Phylum[j]
      if (!is.na(new_names$Kingdom[j])) fullspeclist[fullspeclist$Taxon_name_orig==new_names$Taxon_names_orig[j],]$Kingdom <- new_names$Kingdom[j]
      
      ## remove taxon name from list of missing taxon names
      if (any(new_names$Taxon_names_orig[j]%in%missing)) missing <- missing[!missing%in%new_names$Taxon_names_orig[j]]
    }
    
    write.table(missing,paste0("Output/MissingTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
    
    write.table(dat,paste0("Output/StandardTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
  }
  
  write.table(fullspeclist,"Output/TaxonNamesFullList.csv",row.names=F)

}