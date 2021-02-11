
####### SInAS workflow: Integration and standardisation of alien species data ###########
##
## Step 2c: Standardisation of taxon names
##
## Replacing taxon names by user-defined list "UserDefinedTaxonNames.xlsx"
##
## sTwist workshop
## Hanno Seebens, Frankfurt, 10.03.2020
#########################################################################################


OverwriteTaxonNames <- function (FileInfo){

  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files(file.path("Output/","Intermediate"))
  inputfiles_all <- allfiles[grep("Step4_StandardTaxonNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:nrow(FileInfo)){
    # inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
    inputfiles <- c(inputfiles,paste("Step4_StandardTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv",sep=""))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  
  ## overwrite taxonomic information ################################################
  
  new_names <- read.xlsx(file.path("Config","UserDefinedTaxonNames.xlsx"))
  fullspeclist <- read.table(file.path("Output",paste0(outputfilename,"_",version,"_","FullTaxaList.csv")),stringsAsFactors = F,header=T)

  ## loop over all data sets 
  for (i in 1:length(inputfiles)){ # loop over inputfiles 
    
    dat <- read.table(file.path("Output","Intermediate",inputfiles[i]),header=T,stringsAsFactors = F)
    missing <- read.table(file.path("Output","Check",paste0("Missing_Taxa_",FileInfo[i,"Dataset_brief_name"],".csv")),stringsAsFactors=F)[,1]
    
    if (!any(new_names$Taxon_orig%in%dat$Taxon_orig)) next # jump to next database if no match found
    
    ## replace taxonomic information for each provided new taxon name 
    for (j in 1:nrow(new_names)){
      
      # if (length(unique(dat[dat$Taxon_orig==new_names$Taxon_orig[j],]$Family))>1 & is.na(new_names$Family[j])){
      #   cat(paste0("\n Warning: Taxon name '",new_names$Taxon_orig[j],"' found for more than one family. Add taxonomic information in UserDefinedTaxonNames.xlsx \n"))
      #   next
      # }
  
      ## overwrite taxononomic information in individual database files
      dat[dat$Taxon_orig==new_names$Taxon_orig[j],]$Taxon <- new_names$New_Taxon[j]
      if (!is.na(new_names$New_scientificName[j])) dat[dat$Taxon==new_names$Taxon_orig[j],]$scientificName <- new_names$New_scientificName[j]
      
      ## overwrite taxononomic information in full taxon list
      fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$Taxon <- new_names$New_Taxon[j]
      fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$GBIFstatus <- ""
      if (!is.na(new_names$New_scientificName[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$scientificName <- new_names$New_scientificName[j]
      if (!is.na(new_names$family[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$family <- new_names$family[j]
      if (!is.na(new_names$order[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$order <- new_names$order[j]
      if (!is.na(new_names$class[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$class <- new_names$class[j]
      if (!is.na(new_names$phylum[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$phylum <- new_names$phylum[j]
      if (!is.na(new_names$kingdom[j])) fullspeclist[fullspeclist$Taxon_orig==new_names$Taxon_orig[j],]$kingdom <- new_names$kingdom[j]
      
      ## remove taxon name from list of missing taxon names
      if (any(new_names$Taxon_orig[j]%in%missing)) missing <- missing[!missing%in%new_names$Taxon_orig[j]]
    }
    
    write.table(missing,file.path("Output","Check",paste0("Missing_Taxa_",FileInfo[i,"Dataset_brief_name"],".csv")))
    
    write.table(dat,file.path("Output","Intermediate",paste0("Step4_StandardTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv")))
  }
  
  write.table(fullspeclist,file.path("Output",paste0(outputfilename,"_",version,"_","FullTaxaList.csv")),row.names=F)
}