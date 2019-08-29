#########################################################################################
## Merging databases of alien species distribution and first records
## standardisation of species names using the GBIF backbone taxonomy
##
## Script requires internet connection.
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
##
## sTwist workshop
## Hanno Seebens et al., 06.08.2019
#########################################################################################


StandardiseSpeciesNames <- function (FileInfo){

  ## identify input datasets based on file name "StandardColumns_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardColumns_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  

  ## loop over all data sets ################################################
  fullspeclist <- vector()
  for (i in 1:length(inputfiles)){ # loop over inputfiles 
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    dat$Species_name <- dat$Species_name_orig
    
    # remove subspecies etc #######################################
    dat$Species_name <- gsub("  "," ",dat$Species_name)
    dat$Species_name <- gsub("^\\s+|\\s+$", "",dat$Species_name) # trim leading and trailing whitespace
    dat$Species_name <- gsub("[$,\xc2\xa0]", " ",dat$Species_name) # replace weird white space with recognised white space
    
    # loop over provided list of keywords to identify sub-species level information
    subspIdent <- read.xlsx("Inputfiles/SubspecIdentifier.xlsx",colNames=F)[,1]
    subspIdent <- gsub("\\.","",subspIdent)
    subspIdent <- c(subspIdent,paste0(subspIdent,"\\."))
    subspIdent <- paste0(paste("",subspIdent,""),".*$")
    for (j in 1:length(subspIdent)){
      ind <- grep(subspIdent[j],dat$Species_name)
      dat$Species_name <- gsub(subspIdent[j],"",dat$Species_name)
    }

    #### check griis names using 'rgibf' GBIF taxonomy ###########
    ## can be commented out to run without standardisation
    
    cat(paste0("\n Working on ",FileInfo[i,"Dataset_brief_name"],"... \n"))
    dat <- CheckGBIFTax(dat)

    ## output #####################################################
    
    DB <- dat[[1]]
    mismatches <- dat[[2]]
    
    ## export full species list with original species names and names assigned by GBIF for checking
    fullspeclist <- rbind(fullspeclist,unique(DB[,c("Species_name_orig","Species_name","GBIFSpeciesAuthor","GBIFstatus","Family","Order","Class","Phylum","Kingdom")]))
    
    DB$GBIFstatus[is.na(DB$GBIFstatus)] <- "NoMatch"
    DB <- DB[,!colnames(DB)%in%c("GBIFSpeciesAuthor","GBIFstatus","Family","Order","Class","Phylum","Kingdom")]
    
    write.table(mismatches,paste0("Output/MissingSpecNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
    
    write.table(DB,paste0("Output/StandardSpecNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
  }
  
  oo <- order(fullspeclist$Kingdom,fullspeclist$Phylum,fullspeclist$Class,fullspeclist$Species_name)
  fullspeclist <- fullspeclist[oo,]
  
  write.table(unique(fullspeclist),"Output/SpeciesNamesFullList.csv")
}
