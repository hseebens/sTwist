#########################################################################################
## Merging databases of alien species distribution and first records
##
## Step 2: Standardisation of species names using the GBIF backbone taxonomy
##
## Species names are standardised according to the GBIF backbone taxonomy. The protocol 
## to access GBIF and treat results is implemented in CheckGBIFTax.r.
## Script requires internet connection.
##
## sTwist workshop
## Hanno Seebens et al., 10.12.2019
#########################################################################################


StandardiseSpeciesNames <- function (FileInfo){

  ## identify input datasets based on file name "StandardColumns_....csv"
  allfiles <- list.files("Output")
  inputfiles_all <- allfiles[grep("StandardColumns_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  

  ## loop over all data sets ################################################
  fullspeclist <- vector()
  for (i in 1:length(inputfiles)){ # loop over inputfiles 
    
    dat <- read.table(file.path("Output",paste0(inputfiles[i])),header=T,stringsAsFactors = F)
    
  #   print(inputfiles[i])
  #   print(dim(dat))
  #   print(length(unique(dat$Species_name_orig)))
  #   print(length(unique(dat$Region_name_orig)))
  # }
    
    dat <- dat[!is.na(dat$Taxon_name),]
    dat <- dat[dat$Taxon_name!="",]
    dat$Taxon_name <- dat$Taxon_name_orig
    
    # remove subspecies etc #######################################
    dat$Taxon_name <- gsub("  "," ",dat$Taxon_name)
    dat$Taxon_name <- gsub("^\\s+|\\s+$", "",dat$Taxon_name) # trim leading and trailing whitespace
    dat$Taxon_name <- gsub("[$\xc2\xa0]", " ",dat$Taxon_name) # replace weird white space with recognised white space
    dat$Taxon_name <- gsub("  "," ",dat$Taxon_name)
    
    # loop over provided list of keywords to identify sub-species level information
    # subspIdent <- read.xlsx("Config/SubspecIdentifier.xlsx",colNames=F)[,1]
    # subspIdent <- gsub("\\.","",subspIdent)
    # subspIdent <- c(subspIdent,paste0(subspIdent,"\\."))
    # subspIdent <- paste0(paste("",subspIdent,""),".*$")
    # for (j in 1:length(subspIdent)){
    #   ind <- grep(subspIdent[j],dat$Species_name)
    #   dat$Species_name <- gsub(subspIdent[j],"",dat$Species_name)
    # }

    #### check griis names using 'rgibf' GBIF taxonomy ###########
    ## can be commented out to run without standardisation
    
    cat(paste0("\n Working on ",FileInfo[i,"Dataset_brief_name"],"... \n"))
    dat <- CheckGBIFTax(dat)

    ## output #####################################################
    
    DB <- dat[[1]]
    mismatches <- dat[[2]]
    
    ## export full species list with original species names and names assigned by GBIF for checking
    fullspeclist <- rbind(fullspeclist,unique(DB[,c("Taxon_name_orig","Taxon_name","Scientific_name","GBIFstatus","GBIFmatchtype","GBIFrank","GBIFnote","Species","Genus","Family","Order","Class","Phylum","Kingdom")]))
    
    DB <- unique(DB) # remove duplicates
    DB$GBIFstatus[is.na(DB$GBIFstatus)] <- "NoMatch"
    DB <- DB[,!colnames(DB)%in%c("GBIFstatus","GBIFmatchtype","GBIFrank","GBIFnote","Species","Genus","Family","Order","Class","Phylum","Kingdom")]
    
    write.table(DB,file.path("Output",paste0("StandardTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv")))
    
    oo <- order(mismatches$Taxon_name)
    mismatches <- unique(mismatches[oo,])
    
    write.table(mismatches,file.path("Output",paste0("MissingTaxonNames_",FileInfo[i,"Dataset_brief_name"],".csv")),row.names=F,col.names=F)
  }
  
  oo <- order(fullspeclist$Kingdom,fullspeclist$Phylum,fullspeclist$Class,fullspeclist$Taxon_name)
  fullspeclist <- fullspeclist[oo,]
  
  write.table(unique(fullspeclist),file.path("Output","TaxonNamesFullList.csv"),row.names=F)
}
