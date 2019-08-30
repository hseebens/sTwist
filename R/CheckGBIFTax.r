#### check and replace species names using 'rgibf' GBIF taxonomy ###########
## requires a data.frame with a column 'Species' containing species names ##
##
## sTwist workshop
## Hanno Seebens, 06.08.2019
############################################################################

CheckGBIFTax <- function(dat){
  
  dat$Species_author <- NA
  dat$GBIFstatus <- NA
  dat$Family <- NA
  dat$Class <- NA
  dat$Order <- NA
  dat$Phylum <- NA
  dat$Kingdom <- NA
  
  if (any(colnames(dat)=="Kingdom")){
    speclist_lifeform <- unique(dat[,c("Species_name","Kingdom")])
    speclist <- speclist_lifeform$Species_name
  } else if (any(colnames(dat)=="Author")){
    speclist <- unique(paste(dat$Species_name,dat$Author))
  } else {
    speclist <- unique(dat$Species_name)
  }
  n_species <- length(speclist)
  
  #setup progress bar
  pb <- txtProgressBar(min=0, max=n_species, initial=0,style = 3)
  
  options(warn=-1) # the use of 'tibbles' data frame generates warnings as a bug; if solved this options() should be turned off
  
  mismatches <- vector()
  for (j in 1:n_species){# loop over all species names; takes some hours...
    
    # select species name and download taxonomy
    ind_spec <- which(dat$Species_name_orig==speclist[j])
    db_all <- name_backbone(speclist[j],verbose=T) # check for names and synonyms
    db <- db_all[["data"]]
    alternatives <- db_all$alternatives
    
    # check if any data from GBIF taxonomy exist
    # if (db$matchType!="EXACT" & all(alternatives$matchType!="EXACT")) next # next species if no information is available
    
    # select only accepted names and exact matches
    if (any(db$status=="ACCEPTED" & db$matchType=="EXACT") & (any(colnames(db)=="species") | any(colnames(db)=="canonicalName"))){ 
      if (any(colnames(db)=="species")) dat$Species_name[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$species[1]
      if (any(colnames(db)=="canonicalName")) dat$Species_name[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$canonicalName[1] # species name is sometimes missing (e.g. Anemone sylvestris); option only possible for exact matches
      dat$Species_author[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$scientificName[1]
      dat$GBIFstatus[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$status[1]
      try(dat$Family[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$family[1],silent=T)
      try(dat$Class[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$class[1],silent=T)
      try(dat$Order[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$order[1],silent=T)
      try(dat$Phylum[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$phylum[1],silent=T)
      try(dat$Kingdom[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$kingdom[1],silent=T)
      next
      
      # if name matches are not exact, select name with close match
    } else if (any(db$status=="SYNONYM" & db$matchType=="EXACT") & any(colnames(db)=="species")) { # select synonyms
      dat$Species_name[ind_spec] <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$species[1]
      dat$GBIFstatus[ind_spec] <- db[db$status=="SYNONYM" & db$matchType=="EXACT",]$status[1]
      
      ## try to get author name of synonym (not provided in 'db')      
      db_all_2 <- name_backbone(dat$Species_name[ind_spec][1],verbose=T)
      db_2 <- db_all_2[["data"]]
      if (db_2$matchType=="EXACT"){
        dat$Species_author[ind_spec] <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$scientificName[1]
        try(dat$Family[ind_spec] <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family[1],silent=T)
        try(dat$Class[ind_spec]  <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$class[1],silent=T)
        try(dat$Order[ind_spec]  <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$order[1],silent=T)
        try(dat$Phylum[ind_spec] <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$phylum[1],silent=T)
        try(dat$Kingdom[ind_spec]<- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$kingdom[1],silent=T)
      } else if (dim(db_all_2$alternatives)[1]>0){
        alternatives_2 <- db_all_2$alternatives
        dat$Species_author[ind_spec] <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family==db$family[1],]$scientificName[1]
        try(dat$Family[ind_spec]            <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT",]$family[1],silent=T)
        try(dat$Class[ind_spec]             <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT",]$class[1],silent=T)
        try(dat$Order[ind_spec]             <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT",]$order[1],silent=T)
        try(dat$Phylum[ind_spec]            <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT",]$phylum[1],silent=T)
        try(dat$Kingdom[ind_spec]           <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT",]$kingdom[1],silent=T)
      }
      next
    }

    ## check for alternative names because of e.g. multiple entries for different taxonomic groups in GBIF...
    if (any(alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT") & any(colnames(alternatives)=="species")){
      dat$Species_name[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]

      ## check information of kingdom and selected respective author
      if (any(colnames(dat)=="Kingdom")) {
        if (length(unique(alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$family))>1) print(paste(speclist[j],"name occurrs in more than one family"))
        dat$Species_author[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$scientificName[1]
        dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$status[1]
        try(dat$Family[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$family[1],silent=T)
        try(dat$Class[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$class[1],silent=T)
        try(dat$Order[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$order[1],silent=T)
        try(dat$Phylum[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$phylum[1],silent=T)
        try(dat$Kingdom[ind_spec]           <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom==speclist_lifeform[j,2],]$kingdom[1],silent=T)
        next
      }
        
      ## select entries from cross-taxonomic databases from certain taxa
      if (unique(dat$Taxon_group)!="All"){
        if (grepl("Vascular plants",unique(dat$Taxon_group))){
          dat$Species_author[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$scientificName[1]
          dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$status[1]
          try(dat$Family[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$family[1],silent=T)
          try(dat$Class[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$class[1],silent=T)
          try(dat$Order[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$order[1],silent=T)
          try(dat$Phylum[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$phylum[1],silent=T)
          try(dat$Kingdom[ind_spec]           <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$kingdom[1],silent=T)
        }
        if (grepl("Reptiles",unique(dat$Taxon_group))){
          dat$Species_author[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$scientificName[1]
          dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$status[1]
          try(dat$Family[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$family[1],silent=T)
          try(dat$Class[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$class[1],silent=T)
          try(dat$Order[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$order[1],silent=T)
          try(dat$Phylum[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$phylum[1],silent=T)
          try(dat$Kingdom[ind_spec]           <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$kingdom[1],silent=T)
        }
        if (grepl("Amphibians",unique(dat$Taxon_group))){
          dat$Species_author[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$scientificName[1]
          dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$status[1]
          try(dat$Family[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$family[1],silent=T)
          try(dat$Class[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$class[1],silent=T)
          try(dat$Order[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$order[1],silent=T)
          try(dat$Phylum[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$phylum[1],silent=T)
          try(dat$Kingdom[ind_spec]           <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$kingdom[1],silent=T)
        }
        if (grepl("Birds",unique(dat$Taxon_group))){
          dat$Species_author[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$scientificName[1]
          dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$status[1]
          try(dat$Family[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$family[1],silent=T)
          try(dat$Class[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$class[1],silent=T)
          try(dat$Order[ind_spec]             <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$order[1],silent=T)
          try(dat$Phylum[ind_spec]            <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$phylum[1],silent=T)
          try(dat$Kingdom[ind_spec]           <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$kingdom[1],silent=T)
        }
      }
    } else if (any(alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT") & any(colnames(alternatives)=="species")) { # check for synonyms in 'alternatives'
      
      dat$Species_name[ind_spec]       <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$species[1]
      dat$GBIFstatus[ind_spec]        <- alternatives[alternatives$status=="SYNONYM" & alternatives$matchType=="EXACT",]$status[1]
      
      ## try to get author name of synonym (not provided in 'db')      
      db_all_2 <- name_backbone(dat$Species_name[ind_spec][1],verbose=T)
      db_2 <- db_all_2[["data"]]
      if (db_2$matchType=="EXACT"){
        if (length(unique(db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family))>1) cat(paste0("\n Warning: Multiple entries of ",dat$Species_author[ind_spec]," found in GBIF! Add author to species name or add kingdom information to original database or check GBIF. \n"))
        dat$Species_author[ind_spec] <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$scientificName[1]
        try(dat$Family[ind_spec]            <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family[1],silent=T)
        try(dat$Class[ind_spec]             <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$class[1],silent=T)
        try(dat$Order[ind_spec]             <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$order[1],silent=T)
        try(dat$Phylum[ind_spec]            <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$phylum[1],silent=T)
        try(dat$Kingdom[ind_spec]           <- db_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$kingdom[1],silent=T)
      } else {
        alternatives_2 <- db_all_2$alternatives
        if (length(unique(alternatives_2[db_2$status=="ACCEPTED" & db_2$matchType=="EXACT",]$family))>1) stop("Multiple entries of species names found!")
        dat$Species_author[ind_spec] <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$scientificName[1]
        try(dat$Family[ind_spec]            <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$family[1],silent=T)
        try(dat$Class[ind_spec]             <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$class[1],silent=T)
        try(dat$Order[ind_spec]             <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$order[1],silent=T)
        try(dat$Phylum[ind_spec]            <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$phylum[1],silent=T)
        try(dat$Kingdom[ind_spec]           <- alternatives_2[alternatives_2$status=="ACCEPTED" & alternatives_2$matchType=="EXACT" & alternatives_2$family[1]==db$family[1],]$kingdom[1],silent=T)
      }
      next
      
    } else {
      mismatches <- c(mismatches,speclist[j])
    }
    
    #update progress bar
    info <- sprintf("%d%% done", round((j/n_species)*100))
    setTxtProgressBar(pb, j, label=info)
  }
  close(pb)

  options(warn=0) # the use of 'tibbles' data frame generates warnings as a bug; if solved this options() should be turned off
  
  out <- list()
  out[[1]] <- dat
  out[[2]] <- mismatches

  return(out)
}
