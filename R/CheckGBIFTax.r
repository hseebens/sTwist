#### check and replace species names using 'rgibf' GBIF taxonomy ###########
## requires a data.frame with a column 'Species' containing species names ##
##
## sTwist workshop
## Hanno Seebens, 16.11.2018
############################################################################

CheckGBIFTax <- function(dat,LifeForm="all"){
  
  dat$SpeciesGBIF <- NA
  if (any(colnames(dat)=="LifeForm")){
    speclist_lifeform <- unique(dat[,c("OrigSpecies","LifeForm")])
    speclist <- speclist_lifeform$OrigSpecies
  } else {
    speclist <- unique(dat$OrigSpecies)
  }
  n_species <- length(speclist)
  
  #setup progress bar
  pb <- txtProgressBar(min=0, max=n_species, initial=0,style = 3)
  
  for (i in 1:length(speclist)){# loop over all species names; takes some hours...
    
    # select species name and download taxonomy
    ind_spec <- which(dat$OrigSpecies==speclist[i])
    db_all <- name_backbone(speclist[i],verbose=T) # check for names and synonyms
    db <- db_all[["data"]]
    alternatives <- db_all$alternatives
    
    # check if any data from GBIF taxonomy exist
    # if (dim(db)[1]==0 | db$matchType=="NONE" | nrow(alternatives)) next # next species if no information is available
    if (db$matchType!="EXACT" & all(alternatives$matchType!="EXACT")) next # next species if no information is available
    
    # select only accepted names and exact matches
    if (any(db$status=="ACCEPTED" & db$matchType=="EXACT" & db$rank=="SPECIES") & any(colnames(db)=="species")){ 
      dat$SpeciesGBIF[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$species[1]
      dat$SpeciesAuthorityGBIF[ind_spec] <- db[db$status=="ACCEPTED" & db$matchType=="EXACT",]$scientificName[1]
      next
      
      # if name matches are not exact, select name with close match
    } #else if (any(max(db$confidence)>=90 & any(colnames(db)=="species"))){  
      #dat$SpeciesGBIF[ind_spec] <- db[db$confidence>=90,]$species[1] 
      
    if (any(alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$rank=="SPECIES") & any(colnames(alternatives)=="species")){ 
      dat$SpeciesGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT",]$species[1]
      
      ## select entries from cross-taxonomic databases from certain taxa
      if (LifeForm=="all" & any(colnames(dat)=="LifeForm")){
        if (speclist_lifeform[i]=="Vascular plants"){
          dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$scientificName[1]
        } 
        if (speclist_lifeform[i]=="Reptiles"){
          dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$scientificName[1]
        } 
        if (speclist_lifeform[i]=="Amphibians"){
          dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$scientificName[1]
        } 
        if (speclist_lifeform[i]=="Birds"){
          dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$scientificName[1]
        } 
        next
      }
      if (taxon=="Vascular plants"){
        dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$kingdom=="Plantae",]$scientificName[1]
        next
      } 
      if (taxon=="Herptiles"){
        dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Reptilia",]$scientificName[1]
        next
      } 
      if (taxon=="Herptiles"){
        dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Amphibia",]$scientificName[1]
        next
      } 
      if (taxon=="Birds"){
        dat$SpeciesAuthorityGBIF[ind_spec] <- alternatives[alternatives$status=="ACCEPTED" & alternatives$matchType=="EXACT" & alternatives$class=="Aves",]$scientificName[1]
        next
      } 
    }
    
     # if all previous names not match, check for alternative names; not fully clear what 'alternatives' means!!!
      # } else if (dim(alternatives)[1]>0 & any(colnames(alternatives)=="species")){
      #   if (alternatives$confidence>=90 & alternatives$status=="SYNONYM"){
      #     dat$SpeciesGBIF[ind_spec] <- alternatives[alternatives$confidence>=90 & alternatives$status=="SYNONYM",]$species[1] 
      #   }
    
    #update progress bar
    info <- sprintf("%d%% done", round((i/n_species)*100))
    setTxtProgressBar(pb, i, label=info)
  }
  close(pb)
  return(dat)
}
