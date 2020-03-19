#########################################################################################################
## Merging databases of alien species distribution and first records
##
## Step 4: Standardisation of first records
##
## First records are modified according to a set of rules defined in Guidelines_eventDates.xlsx,
## then converted to numerics and - if necessary - merged to get single first records.
##
## sTwist workshop
## Hanno Seebens, Frankfurt, 10.03.2020
#########################################################################################################


GeteventDate <- function(FileInfo){
 
  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files(file.path("Output","Intermediate"))
  inputfiles_all <- allfiles[grep("Step4_StandardTaxonNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  replacements <- read.xlsx(file.path("Config","Guidelines_eventDates.xlsx"))
  replacements$Replacement[is.na(replacements$Replacement)] <- ""
  
  ## loop over databases ##########
  for (i in 1:length(inputfiles)){
    
    dat <- read.table(file.path("Output","Intermediate",paste0(inputfiles[i])),header=T,stringsAsFactors = F)
    
  #   dat <- dat[dat$GBIFstatus!="Missing",]
  #   print(inputfiles[i])
  #   print(dim(dat))
  #   print(length(unique(dat$Species_name_orig)))
  #   print(length(unique(dat$Location_orig)))
  # }
    
    dat$eventDate_orig <- dat$eventDate # keep original entry
    
    ## treat first records #############
    nonnumeric <- vector()
    if (any(colnames(dat)=="eventDate")){ 
    
      for (j in 1:nrow(replacements)){
        dat$eventDate <- gsub(replacements$Entry[j],replacements$Replacement[j],dat$eventDate)
      }
      dat$eventDate <- gsub("^\\s+|\\s+$", "",dat$eventDate) # trim leading and trailing whitespace
      
      ## test if all first records can be transferred to numeric
      firstrec_test <- dat$eventDate
      firstrec_test <- firstrec_test[!is.na(firstrec_test)]
      suppressWarnings( first2 <- as.numeric(firstrec_test)) # default warning is confusing; print meaningful warning below instead
      if (any(is.na(first2))){
        cat(paste("\n    Warning: First records in",FileInfo[i,1],"contain non-numeric symbols. Converted to missing values. \n"))
        nonnumeric <- unique(firstrec_test[is.na(first2)]) # collect non-numeric entries
      } 
      
      ## convert first records to numeric
      suppressWarnings( dat$eventDate <- as.numeric(dat$eventDate))
    }    
    
    ## treat second first record if available #############
    if (any(colnames(dat)=="eventDate2")){

      for (j in 1:nrow(replacements)){
        dat$eventDate2 <- gsub(replacements$Entry[j],replacements$Replacement[j],dat$eventDate2)
      }
      dat$eventDate2 <- gsub("^\\s+|\\s+$", "",dat$eventDate2) # trim leading and trailing whitespace
      
      ## test if all first records can be transferred to numeric
      firstrec_test <- dat$eventDate2
      firstrec_test <- firstrec_test[!is.na(firstrec_test)]
      suppressWarnings( first2 <- as.numeric(firstrec_test))
      if (any(is.na(first2))){
        nonnumeric <- c(nonnumeric,unique(firstrec_test[is.na(first2)])) # collect non-numeric entries
        cat(paste("\n Warning: First records in",FileInfo[i,1],"contain non-numeric symbols. Converted to missing values. \n"))
      } 
  
      ## convert first records to numeric
      suppressWarnings( dat$eventDate2 <- as.numeric(dat$eventDate2))
      
      ## calculate unique first record if two are provided
      ind_first2 <- !is.na(dat$eventDate2)
      diff_records <- dat$eventDate2[ind_first2] - dat$eventDate[ind_first2] # difference to check
      ## if range between two first records > 1, take mean of both first records; otherwise, take the earliest (keep the one provided in 'eventDate')
      ind_largediff <- diff_records>1
      dat$eventDate[ind_first2][ind_largediff] <- round(mean(c(dat$eventDate2[ind_first2][ind_largediff],dat$eventDate[ind_first2][ind_largediff]),na.rm=T))
    } 
    
    ## Output #######################################
    
    if (length(nonnumeric)>0) write.table(nonnumeric,file.path("Output","Check",paste0("NonNumeric_eventDates_",FileInfo[i,"Dataset_brief_name"],".csv")),row.names=F,col.names=F)
    
    write.table(dat,file.path("Output","Intermediate",paste0("Step5_StandardIntroYear_",FileInfo[i,"Dataset_brief_name"],".csv")))
    
  } 
}
  