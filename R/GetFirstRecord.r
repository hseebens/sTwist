#########################################################################################################
## Merging databases of alien species distribution and first records
##
## Step 4: Standardisation of first records
##
## First records are modified according to a set of rules defined in Guidelines_FirstRecords.xlsx,
## then converted to numerics and - if necessary - merged to get single first records.
##
## sTwist workshop
## Hanno Seebens, 06.08.2019
#########################################################################################################


GetFirstRecord <- function(FileInfo){
 
  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardRegionNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  replacements <- read.xlsx("Config/Guidelines_FirstRecords.xlsx")
  replacements$Replacement[is.na(replacements$Replacement)] <- ""
  
  ## loop over databases ##########
  for (i in 1:length(inputfiles)){
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    
    dat$First_record_orig <- dat$First_record # keep original entry
    
    ## treat first records #############
    nonnumeric <- vector()
    if (any(colnames(dat)=="First_record")){ 
    
      for (j in 1:nrow(replacements)){
        dat$First_record <- gsub(replacements$Entry[j],replacements$Replacement[j],dat$First_record)
      }
      dat$First_record <- gsub("^\\s+|\\s+$", "",dat$First_record) # trim leading and trailing whitespace
      
      ## test if all first records can be transferred to numeric
      firstrec_test <- dat$First_record
      firstrec_test <- firstrec_test[!is.na(firstrec_test)]
      suppressWarnings( first2 <- as.numeric(firstrec_test)) # default warning is confusing; print meaningful warning below instead
      if (any(is.na(first2))){
        cat(paste("\n Warning: First records in",FileInfo[i,1],"contain non-numeric symbols. Converted to missing values. \n"))
        nonnumeric <- unique(firstrec_test[is.na(first2)]) # collect non-numeric entries
      } 
      
      ## convert first records to numeric
      suppressWarnings( dat$First_record <- as.numeric(dat$First_record))
    }    
    
    ## treat second first record if available #############
    if (any(colnames(dat)=="First_record2")){

      for (j in 1:nrow(replacements)){
        dat$First_record2 <- gsub(replacements$Entry[j],replacements$Replacement[j],dat$First_record2)
      }
      dat$First_record2 <- gsub("^\\s+|\\s+$", "",dat$First_record2) # trim leading and trailing whitespace
      
      ## test if all first records can be transferred to numeric
      firstrec_test <- dat$First_record2
      firstrec_test <- firstrec_test[!is.na(firstrec_test)]
      suppressWarnings( first2 <- as.numeric(firstrec_test))
      if (any(is.na(first2))){
        nonnumeric <- c(nonnumeric,unique(firstrec_test[is.na(first2)])) # collect non-numeric entries
        cat(paste("\n Warning: First records in",FileInfo[i,1],"contain non-numeric symbols. Converted to missing values. \n"))
      } 
  
      ## convert first records to numeric
      suppressWarnings( dat$First_record2 <- as.numeric(dat$First_record2))
      
      ## calculate unique first record if two are provided
      ind_first2 <- !is.na(dat$First_record2)
      diff_records <- dat$First_record2[ind_first2] - dat$First_record[ind_first2] # difference to check
      ## if range between two first records > 1, take mean of both first records; otherwise, take the earliest (keep the one provided in 'First_record')
      ind_largediff <- diff_records>1
      dat$First_record[ind_first2][ind_largediff] <- round(mean(c(dat$First_record2[ind_first2][ind_largediff],dat$First_record[ind_first2][ind_largediff]),na.rm=T))
    } 
    
    ## Output #######################################
    
    if (length(nonnumeric)>0) write.table(nonnumeric,paste0("Output/NonNumericFirstRecords_",FileInfo[i,"Dataset_brief_name"],".csv"),row.names=F,col.names=F)
    
    write.table(dat,paste0("Output/StandardIntroYear_",FileInfo[i,"Dataset_brief_name"],".csv"))
    
  } 
}
  