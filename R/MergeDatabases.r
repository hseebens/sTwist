#########################################################################################
## Merging databases of alien species distribution and first records
## merge datababases after harmonisation of species and country names 
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, amphibians + reptiles
##
## sTwist workshop
## Hanno Seebens, 08.08.2019
#########################################################################################


MergeDatabases <- function(FileInfo,version,outputfilename,output){
  
  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardIntroYear_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  for (i in 1:length(inputfiles)){#
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    
    cnames <- colnames(dat)
    cnames <- cnames[!cnames%in%c("Species_name_orig","Region_name_orig","Kingdom","Country_ISO","GBIFstatus","ISO2","CountryID","Taxon_group")]
    dat <- dat[,colnames(dat)%in%cnames]

    eval(parse(text=paste0("dat$",substitute(a,list(a=FileInfo[i,1])),"<-\"x\""))) # add column with database information

    if (i==1){
      alldat <- dat
    } else {
      
      alldat <- merge(alldat,dat,by=c("Region_name","Species_name","Species_author","Family"),all=T) # merge databases

      ## treat duplicated columns (named by R default ".x" and ".y")
      if (any(grepl("\\.y",colnames(alldat)))){
        if (any(colnames(alldat)=="First_record.x")){ # solve multiple first records
          ## select the minimum of multiple first records
          alldat$First_record <- apply(alldat[,c("First_record.x","First_record.y")],1,function(s) ifelse(all(is.na(s)),NA,min(s,na.rm=T)))
          alldat <- alldat[,!colnames(alldat)%in%c("First_record.x","First_record.y")]
        }
        while(any(grepl("\\.y",colnames(alldat)))){ # check if discrepancy still exists
          colname_dupl <- colnames(alldat)[grep("\\.y",colnames(alldat))][1] # identify duplicated column
          colname_dupl <- gsub("\\..+$","",colname_dupl) # create new column new
          colnames_dupl <- colnames(alldat)[grep(colname_dupl,colnames(alldat))] # identify .x and .y component
          ## merge both columns into a new one by combining the content separated by ';'
          eval(parse(text=paste0("alldat$",substitute(a,list(a=colname_dupl)),"<-paste(alldat$",substitute(a,
                        list(a=colname_dupl)),".x,alldat$",substitute(a,list(a=colname_dupl)),".y,sep=\";\")"))) # add column with database information
          alldat <- alldat[,!colnames(alldat)%in%colnames_dupl] # remove .x and .y columns
          alldat[,colname_dupl] <- gsub(";NA","",alldat[,colname_dupl]) # clean new column
          alldat[,colname_dupl] <- gsub("NA;","",alldat[,colname_dupl]) # clean new column
        }
      }
    }
  }
  
  ## check: Achyranthes aspera
  
  ## remove duplicated entries
  ind_dupl <- duplicated(alldat) # remove identical lines
  alldat <- alldat[!ind_dupl,]
  
  ind_dupl <- duplicated(alldat[,c("Species_name","Region_name")]) # remove duplicated species-region combinations
  all_dat_dupl <- unique(alldat[ind_dupl,c("Species_name","Region_name")])
  ind_rm <- col_dupl <- vector()
  for (j in 1:nrow(all_dat_dupl)){
    ind_each <- which(alldat$Species_name==all_dat_dupl$Species_name[j] & alldat$Region_name==all_dat_dupl$Region_name[j])
    for (k in 1:dim(alldat)[2]){ # loop over columns
      if (colnames(alldat)[k]%in%c("Region_name","Species_name")) next # ignore these columns
      if (all(is.na(alldat[ind_each,k]))) next # skip if all NA
      if (all(duplicated(alldat[ind_each,k])[-1])){ # skip if all equal (non-NA)
        next 
      } else {
        ind_NA <- is.na(alldat[ind_each,k]) # avoid NAs
        if (length(unique(alldat[ind_each,k][!ind_NA]))==1){ # if only a single value is non-NA, add non-NA information to first row
          alldat[ind_each,k][1] <- alldat[ind_each,k][!ind_NA][1]
        } else { # if deviating entries exit, merge content into first row
          if (colnames(alldat)[k]=="First_record"){ # treat first records differently
            alldat[ind_each,k][1] <- min(alldat[ind_each,k],na.rm = T) # select earliest first record
          } else {
            alldat[ind_each,k][1] <- paste(alldat[ind_each,k][!ind_NA],collapse="; ") # concatenate unequal row entries
            col_dupl <- c(col_dupl,colnames(alldat)[k]) # store column with deviating information for report
          }
        }
        ind_rm <- c(ind_rm,ind_each[-1]) # collect rows to remove (all except the first one)
      }
    }
  }
  alldat <- alldat[-unique(ind_rm),] # remove all duplicates
  if (length(col_dupl)>0) cat(paste0("\n Warning: Multiple entries for the same record. Check column '",unique(col_dupl),"' in final data set for entries separated by ';'. \n"))

  ## output #############################################

  all_addit_cols <- paste(FileInfo$Column_additional,collapse="; ")
  all_addit_cols <- unlist(strsplit(all_addit_cols,"; "))
  all_addit_cols <- all_addit_cols[all_addit_cols!="NA"]
  if (any(colnames(alldat)=="First_record")){
    columns_out <- c("Region_name","Species_name","Species_author","First_record",FileInfo[,1],all_addit_cols)
  } else {
    columns_out <- c("Region_name","Species_name","Species_author",FileInfo[,1],all_addit_cols)
  }

  alldat_out <- alldat[,columns_out]
  alldat_out[is.na(alldat_out)] <- ""
  
  write.table(alldat_out,paste("Output/",outputfilename,version,".csv",sep=""),row.names=F)
  
  # dat <- read.table(paste("Output/",outputfilename,version,".csv",sep=""),stringsAsFactors = F,header=T)
  
  ## ending line
  cat(paste("\n Successfully established version",version,"of",outputfilename,"file. \n"))
  
  ## delete intermediate results if selected ########
  if (!output) {
    interm_res <- list.files("Output/")
    interm_res <- interm_res[grep("Standard",interm_res)]
    file.remove(paste0("Output/",interm_res))
  }
}
