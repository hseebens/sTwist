#########################################################################################
## Merging databases of alien species distribution and first records
##
## Step 3: Standardisation of country names 
##
## Country names are standardised according to the specification provided in 
## AllRegionsList.xlsx.
##
## sTwist workshop
## Hanno Seebens, 30.08.2019
#########################################################################################


StandardiseCountryNames <- function(FileInfo){

  ## identify input datasets based on file name "StandardSpec_....csv"
  allfiles <- list.files("Output/")
  inputfiles_all <- allfiles[grep("StandardSpecNames_",allfiles)]
  inputfiles <- vector()
  for (i in 1:length(inputfiles_all)){
    inputfiles <- c(inputfiles,grep(FileInfo[i,"Dataset_brief_name"],inputfiles_all,value=T))
  }
  inputfiles <- inputfiles[!is.na(inputfiles)]
  
  ## load region table #################################################
  regions <- read.xlsx("Config/AllRegionsList.xlsx",sheet=1,na.strings ="")
  regions$keywords <- gsub("\\(","\\\\(",regions$keywords)
  regions$keywords <- gsub("\\)","\\\\)",regions$keywords)
  
  ## loop over all data set ############################################
  for (i in 1:length(inputfiles)){
    
    dat <- read.table(paste0("Output/",inputfiles[i]),header=T,stringsAsFactors = F)
    
    dat_match1 <- dat ## use another dat set for region matching to keep the original names
    dat_match1$order <- 1:nrow(dat_match1)
    dat_match1$Region_name_orig <- gsub("\\xa0|\\xc2", " ",dat_match1$Region_name_orig) # replace weird white space with recognised white space
    dat_match1$Region_name_orig <- gsub("^\\s+|\\s+$", "",dat_match1$Region_name_orig) # trim leading and trailing whitespace
    dat_match1$Region_name_orig <- gsub("  ", " ",dat_match1$Region_name_orig) # turn two spaces into one
    dat_match1$Region_name_orig <- gsub(" \\(the\\)", "",dat_match1$Region_name_orig) # remove " (the)" 
    
    ## step 0: if provided in FileInfo.xlsx, select R script to transform country names
    if (!is.na(FileInfo[i,"R_countrynames"])){ # check if R script name is provided
      eval(bquote(source(.(paste0("R/",FileInfo[i,"R_countrynames"]))))) # load R script
      exec <- gsub("\\.r","",FileInfo[i,"R_countrynames"])
      dat_match1 <- eval(parse(text=paste0(substitute(exec,list(exec=exec)),"(dat_match1)")))
    }
    
    # ## Step 1: Match country names by ISO codes (ISO2 or ISO3) if provideds (currently not working due to multiple ISO entries)
    # if (!is.na(FileInfo[i,"Column_country_ISO"])){
    #   if (nchar(dat_match1$Country_ISO[1])==2) {
    #     dat_match1 <- merge(regions,dat_match1,by.y="Country_ISO",by.x="ISO2",all.y=T)
    #     dat_match1$Region_name_orig[!is.na(dat_match1$Region)] <- dat_match1$Region[!is.na(dat_match1$Region)]
    #   } else if (nchar(dat_match1$Country_ISO[1])==3) {
    #     dat_match1 <- merge(regions,dat_match1,by.y="Country_ISO",by.x="ISO3",all.y=T)
    #     dat_match1$Region_name_orig[!is.na(dat_match1$Region)] <- dat_match1$Region[!is.na(dat_match1$Region)]
    #   } else {
    #     print("Warning: Provided country ISO codes do not match with implemented standard (ISO2, ISO3). Country names are used instead.")
    #   }
    # } else {
    # ## or match names of 'dat' with region names of 'regions'
    #   dat_match1 <- merge(dat_match1,regions,by.x="Region_name_orig",by.y="Region",all.x=T)
    # }

    ## step 1: match names of 'dat' with region names of 'regions'
    dat_match1 <- merge(dat_match1,regions,by.x="Region_name_orig",by.y="Region",all.x=T)

    ## step 3: match names by using keywords in 'regions
    ind_keys <- which(!is.na(regions$keywords))
    for (j in 1:length(ind_keys)){ # loop over rows with multiple keywords
      if (any(grepl("; ",regions$keywords[ind_keys[j]]))){ # check if multiple keywords provided
        keywords <- unlist(strsplit(regions$keywords[ind_keys[j]],"; "))
      } else {
        keywords <- regions$keywords[ind_keys[j]]
      }
      for (k in 1:length(keywords)){
        ind_match <- grep(keywords[k],dat_match1$Region_name_orig) 
        dat_match1$Region_name_orig[ind_match] <- regions$Region[ind_keys[j]]
        dat_match1$ISO2[ind_match]             <- regions$ISO2[ind_keys[j]]
      }
    }

    # sort(unique(dat_match1[is.na(dat_match1$ISO2),]$Region))
    
    ## final merging of both data sets with standardised region names
    dat_match1 <- dat_match1[order(dat_match1$order),]
    if (!identical(dat_match1$Species_name_orig,dat$Species_name_orig)) stop("Data sets not sorted equally!")
    dat$Region_name <- dat_match1$Region_name_orig
    
    dat_regnames <- merge(dat,regions[,c("CountryID","ISO2","Region")],by.x="Region_name",by.y="Region",all.x=T)

    ## remove duplicated entries ##############
    
    ind <- which(!duplicated(dat_regnames))
    dat_regnames <- dat_regnames[ind,]

    ## keep only earliest first record
    if (any(colnames(dat_regnames)=="First_record")){
      oo <- order(dat_regnames$Region_name,dat_regnames$Species_name,dat_regnames$First_record) # sort ascending order
      dat_regnames <- dat_regnames[oo,]
      ind <- which(!duplicated(dat_regnames[,c("Region_name","Species_name")])) # identify duplicates (the first match is not counted, only the subsequent duplicates)
      dat_regnames <- dat_regnames[ind,] # delete duplicates
    }

    ## output ###############################################################################
    
    missing <- dat_regnames$Region_name_orig[is.na(dat_regnames$CountryID)]
    
    if (length(missing)>0){ # export missing country names
      write.table(sort(unique(missing)),paste0("Output/MissingRegions_",FileInfo[i,"Dataset_brief_name"],".csv"))
    }
    
    dat_regnames <- dat_regnames[!is.na(dat_regnames$CountryID),]
    write.table(dat_regnames,paste0("Output/StandardRegionNames_",FileInfo[i,"Dataset_brief_name"],".csv"))
  }
  
  reg_names <- vector()
  for (i in 1:length(inputfiles)){
    dat <- read.table(paste0("Output/StandardRegionNames_",FileInfo[i,"Dataset_brief_name"],".csv"),stringsAsFactors = F)
    reg_names <- rbind(reg_names,cbind(dat[,c("Region_name","Region_name_orig")],FileInfo[i,1]))
  }
  reg_names <- reg_names[reg_names$Region_name!=reg_names$Region_name_orig,] # export only region names deviating from the original
  reg_names <- unique(reg_names[order(reg_names$Region_name),])
  colnames(reg_names) <- c("Region_name","Region_name_orig","Database")
  
  write.table(reg_names,"Output/TranslationRegionNames.csv",row.names=F)
}

