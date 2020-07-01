
####### SInAS workflow: Integration and standardisation of alien species data ###########
##
## Step 1: Prepare databases of alien taxon distribution and first records
## as input datasets to create a merged database
## 
## sTwist workshop
## Hanno Seebens, Frankfurt, 10.03.2020
#########################################################################################


PrepareDatasets <- function (FileInfo){
  
  ## create output folder #####
  if (!file.exists("Output")){
    dir.create("Output")
    dir.create(file.path("Output","Intermediate"))
    dir.create(file.path("Output","Check"))
  }
  
  ######## Load data sets ########################################################
  
  for (i in 1:nrow(FileInfo)){#
    
    ## load data set
    data_name <- FileInfo[i,"File_name_to_load"]
    dat <- read.xlsx(file.path("Inputfiles",data_name),sheet=1)
    
  #   print(inputfiles[i])
  #   print(dim(dat))
  #   print(length(unique(dat$scientificName)))
  #   print(length(unique(dat$Species)))
  #   print(length(unique(paste(dat$standardized_name,dat$author))))
  #   print(length(unique(dat$CountryName)))
  #   print(length(unique(dat$country)))
  #   print(length(unique(dat$tdwg4_name)))
  # }
  
    ## correct modification of import of column names through R
    col_names_import <- colnames(dat)
    # col_names_import <- gsub("\\.+"," ",col_names_import)
    # col_names_import <- gsub("^\\s+|\\s+$", "",col_names_import) # trim leading and trailing whitespace
  
    ## check and rename required column names

    all_column_names <- vector()
    
    if (!is.na(FileInfo[i,"Column_recordID"]) & FileInfo[i,"Column_recordID"]!=""){
      col_recordID <- FileInfo[i,"Column_recordID"]
      colnames(dat)[col_names_import==col_recordID] <- paste("recordID",FileInfo[i,"Dataset_brief_name"],sep="_")
      all_column_names <- c(all_column_names,paste("recordID",FileInfo[i,"Dataset_brief_name"],sep="_"))
    }
    
    if (!is.na(FileInfo[i,"Column_Taxon"]) & FileInfo[i,"Column_Taxon"]!=""){
      col_spec_names <- FileInfo[i,"Column_Taxon"]
      all_column_names <- "Taxon_orig"
      if (is.na(col_spec_names)) stop(paste("Column with taxon names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
      colnames(dat)[col_names_import==col_spec_names] <- "Taxon_orig"
      if (!is.na(FileInfo[i,"Column_author"]) & FileInfo[i,"Column_author"]!=""){
        col_author <- FileInfo[i,"Column_author"]
        colnames(dat)[col_names_import==col_author] <- "Author"
        # all_column_names <- c(all_column_names,"Author")
        dat$Taxon_orig <- paste(dat$Taxon_orig,dat$Author) # add author to taxon name
        dat$Taxon_orig <- gsub(" NA","",dat$Taxon_orig) # remove missing author names
      }
    }
      
    if (!is.na(FileInfo[i,"Column_scientificName"]) & FileInfo[i,"Column_scientificName"]!=""){
      col_spec_names <- FileInfo[i,"Column_scientificName"]
      if (is.na(col_spec_names)) stop(paste("Column with taxon names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
      colnames(dat)[col_names_import==col_spec_names] <- "Taxon_orig"
      all_column_names <- "Taxon_orig"
    }

    col_reg_names <- FileInfo[i,"Column_Location"]
    if (is.na(col_reg_names)) stop(paste("Column with location names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
    colnames(dat)[col_names_import==col_reg_names] <- "Location_orig"
    all_column_names <- c(all_column_names,"Location_orig")

    ## check and rename optional column names
    if (!is.na(FileInfo[i,"Column_kingdom"]) & FileInfo[i,"Column_kingdom"]!=""){
      col_kingdom <- FileInfo[i,"Column_kingdom"]
      colnames(dat)[col_names_import==col_kingdom] <- "Kingdom_user"
      all_column_names <- c(all_column_names,"Kingdom_user")
    }
    # if (!is.na(FileInfo[i,"Column_island_name"]) & FileInfo[i,"Column_island_name"]!=""){
    #   col_islandname <- FileInfo[i,"Column_island_name"]
    #   ind_NA <- is.na(dat$island)
    #   dat$Location_orig[!ind_NA] <- dat$island[!ind_NA] # replace country names by island names
    # }
    if (!is.na(FileInfo[i,"Column_country_ISO"]) & FileInfo[i,"Column_country_ISO"]!=""){
      col_country_code <- FileInfo[i,"Column_country_ISO"]
      colnames(dat)[col_names_import==col_country_code] <- "Country_ISO"
      all_column_names <- c(all_column_names,"Country_ISO")
    }
    if (!is.na(FileInfo[i,"Column_eventDate1"]) & FileInfo[i,"Column_eventDate1"]!=""){
      col_eventDate_1 <- FileInfo[i,"Column_eventDate1"]
      colnames(dat)[col_names_import==col_eventDate_1] <- "eventDate"
      all_column_names <- c(all_column_names,"eventDate")
    }
    if (!is.na(FileInfo[i,"Column_eventDate2"]) & FileInfo[i,"Column_eventDate2"]!=""){
      col_eventDate_2 <- FileInfo[i,"Column_eventDate2"]
      colnames(dat)[col_names_import==col_eventDate_2] <- "eventDate2"
      all_column_names <- c(all_column_names,"eventDate2")
    }
    if (!is.na(FileInfo[i,"Column_establishmentMeans"]) & FileInfo[i,"Column_establishmentMeans"]!=""){
      col_establishmentMeans <- FileInfo[i,"Column_establishmentMeans"]
      colnames(dat)[col_names_import==col_establishmentMeans] <- "establishmentMeans"
      all_column_names <- c(all_column_names,"establishmentMeans")
      dat$establishmentMeans <- tolower(dat$establishmentMeans)
    }
    if (!is.na(FileInfo[i,"Column_occurrenceStatus"]) & FileInfo[i,"Column_occurrenceStatus"]!=""){
      col_occurrenceStatus <- FileInfo[i,"Column_occurrenceStatus"]
      if (col_establishmentMeans==col_occurrenceStatus){ # check if same column has been assigned before in establishmentMeans
        dat$occurrenceStatus <- dat$establishmentMeans # if yes, duplicate column
      } else {
        colnames(dat)[col_names_import==col_occurrenceStatus] <- "occurrenceStatus"
      }
      all_column_names <- c(all_column_names,"occurrenceStatus")
      dat$occurrenceStatus <- tolower(dat$occurrenceStatus)
    }
    if (!is.na(FileInfo[i,"Column_degreeOfEstablishment"]) & FileInfo[i,"Column_degreeOfEstablishment"]!=""){
      col_degreeOfEstablishment <- FileInfo[i,"Column_degreeOfEstablishment"]
      if (col_establishmentMeans==col_degreeOfEstablishment){ # check if same column has been assigned before in establishmentMeans
        dat$degreeOfEstablishment <- dat$establishmentMeans # if yes, duplicate column
      } else if (col_establishmentMeans==occurrenceStatus){
        dat$degreeOfEstablishment <- dat$occurrenceStatus # if yes, duplicate column
      } else {
        colnames(dat)[col_names_import==col_degreeOfEstablishment] <- "degreeOfEstablishment"
      }
      all_column_names <- c(all_column_names,"degreeOfEstablishment")
      dat$degreeOfEstablishment <- tolower(dat$degreeOfEstablishment)
    }
    if (!is.na(FileInfo[i,"Column_habitat"]) & FileInfo[i,"Column_habitat"]!=""){
      col_habitat <- FileInfo[i,"Column_habitat"]
      colnames(dat)[col_names_import==col_habitat] <- "habitat"
      all_column_names <- c(all_column_names,"habitat")
      dat$habitat <- tolower(dat$habitat)
    }
    if (!is.na(FileInfo[i,"Column_references"]) & FileInfo[i,"Column_references"]!=""){
      col_references <- FileInfo[i,"Column_references"]
      colnames(dat)[col_names_import==col_references] <- "references"
      all_column_names <- c(all_column_names,"references")
    }

    if (!is.na(FileInfo[i,"Column_additional"]) & FileInfo[i,"Column_additional"]!=""){
      col_additional <- FileInfo[i,"Column_additional"]
      addit_cols <- unlist(strsplit(col_additional,"; "))
      all_column_names <- c(all_column_names,colnames(dat)[pmatch(addit_cols,colnames(dat))])
    }
    
    ## keep required, optional and additional columns
    dat_out <- dat[,all_column_names]
    dat_out[dat_out=="Null"] <- ""
    dat_out[is.na(dat_out)] <- ""
    
    ## remove rows with missing taxon and region names
    dat_out <- dat_out[!dat_out$Location_orig=="",]
    dat_out <- dat_out[!dat_out$Taxon_orig=="",]
    
    dat_out$Taxon_group <- FileInfo[i,"Taxon_group"]
    
    colnames(dat_out) <- gsub("\\.+","_",colnames(dat_out))
    dat_out$Taxon_orig <- gsub("\"","",dat_out$Taxon_orig) # remove additional quotes to avoid difficulties with export
    dat_out$Taxon_orig <- gsub("\\\\","",dat_out$Taxon_orig) # remove backshlashes

    dat_out <- unique(dat_out) # remove duplicates
    
    write.table(dat_out,file.path("Output","Intermediate",paste("Step1_StandardColumns_",FileInfo[i,"Dataset_brief_name"],".csv",sep="")),row.names = F)
  }
}
