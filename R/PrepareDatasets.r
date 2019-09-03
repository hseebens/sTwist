#########################################################################################
## Prepare example databases of alien species distribution and first records
## as input datasets to create a merged database
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
##
## sTwist workshop
## Hanno Seebens et al., 06.08.2019
#########################################################################################


PrepareDatasets <- function (FileInfo){
  
  ## create output folder #####
  if (!file.exists("Output")){
    dir.create("Output")
  }
  
  ######## Load data sets ########################################################
  
  for (i in 1:nrow(FileInfo)){#
    
    ## load data set
    data_name <- FileInfo[i,"File_name_to_load"]
    dat <- read.xlsx(paste("Inputfiles/",data_name,sep=""),sheet=1)

    ## correct modification of import of column names through R
    col_names_import <- colnames(dat)
    col_names_import <- gsub("\\.+"," ",col_names_import)
    col_names_import <- gsub("^\\s+|\\s+$", "",col_names_import) # trim leading and trailing whitespace
  
    ## check and rename required column names
    if (!is.na(FileInfo[i,"Column_species_name"]) & FileInfo[i,"Column_species_name"]!=""){
      col_spec_names <- FileInfo[i,"Column_species_name"]
      all_column_names <- "Species_name_orig"
      if (is.na(col_spec_names)) stop(paste("Column with species names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
      colnames(dat)[col_names_import==col_spec_names] <- "Species_name_orig"
      if (!is.na(FileInfo[i,"Column_author"]) & FileInfo[i,"Column_author"]!=""){
        col_author <- FileInfo[i,"Column_author"]
        colnames(dat)[col_names_import==col_author] <- "Author"
        # all_column_names <- c(all_column_names,"Author")
        dat$Species_name_orig <- paste(dat$Species_name_orig,dat$Author) # add author to species name
      }
    }
      
    if (!is.na(FileInfo[i,"Column_scientificName"]) & FileInfo[i,"Column_scientificName"]!=""){
      col_spec_names <- FileInfo[i,"Column_scientificName"]
      if (is.na(col_spec_names)) stop(paste("Column with species names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
      colnames(dat)[col_names_import==col_spec_names] <- "Species_name_orig"
      all_column_names <- "Species_name_orig"
    }

    col_reg_names <- FileInfo[i,"Column_region_name"]
    if (is.na(col_reg_names)) stop(paste("Column with region names not found in",FileInfo[i,"Dataset_brief_name"],"file!"))
    colnames(dat)[col_names_import==col_reg_names] <- "Region_name_orig"
    all_column_names <- c(all_column_names,"Region_name_orig")

    ## check and rename optional column names
    if (!is.na(FileInfo[i,"Column_kingdom"]) & FileInfo[i,"Column_kingdom"]!=""){
      col_kingdom <- FileInfo[i,"Column_kingdom"]
      colnames(dat)[col_names_import==col_kingdom] <- "Kingdom_user"
      all_column_names <- c(all_column_names,"Kingdom_user")
    }
    if (!is.na(FileInfo[i,"Column_country_ISO"]) & FileInfo[i,"Column_country_ISO"]!=""){
      col_country_code <- FileInfo[i,"Column_country_ISO"]
      colnames(dat)[col_names_import==col_country_code] <- "Country_ISO"
      all_column_names <- c(all_column_names,"Country_ISO")
    }
    if (!is.na(FileInfo[i,"Column_status"]) & FileInfo[i,"Column_status"]!=""){
      col_status <- FileInfo[i,"Column_status"]
      colnames(dat)[col_names_import==col_status] <- "Status"
      all_column_names <- c(all_column_names,"Status")
    }
    if (!is.na(FileInfo[i,"Column_first_record1"]) & FileInfo[i,"Column_first_record1"]!=""){
      col_firstrecord_1 <- FileInfo[i,"Column_first_record1"]
      colnames(dat)[col_names_import==col_firstrecord_1] <- "First_record"
      all_column_names <- c(all_column_names,"First_record")
    }
    if (!is.na(FileInfo[i,"Column_first_record2"]) & FileInfo[i,"Column_first_record2"]!=""){
      col_firstrecord_2 <- FileInfo[i,"Column_first_record2"]
      colnames(dat)[col_names_import==col_firstrecord_2] <- "First_record2"
      all_column_names <- c(all_column_names,"First_record2")
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
    
    dat_out$Taxon_group <- FileInfo[i,"Taxon_group"]
    
    colnames(dat_out) <- gsub("\\.+","_",colnames(dat_out))
    dat_out$Species_name_orig <- gsub("\"","",dat_out$Species_name_orig) # remove additional quotes to avoid difficulties with export
    dat_out$Species_name_orig <- gsub("\\\\","",dat_out$Species_name_orig) # remove backshlashes

    write.table(dat_out,paste("Output/StandardColumns_",FileInfo[i,"Dataset_brief_name"],".csv",sep=""))
  }
}
