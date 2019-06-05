#########################################################################################
## Merging databases of alien species distribution and first records
## merge datababases after harmonisation of species and country names 
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA
##
## sTwist workshop
## Hanno Seebens, 28.03.2019
#########################################################################################


MergeDatabases <- function(output,version){
  
  
  ## loading databases...
  
  griis <- read.table("Data/GRIIS_HarmSpecCountry.csv",header=T,stringsAsFactors = F,sep=";")
  if (any(colnames(griis)=="OrigSpecies")) griis <- griis[,-which(colnames(griis)=="OrigSpecies")]
  
  firstrecords <- read.table("Data/FirstRecords_HarmSpecCountry.csv",stringsAsFactors = F,sep=";",header=T)#
  if (any(colnames(firstrecords)=="OrigSpecies")) firstrecords <- firstrecords[,-which(colnames(firstrecords)=="OrigSpecies")]
  
  glonaf <- read.table("Data/GloNAF_HarmSpecCountry.csv",stringsAsFactors = F,sep=";",header=T)
  if (any(colnames(glonaf)=="OrigSpecies")) glonaf <- glonaf[,-which(colnames(glonaf)=="OrigSpecies")]
  
  gavia <- read.table("Data/GAVIA_HarmSpecCountry.csv",stringsAsFactors = F,sep=";",header=T)
  if (any(colnames(gavia)=="OrigSpecies")) gavia <- gavia[,-which(colnames(gavia)=="OrigSpecies")]
  
  amphrep <- read.table("Data/AmphRep_HarmSpecCountry.csv",header=T,stringsAsFactors = F,sep=";")
  if (any(colnames(amphrep)=="OrigSpecies")) amphrep <- amphrep[,-which(colnames(amphrep)=="OrigSpecies")]
  
  
  ##################################################################################
  ### Merge all databases ##########################################################
  
  ## check country names among databases...
  griis$GRIIS <- "x"
  firstrecords$FirstRecordDB <- "x"
  glonaf$GloNAF <- "x"
  gavia$GAVIA <- "x"
  amphrep$AmRep <- "x"
  
  all_countries <- merge(unique(griis[,c("Country","GRIIS")]),unique(firstrecords[,c("Country","FirstRecordDB")]),all=T)
  all_countries <- merge(unique(all_countries),unique(glonaf[,c("Country","GloNAF")]),all=T)
  all_countries <- merge(unique(all_countries),unique(gavia[,c("Country","GAVIA")]),all=T)
  all_countries <- merge(unique(all_countries),unique(amphrep[,c("Country","AmRep","Group")]),all=T)
  # all_countries[order(all_countries$Country),][166:(166+80),] # use this file to compare country names!
  
  
  ### merge databases...
  
  all_records <- merge(griis,firstrecords,by=c("SpeciesGBIF","Country"),all=T)
  all_records <- merge(all_records,glonaf,by=c("SpeciesGBIF","Country"),all=T)
  all_records <- merge(all_records,gavia,by=c("SpeciesGBIF","Country"),all=T)
  all_records <- merge(all_records,amphrep,by=c("SpeciesGBIF","Country"),all=T)
  
  all_records <- all_records[!is.na(all_records$SpeciesGBIF),] # remove entries without a GBIF record
  all_records <- all_records[,c("SpeciesGBIF","LifeForm","Group","Country","Status","Habitat","GRIIS","FirstRecordDB","GloNAF","GAVIA","AmRep","GRIISFirstRecord"
                                ,"FirstRecord","GRIISimpact_evidence","Country_ISO","TDWG1_name","Continent","Continent1","Continent2")]#

  ## fill some gaps with ISO codes 
  isocodes <- unique(all_records[,c("Country","Country_ISO")])
  isocodes <- subset(isocodes,!is.na(isocodes$Country_ISO))
  all_records <- all_records[,-which(colnames(all_records)=="Country_ISO")]
  all_records <- merge(all_records,isocodes,by="Country",all.x=T)
  
  ## the following countries do not have an ISO code yet in the database:
  # [1] "Andaman and Nicobar"                    "Andorra"                                "Anguilla"                               "Antigua and Barbuda"                   
  # [5] "Azerbaijan"                             "Bahrain"                                "Barbados"                               "Bhutan"                                
  # [9] "Bosnia and Herzegovina"                 "Brunei Darussalam"                      "Cambodia"                               "Cape Verde"                            
  # [13] "Cayman Islands (the)"                   "Clipperton Island"                      "Comoros (the)"                          "Dominica"                              
  # [17] "East Timor"                             "El Salvador"                            "Falkland Islands"                       "Gibraltar"                             
  # [21] "Grenada"                                "Guadeloupe"                             "Guam"                                   "Guatemala"                             
  # [25] "Haiti"                                  "Honduras"                               "Iraq"                                   "Jordan"                                
  # [29] "Kazakhstan"                             "Kuwait"                                 "Lao People's Democratic Republic (the)" "Lebanon"                               
  # [33] "Maldives"                               "Martinique"                             "Mayotte"                                "Micronesia, Federated States of"       
  # [37] "Monaco"                                 "Montenegro"                             "Mozambique"                             "Myanmar"                               
  # [41] "Nicaragua"                              "Niue"                                   "Norfolk Islands"                        "Northern Mariana Islands (the)"        
  # [45] "Pakistan"                               "Palau"                                  "Palestinian Territory, Occupied"        "Peru"                                  
  # [49] "Saint-Barthelemy"                       "Saint Kitts and Nevis"                  "Saint Lucia"                            "Saint-Martin"                          
  # [53] "Saint Vincent and the Grenadines"       "San Marino"                             "Serbia"                                 "Society Islands"                       
  # [57] "Solomon Islands"                        "South Sudan"                            "Sulawesi"                               "Syria"                                 
  # [61] "Timor Leste"                            "Tokelau"                                "Trinidad and Tobago"                    "Turkmenistan"                          
  # [65] "Turks and Caicos Islands (the)"         "Tuvalu"                                 "Uzbekistan"                             "Vanuatu"                               
  # [69] "Vietnam"                                "Wallis and Futuna"        
  
  # sort(table(all_records[is.na(all_records$GRIIS),]$Country))
  
  
  ## fill some gaps with TDWG1 names 
  TDWG1 <- unique(all_records[,c("Country","TDWG1_name")])
  TDWG1 <- unique(subset(TDWG1,!is.na(TDWG1$TDWG1_name)))
  TDWG1$TDWG1_name[TDWG1$Country=="Chile"] <- "Southern America"
  TDWG1$TDWG1_name[TDWG1$Country=="French Polynesia"] <- "Pacific"
  TDWG1$TDWG1_name[TDWG1$Country=="Portugal"] <- "Europe"
  TDWG1$TDWG1_name[TDWG1$Country=="Russian Federation (the)"] <- "Asia-Temperate"
  TDWG1$TDWG1_name[TDWG1$Country=="Saint Helena, Ascension and Tristan da Cunha"] <- "Africa"
  TDWG1$TDWG1_name[TDWG1$Country=="South Africa"] <- "Africa"
  TDWG1$TDWG1_name[TDWG1$Country=="Spain"] <- "Europe"
  TDWG1$TDWG1_name[TDWG1$Country=="United States Minor Outlying Islands (the)"] <- "Pacific"
  TDWG1$TDWG1_name[TDWG1$Country=="United States of America (the)"] <- "Northern America"
  TDWG1 <- unique(TDWG1)
  all_records <- all_records[,-which(colnames(all_records)=="TDWG1_name")]
  all_records <- merge(all_records,TDWG1,by="Country",all.x=T)
  
  ## fill some gaps with continent names 
  Continent_names <- unique(all_records[,c("Country","Continent")])
  Continent_names <- subset(Continent_names,!is.na(Continent_names$Continent))
  Continent_names$Continent[Continent_names$Country=="French Southern Territories (the)"] <- "Antarctica"
  Continent_names$Continent[Continent_names$Country=="New Zealand"] <- "Australasia"
  Continent_names$Continent[Continent_names$Country=="Saint Helena, Ascension and Tristan da Cunha"] <- "Africa"
  Continent_names$Continent[Continent_names$Country=="United States of America (the)"] <- "North America"
  Continent_names <- unique(Continent_names)
  all_records <- all_records[,-which(colnames(all_records)=="Continent")]
  all_records <- merge(all_records,Continent_names,by="Country",all.x=T)
  
  ## fill some gaps with continent names 
  Continent1_names <- unique(all_records[,c("Country","Continent1")])
  Continent1_names <- subset(Continent1_names,!is.na(Continent1_names$Continent1))
  Continent1_names$Continent1[Continent1_names$Country=="Denmark"] <- "Europe"
  Continent1_names$Continent1[Continent1_names$Country=="French Southern Territories (the)"] <- "Antarctica"
  Continent1_names$Continent1[Continent1_names$Country=="New Zealand"] <- "Australasia"
  Continent1_names$Continent1[Continent1_names$Country=="Saint Helena, Ascension and Tristan da Cunha"] <- "Africa"
  Continent1_names$Continent1[Continent1_names$Country=="United States of America (the)"] <- "Northern America"
  Continent1_names <- unique(Continent1_names)
  all_records <- all_records[,-which(colnames(all_records)=="Continent1")]
  all_records <- merge(all_records,Continent1_names,by="Country",all.x=T)
  
  ## fill some gaps with continent names 
  Continent2_names <- unique(all_records[,c("Country","Continent2")])
  Continent2_names <- subset(Continent2_names,!is.na(Continent2_names$Continent2))
  ind <- which(duplicated(Continent2_names$Country))
  Continent2_names$Continent2[Continent2_names$Country=="Denmark"] <- "Europe"
  Continent2_names$Continent2[Continent2_names$Country=="French Southern Territories (the)"] <- "Antarctica"
  Continent2_names$Continent2[Continent2_names$Country=="New Zealand"] <- "Australasia"
  Continent2_names$Continent2[Continent2_names$Country=="Saint Helena, Ascension and Tristan da Cunha"] <- "Africa"
  Continent2_names$Continent2[Continent2_names$Country=="United States of America (the)"] <- "Northern America"
  Continent2_names <- unique(Continent2_names)
  
  all_records <- all_records[,-which(colnames(all_records)=="Continent2")]
  all_records <- merge(all_records,Continent2_names,by="Country",all.x=T)
  
  ## remove NAs for output...
  all_records[is.na(all_records)] <- ""
  
  ## harmonse life forms...
  # colnames(all_records)[colnames(all_records)=="LifeForm.y"] <- "LifeForm"
  all_records[all_records$GloNAF=="x",]$LifeForm <- "Vascular plants"
  all_records[all_records$GAVIA=="x",]$LifeForm <- "Birds"
  all_records[all_records$AmRep=="x" & all_records$Group=="Amphibia",]$LifeForm <- "Amphibians"
  all_records[all_records$AmRep=="x" & all_records$Group=="Reptilia",]$LifeForm <- "Reptiles"
  # all_records[is.na(all_records$LifeForm),]$LifeForm <- all_records[is.na(all_records$LifeForm),]$LifeForm.x
  # all_records$LifeForm.x <- NULL
  # ind <- is.na(all_records$LifeForm) & (all_records$GloNAF=="x" | all_records$GAVIA=="x" | all_records$FirstRecordDB=="x" | all_records$AmRep=="x")
  # all_records[ind,]
  
  all_records$LifeForm[all_records$LifeForm=="Vascular plant"] <- "Vascular plants"
  all_records$LifeForm[all_records$LifeForm=="Insect"] <- "Insects"
  all_records$LifeForm[all_records$LifeForm=="Bird"] <- "Birds"
  all_records$LifeForm[all_records$LifeForm=="Alga"] <- "Algae"
  all_records$LifeForm[all_records$LifeForm=="Amphibian"] <- "Amphibians"
  all_records$LifeForm[all_records$LifeForm=="Bacterium"] <- "Bacteria and protozoans"
  all_records$LifeForm[grep(" fish",all_records$LifeForm)] <- "Fishes"
  all_records$LifeForm[all_records$LifeForm=="Copepod"] <- "Crustacean"
  all_records$LifeForm[all_records$LifeForm=="Mammal"] <- "Mammals"
  all_records$LifeForm[all_records$LifeForm=="Mollusc"] <- "Molluscs"
  all_records$LifeForm[all_records$LifeForm=="Protozoan"] <- "Bacteria and protozoans"
  all_records$LifeForm[all_records$LifeForm=="Reptile"] <- "Reptiles"
  all_records$LifeForm[all_records$LifeForm=="Virus"] <- "Viruses"
  
  ## fill some gaps with lifeform names 
  lifeforms <- unique(all_records[,c("SpeciesGBIF","LifeForm")])
  lifeforms <- unique(subset(lifeforms,lifeforms$LifeForm!=""))
  # all_records[all_records$SpeciesGBIF=="Lecanosticta acicola",]
  
  ## quick-and-dirty solution to remove wrong entries of life forms; needs to be changed in first record dabase!
  ind <- which(duplicated(lifeforms$SpeciesGBIF))
  dupl_spec <- lifeforms[ind,]
  lifeforms$LifeForm[lifeforms$SpeciesGBIF%in%dupl_spec$SpeciesGBIF] <- ""
  lifeforms <- unique(subset(lifeforms,lifeforms$LifeForm!=""))

  all_records <- all_records[,-which(colnames(all_records)=="LifeForm")]
  all_records <- merge(all_records,lifeforms,by="SpeciesGBIF",all.x=T)
  
  all_records$LifeForm[is.na(all_records$LifeForm)] <- ""
  
  # table(is.na(all_records$LifeForm))
  # sort(names(table(all_records$LifeForm)))
  
  
  ## output #############################################
  
  ## remove NAs for output...
  all_records[is.na(all_records)] <- ""
  all_records$GRIISimpact_evidence[all_records$GRIISimpact_evidence=="Null"] <- ""

  all_records <- all_records[,c("SpeciesGBIF","LifeForm","Country","Status","Habitat","GRIIS","FirstRecordDB","GloNAF","GAVIA","AmRep","GRIISFirstRecord",
                                "FirstRecord","GRIISimpact_evidence","Country_ISO","TDWG1_name","Continent","Continent1","Continent2")]#
  
  write.table(all_records,paste("Data/AlienSpecies_MultipleDBs_Masterfile_vs",version,".csv",sep=""),sep=";",row.names=F)
}
