#########################################################################################
## Merging databases of alien species distribution and first records
## harmonisation of country names 
##
## Databases: GRIIS, GloNAF, FirstRecords, GAVIA, Alien amphibians and reptiles
##
## sTwist workshop
## Hanno Seebens, 28.03.2019
#########################################################################################


HarmoniseCountryNames <- function(output){
  
  
  ## loading databases...
  
  griis <- read.table("Data/GRIIS_GBIFharmonised.csv",header=T,stringsAsFactors = F,sep=";")
  colnames(griis) <- c("Country","OrigSpecies","Habitat","Status","GRIISimpact_evidence","GRIISFirstRecord","SpeciesGBIF")
  
  firstrecords <- read.table("Data/FirstRecords_GBIFharmonised.csv",stringsAsFactors = F,sep=";",header=T)#
  
  glonaf <- read.table("Data/GloNAF_GBIFharmonised.csv",stringsAsFactors = F,sep=";",header=T)
  
  gavia <- read.table("Data/GAVIA_GBIFharmonised.csv",stringsAsFactors = F,sep=";",header=T)
  
  amphrep <- read.table("Data/AmphRep_GBIFharmonised.csv",stringsAsFactors = F,sep=";",header=T)
  
  
  ################################################################################
  ## Harmonise country names accoring to ISO standards provided in GloNAF       ##
  
  
  glonaf$Country[glonaf$Country=="C“te d'Ivoire"] <- "Cote d'Ivoire"
  glonaf$Country[glonaf$Country=="R‚union"] <- "Reunion"
  glonaf$Country[glonaf$Country=="Western Sahara*"] <- "Western Sahara"
  glonaf <- glonaf[,c("SpeciesGBIF","OrigSpecies","Country","Country_ISO","TDWG1_name")]
  
  if (output) write.table(glonaf,"Data/GloNAF_HarmSpecCountry.csv",sep=";",row.names=F)
  
  
  
  ##### Harmonisation of first record DB and GloNAF ##############################
  firstrecords[firstrecords$Country=="UK Overseas",]$Country <- firstrecords[firstrecords$Country=="UK Overseas",]$Region
  firstrecords[firstrecords$Country=="French Overseas",]$Country <- firstrecords[firstrecords$Country=="French Overseas",]$Region
  firstrecords <- subset(firstrecords,Country!="USACanada")
  
  
  ## check missing names...
  fr_countnames <- sort(unique(firstrecords$Country))
  glonaf_countnames <- sort(unique(glonaf$Country))
  
  # missing <- fr_countnames[!fr_countnames%in%glonaf_countnames]
  # missing <- glonaf_countnames[!glonaf_countnames%in%fr_countnames]
  # missing[!is.na(pmatch(missing,glonaf_countnames))]
  
  ## set country names in first record database to standard GloNAF country names
  firstrecords$Country[firstrecords$Country=="Taiwan"] <- glonaf_countnames[grep("Taiwan",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Czech Republic"] <- glonaf_countnames[grep("Czech Republic",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="United Kingdom"] <- glonaf_countnames[grep("United Kingdom",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Russia"] <- glonaf_countnames[grep("Russia",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="United Arab Emirates"] <- glonaf_countnames[grep("United Arab Emirates",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="French Southern Territories"] <- glonaf_countnames[grep("French Southern Territories",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Bahamas"] <- glonaf_countnames[grep("Bahamas",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Dominican Republic"] <- glonaf_countnames[grep("Dominican Republic",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Philippines"] <- glonaf_countnames[grep("Philippines",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Comoros"] <- "Comoros (the)"
  firstrecords$Country[firstrecords$Country=="Cook Islands"] <- glonaf_countnames[grep("Cook Islands",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Marshall Islands"] <- glonaf_countnames[grep("Marshall Islands",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Bolivia"] <- glonaf_countnames[grep("Bolivia",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Tanzania"] <- glonaf_countnames[grep("Tanzania",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Northern Mariana Islands"] <- "Northern Mariana Islands (the)"
  firstrecords$Country[firstrecords$Country=="Venezuela"] <- glonaf_countnames[grep("Venezuela",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Moldova"] <- glonaf_countnames[grep("Moldova",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Central African Republic"] <- glonaf_countnames[grep("Central African Republic",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Sudan"] <- glonaf_countnames[grep("Sudan",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Iran"] <- glonaf_countnames[grep("Iran",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Macedonia"] <- glonaf_countnames[grep("Macedonia",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Gambia"] <- glonaf_countnames[grep("Gambia",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Western Sahara"] <- glonaf_countnames[grep("Western Sahara",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Niger"] <- glonaf_countnames[grep("Niger ",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Laos"] <- "Lao People's Democratic Republic (the)"
  firstrecords$Country[firstrecords$Country=="Cote D'Ivoire"] <- "Cote d'Ivoire"
  firstrecords$Country[firstrecords$Country=="Congo, Democratic Republic of the"] <- "Congo (the Democratic Republic of the)"
  firstrecords$Country[firstrecords$Country=="Rodriguez Island"] <- glonaf_countnames[grep("Mauritius",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Virgin Islands, US"] <- glonaf_countnames[grep("Virgin Islands \\(U",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="Cape Verde"] <- glonaf_countnames[grep("Cabo Verde",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="US Minor Outlying Islands"] <- "United States Minor Outlying Islands (the)"
  firstrecords$Country[firstrecords$Country=="South Korea"] <- "Korea (the Republic of)"
  firstrecords$Country[firstrecords$Country=="North Korea"] <- "Korea (the Democratic People's Republic of)"
  firstrecords$Country[firstrecords$Country=="Netherlands"] <- glonaf_countnames[grep("Netherlands \\(the\\)",glonaf_countnames)]
  firstrecords$Country[firstrecords$Country=="United States"] <- "United States of America (the)"
  firstrecords$Country[firstrecords$Country=="Saint Martin"] <- "Saint-Martin"
  firstrecords$Country[firstrecords$Country=="Saint Barthelemy"] <- "Saint-Barthelemy"
  firstrecords$Country[firstrecords$Country=="Reunion"] <- "Reunion"
  firstrecords$Country[firstrecords$Country=="Ascension"] <- "Saint Helena, Ascension and Tristan da Cunha"
  firstrecords$Country[firstrecords$Country=="Saint Helena"] <- "Saint Helena, Ascension and Tristan da Cunha"
  firstrecords$Country[firstrecords$Country=="Tristan da Cunha"] <- "Saint Helena, Ascension and Tristan da Cunha"
  firstrecords$Country[firstrecords$Country=="British Virgin Islands"] <- "Virgin Islands (British)"
  firstrecords$Country[firstrecords$Country=="Cayman Islands"] <- "Cayman Islands (the)"
  firstrecords$Country[firstrecords$Country=="Chagos Archipelago"] <- "British Indian Ocean Territory (the)"
  firstrecords$Country[firstrecords$Country=="Pitcairn Islands"] <- "Pitcairn"
  firstrecords$Country[firstrecords$Country=="Turks and Caicos"] <- "Turks and Caicos Islands (the)"
  firstrecords$Country[firstrecords$Region=="Curacao"] <- "Netherlands Antilles"
  firstrecords$Country[firstrecords$Region=="Bonaire"] <- "Netherlands Antilles"
  firstrecords$Country[firstrecords$Region=="Aruba"] <- "Netherlands Antilles"
  firstrecords$Country[firstrecords$Region=="Sint Maarten"] <- "Netherlands Antilles"
  firstrecords$Country[firstrecords$Region=="Cocos (Keeling) Islands"] <- "Cocos (Keeling) Islands (the)"
  firstrecords$Country[firstrecords$Region=="Faroe Islands"] <- "Faroe Islands (the)"
  firstrecords$Country[firstrecords$Region=="Svalbard and Jan Mayen"] <- "Svalbard and Jan Mayen"
  firstrecords$Country[firstrecords$Region=="Saint Pierre and Miquelon"] <- "Saint Pierre and Miquelon"
  firstrecords$Country[firstrecords$Region=="Hong Kong"] <- "Hong Kong"
  
  ## missing country names in GloNAF, but in FirstRecord DB:
  ## Tuvalu, Timor Leste, Niue, Cambodia, Bhutan, Honduras, Myanmar, Vietnam, San Marino, Andorra, El Salvador
  ## Palau, Sulawesi, Azerbaijan, Pakistan, Gibraltar, Monaco, Trinidad and Tobago, Syria, Montenegro, Guatemala
  ## Bosnia and Herzegovina, Solomon Islands, Micronesia, Federated States of, Lebanon, Jordan, Iraq,
  ## Brunei Darussalam, Kuwait, Bahrain, Serbia, Vanuatu, Uzbekistan, Turkmenistan, Kazakhstan, Wallis and Futuna
  ## Saint Barthelemy, Saint Martin, Falkland Islands, 
  ## St Barthelemy and St Martin are in GloNAF part of Guadeloupe! On purpose???
  ## Clipperton Island is in GloNAF part of French Polynesia! On purpose??? (EDIT: Officially, clipperton island is part of France. For biogeographical reasons, it make sense to add it to French Polynesia.)
  ## In GloNAF, both Aruba and Netherlands Antilles are included although Aruba is part of Netherlands Antilles...
  
  write.table(unique(firstrecords[,c("Country","Region")]),"Data/CountryRegionsTranslation.csv",sep=";",row.names = F) # necessary for harmonisation of country names after extraction of GBIF records
  
  # ### keep only earliest first record for a country ####
  # ## using R library data.table because of performance
  dt <- data.table(firstrecords,key=c("OrigSpecies","SpeciesGBIF","LifeForm","Country","Continent","Continent1","Continent2"))
  dt_min <- dt[,list(FirstRecord=min(FirstRecord)),by=c("OrigSpecies","SpeciesGBIF","LifeForm","Country","Continent","Continent1","Continent2")]
  firstrecords <- as.data.frame(dt_min)
  
  firstrecords <- firstrecords[!duplicated(firstrecords),]
  firstrecords <- firstrecords[,c("SpeciesGBIF","OrigSpecies","LifeForm","FirstRecord","Country","Continent","Continent1","Continent2")]
  
  
  if (output) write.table(firstrecords,"Data/FirstRecords_HarmSpecCountry.csv",sep=";",row.names=F)
  
  
  
  ##### Harmonisation of GRIIS and GloNAF ##############################
  
  griis$Country <- gsub("^\\s+|\\s+$", "",griis$Country) # trim leading and trailing whitespace
  griis$Country <- gsub(" ", "",griis$Country) # trim leading and trailing whitespace
  griis$Species <- gsub("^\\s+|\\s+$", "",griis$Species) # trim leading and trailing whitespace
  
  ## check missing names
  griis_countnames <- sort(unique(griis$Country))
  glonaf_countnames <- unique(glonaf$Country)
  
  # missing <- griis_countnames[!griis_countnames%in%glonaf_countnames]
  # missing <- griis_countnames[!griis_countnames%in%fr_countnames]
  # # missing <- glonaf_countnames[!glonaf_countnames%in%griis_countnames]
  # missing[!is.na(pmatch(missing,glonaf_countnames))]
  
  griis$Country[griis$Country=="Bahamas"] <- glonaf_countnames[grep("Bahamas",glonaf_countnames)]
  griis$Country[griis$Country=="Bolivia"] <- glonaf_countnames[grep("Bolivia",glonaf_countnames)]
  griis$Country[griis$Country=="Central African Republic"] <- glonaf_countnames[grep("Central African Republic",glonaf_countnames)]
  griis$Country[griis$Country=="Central African Republic, the Congo"] <- glonaf_countnames[grep("Central African Republic",glonaf_countnames)]
  griis$Country[griis$Country=="Comoros"] <- "Comoros (the)"
  griis$Country[griis$Country=="Cook Islands"] <- glonaf_countnames[grep("Cook Islands",glonaf_countnames)]
  griis$Country[griis$Country=="Czech Republic"] <- glonaf_countnames[grep("Czech Republic",glonaf_countnames)]
  griis$Country[griis$Country=="Dominican Republic"] <- glonaf_countnames[grep("Dominican Republic",glonaf_countnames)]
  griis$Country[griis$Country=="Marshall Islands"] <- glonaf_countnames[grep("Marshall Islands",glonaf_countnames)]
  griis$Country[griis$Country=="Philippines"] <- glonaf_countnames[grep("Philippines",glonaf_countnames)]
  griis$Country[griis$Country=="Russian Federation"] <- glonaf_countnames[grep("Russian Federation",glonaf_countnames)]
  griis$Country[griis$Country=="Netherlands"] <- glonaf_countnames[grep("Netherlands \\(",glonaf_countnames)]
  griis$Country[griis$Country=="Niger"] <- glonaf_countnames[grep("Niger ",glonaf_countnames)]
  griis$Country[griis$Country=="Chile- Juan Fernandez Islands"] <- "Chile"
  griis$Country[griis$Country=="Chile-Rapa Nui - Easter Islands"] <- "Chile"
  griis$Country[griis$Country=="Côte d'Ivoire"] <- "Cote d'Ivoire"
  griis$Country[griis$Country=="Democratic Republic of the Congo"] <- "Congo (the Democratic Republic of the)"
  griis$Country[griis$Country=="Democratic Republic of Congo"] <- "Congo (the Democratic Republic of the)"
  griis$Country[griis$Country=="DR Congo"] <- "Congo (the Democratic Republic of the)"
  griis$Country[griis$Country=="Congo"] <- "Congo (the)"
  griis$Country[griis$Country=="Republic of Korea"] <- "Korea (the Republic of)"
  griis$Country[griis$Country=="Democratic Repubic of Korea"] <- "Korea (the Democratic People's Republic of)"
  griis$Country[griis$Country=="Saint Vicent and the Grenadines"] <- "Saint Vincent and the Grenadines"
  griis$Country[griis$Country=="Republic of Moldova"] <- "Moldova (the Republic of)"
  griis$Country[griis$Country=="Gambia"] <- "Gambia (the)"
  griis$Country[griis$Country=="Guinea Bissau"] <- "Guinea-Bissau"
  griis$Country[griis$Country=="Iran"] <- "Iran (Islamic Republic of)"
  griis$Country[griis$Country=="Macedonia"] <- "Macedonia (the former Yugoslav Republic of)"
  griis$Country[griis$Country=="Moldova"] <- "Moldova (the Republic of)"
  griis$Country[griis$Country=="Palestine"] <- "Palestinian Territory, Occupied"
  griis$Country[griis$Country=="St. Kitts and Nevis"] <- "Saint Kitts and Nevis"
  griis$Country[griis$Country=="Sudan"] <- "Sudan (the)"
  griis$Country[griis$Country=="Syrian Arab Republic"] <- "Syria"
  griis$Country[griis$Country=="Tanzania"] <- "Tanzania, United Republic of"
  griis$Country[griis$Country=="United Arab Emirates"] <- "United Arab Emirates (the)"
  griis$Country[griis$Country=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland (the)"
  griis$Country[griis$Country=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
  griis$Country[griis$Country=="Yemen- Soqotra"] <- "Yemen"
  griis$Country[griis$Country=="Taiwan"] <- "Taiwan (Province of China)"
  griis$Country[griis$Country=="United States of America"] <- "United States of America (the)"
  griis$Country[griis$Country=="Cape Verde"] <- "Cabo Verde"
  
  griis$Country[grep("Liechtenstein",griis$Country)] <- "Liechtenstein"
  griis$Country[grep("Luxembourg",griis$Country)] <- "Luxembourg"
  griis$Country[grep("Malaysia",griis$Country)] <- "Malaysia"
  griis$Country[grep("Micronesia \\(Federated States of\\)",griis$Country)] <- "Micronesia, Federated States of"
  griis$Country[grep("Federated States of Micronesia",griis$Country)] <- "Micronesia, Federated States of"
  griis$Country[grep("Lao People's Democratic Republic",griis$Country)] <- "Lao People's Democratic Republic (the)"
  
   
  griis <- griis[!duplicated(griis),]
  griis <- griis[,c("SpeciesGBIF","OrigSpecies","Country","Habitat","Status","GRIISimpact_evidence","GRIISFirstRecord")]

  if (output) write.table(griis,"Data/GRIIS_HarmSpecCountry.csv",sep=";",row.names=F)
  
  
  
  ##### Harmonisation of GAVIA ##############################
  
  ## check missing names
  gavia_countnames <- sort(unique(gavia$Country))
  glonaf_countnames <- sort(unique(glonaf$Country))
  
  # missing <- fr_countnames[!fr_countnames%in%glonaf_countnames]
  # missing <- glonaf_countnames[!glonaf_countnames%in%fr_countnames]
  # missing[!is.na(pmatch(missing,glonaf_countnames))]
  
  gavia$Country[gavia$Country=="Bahamas"] <- glonaf_countnames[grep("Bahamas",glonaf_countnames)]
  gavia$Country[gavia$Country=="Bolivia"] <- glonaf_countnames[grep("Bolivia",glonaf_countnames)]
  # gavia$Country[gavia$Country=="Central African Republic"] <- glonaf_countnames[grep("Central African Republic",glonaf_countnames)]
  gavia$Country[gavia$Country=="Comoros"] <- "Comoros (the)"
  gavia$Country[gavia$Country=="Cook Islands"] <- glonaf_countnames[grep("Cook Islands",glonaf_countnames)]
  gavia$Country[gavia$Country=="Czech Republic"] <- glonaf_countnames[grep("Czech Republic",glonaf_countnames)]
  gavia$Country[gavia$Country=="Dominican Republic"] <- glonaf_countnames[grep("Dominican Republic",glonaf_countnames)]
  gavia$Country[gavia$Country=="Marshall Islands"] <- glonaf_countnames[grep("Marshall Islands",glonaf_countnames)]
  gavia$Country[gavia$Country=="Philippines"] <- glonaf_countnames[grep("Philippines",glonaf_countnames)]
  gavia$Country[gavia$Country=="Russian Federation"] <- glonaf_countnames[grep("Russian Federation",glonaf_countnames)]
  gavia$Country[gavia$Country=="Netherlands"] <- glonaf_countnames[grep("Netherlands \\(",glonaf_countnames)]
  gavia$Country[gavia$Country=="Niger"] <- glonaf_countnames[grep("Niger ",glonaf_countnames)]
  gavia$Country[gavia$Country=="British Indian Ocean Territory"] <- glonaf_countnames[grep("British Indian Ocean Territory",glonaf_countnames)]
  gavia$Country[gavia$Country=="Cape Verde"] <- glonaf_countnames[grep("Cabo Verde",glonaf_countnames)]
  gavia$Country[gavia$Country=="Cayman Islands"] <- "Cayman Islands (the)"
  gavia$Country[gavia$Country=="Cocos (Keeling) Islands"] <- "Cocos (Keeling) Islands (the)"
  gavia$Country[gavia$Country=="Congo"] <- "Congo (the)"
  gavia$Country[gavia$Country=="Côte D'ivoire"] <- "Cote d'Ivoire"
  gavia$Country[gavia$Country=="Congo, Democratic Republic of the"] <- "Congo (the Democratic Republic of the)"
  gavia$Country[gavia$Country=="Faroe Islands"] <- "Faroe Islands (the)"
  gavia$Country[gavia$Country=="French Southern Territories"] <- "French Southern Territories (the)"
  gavia$Country[gavia$Country=="Gambia"] <- "Gambia (the)"
  gavia$Country[gavia$Country=="Iran"] <- "Iran (Islamic Republic of)"
  gavia$Country[gavia$Country=="Laos"] <- "Lao People's Democratic Republic (the)"
  gavia$Country[gavia$Country=="Macedonia"] <- "Macedonia (the former Yugoslav Republic of)"
  gavia$Country[gavia$Country=="Northern Mariana Islands"] <- "Northern Mariana Islands (the)"
  gavia$Country[gavia$Country=="North Korea"] <- "Korea (the Democratic People's Republic of)"
  gavia$Country[gavia$Country=="Pitcairn Islands"] <- "Pitcairn"
  gavia$Country[gavia$Country=="Russia"] <- "Russian Federation (the)"
  gavia$Country[gavia$Country=="Saint Helena"] <- "Saint Helena, Ascension and Tristan da Cunha"
  gavia$Country[gavia$Country=="Åland Islands"] <- "Finland"
  gavia$Country[gavia$Country=="South Korea"] <- "Korea (the Republic of)"
  gavia$Country[gavia$Country=="Sudan"] <- "Sudan (the)"
  gavia$Country[gavia$Country=="Taiwan"] <- "Taiwan (Province of China)"
  gavia$Country[gavia$Country=="Tanzania"] <- "Tanzania, United Republic of"
  gavia$Country[gavia$Country=="Turks and Caicos"] <- "Turks and Caicos Islands (the)"
  gavia$Country[gavia$Country=="United Arab Emirates"] <- "United Arab Emirates (the)"
  gavia$Country[gavia$Country=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland (the)"
  gavia$Country[gavia$Country=="United States"] <- "United States of America (the)"
  gavia$Country[gavia$Country=="US Minor Outlying Islands"] <- "United States Minor Outlying Islands (the)"
  gavia$Country[gavia$Country=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
  gavia$Country[gavia$Country=="Virgin Islands, US"] <- "Virgin Islands (U.S.)"
  gavia$Country[gavia$Country=="Moldova"] <- "Moldova (the Republic of)"
  gavia$Country[gavia$Country=="British Virgin Islands"] <- "Virgin Islands (British)"
  
  gavia <- gavia[!duplicated(gavia),]
  gavia <- gavia[,c("SpeciesGBIF","OrigSpecies","Country")]
  
  if (output) write.table(gavia,"Data/GAVIA_HarmSpecCountry.csv",sep=";",row.names=F)
  
  
  
  
  ##### Harmonisation of amphibians and reptiles ##############################
  
  amphrep <- amphrep[, c("SpeciesGBIF","OrigSpecies","Code","Group")]
  amphrep <- subset(amphrep,!is.na(amphrep$Species))
  
  glonaf_regions_all <- read.table("Data/GloNAF_Regions.csv",stringsAsFactors = F,sep=";",header=T)
  glonaf_regions <- unique(glonaf_regions_all[,c("tdwg4","country")])
  colnames(glonaf_regions) <- c("tdwg4","Country")
  
  ## translate twdg4 codes into country names....
  amphrep_country <- merge(amphrep,glonaf_regions,by.x="Code",by.y="tdwg4",all.x=T)
  
  amphrep_country$Country[amphrep_country$Code=="AGE-CO"] <- "Argentina"
  amphrep_country$Country[amphrep_country$Code=="Brazil"] <- "Brazil"
  amphrep_country$Country[amphrep_country$Code=="Chile"] <- "Chile"
  amphrep_country$Country[amphrep_country$Code=="India"] <- "India"
  amphrep_country$Country[amphrep_country$Code=="Indonesia"] <- "Indonesia"
  amphrep_country$Country[amphrep_country$Code=="Japan"] <- "Japan"
  amphrep_country$Country[amphrep_country$Code=="China"] <- "China"
  amphrep_country$Country[amphrep_country$Code=="New Zealand"] <- "New Zealand"
  amphrep_country$Country[amphrep_country$Code=="Russia"] <- "Russian Federation (the)"
  amphrep_country$Country[amphrep_country$Code=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland (the)"
  amphrep_country$Country[amphrep_country$Code=="ALD-OO"] <- "Seychelles"
  amphrep_country$Country[amphrep_country$Code=="AND-AN"] <- "Andaman and Nicobar"
  amphrep_country$Country[amphrep_country$Code=="CBD-OO"] <- "Cambodia"
  amphrep_country$Country[amphrep_country$Code=="CRL-MF"] <- "Micronesia, Federated States of"
  amphrep_country$Country[amphrep_country$Code=="CRL-PA"] <- "Palau"
  amphrep_country$Country[amphrep_country$Code=="CZE-SL"] <- "Slovakia"
  amphrep_country$Country[amphrep_country$Code=="EHM-BH"] <- "Bhutan"
  amphrep_country$Country[amphrep_country$Code=="ELS-OO"] <- "El Salvador"
  amphrep_country$Country[amphrep_country$Code=="GGI-PR"] <- "Sao Tome and Principe"
  amphrep_country$Country[amphrep_country$Code=="GGI-ST"] <- "Sao Tome and Principe"
  amphrep_country$Country[amphrep_country$Code=="GST-BA"] <- "Bahrain"
  amphrep_country$Country[amphrep_country$Code=="GUA-OO"] <- "Guatemala"
  amphrep_country$Country[amphrep_country$Code=="HON-OO"] <- "Honduras"
  amphrep_country$Country[amphrep_country$Code=="IRQ-OO"] <- "Iraq"
  amphrep_country$Country[amphrep_country$Code=="KUW-OO"] <- "Kuwait"
  amphrep_country$Country[amphrep_country$Code=="LSI-ET"] <- "East Timor"
  amphrep_country$Country[amphrep_country$Code=="NCB-OO"] <- "Andaman and Nicobar"
  amphrep_country$Country[amphrep_country$Code=="NFK-LH"] <- "Norfolk Islands"
  amphrep_country$Country[amphrep_country$Code=="NUE-OO"] <- "Niue"
  amphrep_country$Country[amphrep_country$Code=="NWG-PN"] <- "Papua New Guinea"
  amphrep_country$Country[amphrep_country$Code=="PAK-OO"] <- "Pakistan"
  amphrep_country$Country[amphrep_country$Code=="SOC-OO"] <- "Society Islands"
  amphrep_country$Country[amphrep_country$Code=="TCI-OO"] <- "Turks and Caicos Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="TRT-OO"] <- "Trinidad and Tobago"
  amphrep_country$Country[amphrep_country$Code=="TUR-OO"] <- "Turkey"
  amphrep_country$Country[amphrep_country$Code=="TUV-OO"] <- "Tuvalu"
  amphrep_country$Country[amphrep_country$Code=="VAN-OO"] <- "Vanuatu"
  amphrep_country$Country[amphrep_country$Code=="VIE-OO"] <- "Vietnam"
  amphrep_country$Country[amphrep_country$Code=="WAL-OO"] <- "Wallis and Futuna"
  amphrep_country$Country[amphrep_country$Code=="YUG-MN"] <- "Montenegro"
  amphrep_country$Country[amphrep_country$Country=="Cayman Islands"] <- "Cayman Islands (the)"
  amphrep_country$Country[amphrep_country$Country=="British Indian Ocean Territory"] <- "British Indian Ocean Territory (the)"
  amphrep_country$Country[amphrep_country$Country=="Cocos (Keeling) Islands"] <- "Cocos (Keeling) Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="Comoros"] <- "Comoros (the)"
  amphrep_country$Country[amphrep_country$Code=="Congo"] <- "Congo (the)"
  amphrep_country$Country[amphrep_country$Code=="Congo, Democratic Republic of the"] <- "Congo (the Democratic Republic of the)"
  amphrep_country$Country[amphrep_country$Code=="Cook Islands"] <- "Cook Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="Côte D'ivoire"] <- "Cote d'Ivoire"
  amphrep_country$Country[amphrep_country$Code=="Dominican Republic"] <- "Dominican Republic (the)"
  amphrep_country$Country[amphrep_country$Code=="Faroe Islands"] <- "Faroe Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="French Southern Territories"] <- "French Southern Territories (the)"
  amphrep_country$Country[amphrep_country$Code=="Gambia"] <- "Gambia (the)"
  amphrep_country$Country[amphrep_country$Code=="Iran"] <- "Iran (Islamic Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Laos"] <- "Lao People's Democratic Republic (the)"
  amphrep_country$Country[amphrep_country$Code=="Macedonia"] <- "Macedonia (the former Yugoslav Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Marshall Islands"] <- "Marshall Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="Moldova"] <- "Moldova (the Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Netherlands"] <- "Netherlands (the)"
  amphrep_country$Country[amphrep_country$Code=="Niger"] <- "Niger (the)"
  amphrep_country$Country[amphrep_country$Code=="Northern Mariana Islands"] <- "Northern Mariana Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="North Korea"] <- "Korea (the Democratic People's Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Philippines"] <- "Philippines (the)"
  amphrep_country$Country[amphrep_country$Code=="Pitcairn Islands"] <- "Pitcairn"
  amphrep_country$Country[amphrep_country$Code=="South Korea"] <- "Korea (the Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Sudan"] <- "Sudan (the)"
  amphrep_country$Country[amphrep_country$Code=="Tanzania"] <- "Tanzania, United Republic of"
  amphrep_country$Country[amphrep_country$Code=="Turks and Caicos"] <- "Turks and Caicos Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="United Arab Emirates"] <- "United Arab Emirates (the)"
  amphrep_country$Country[amphrep_country$Code=="United States"] <- "United States of America"
  amphrep_country$Country[amphrep_country$Code=="US Minor Outlying Islands"] <- "United States Minor Outlying Islands (the)"
  amphrep_country$Country[amphrep_country$Code=="Venezuela"] <- "Venezuela (Bolivarian Republic of)"
  amphrep_country$Country[amphrep_country$Code=="Virgin Islands, US"] <- "Virgin Islands (U.S.)"
  amphrep_country$Country[amphrep_country$Code=="British Virgin Islands"] <- "Virgin Islands (British)"
  
  amphrep_country$Country[amphrep_country$Country=="C“te d'Ivoire"] <- "Cote d'Ivoire"
  amphrep_country$Country[amphrep_country$Country=="R‚union"] <- "Reunion"
  
  ## check missing names
  
  # ind <- is.na(amphrep_country$Country)
  # table(amphrep_country$Code[ind])
  
  # amphrep_countnames <- sort(unique(amphrep_country$Country))
  # glonaf_countnames <- unique(glonaf$Country)
  
  # missing <- amphrep_countnames[!amphrep_countnames%in%glonaf_countnames]
  # missing <- glonaf_countnames[!glonaf_countnames%in%amphrep_countnames]
  # missing[!is.na(pmatch(missing,glonaf_countnames))]

  amphrep_country <- amphrep_country[,c("SpeciesGBIF","OrigSpecies","Group","Country")]
  amphrep_country <- amphrep_country[!duplicated(amphrep_country),]
  
  if (output) write.table(amphrep_country,"Data/AmphRep_HarmSpecCountry.csv",sep=";",row.names=F)
  
}

