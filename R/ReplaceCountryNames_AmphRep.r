ReplaceCountryNames_AmphRep <- function(cdat) {

  glonaf_regions_all <- read.xlsx("Inputfiles/GloNAF_Regions.xlsx")
  glonaf_regions <- unique(glonaf_regions_all[,c("tdwg4","country")])
  colnames(glonaf_regions) <- c("tdwg4","Country")
  
  # colnames(cdat)[3] <- "Code"
  
  ## translate twdg4 codes into country names....
  ind_match <- match(cdat$Code,glonaf_regions$tdwg4)
  allcounts <- glonaf_regions$Country[ind_match]
  ind_NA <- is.na(allcounts)
  cdat$Region_name_orig[!ind_NA] <- allcounts[!ind_NA]
  
  # cdat <- merge(cdat,glonaf_regions,by.x="Code",by.y="tdwg4",all.x=T,sort=F)

  cdat$Region_name_orig[cdat$Code=="AGE-CO"] <- "Argentina"
  cdat$Region_name_orig[cdat$Region_name_orig=="Australian Capital Territory"] <- "Australia"
  # cdat$Region_name_orig[cdat$Code=="Chile"] <- "Chile"
  # cdat$Region_name_orig[cdat$Code=="India"] <- "India"
  # cdat$Region_name_orig[cdat$Code=="Indonesia"] <- "Indonesia"
  # cdat$Region_name_orig[cdat$Code=="Japan"] <- "Japan"
  # cdat$Region_name_orig[cdat$Code=="China"] <- "China"
  # cdat$Region_name_orig[cdat$Code=="New Zealand"] <- "New Zealand"
  # cdat$Region_name_orig[cdat$Code=="Russia"] <- "Russian Federation (the)"
  # cdat$Region_name_orig[cdat$Code=="United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland (the)"
  cdat$Region_name_orig[cdat$Code=="ALD-OO"] <- "Seychelles"
  cdat$Region_name_orig[cdat$Code=="AND-AN"] <- "India"
  # cdat$Region_name_orig[cdat$Code=="CBD-OO"] <- "Cambodia"
  cdat$Region_name_orig[cdat$Code=="CRL-MF"] <- "Micronesia, Federated States of"
  # cdat$Region_name_orig[cdat$Code=="CRL-PA"] <- "Palau"
  # cdat$Region_name_orig[cdat$Code=="CZE-SL"] <- "Slovakia"
  # cdat$Region_name_orig[cdat$Code=="EHM-BH"] <- "Bhutan"
  # cdat$Region_name_orig[cdat$Code=="ELS-OO"] <- "El Salvador"
  cdat$Region_name_orig[cdat$Code=="GGI-PR"] <- "Sao Tome and Principe"
  cdat$Region_name_orig[cdat$Code=="GGI-ST"] <- "Sao Tome and Principe"
  cdat$Region_name_orig[cdat$Code=="GST-BA"] <- "Bahrain"
  # cdat$Region_name_orig[cdat$Code=="GUA-OO"] <- "Guatemala"
  # cdat$Region_name_orig[cdat$Code=="HON-OO"] <- "Honduras"
  cdat$Region_name_orig[cdat$Code=="IRQ-OO"] <- "Iraq"
  cdat$Region_name_orig[cdat$Code=="KUW-OO"] <- "Kuwait"
  # cdat$Region_name_orig[cdat$Code=="LSI-ET"] <- "East Timor"
  # cdat$Region_name_orig[cdat$Code=="NCB-OO"] <- "Andaman and Nicobar"
  cdat$Region_name_orig[cdat$Code=="NFK-LH"] <- "Norfolk Islands"
  cdat$Region_name_orig[cdat$Code=="NUE-OO"] <- "Niue"
  # cdat$Region_name_orig[cdat$Code=="NWG-PN"] <- "Papua New Guinea"
  # cdat$Region_name_orig[cdat$Code=="PAK-OO"] <- "Pakistan"
  cdat$Region_name_orig[cdat$Code=="SOC-OO"] <- "Yemen"
  # cdat$Region_name_orig[cdat$Code=="TCI-OO"] <- "Turks and Caicos Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="TRT-OO"] <- "Trinidad and Tobago"
  # cdat$Region_name_orig[cdat$Code=="TUR-OO"] <- "Turkey"
  # cdat$Region_name_orig[cdat$Code=="TUV-OO"] <- "Tuvalu"
  # cdat$Region_name_orig[cdat$Code=="VAN-OO"] <- "Vanuatu"
  # cdat$Region_name_orig[cdat$Code=="VIE-OO"] <- "Vietnam"
  # cdat$Region_name_orig[cdat$Code=="WAL-OO"] <- "Wallis and Futuna"
  # cdat$Region_name_orig[cdat$Code=="YUG-MN"] <- "Montenegro"
  # cdat$Region_name_orig[cdat$Region_name_orig=="Cayman Islands"] <- "Cayman Islands (the)"
  # cdat$Region_name_orig[cdat$Region_name_orig=="British Indian Ocean Territory"] <- "British Indian Ocean Territory (the)"
  # cdat$Region_name_orig[cdat$Region_name_orig=="Cocos (Keeling) Islands"] <- "Cocos (Keeling) Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="Comoros"] <- "Comoros (the)"
  # cdat$Region_name_orig[cdat$Code=="Congo"] <- "Congo (the)"
  # cdat$Region_name_orig[cdat$Code=="Congo, Democratic Republic of the"] <- "Congo (the Democratic Republic of the)"
  # cdat$Region_name_orig[cdat$Code=="Cook Islands"] <- "Cook Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="Côte D'ivoire"] <- "Cote d'Ivoire"
  # cdat$Region_name_orig[cdat$Code=="Dominican Republic"] <- "Dominican Republic (the)"
  # cdat$Region_name_orig[cdat$Code=="Faroe Islands"] <- "Faroe Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="French Southern Territories"] <- "French Southern Territories (the)"
  # cdat$Region_name_orig[cdat$Code=="Gambia"] <- "Gambia (the)"
  # cdat$Region_name_orig[cdat$Code=="Iran"] <- "Iran (Islamic Republic of)"
  # cdat$Region_name_orig[cdat$Code=="Laos"] <- "Lao People's Democratic Republic (the)"
  # cdat$Region_name_orig[cdat$Code=="Macedonia"] <- "Macedonia (the former Yugoslav Republic of)"
  # cdat$Region_name_orig[cdat$Code=="Marshall Islands"] <- "Marshall Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="Moldova"] <- "Moldova (the Republic of)"
  # cdat$Region_name_orig[cdat$Code=="Netherlands"] <- "Netherlands (the)"
  # cdat$Region_name_orig[cdat$Code=="Niger"] <- "Niger (the)"
  # cdat$Region_name_orig[cdat$Code=="Northern Mariana Islands"] <- "Northern Mariana Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="North Korea"] <- "Korea (the Democratic People's Republic of)"
  # cdat$Region_name_orig[cdat$Code=="Philippines"] <- "Philippines (the)"
  # cdat$Region_name_orig[cdat$Code=="Pitcairn Islands"] <- "Pitcairn"
  # cdat$Region_name_orig[cdat$Code=="South Korea"] <- "Korea (the Republic of)"
  # cdat$Region_name_orig[cdat$Code=="Sudan"] <- "Sudan (the)"
  # cdat$Region_name_orig[cdat$Code=="Tanzania"] <- "Tanzania, United Republic of"
  # cdat$Region_name_orig[cdat$Code=="Turks and Caicos"] <- "Turks and Caicos Islands (the)"
  # cdat$Region_name_orig[cdat$Code=="United Arab Emirates"] <- "United Arab Emirates (the)"
  # cdat$Region_name_orig[cdat$Code=="United States"] <- "United States of America"
  # cdat$Region_name_orig[cdat$Code=="US Minor Outlying Islands"] <- "United States Minor Outlying Islands (the)"
  cdat$Region_name_orig[cdat$Code=="Bolivia (Plurinational State of)"] <- "Bolivia"
  # cdat$Region_name_orig[cdat$Code=="Virgin Islands, US"] <- "Virgin Islands (U.S.)"
  # cdat$Region_name_orig[cdat$Code=="British Virgin Islands"] <- "Virgin Islands (British)"
  
  cdat$Region_name_orig[cdat$Region_name_orig=="C“te d'Ivoire"] <- "Cote d'Ivoire"
  cdat$Region_name_orig[cdat$Region_name_orig=="R‚union"] <- "Reunion"  
  cdat$Region_name_orig[cdat$Region_name_orig=="Western Sahara*"] <- "Western Sahara"  
  
  cdat$Region_name_orig <- gsub(" \\(the\\)","",cdat$Region_name_orig)
  cdat$Region_name_orig <- gsub(" Is\\."," Islands",cdat$Region_name_orig)
  cdat$Region_name_orig <- gsub(" I\\."," Island",cdat$Region_name_orig)
  
  # grep(" \\(the\\)",cdat$Region_name_orig)
  # cdat$Region_name_orig <- cdat$Country
  
  return(cdat)
}

