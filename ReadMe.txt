#########################################################################################
## Merging databases of alien species distribution and first records
##
## sTwist workshop
## Hanno Seebens, 30.10.2019
#########################################################################################

The R scripts contain the implementation of a workflow to standardise and merge 
databases of alien species occurrences and years of first record. The workflow is 
described in detail in Seebens et al......

The R scripts can be used and modified freely as long as the work is properly cited
with the aforementioned citation.

In brief, the workflow consists of the following five steps:
1. step: prepare column names of secies databases  
2. step: standardisation of species names using GBIF backbone taxonomy; user-defined
         species names and taxonomic information can be added afterwards
3. step: standardisation of region names
4. step: standardisation of first records
5. step: merging all databases

Input: 
Information about databases has to provided in DatabaseInfo.xlsx.
Modification of country names, species names and rules to treat first records can be
done in UserDefinedSpeciesNames.xlsx, AllRegionsList.xlsx, SubspecIdentifier.xlsx 
and Guidelines_FirstRecords.xlsx
Note that only the first sheet of the Excel file is read in. Others are ignored.

Output: 
A standardised masterfile built from all databases.
The masterfile is called AlienSpecies_MultipleDBs_Masterfile_[version].csv and contains a 
column for each database with an 'x' indicating the respective database.
Intermediate output files provide information about species names not resoled at
GBIF, a full list of species names with taxonomic information, missing country names,
list of original and new country names and a list of unclear first records.
Additional output from each step can be stored if 'Output' is set to TRUE. This is 
required if step are run individually.
