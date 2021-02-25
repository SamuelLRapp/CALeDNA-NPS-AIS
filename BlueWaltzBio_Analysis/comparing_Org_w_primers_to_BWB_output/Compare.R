### Samuel Rapp 1/18/21
###I want to see if which AIS species with existing primers appear in the CRUX databases and NCBI
####

#import data
??setdir
getwd()
setwd(Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/comparing_Org_w_primers_to_BWB_output)
NCBI_SPPgenus<-read.csv('input/input_bwb/CovM_NCBI_SPPgenus_NPS.csv.csv')
CRUX_SPPgenus<-read.csv('input/input_bwb/crux_nps.csv')
Primer_SPPgenus<-read.csv('input/input_species_with_existing_primers/ais-species_with_existing-primers-PCR-primer pairs.csv')

#convert primer_spp-dataframe species_names to GenBank Ncbi names using taxize


#convert primer_species_dataframe locus column to same formatting as CRUX/NCBI
  ###this can be done with annotatng_tree.R functons and GREP but it won't be supre clean.....

###maybe i should just search these species and their gene's in BWB...

#create a 4 column dataframe
#species with data in crux databases, species with equivalent data on ncbi
#1. Species/barcode-Gene Combo found in NCBI and CRUX search
#2. Species/barcode-Gene Combo found in NCBI not in CRUX search
#3. Species/barcode-Gene Combo not found in NCBI but found in CRUX search
#4. Species/barcode-Gene Combo not found in either NCBI or CRUX search

###convert BWB input data into long format, remove the rows where column = 0, and then search through it using the original....
##for loop  ####fill 4 column dataframe