library(taxize)
library(rentrez)


# importing species list --------------------------------------------------

fileBig <- file.choose()
GoogleSheetData <- read.csv(file = fileBig) #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
#GoogleSheetData <- read.csv(file = "CALeDNA-NPS_aquatic-invasive_species_eDNA-primers_SPECIES_ANALYSIS.csv") #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
class(GoogleSheetData) #its a dataframe
#GoogleSheetData2 <- GoogleSheetData#[1:100,]#[1:5,] #SUBSET THE dataframe to do testing 

taxonomy_dataframe<-Generate_DF_All_Ranks_from_latin_species_list(GoogleSheetData)
unique<-unique(taxonomy_dataframe)# I recommeend to make sure all the content is unique


# function to generate dataframe with all taxonomic ranks -----------------


Generate_DF_All_Ranks_from_latin_species_list <- function( 
  Species_name_DataFrame #A dataframe where the first Column contains one species-Genus latin name per row
  #returns a dataframe with the columns speciesgenus", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format"
  #the "speciesgenus" column is created using gnr_resolve(), the following columns are created with tax_name() from the taxize package
#Note: tax_name() may cause two names like Sphacelaria fluviatilis & Sphacelaria lacustris to both become Sphacelaria, thus leading to duplicate data. 
#This is caused by NCBI's taxonomy missing namess
  )
{
  GoogleSheetData <-  Species_name_DataFrame #because I wrote the code with GoogleSheetData prior
  num_rows <- nrow(GoogleSheetData) #get row count
  column_names <- c("speciesgenus", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format")
  num_colm <- length(column_names) #8
  taxonomy_dataframe <- data.frame(matrix(ncol=num_colm, nrow = 0))
  colnames(taxonomy_dataframe) <- column_names #set column names
  
  for(i in 1:num_rows) #fill out the first column with  "proper names"
  {
    gnr <- gnr_resolve(sci = GoogleSheetData[i,1], data_source_ids = 4) #hopefully solve formatting, spelling issues, prior to searching for higher taxonomic names
    if(is.data.frame(gnr) && nrow(gnr)!=0)#check if gnr_resolve turns up no results (based on 2012 database)
    {
      taxonomy_dataframe[i,1] <- gnr[1,3]#use gnr name, only using 1 even if multiple rows
    }
    else {
      taxonomy_dataframe[i,1] <- GoogleSheetData[i,1] #if no gnr resolve found use given value and hope for the best
      #Eichhornia crassipes is the only one grn hasn't been able to find so far 
    }
  }
  
  for(i in 1:num_rows)
  {
    all_taxonomic_ranks <- tax_name(sci = taxonomy_dataframe[i,1], get = c("genus", "family", "order", "class","phylum", "domain"), db = "ncbi")
    for (j in 2:7) #2 because the first column is filled in already,  -1 bc CALeDNA format (num_colm-1)
    {
      taxonomy_dataframe[i,j] <- all_taxonomic_ranks[1, (j+1)] #make sure we iterate through the genus-family-order eetc
    }
    #populating the final column, domain;phylum;class;order;family;genus;genus_species
    #CALeDNA dataformatting
    taxonomy_dataframe[i,8] <- paste(  taxonomy_dataframe[i,7],taxonomy_dataframe[i,6], taxonomy_dataframe[i,5],taxonomy_dataframe[i,4],taxonomy_dataframe[i,3],taxonomy_dataframe[i,2],taxonomy_dataframe[i,1], sep = ";", collapse = '')
  }
  
  taxonomy_dataframe
}

# #Downloading and Exporting results in various ways! ---------------------

#to download full dataset
write.csv(taxonomy_dataframe, file = "taxonomy_dataframeRCODE_selected_homonym", row.names= FALSE)

#to downloadjust a single column as a csv: (THERE IS DEF A BETTER WAY TO DO THIS***)
gnr_species <- data.frame(matrix(ncol=1, nrow = num_rows)) #initialize dataframe with 1 col
gnr_species$SpeciesName <- taxonomy_dataframe$speciesgenus #First $ names the column, Second $ selects the column to paste into our new DF
gnr_species <- gnr_species[c(2,1)] #reorder columns
write.csv(gnr_species, file = "firstconvert2.csv", row.names= FALSE)

#to create a comma seperated .txt file of a single row
CleanDataframe <- taxonomy_dataframe#make a df copy to manipulate
#remove rows if there is a NA value in a specified cols: #"speciesgenus", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format")
CleanDataframe<- CleanDataframe[!is.na(CleanDataframe$Family), ] 
#removing rows in the dataframe where duplicate values are found in a specified cols
CleanDataframe<- unique(CleanDataframe[,3])#create vector of unique values in specified subset of dataframe 
Family_comma_sep_list.txt <- CleanDataframe

#if you dont want to deduplicate or remove NAs just run the line below
single_tax_list_GENUS.txt <- taxonomy_dataframe[,2] #1=species-gen, 2 = genus, 3 = family, etc 

##additional ways to export data
vectStr=paste(as.character(Family_comma_sep_list.txt), sep="' '", collapse=",")
sink("Family_comma_sep_list.txt")
cat(vectStr) 
#cat("\n")
sink()
file.show("Family_comma_sep_list.txt")
#this output can be taken directly to the Sequence Coverage Browser

view(taxonomy_dataframe)



