library(taxize)
library(rentrez)

fileBig <- file.choose()
GoogleSheetData <- read.csv(file = fileBig) #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
#GoogleSheetData <- read.csv(file = "CALeDNA-NPS_aquatic-invasive_species_eDNA-primers_SPECIES_ANALYSIS.csv") #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
class(GoogleSheetData) #its a dataframe
GoogleSheetData <- GoogleSheetData#[1:100,]#[1:5,] #SUBSET THE dataframe to do testing 

set_entrez_key("4e400b86621f28a75c9025133cb1cea4f108")
Sys.getenv("ENTREZ_KEY")

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
    print(i)
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

#Downloading and Exporting results in various ways!

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
#single_tax_list_GENUS.txt <- taxonomy_dataframe[,2] #1=species-gen, 2 = genus, 3 = family, etc 

vectStr=paste(as.character(Family_comma_sep_list.txt), sep="' '", collapse=",")
sink("Family_comma_sep_list.txt")
cat(vectStr) 
#cat("\n")
sink()
file.show("Family_comma_sep_list.txt")
#this output can be taken directly to the Sequence Coverage Browser

view(taxonomy_dataframe)

