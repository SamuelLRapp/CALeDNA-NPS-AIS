
# title and description ---------------------------------------------------

#Creating a single script that will hold functions for processing and analysising the output of RefSeqCoverageMatrix


# function Comparing_NCBIvsCRUX 1 ---------------------------------------------------------
Generate_DF_All_Ranks_from_latin_species_list <- function(  #comes from Generating_ALL-taxonomic-ranks.r file
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



