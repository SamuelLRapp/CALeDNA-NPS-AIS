
# title -------------------------------------------------------------------

#created on march 9th to test if Sam can find full genome sequences on NCBI using the genome database or the refseq database. This data would complement the information coming from the BWB refseq browser

# libraries ---------------------------------------------------------------

library(rentrez)
set_entrez_key("4e400b86621f28a75c9025133cb1cea4f108")
Sys.getenv("ENTREZ_KEY")

# import datasets ---------------------------------------------------------

setwd('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/full_genomes_search')
dataset<-read.csv('input/taxonomy_dataframeRCODE_selected_homonym_2_1_21.csv')
getwd()

# *code --------------------------------------------------------------------
paste0('',dataset[3,2],'')
Results <- data.frame(matrix(0, ncol = 2, nrow = nrow(dataset)))
Results <- dataset[,1]
Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows)) #creating dataframe
taxa_of_interest <- dataset[,1] #vectorizing the species of interest
Results$taxaname <- taxa_of_interest #add the vector to the column
names(Results) <- c('taxaname', 'present_in_genome')
test1<- Is_the_taxa_in_the_NCBI_genome_DB(dataset,1)

genome_SearchTerm <- paste0('',dataset[1,2],'')
genome_result<- entrez_search(db = "genome", term = genome_SearchTerm, retmax = 5)

# functions ---------------------------------------------------------------

Is_the_taxa_in_the_NCBI_genome_DB <- function( 
  #the results of this function can be easily verified here: #https://www.ncbi.nlm.nih.gov/genome/browse#!/overview/
  taxa_dataframe,  #input a list or datafrarme and return a datafram
  column_number #the column number from the dataframe you want to check
) 
{
  num_rows <- nrow(taxa_dataframe)
  Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
  class(Results)#creating dataframe
  names(Results) <- c('taxaname', 'present_in_genome')
  taxa_of_interest <- taxa_dataframe[,column_number] #vectorizing the species of interest
  Results$taxaname <- taxa_of_interest #add the vector to the column
  
  for(i in 1:40)#num_rows)#num_rows
  {
    genome_SearchTerm <- paste0('',taxa_dataframe[i,column_number],'')
    print(i)
    genome_result<- entrez_search(db = "genome", term = genome_SearchTerm, retmax = 5)
    Results[i,2] <- genome_result$count #add zero
    if(genome_result$count > 0)
    {
       print(genome_SearchTerm) #Results[] <- genome_result$count #add count
     }#else
    # {
    #   Results[] <- genome_result$count #add zero
    # }
    
  }
  print("goodbye")
  Results
}



