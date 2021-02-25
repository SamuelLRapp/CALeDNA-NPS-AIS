setwd('Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references')
crux <- read.csv('../input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('../input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')

tester <- convert_CRUX(crux)
hello <- which_rows_are_empty_and_arenot(ncbi)



convert_CRUX <- function(crux_output) #take a crux output matrix and  turn the characters "genus, spp, etc" into  0s/1s
{
  crux_without_taxonomic_names <- crux_output
  non_number_values <- c('genus', 'family', 'class', 'order')
  
  ncols <- ncol(crux_output)
  nrows <- nrow(crux_output)
  
for(i in 1:ncols)
   {
    for(j in 1:nrows)
    {
      boolean <- crux_without_taxonomic_names[j,i]%in%non_number_values
    #  if(!is.null(boolean) &  isTRUE(boolean)) #if true, ie it matches genus, family, class, order
        if(isTRUE(boolean)) #if true, ie it matches genus, family, class, order
         {
        print(paste(i,'space',j))
        crux_without_taxonomic_names[j,i] <- 0
      }
     # print(paste("WORKING", i))
    }
  }
 print(crux_without_taxonomic_names)
 return(crux_without_taxonomic_names)
}

which_rows_are_empty_and_arenot <- function(dataframe) #returns 2 lists, one of species with seqs, and one of species without any sequences
{
  #add a arguement in the functiono so i know if i should call convert_CRUX inside or not....
  #dataframe <- convert_CRUX(dataframe)
  haveSomeSeq <- c()
  haveZeroSeq <- c()
  
  ncols <- ncol(dataframe)
  nrows <- nrow(dataframe)
  
  for(i in 1:nrows) #we will skip the first column because it has names
    {
    total <- 0
      for(j in 2:ncols)
        {
        total <- total + as.numeric(dataframe[i,j])
      }
    if(total > 0)
    {
      haveSomeSeq <- c(haveSomeSeq, dataframe[i,1])
    } else
    {
      haveZeroSeq <- c(haveZeroSeq, dataframe[i,1])
    }
  }
  results <- c(haveSomeSeq, haveZeroSeq)
  results
}
