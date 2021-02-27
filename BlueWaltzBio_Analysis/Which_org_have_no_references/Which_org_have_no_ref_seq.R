setwd('Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references')
crux <- read.csv('../input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('../input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')

ncbi_lists <- which_rows_are_empty_and_arenot(ncbi)
crux_lists <- which_rows_are_empty_and_arenot(convC)

comparing


write.table(crux_lists_ma,file = "/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/CruxLists.csv", col.names = c('HaveSeq', 'havenoSeq'), row.names = FALSE)

crux_lists_ma =as.matrix(crux_lists)

test<-crux_lists
names(test) <- paste("V", seq_along(test), sep = "")
SampleData3 <- lapply(test, unlist)
sampledata5<-as.matrix(SampleData3)

crux_lists$haveZeroSeqs %in% ncbi_lists$haveZeroSeqs
intersect(crux_lists$haveZeroSeqs,ncbi_lists$haveZeroSeqs)
setdiff(crux_lists$haveZeroSeqs,ncbi_lists$haveZeroSeqs)

# playground --------------------------------------------------------------
# https://stackoverflow.com/questions/17598134/compare-two-character-vectors-in-r
#  
# > A = c("Dog", "Cat", "Mouse")
# > B = c("Tiger","Lion","Cat")
# > A %in% B
# [1] FALSE  TRUE FALSE
# > intersect(A,B)
# [1] "Cat"
# > setdiff(A,B)
# [1] "Dog"   "Mouse"
# > setdiff(B,A)
# [1] "Tiger" "Lion" 


# functions area ----------------------------------------------------------

convert_CRUX <- function(crux_output) #take a crux output matrix and  turn the characters "genus, spp, etc" into  0s/1s
{
  print("I'm in")
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

which_rows_are_empty_and_arenot <- function(dataframe) #returns list of 2 lists, one of species with seqs, and one of species without any sequences
{
  #add a arguement in the functiono so i know if i should call convert_CRUX inside or not....
  dataframe <- convert_CRUX(dataframe)
  ####
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
    if(!is.null(total) && total > 0)
    {
      haveSomeSeq <- c(haveSomeSeq, dataframe[i,1])
      print(dataframe[i,1])
    } else
    {
      print(dataframe[i,1])
      haveZeroSeq <- c(haveZeroSeq, dataframe[i,1])
    }
  }
  results <- list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs =haveZeroSeq)
  results<- as.matrix(results)
  results
  #haveSomeSeq
}