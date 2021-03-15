
# uploading data ----------------------------------------------------------


setwd('Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references')
getwd()
crux <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')
ncbi_genus <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_genus_NPS.csv')
ncbi_family<- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_fam_NPS.csv')
crux <- read.csv('../input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('../input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')


# main script -------------------------------------------------------------


ncbi_lists <- which_rows_are_empty_and_arenot(ncbi,Which_Column = -1)
ncbi_lists <- which_rows_are_empty_and_arenot(ncbi,Which_Column = 9)
ncbi_lists %>% Sentence_List_breakdown()
sum_counts_in_columns(ncbi)
statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))

dataframe <- convert_CRUX(ncbi)

new_row_names <- "total"
new_row_names<-  c(new_row_names, colnames(dataframe[-1]))#doesn't include column with taxa snames
print(new_row_names)

statistics_df <- data.frame(matrix(ncol = 4, nrow = 0))
new_col_names <- c("number of sequences found", "percent of total sequences found", "num of organism with at least one sequence", "num of organisms with no sequences")
colnames(statistics_df) <- new_col_names
class(new_row_names[3])
for(i in 1:new_row_names)
{
  print(new_row_names[1])
  #print("helo")
  # j<- (i+1)
  # statistics_df[j,1]<-new_row_names[i]
}




crux_lists <- which_rows_are_empty_and_arenot(crux)
ncbi_genus_lists <- which_rows_are_empty_and_arenot(ncbi_genus)
Sentence_List_breakdown(ncbi_genus_lists)
ncbi_family_lists <- which_rows_are_empty_and_arenot(ncbi_family)
Sentence_List_breakdown(ncbi_family_lists)
as.matrix(ncbi_genus_lists)
class(ncbi_genus_lists)

#export
write.table(crux_lists_ma,file = "/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/CruxLists.csv", col.names = c('HaveSeq', 'havenoSeq'), row.names = FALSE)
write.table(ncbi_genus_lists,file = "/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/CruxLists.csv", col.names = c('HaveSeq', 'havenoSeq'), row.names = FALSE)
write.csv(ncbi_genus_lists, file  = "/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/CruxLists.csv")
crux_lists_ma =as.matrix(crux_lists)


# intersections -----------------------------------------------------------


###looking for intersections
crux_lists$haveZeroSeqs %in% ncbi_lists$haveZeroSeqs
intersect(crux_lists$haveZeroSeqs,ncbi_lists$haveZeroSeqs)

intersect_taxa<-intersect(crux_lists[[2]],ncbi_lists[[2]])
CRUX_SD<- setdiff(crux_lists[[2]],ncbi_lists[[2]]) #had zero seqin CRUX had some in NCBI
NCBI_SD<- setdiff(ncbi_lists[[2]], crux_lists[[2]]) #had zero seq in NCBI had some in CRUX

check<-intersect(crux_lists[[2]],Organisms_with_GENOME)
check<-intersect(ncbi_lists[[2]],Organisms_with_GENOME)

check<-intersect(crux_lists[[2]],Chloroplast_taxa)
check<-intersect(ncbi_lists[[2]],Chloroplast_taxa)

check<-intersect(crux_lists[[2]],Mitocondonrial_taxa)
check<-intersect(ncbi_lists[[2]],Mitocondonrial_taxa)

# functions area ----------------------------------------------------------

Sentence_List_breakdown <- function(output_of_which_rows_are_empty_and_arenot #this code writes a setence describing the output of which_rows_are_empty_and_arenot()
)
{
  #we know list 1 is the species with some seqs
  #list 2 is thee species without any seqs
  num_spp_with_zero <- length(output_of_which_rows_are_empty_and_arenot[[2]])
  num_sup_with_some <-length(output_of_which_rows_are_empty_and_arenot[[1]])
  output<-paste0('There are ',num_spp_with_zero, ' taxa with no sequence information and ', num_sup_with_some, ' taxa with at least 1 sequence found')
  output
}

convert_CRUX <- function(crux_output #take a crux output matrix and  turn the characters "genus, spp, etc" into  0s/1s
                         #this function is used by which_rows_are_empty_and_arenot()
)
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
       # print(paste(i,'space',j))
        crux_without_taxonomic_names[j,i] <- 0
      }
      # print(paste("WORKING", i))
    }
  }
 # print(crux_without_taxonomic_names)
  return(crux_without_taxonomic_names)
}

which_rows_are_empty_and_arenot <- function(dataframe, Which_Column#-1 means do all rows, a column number is gvenn the function will only run on said column of the dataframe
                                            ) #returns list of 2 lists, one of species with seqs, and one of species without any sequences
{
  Which_Column <- Which_Column
  if(is.null(Which_Column))
     {
       Which_Column <- -1
  }
  print(Which_Column)
  
  dataframe <- convert_CRUX(dataframe)

  #create two lists
  haveSomeSeq <- c()
  haveZeroSeq <- c()
  
  ncols <- ncol(dataframe)
  nrows <- nrow(dataframe)
  
  if(Which_Column < 0){
    
    for(i in 1:nrows) #we will skip the first column because it has names
    {
      total <- 0
      for(j in 2:ncols)
      {
        total <- total + as.numeric(dataframe[i,j])
      }
      
      if(!is.null(total) && total > 0)
      {
       # haveSomeSeq <- c(haveSomeSeq, dataframe[i,1])
        haveSomeSeq <- c(haveSomeSeq, total)
        
   #     print(dataframe[i,1])
      } else
      {
    #    print(dataframe[i,1])
        haveZeroSeq <- c(haveZeroSeq, total)
        print(i)
      #  haveZeroSeq <- c(haveZeroSeq, dataframe[i,1])
      }
    }
  }else #if a specific columnn
  {
    for(i in 1:nrows) #we will skip the first column because it has names
    {
      seqs <- 0
      seqs <- 0 + as.numeric(dataframe[i,Which_Column]) 

      if(!is.null(seqs) && seqs > 0)
      {
        haveSomeSeq <- c(haveSomeSeq, dataframe[i,Which_Column])
        #print(seqs)
      } else
      {
       # print(seqs)
        haveZeroSeq <- c(haveZeroSeq, dataframe[i,Which_Column])
      }
    }
  }
  
  if(Which_Column < 0){
    results <- list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs =haveZeroSeq)
    results<- as.matrix(results)
  }else
  {
    COLNam <- colnames(dataframe)
    column_name <- paste0("Have",COLNam[Which_Column],"Seq")
    print(column_name)
    results <- list(single_Barcode_haveSomeseq = haveSomeSeq, single_Barcode_haveZeroSeqs =haveZeroSeq)
    results<- as.matrix(results)
  }
  results
}


sum_counts_in_columns <- function(dataframe)
{
  #calls convert_CRUX()s
  dataframe <- convert_CRUX(dataframe)
  
  new_row_names <- "total"
  new_row_names<-  c(new_row_names, colnames(dataframe[-1]))#doesn't include column with taxa snames
  print(new_row_names)
  
  statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
  new_col_names <- c("category","number of sequences found", "percent of total sequences found", "num of organism with at least one sequence", "num of organisms with no sequences")
  colnames(statistics_df) <- new_col_names
  #get list of columns + a column callede "total"
  
  print("hello")
  #add row names
  for(i in 1:new_row_names)
  {
    print("helo")
    statistics_df[i,1]<-new_row_names[i]
  }
  
  barcodeSums <- colSums(dataframe[,-1]) #doesn't include column with taxa snames
  Total_seq_found <- sum(barcodeSums)
  statistics_df[2,2] <- Total_seq_found
  statistics_df[2,3] <- 100
  
  for(i in 3:new_row_names)
  {
    statistics_df[i,2] <- barcodeSums[i]
    statistics_df[i,3] <- (barcodeSums[i]/Total_seq_found)
  }
  
  # for(for(i in 3:new_row_names)
  # {
  #   #call organiisms with no ref seq funcition
  #   #statistics_df[i,2] <- barcodeSums[i]
  #   #statistics_df[i,3] <- (barcodeSums[i]/Total_seq_found)
  # })
  # 
  
  
  #for loop through the rows
  #   fill in total sequences found
  #number of organisms with one or more refseq 
  #number organsms with no ref seq
  
  #fill in total row
  #total sequences found, 
  #add percent totals to other rows
  #
  statistics_df
}


# playground --------------------------------------------------------------
# https://stackoverflow.com/questions/17598134/compare-two-character-vectors-in-r
#  
A = c("Dog", "Cat", "Mouse")
B = c("Tiger","Lion","Cat")
class(B)
 A %in% B
# [1] FALSE  TRUE FALSE
# > intersect(A,B)
# [1] "Cat"
# > setdiff(A,B)
# [1] "Dog"   "Mouse"
# > setdiff(B,A)
# [1] "Tiger" "Lion" 


