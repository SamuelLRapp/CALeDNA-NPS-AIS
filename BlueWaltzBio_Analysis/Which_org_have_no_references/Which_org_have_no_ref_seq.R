
# uploading data ----------------------------------------------------------


setwd('Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references')
getwd()
crux <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')
ncbi_genus <- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_genus_NPS.csv')
ncbi_family<- read.csv('/Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/input_ie_RefSeq_output/CovM_NCBI_fam_NPS.csv')
crux <- read.csv('../input_ie_RefSeq_output/crux_nps.csv')
ncbi <- read.csv('../input_ie_RefSeq_output/CovM_NCBI_SPPgenus_NPS.csv')

# combining COI/COX1/CO1 columns in NCBI search ---------------------------


#combine multiple COI NCBI names into 1
colnms=c("X.CO1", "X.COX1", "X.COI")
ncbi$Combined_COI<-rowSums(ncbi[,colnms])
#removing columns that were combined
drops <- c("X.CO1", "X.COX1", "X.COI")
ncbi<-ncbi[ , !(names(ncbi) %in% drops)]

#combine multiple COI NCBI names into 1
colnms=c("X.CO1", "X.COX1", "X.COI")
ncbi_genus$Combined_COI<-rowSums(ncbi_genus[,colnms])
#removing columns that were combined
drops <- c("X.CO1", "X.COX1", "X.COI")
ncbi_genus<-ncbi_genus[ , !(names(ncbi_genus) %in% drops)]

#combine multiple COI NCBI names into 1
colnms=c("X.CO1", "X.COX1", "X.COI")
ncbi_family$Combined_COI<-rowSums(ncbi_family[,colnms])
#removing columns that were combined
drops <- c("X.CO1", "X.COX1", "X.COI")
ncbi_family<-ncbi_family[ , !(names(ncbi_family) %in% drops)]

# main script -------------------------------------------------------------
CRUX_summary_report2<-summary_report(crux)

NCBI_summary_report<-summary_report(ncbi)

G_NCBI_summary_report<-summary_report(ncbi_genus)
F_NCBI_summary_report<-summary_report(ncbi_family)

#export
write.csv(CRUX_summary_report2, file ="/Users/samuelrapp/Desktop/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/CRUX_summary_report.csv")
write.csv(NCBI_summary_report, file ="/Users/samuelrapp/Desktop/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/Species_Genus_NCBI_summary_report.csv")
write.csv(G_NCBI_summary_report, file ="/Users/samuelrapp/Desktop/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/Genus_NCBI_summary_report.csv")
write.csv(F_NCBI_summary_report, file ="/Users/samuelrapp/Desktop/CALeDNA-NPS-AIS/BlueWaltzBio_Analysis/Which_org_have_no_references/out_put/Family_NCBI_summary_report.csv")


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
  crux_without_taxonomic_names<-  na.omit(crux_without_taxonomic_names)
  
  non_number_values <- c('genus', 'family', 'class', 'order')
  
  ncols <- ncol(crux_output)
  nrows <- nrow(crux_output)
  print("hello")
  
  for(i in 1:ncols)
  {
    for(j in 1:nrows)
    {
      boolean <- crux_without_taxonomic_names[j,i]%in%non_number_values
      #  if(!is.null(boolean) &  isTRUE(boolean)) #if true, ie it matches genus, family, class, order
      if(isTRUE(boolean)) #if true, ie it matches genus, family, class, order
      {
       # print(paste(i,'space',j))
        print(class(crux_without_taxonomic_names[j,i]))
        crux_without_taxonomic_names[j,i] <- as.numeric(0)
#       print(class(crux_without_taxonomic_names[j,i]))
      }
      print(class(crux_without_taxonomic_names[j,i]))
      
      # print(paste("WORKING", i))
    }
  }
 # print(crux_without_taxonomic_names)
  firstcolumn <- crux_without_taxonomic_names[,1]
  crux_without_taxonomic_names <- as.matrix(crux_without_taxonomic_names[,-1])
  
  crux_without_taxonomic_names <- as.data.frame(apply(crux_without_taxonomic_names, 2, as.numeric)) 
  
  print("I'm outish1")
  
  print(class(crux_without_taxonomic_names[2,2]))
  print(class(crux_without_taxonomic_names[1,2]))
  
  crux_without_taxonomic_names<-cbind.data.frame(firstcolumn,crux_without_taxonomic_names)
  print("I'm outish")
  
  print(class(crux_without_taxonomic_names[2,2]))
  print(class(crux_without_taxonomic_names[1,2]))
  
  crux_without_taxonomic_names
}


which_rows_are_empty_and_arenot <- function(dataframe, Which_Column#-1 means do all rows, a column number is gvenn the function will only run on said column of the dataframe
                                            ) #returns list of 2 lists, one of species with seqs, and one of species without any sequences
{
  print(Which_Column)
  if(is.null(Which_Column))
     {
       Which_Column <- -1
  }
  Which_Column <- Which_Column
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


summary_report <- function(dataframe)
{
  #calls convert_CRUX()s
  dataframe <- convert_CRUX(dataframe)
  class(dataframe)
  class(dataframe[,1])
  class(dataframe[,2])
  options(scipen=999) #scientific notion  

  new_row_names <- "total"
  new_row_names<-  c(new_row_names, colnames(dataframe[-1]))#doesn't include column with taxa snames
  print(new_row_names)
  
  statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
  new_col_names <- c("category","number of sequences found", "percent of total sequences found", "num of organism with at least one sequence", "num of organisms with no sequences")
  colnames(statistics_df) <- new_col_names
  #get list of columns + a column callede "total"
  
  print("test1")
  #add row names
  for(i in 1:length(new_row_names))
  {
    statistics_df[i,1]<-new_row_names[i]
  }
  print("test1.5")
  barcodeSums <- colSums(dataframe[,-1]) #doesn't include column with taxa snames
  print("test1.6")
  
  Total_seq_found <- sum(barcodeSums)
  print("test1.7")
  
  #hard code in the totals
  statistics_df[1,2] <- Total_seq_found
  print("test1.8")
  statistics_df[1,3] <- 100
  print("test2")
  for(i in 2:length(new_row_names))
  {
    x <- i-1
    statistics_df[i,2] <- barcodeSums[x]
    statistics_df[i,3] <- (barcodeSums[x]/Total_seq_found)
  }
  print(barcodeSums)
  
  #hard code in the totals
  output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, -1)
  statistics_df[1,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])    #list 2 is thee species without any seqs
  statistics_df[1,4] <-length(output_of_which_rows_are_empty_and_arenot[[1]])   #we know list 1 is the species with some seqs
  print("almosthome")
  
   for(i in 2:length(new_row_names))
  {
  x <- i#-1
  output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, Which_Column = x)
  statistics_df[i,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])     #list 2 is the species without any seqs 
  statistics_df[i,4] <- length(output_of_which_rows_are_empty_and_arenot[[1]])  #we know list 1 is the species with some seqs
   }
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


