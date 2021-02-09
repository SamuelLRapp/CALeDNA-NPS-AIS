library(ggplot2)
require(reshape2)
library(gplots)
require(stats)
library(qwraps2)
library(VennDiagram)
# install.packages("qwraps2")
# install.packages("gplots")
# install.packages("VennDiagram")

importedf <- file.choose("gnr_genus_species_NCBI_.csv")
Taxonomy_DF <- read.csv(file = importedf) #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
column_names <- c("ORGN Name","18S", "16S","PITS", "CO1", "COI", "COX1", "FITS","ITS2", "ITS1", "trnl", "12S") #csv got naming wierd
colnames(Taxonomy_DF) <-column_names

#^dataframe is all set up

BinaryTaxonomy_DF <- Taxonomy_DF
class(BinaryTaxonomy_DF)
#BinaryTaxonomy_DF[] <- cbind(lapply(BinaryTaxonomy_DF[,2:ncol(Taxonomy_DF)], function(x) ifelse(x>=1,BinaryTaxonomy_DF[,1],"zero")), Taxonomy_DF[,1])
BinaryTaxonomy_DF[] <- lapply(BinaryTaxonomy_DF, function(x) ifelse(x>=1,BinaryTaxonomy_DF[,1],"zero"))
write.csv(BinaryTaxonomy_DF, file = "BinaryTaxonomy_DF.csv", row.names= FALSE)
#cbind was throwing a warning that may have been causing column 6 to empty itself
#BinaryTaxonomy_DF[] <- lapply(BinaryTaxonomy_DF[,2:ncol(BinaryTaxonomy_DF)], function(x) ifelse(BinaryTaxonomy_DF>=1,BinaryTaxonomy_DF[,1],"zero"))
#Checks values in a row and labels them with ORGN name if they are greater than 0, replaces 0 with "zero"
#can't get rid of that first column with cbind... so I remove the first column below
BinaryTaxonomy_DF_no_spp <- BinaryTaxonomy_DF[,2:ncol(BinaryTaxonomy_DF)]
#remove zero values when converting to set?

# barcode1 <- as.character(BinaryTaxonomy_DF_no_spp[,6])
# barcode1 <- barcode1[lapply(barcode1,function(x) length(grep("zero",x,value=FALSE)))==0]
# print(barcode1)

BinaryTaxonomy_DF_no_spp <- BinaryTaxonomy_DF_no_spp[,c(5, 8:10)]
column_names_for_ven <- colnames(BinaryTaxonomy_DF_no_spp)
class(num_barcodes)
num_barcodes<-ncol(BinaryTaxonomy_DF_no_spp) #could be used in a function context
ncol(BinaryTaxonomy_DF_no_spp)
class(num_barcodes)
class(column_names_for_ven)
length(column_names_for_ven)
length(num_barcodes)
if(length(column_names_for_ven) ==num_barcodes)
{
  print("EQIAL")
}

list_of_vectors <- list()
for(i in 1:num_barcodes){
  barcode1 <- as.character(BinaryTaxonomy_DF_no_spp[,i])
  barcode1 <- barcode1[lapply(barcode1,function(x) length(grep("zero",x,value=FALSE)))==0] 
  #print(barcode1)
  length(barcode1)
  list_of_vectors[[i]] <- barcode1
}

venn.diagram( 
  x = list_of_vectors, 
  category.names = column_names_for_ven, #c("barcode 1", "barcode 2", "'barcode3'", "barcode4"), #colnames(dataframe)
  filename = 'Venn_4sets.png',
  output =TRUE)

#error: Error: Incorrect number of elements. == selected more than 5 vectors/circles for the ven diagram. 
#The funciton is limited to 5, but probably looks bettere for 4 tbh

