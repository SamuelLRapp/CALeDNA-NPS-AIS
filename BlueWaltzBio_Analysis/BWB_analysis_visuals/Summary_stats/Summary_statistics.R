library(ggplot2)
require(reshape2)
library(gplots)
require(stats)
library(qwraps2)
library(VennDiagram)
# install.packages(qwraps2)
# install.packages("gplots")
# install.packages("VennDiagram")

importedf <- file.choose("gnr_genus_species_NCBI_.csv")
Taxonomy_DF <- read.csv(file = importedf) #google sheet where the first column is the list of genus-species names you want higher taxonomic rankings of
column_names <- c("ORGN Name","18S", "16S", "PITS", "CO1", "COI", "COX1", "FITS","ITS2", "ITS1", "trnl", "12S")
colnames(Taxonomy_DF) <-column_names
tax <- Taxonomy_DF
row.names.data.frame(tax)<- c("mean", "mode", "total sequences", "percent sequences found", "max sequence count")
#creating an empty dataframe
col_num <-ncol(Taxonomy_DF)
colnames(Taxonomy_DF)
col_name <- colnames(Taxonomy_DF)
col_name <- col_name[2:length(col_name)] #remove species column
SummaryStats_DF <- setNames(data.frame(matrix(ncol = (col_num-1), nrow = 0)), col_name)

row.names(SummaryStats_DF) <- c("mean", "mode", "total sequences", "percent sequences found", "max sequence count")

summary_stats <- summary(Taxonomy_DF)
summary_stats_removed_quartiles <- summary_stats[c(1,3,4,6),] #slecting min, median, mean, and max
write.csv(summary_stats_removed_quartiles, file = "summary_stats_removed_quartiles", row.names= FALSE)

dataframe_withoutnames <-Taxonomy_DF[,2:ncol(Taxonomy_DF)]

max<-sapply(dataframe_withoutnames, max,na.rm=TRUE)
min<-sapply(dataframe_withoutnames, min,na.rm=TRUE)
mean<-sapply(dataframe_withoutnames, mean,na.rm=TRUE)
print(mean)
median<-sapply(dataframe_withoutnames, median,na.rm=TRUE)

combine <- rbind(SummaryStats_DF, max, mean, median)
colnames(combine) <- col_name
#row.names<-(combine, c("mean", "mode", "total sequences", "percent sequences found", "max sequence count"))
`row.names[1]<-`(combine, "mean", "mode", "total sequences", "percent sequences found", "max sequence count")
combine[1,0] <- mean
rownames(combine) <- c("mean", "mode", "total sequences", "percent sequences found", "max sequence count")
rownames(combine)
total <-rbind(summary_stats_removed_quartiles, Percentage_organisms_a_seqeuence_was_found_per_barcode)#precentage zeros

num_col <- ncol(Taxonomy_DF)
num_rows <-nrow(Taxonomy_DF)
Percentage_organisms_a_seqeuence_was_found_per_barcode <- character((1+length(num_col)))
Percentage_organisms_a_seqeuence_was_found_per_barcode[1] <-"The % of the total organism list with at least 1 sequence found, for a given barcode."
print(Percentage_organisms_a_seqeuence_was_found_per_barcode)
for(i in 2:num_col) #starting at 2 because first column is species names
{
  removedzeros <- Taxonomy_DF[Taxonomy_DF[,i] != 0, ] #selected rows in dataframe that are not zero in a given column (barcode)
  num_rows_without_zeros <- nrow(removedzeros) #number of organisms with had a sequence detected per column (barcode)
  percentage_of_total<-as.character((num_rows_without_zeros/num_rows))   #percentage of species with at least 1 sequence found
  print(percentage_of_total)
  Percentage_organisms_a_seqeuence_was_found_per_barcode[i]  <- percentage_of_total
  }

class(combine)
removedzeros4 <- Taxonomy_DF[Taxonomy_DF[,2] != 0, ]
class(Percentage_organisms_a_seqeuence_was_found_per_barcode)

######




 #want to be able to check between for overlap.....
 #create a ven diagram where circles are barcodes and they overlap when the each have a sequence for a given animal

#convert dataframe to T/F based on 

melted_df <- melt(Taxonomy_DF)
column_names <- c("Species_Genus","Barcode","Sequence_Count")
colnames(melted_df) <- column_names #set column names

num_rows <- nrow(melted_df)
#percentage of non-zero results per barcode. May help users determine which barcodes work for them over others
removedzeros <- melted_df[melted_df$Sequence_Count != 0, ] #removing zeros from melted dataframe
num_rows_without_zeros <-nrow(removedzeros)
class(x)
percentage_of_species_with_1_or_more_seq <- (1-(num_rows_without_zeros/num_rows))




removedzerosplot<-ggplot(data = removedzeros, aes(x = Barcode, y = Sequence_Count )) +
  geom_boxplot(aes(fill=Species_Genus))+
  geom_point(aes(color = X))

includiesoutliers <- ggplot(data = melted_df, aes(x=Barcode, y=Sequence_Count)) + geom_boxplot(aes(fill=Species_Genus))


stats <- boxplot.stats(numvec)$stats
class(Taxonomy_DF)
numvec<-data.matrix(melted_df, rownames.force = NA)


# compute lower and upper whiskers
ylim1 = boxplot.stats(melted_df)#$stats #[c(1, 5)]
boxplot.stats()!
  ylim1 <- boxplot.stats(Taxonomy_DF)

# scale y limits based on ylim1
p1 = includiesoutliers + coord_cartesian(ylim = ylim1*1.05)






