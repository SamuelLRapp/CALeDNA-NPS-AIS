library(taxize)
library(rentrez)


GoogleSheetData <-read.csv(file = "CALeDNA-NPS_aquatic-invasive_species_eDNA-primers_SPECIES_ANALYSIS.csv")
class(GoogleSheetData) #its a dataframe
GoogleSheetData <- GoogleSheetData[1:5,1:2] #SUBSET THE dataframe to do testing

#have a single list of species
#create a data frame with columns "speciesgenus","Common", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format"
#for loop (i in 1:rownum)
#for loop( j in 1:number of columns)
#{
#taxize for column name X in [i,j]
# $ selects a particular column from a data frame by name
#dataframename$temperature gives you the column called temperature
#}

num_rows <- nrow(GoogleSheetData) #get row count
column_names <- c("speciesgenus","Common", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format")
num_colm <- length(column_names) #9
taxonomy_dataframe <- data.frame(matrix(ncol=num_colm, nrow = 0))
colnames(taxonomy_dataframe) <- column_names #set column names

for(i in 1:num_rows) #fill out the first column with  "proper names"
{
  taxonomy_dataframe[i,1] <- gnr_resolve(sci = GoogleSheetData[i,1], data_source_ids = 4)[1,3] #get proper name, only using 1
}


#testing differences between functions
# gnr_datasources <- gnr_datasources()
# all_taxonomic_ranks_TF <- tax_name(sci = GoogleSheetData[4,1], get = c("Genus", "Family", "order", "class","phylum", "domain"), db = "ncbi")
# print(tax_name(sci = GoogleSheetData[1,1], get = c("species"), db = "ncbi"))
# print(gnr_resolve(sci = GoogleSheetData[1,1], data_source_ids = 4)[1,3])
# all_taxonomic_ranks <- tax_name(sci = taxonomy_dataframe[4,1], get = c("Genus", "Family", "order", "class","phylum", "domain"), db = "ncbi")
# 
#----
#I cry
#https://www.rdocumentation.org/packages/taxize/versions/0.9.99/topics/ncbi_downstream
#data(rank_ref)
#----

#wolftest <- tax_name(sci = "canis lupus", get=c("phylum","domain"), db ="ncbi")
all_taxonomic_ranks1 <- tax_name(sci = taxonomy_dataframe[3,1], get = c("Genus", "Family", "order", "class","phylum", "domain"), db = "ncbi")

for(i in 1:num_rows)
{
  print(i)
  all_taxonomic_ranks <- tax_name(sci = taxonomy_dataframe[i,1], get = c("Genus", "Family", "order", "class","phylum", "domain"), db = "ncbi")
  #subset?
 # print(class(tax_name(sci = taxonomy_dataframe[1,1], get = c("Genus", "Family", "order", "class","phylum", "domain"), db = "ncbi")))
  for (j in 2:8) #2 because the first column is filled in already,  -1 bc CALeDNA format (num_colm-1)
  {
    # print(j)
    # all_taxonomic_ranks[1, (1+j)]
    # name<-all_taxonomic_ranks[1, (1+j)]
    # print(class(name))
    # #name <- name + "hi"
    # print(name)
    # if(is.na(name)==TRUE)
    # {
    # 
    # }
    #    else
    #    {
         taxonomy_dataframe[i,j] <- all_taxonomic_ranks[1, (j)] #make sure we iterate through the genus-family-order eetc
   #    }
   
  }
}




### CALeDNA data format: Eukaryota;Annelida;Clitellata;Enchytraeida;Enchytraeidae;Lumbricillus;Lumbricillus dubius
#####taxonomy_only_table <-select(taxonomytable, sum.taxonomy) %>% 
#separate(sum.taxonomy, c("domain", "phylum", "class", "order", "family", "genus", "genus species"), sep = ";",remove=FALSE)

--------#this is not actively in the code: 
  # test <-tax_name(sci = GoogleSheetData[2,1], get = c("species genus","family","order"), db= "ncbi")#[1,3]
  # print(test)
  # for(i in 1:num_rows)
  # {
  #   for(j in 1:num_colm)
  #   taxonomic_rank <- column_names[j]  
  #   taxonomy_dataframe[i,j] <-  tax_name(sci = GoogleSheetData[j,1], get =taxonomic_rank, db= "ncbi")[1,3]
  #   #how to ge t
  #   print(tax_name(sci = GoogleSheetData[1,1], get = genus, db= "ncbi")[1,3])
  # }
  # 
# ------

# 
# num_rows <- nrow(species_names) #get row count
# for(i in 1:num_rows)
# {
#   #create a vector of taxonomic names
#   NCBI_name <- gnr_resolve(sci = GoogleSheetData[1,1], data_source_ids = 4)[1,3] 
#   #get proper name, column 3 is where the proper name is, but there nay be multiple outputs!
#   #see server.r code for more info on running more output
#   genus <- tax_name(query= NCBI_names, get = "genus", db= "ncbi")[1,3] #get proper higher level taxonomic name based on proper name
#   
#   taxonomic_rank <- c(taxonomic_rank,genus)
#   
# }
# species_names <-read.csv(file = "Organism_names.csv")
# #Taxonomic name <-  (species_names, experience)
# FullList <- data.frame( "domain", "phylum", "class", "order", "family", "genus", "genus_species")
# combinedlist <- cbind(species_names,GoogleSheetData)
# str(combinedlist)
# class(combinedlist)
# class(GoogleSheetData)
# species_names<- cbind(data, new_col = "Genus")   
# species_names$genus = genus
# Genus<- tax_name(query= GoogleSheetData[1,1], get = "genus", db= "ncbi")[1,3] #get genus from taxize
# NCBI_names <- gnr_resolve(sci = GoogleSheetData[1,1], data_source_ids = 4)[1,3] #get proper name
# tax_name(query= NCBI_names, get = "genus", db= "ncbi")[1,3] #get proper higher level taxonomic name based on proper name
# 
# NCBI_names 
# Genus
# homo<- gnr_resolve(sci = "homo sapein", data_source_ids = 4)
# searchTerm <- tax_name(query= organism, get= "genus", db= "ncbi")[1,3] #get family from taxize 
# 
# #what is the CALeDNA foormatting domain/genus/etc...
# employee <- c('John Doe','Peter Gynn','Jolie Hope')
# salary <- c(21000, 23400, 26800)
# salary <- c(322, 23400, 26800)
# startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
# employ.data <- data.frame(employee, salary, startdate)
# data.frame()
# #for loop

#