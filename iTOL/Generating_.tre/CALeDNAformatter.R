#### take in google sheet/data frame and bring them all into 1 column

X<-file.choose() #
GoogleSheetData_domain <- read.csv(file = X) #filled in domain info, because it was missing on taxize
#https://docs.google.com/spreadsheets/d/1czLS8ZBG-2ykZzEPpOLvWCmkyVJF2SnatQhcJlz3aFA/edit?usp=sharing

num_rows <- nrow(GoogleSheetData_domain)

CALeDNAformat  <- as.data.frame(matrix(nrow = num_rows, ncol = 10))

for(i in 1:num_rows)
{
  print(i)
  CALeDNAformat[i,1] <- paste(GoogleSheetData_domain[i,7],GoogleSheetData_domain[i,6], GoogleSheetData_domain[i,5],GoogleSheetData_domain[i,4],GoogleSheetData_domain[i,3],GoogleSheetData_domain[i,2],GoogleSheetData_domain[i,1], sep = ";", collapse = '')
}

#export
write.table(CALeDNAformat[,1], file = "CALeDNAformatT.txt",  row.names= FALSE,  quote=FALSE, col.names=FALSE)



#to create a comma seperated .txt file of a single row
CleanDataframe <- GoogleSheetData_domain#make a df copy to manipulate
#remove rows if there is a NA value in a specified cols: #"speciesgenus", "Genus", "Family", "order", "class","phylum", "domain", "CALeDNA format")
CleanDataframe<- CleanDataframe[!is.na(CleanDataframe$speciesgenus), ] 
#removing rows in the dataframe where duplicate values are found in a specified cols
CleanDataframe<- unique(CleanDataframe[,1])#create vector of unique values in specified subset of dataframe 
Family_comma_sep_list.txt <- CleanDataframe

