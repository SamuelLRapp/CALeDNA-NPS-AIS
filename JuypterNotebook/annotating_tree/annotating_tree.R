######### Samuel Rapp, 2/11/2021
###the goal of this R code is to take a Nexus .txt file from iTOL, and annotate it with CRUX/NCBI reference database information
###########
setwd("~/GitHub/CALeDNA-NPS-AIS/JuypterNotebook/annotating_tree")
OTL_dataframe  <- read.csv(file = "NPS_AIS_GENUS_taxon_names_according_to_OTL.txt")
CoverageMatrix_NCBI_genus_DF<- read.csv(file = "CovM_NCBI_genus_NPS.csv")
Output_DF <- CoverageMatrix_NCBI_genus_DF

#forloop going through Output_DF
#get
#
  #inner forloop going through OTL_dataframe 
  #if find grep match replace Output_DF[i,1] with OTL_dataframe[j,1]
   #found anything = true

#if found anything == false
#delete row

#found anything = false turned

for (i in 1:nrow(Output_DF)) {
  needle <- Output_DF[i,1]
  #print(needle)
  foundHaystack <- "FALSE"
  for(j in 1:nrow(OTL_dataframe))
  {
    haystack<-OTL_dataframe[j,1]
   # print(haystack)
    if(grepl(needle, haystack, fixed = TRUE) == T)
       {
        haystack <- trimws(haystack, "r")
         Output_DF[i,1] <- haystack
       #  print("found")
         foundHaystack <-"TRUE"
    }
  }
  if(foundHaystack == "FALSE") #no OTL value found
  { 
    print(Output_DF[i,1])
    Output_DF <- Output_DF[-c(i),]#delete row
  }
}

write.csv(Output_DF, file = "OTT_IDD_Annotated", row.names = FALSE)
#to download full dataset
write.csv(taxonomy_dataframe, file = "taxonomy_dataframeRCODE_selected_homonym", row.names= FALSE)

chars <- OTL_dataframe[6,1]#"MRCA of taxa in Boehmeria ott594987"
grepl("Boehmeria", chars, fixed = TRUE)

searchOrganism <- sub(" .*", "", OTL_dataframe[1,1]) 
data_frame_Row_columns <- which(Output_DF == searchOrganism, arr.ind = TRUE)

###take OTL_dataframe values and find them inside CoverageMatrix_NCBI_genus_DF, 
for(i in OTL_dataframe)
{
  searchOrganism <- sub(" .*", "", OTL_dataframe[3,1]) 
  
  #can add a check for MRCA values
  if(searchOrganism == "MRCA")
  {
    print("false")  #somee grep to look for characters at end.
  }
  
  data_frame_Row_columns <- which(Output_DF == searchOrganism, arr.ind = TRUE)
  if(nrow(data_frame_Row_columns)!=1)
  {
    print("not 1 row")
  }else
    {
      print("one row")
      
    }
}

##compare 

#add barcode counts, or just replace the CM namee with the OTL name! 
#if nothing is found in OTL then delete the row in CM