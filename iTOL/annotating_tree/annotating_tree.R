######### Samuel Rapp, 2/11/2021
###the goal of this R code is to take a Nexus .txt file from iTOL, and annotate it with CRUX/NCBI reference database information
###########
setwd("~/GitHub/CALeDNA-NPS-AIS/JuypterNotebook/annotating_tree")
OTL_dataframe  <- read.csv(file = "input_iTOL/NPS_AIS_GENUS_taxon_names_according_to_OTL.txt")
CoverageMatrix_NCBI_genus_DF<- read.csv(file = "input_iTOL/CovM_NCBI_genus_NPS.csv")



function1output <-Annotated_OTL_IDS(OTL_dataframe, CoverageMatrix_NCBI_genus_DF)
OTL_w_underscores<- add_underscores(function1output)
write.csv(OTL_w_underscores, file = "OTT_IDD_Annotated", row.names = FALSE)

OTLs_left_behind<-compare_firstC_in_DFs(OTL_dataframe,function1output)      #find OTL values that didn't get replaced
NUM_BWB_SPP_left_behind(CoverageMatrix_NCBI_genus_DF,function1output) #number of BWB values that didn't get replaced 





 add_underscores <- function(ddata.frame) #takes OTL list and adds _ in betweeen spaces between name and ottid
{
  for(i in 1:nrow(ddata.frame))
  {
    ddata.frame[i,1] <- trimws(ddata.frame[i,1], "r")
    ddata.frame[i,1] <- gsub(" ", "_",  ddata.frame[i,1]) #change gsub to work with genu spp ott
  }
  ddata.frame
}

NUM_BWB_SPP_left_behind <- function(BWB_spp_DF,Annotated_OTL_DF)
{
  output <- (nrow(BWB_spp_DF) - nrow(Annotated_OTL_DF))
  output
}

Annotated_OTL_IDS <- function(OTL_df, BWB_df) #takes in OTL_df dataframe, BWB dataframe and outputs the BWB dataframe with OTL names
{
  rows_to_remove <- c()
  Output_DF <- BWB_df
    for(i in 1:nrow(Output_DF))
    {
      needle <- Output_DF[i,1]
      foundHaystack <- "FALSE"
        for(j in 1:nrow(OTL_df))
        {
          haystack<-OTL_dataframe[j,1]
            if(grepl(needle, haystack, fixed = TRUE) == T)
            {
              haystack <- trimws(haystack, "r")
              Output_DF[i,1] <- haystack
              foundHaystack <-"TRUE"
            }
        }
      
      if(foundHaystack == "FALSE") #no OTL value found
      { 
        rows_to_remove <- c(rows_to_remove, i)
      }
    }
  Output_DF <- Output_DF[-rows_to_remove,]#delete rows where CovMatrixvalue wasn't replaced, ie haystack not found
  #some of those values are due to 
  Output_DF
}


compare_firstC_in_DFs <- function(OTU_longerlist,ShorterList) #comparing first column of two dataframes (should besimilar) and outputting values that aren't in both
{
  output <- c() 
  for(i in 1:nrow(OTU_longerlist))
  {
    found <- "FALSE"
    value <-trimws(OTU_longerlist[i,1],"r") #trim whitespace that OTL nexus files create when imported into R
  
    for(j in 1:nrow(ShorterList))
    {
      if(value == ShorterList[j,1])
      {
        found <- "TRUE" #there was a match in there!
      }
    }
    
    if(found == "FALSE") #no match was found to 'value'
    {
      output <- c(output, value)
    }
    }
  output
}


