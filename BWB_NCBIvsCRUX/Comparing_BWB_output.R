

#import NCBI spp, genus, family values and CRUX output and original taxize output

ncbi_spp <-read.csv(file = "CovM_NCBI_SPPgenus_NPS.csv")
ncbi_genus <-read.csv(file = "CovM_NCBI_genus_NPS.csv")
ncbi_family <- read.csv(file = "CovM_NCBI_fam_NPS.csv")
crux_all <-read.csv(file = "crux_nps.csv")
all_Taxonomic_info<-read.csv(file = "taxonomy_dataframeRCODE_selected_homonym_2_1_21.csv")
back_Up_all_taxonomic_info <- all_Taxonomic_info


importCSVfile(testfunction, CovM_NCBI_SPPgenus_NPS)
importCSVfile <- function(name = dataframe_of_csv, file)
{
 filenaming <-paste(",file,.csv,")
 print(filenaming)
  name <-read.csv(file = filenaming)
  name
}

comparing_results <- data.frame()

for(i in ncol(crux_all))
{
  for(x in nrow(crux_all))
  {
    
  }
}








back_Up_all_taxonomic_info <- all_Taxonomic_info[!is.na(all_Taxonomic_info$Family), ] 
back_Up_all_taxonomic_info<- unique(back_Up_all_taxonomic_info[,2])#create vector of unique values in specified subset of dataframe 
length(back_Up_all_taxonomic_info)

for(i in 1:nrow(all_Taxonomic_info))
{
  if(identical(all_Taxonomic_info[i,1],all_Taxonomic_info[(i+1),1]))
  {
    all_Taxonomic_info <- all_Taxonomic_info[-c(i),]
  }
  
}

all_Taxonomic_info[137,1]

x <- 0

for(i in 1:nrow(all_Taxonomic_info))
{
  if(identical(all_Taxonomic_info[i,1],all_Taxonomic_info[(i+1),1]))
  {
    print(paste0("there's was duplicate called ",all_Taxonomic_info[i,1], " index:", i))
    x <- (x+1)
  }
  else if(identical(all_Taxonomic_info[(i),1],ncbi_spp[i,1]))
  {
    #print("hello")
     # print(all_Taxonomic_info[i,1])
    #print(all_Taxonomic_info[i,1])
  }
  print(x)
}


for(i in 1:nrow(all_Taxonomic_info))
{
 
    
  }
  if(identical(all_Taxonomic_info[(i),1],ncbi_spp[i,1]))
  {
    print(all_Taxonomic_info[i,1])
  }
  else
  {
    print("theres a mismatch")
    print(i)
    break
  }
}

#######
