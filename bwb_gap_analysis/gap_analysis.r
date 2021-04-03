
# title -------------------------------------------------------------------

#using this document to test the bwb tool for species gap analysis
#The question: What portion of the taxa, at species level, genus level, 
#etc are only found in traditional samping due
#to reference sequence availability and reference sequence coverage?

#Answer:
#I assume that the plants (Streptophyta) should be represented and the animals (Chordata) will be better represented than the Arthropoda (inverts) 


# import data -------------------------------------------------------------

fileBig <- file.choose()
UCNRS_site_comparison_data<- read.csv(file = fileBig)

# analysis ----------------------------------------------------------------

#sort data into Anthropoda, Chordata, Streptophyta phylum
Anthropoda_df <- UCNRS_site_comparison_data[UCNRS_site_comparison_data$Phylum == 'Arthropoda',] 
Chordata_df <- UCNRS_site_comparison_data[UCNRS_site_comparison_data$Phylum == 'Chordata',] 
Streptophyta_df <- UCNRS_site_comparison_data[UCNRS_site_comparison_data$Phylum == 'Streptophyta',] 

#sort data into species level, genus level, family level data
Anthropoda_genus_df <- Anthropoda_df[Anthropoda_df$taxlevel == 'Genus',]
Anthropoda_family_df <- Anthropoda_df[Anthropoda_df$taxlevel == 'Family',]
Anthropoda_order_df <- Anthropoda_df[Anthropoda_df$taxlevel == 'Order',]

Chordata_genus_df <- Chordata_df[Chordata_df$taxlevel == 'Genus',] 
Chordata_family_df <- Chordata_df[Chordata_df$taxlevel == 'Family',] 
Chordata_order_df <- Chordata_df[Chordata_df$taxlevel == 'Order',] 

Streptophyta_genus_df <- Streptophyta_df[Streptophyta_df$taxlevel == 'Genus',]
Streptophyta_family_df <- Streptophyta_df[Streptophyta_df$taxlevel == 'Family',]
Streptophyta_order_df <- Streptophyta_df[Streptophyta_df$taxlevel == 'Order',]
#how big is the dataset? ie how long will it take to run on bwb ref seq
#pretty large, def a over night type of operation

#get the unique to traditional surveys taxa names into a list & deduplicate the list
#7 is the column with unique traditional taxa, 6 is the column with unique eDNA taxa
Anthropoda_unique_traditional_genus <- create_list_then_deduplicate(Anthropoda_genus_df,7) #733 unique anthropoda genus to traditional survey
Anthropoda_unique_traditional_family <- create_list_then_deduplicate(Anthropoda_family_df,7) #153unique anthropoda family to traditional survey
Anthropoda_unique_traditional_order <- create_list_then_deduplicate(Anthropoda_order_df,7) #14 unique anthropoda orders to traditional survey

Chordata_unique_traditional_genus <- create_list_then_deduplicate(Chordata_genus_df,7) #346 unique Chordata genus to traditional survey
Chordata_unique_traditional_family <- create_list_then_deduplicate(Chordata_family_df,7) #115 unique Chordata genus to traditional survey
Chordata_unique_traditional_order <- create_list_then_deduplicate(Chordata_order_df,7) #34 unique Chordata genus to traditional survey


Streptophyta_unique_traditional_genus <- create_list_then_deduplicate(Streptophyta_genus_df,7) #680unique Streptophyta genus to traditional survey
Streptophyta_unique_traditional_family <- create_list_then_deduplicate(Streptophyta_family_df,7) #140 unique Streptophyta family to traditional survey
Streptophyta_unique_traditional_order <- create_list_then_deduplicate(Streptophyta_order_df,7) #46 unique Streptophyta orders to traditional survey



#print the list and take it to bwb reference sequence browser
print_list_comma_seperated(Anthropoda_unique_traditional_family)#on desktop
print_list_comma_seperated(Anthropoda_unique_traditional_genus)



# messing around ----------------------------------------------------------


hi<- str_split(UCNRS_site_comparison_data[1,7],";", n = Inf, simplify = FALSE)
hi[[1]][2]

hi<-strsplit(UCNRS_site_comparison_data[1,7],";")
class(UCNRS_site_comparison_data[1,7])
length(hi[1,1])
class(hi)
#sort data into Anthropoda, Chordata, Streptophyta phylum
studentdata[studentdata$Drink == 'water',]

Anthropoda_df <- UCNRS_site_comparison_data[UCNRS_site_comparison_data$Phylum == 'Arthropoda',] 
Anthropoda_genus_df <- Anthropoda_df[Anthropoda_df$taxlevel == 'Genus',]


cheeck <- create_list_then_deduplicate(Anthropoda_genus_df,7)
print(cheeck)
length(cheeck[[1]])
print_list_comma_seperated(cheeck)
cheeck[[1]]
length(hi[[1]])
class(cheeck)
file.show("cheeck")

write.csv(cheeck, file = "firstconvert2.csv", row.names= FALSE)
vectStr=paste(as.character(cheeck), sep="' '", collapse=",")

file.show("cheeck")
sink("newcheck")
newcheck<-cat(vectStr) 
#cat("\n")
sink()

# function ----------------------------------------------------------------

print_list_comma_seperated <- function(list)#puts list or column of a dataframe into your working directory as a comma seperated files
{
  vectStr=paste(as.character(list), sep="' '", collapse=",")
  cat(vectStr) 
  
}

create_list_then_deduplicate <- function(dataframe,# take in this data that is all ; seperated and put it into a list then dedup it
                                         column#column with taxa names seperated by ;  
                                         #for this we will use the column with unique traditional observations taxa (7)
)
{
  list <- c() # list
  x<-nrow(dataframe)
  for(i in 1:x)
  {
    if(   is.na(dataframe[i,column])  )
    {
      #print hello
      #print(i)
    }else
    {
      splitstring <- strsplit(dataframe[i,column],";")
      for(y in 1:length(splitstring[[1]])) #length of first list, rather than length of list of lists
      {
        #print(splitstring[[1]][y]) #list 1, entry y in list 1
       # class(splitstring)
        list <- c(list,splitstring[[1]][y])
        #list <- list + splitstring[y]
        
      }
    }
  }
  print(length(list))
  return<-unique(list)
  print(length(return))
  return
}

