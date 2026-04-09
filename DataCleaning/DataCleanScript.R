## Shelter dog blood project - TBD##
# Rec'd 273 samples from Gigi's to test for A. platys and E. canis in 2020.
# Goal is to determine prevalence of these pathogens, co-infection, and whether location, age, or sex is a significant risk factor. 

#need to rerun all log reg with fixed data and find a place to keep notes - make a note in remarkable my files - tick stuff - tbd research notebook

#setwd("~/Career/Research/Tick Study 2020/Tick data") this is old code holding incase i mess somethign up

#setwd("C:/Users/Colleen/OneDrive/Documents/Career/Research/Tick Study 2020") #have to run this every time you start
#setwd("D:/manuscript 2026")

setwd("C:/Users/zeige/Documents/School/ENR_PhD_Courses/6193_Stat/git/ENR6193_Biostats/DataCleaning")
library(plyr) #
library(dplyr) #
library(tidyverse) #used for coding styles 
library(data.table) # 

#install.packages("AICcmodavg")
library(AICcmodavg)

# Viewing Data ----------------------------------

#library(readr) #updated 2.20.26
#Tick_Data_5_10_24_cs_2_20_26 <- read_csv("Tick Data  5.10.24 cs  2.20.26.csv")
#View(Tick_Data_5_10_24_cs_2_20_26)
#ShelterTBD  <- read_csv("Tick Data  5.10.24 cs  2.20.26.csv")

#library(readr) #updated 2.20.26
#Tick_Data_5_10_24_cs_2_20_26 <- read_csv("Tick Data  5.10.24 cs  2.20.26.csv")
#View(Tick_Data_5_10_24_cs_2_20_26)
#ShelterTBD  <- read_csv("Tick Data  5.10.24 cs  2.20.26.csv")

library(readxl) #updated 2.20.26
#Tick_Data_01_23_2024_final_CORRECTED_5_10_24_cs_updated_again <- read_csv("Tick Data 01.23.2024 - final - CORRECTED 5.10.24 cs updated again 2.20.26.csv")
#View(Tick_Data_01_23_2024_final_CORRECTED_5_10_24_cs_updated_again_2_20_26)
#ShelterTBD  <- read_excel("Tick Data 01.23.2024 - final - CORRECTED 5.10.24 cs updated again 2.20.26.csv")

#library(readxl)
#Tick_Data_01_23_2024_final_CORRECTED_5_9_24_cs <- read_excel("Tick data/Tick Data 01.23.2024 - final - CORRECTED 5.9.24 cs.xlsx")
#View(Tick_Data_01_23_2024_final_CORRECTED_5_9_24_cs)
#ShelterTBD <- read_excel("Tick data/Tick Data 01.23.2024 - final - CORRECTED 5.9.24 cs.xlsx")
#View(ShelterTBD)  #i think i worked! 

Tick_Data_final_2_20_26 <- read.csv("Tick Data  final  2.20.26.csv")  #updated 3.25.26 due to moving to different computer 
view(Tick_Data_final_2_20_26)
df <-read.csv("Tick Data  final  2.20.26.csv")
view(df)

getwd()

df %>% count(sex)
count

str(df$sex)

df$sex <- df$sex %>%
  str_squish()

df$sex <- as.factor(df$sex)
