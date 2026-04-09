## Shelter dog blood project - TBD##
# Rec'd 273 samples from Gigi's to test for A. platys and E. canis in 2020.
# Goal is to determine prevalence of these pathogens, co-infection, and whether location, age, or sex is a significant risk factor. 

#need to rerun all log reg with fixed data and find a place to keep notes - make a note in remarkable my files - tick stuff - tbd research notebook

#setwd("~/Career/Research/Tick Study 2020/Tick data") this is old code holding incase i mess somethign up

#setwd("C:/Users/Colleen/OneDrive/Documents/Career/Research/Tick Study 2020") #have to run this every time you start
#setwd("D:/manuscript 2026")

setwd("~/manuscript 2026")
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
ShelterTBD <-read.csv("Tick Data  final  2.20.26.csv")
view(ShelterTBD)


# Wish to calculate prevalence of TBD in dogs overall, and by county 

#Helpful codes to troubleshoot stuff
str(ShelterTBD)  ### What does this mean?  displays the internal structure of an object such as an array, list, matrix, factor, or data frame.
glimpse(ShelterTBD) # view the data columns run down the page, and data runs across. This makes it possible to see every column in a data frame. It's a little like str() applied to a data frame but it tries to show you as much data as possible
# observations = _____  = samples
ShelterTBD$sex <- trimws(ShelterTBD$sex) #had to run this to remove spaces that was causing the issue with multiple f/m and m columns
ShelterTBD$ticks <- trimws(ShelterTBD$ticks)#also used the above for ticks Y/N
#na.strings=c(" ") # to convert all blanks to NA use this code if you are importing the data
ShelterTBD[ShelterTBD == ""] <- NA #if data is already imported
# , useNA = "always" in this R you have to run this in every code if you want the table to include NA
#names() will show all column names exactly as R sees them.
#str() will show column types and the number of rows.
#colSums(is.na(ShelterTBD)) will show columns with NA to check for missingness



# Descriptive Data------------------------------

#ShelterTBD$sex <- trimws(ShelterTBD$sex) had to run this to remove spaces that was causing the issue with multiple f/m and m columns

table(ShelterTBD$sex) # prints a table showing how many of each category, confirms that all NA are removed      
# Female   Male 
# 134    139 

table(ShelterTBD$county)
# Athens  Delaware  Franklin  Gallia  Jackson  Lawrence  Richland  Ross Scioto
# 2         2             3      68      67       38        1       43    49

table(ShelterTBD$county, ShelterTBD$sex) 
#           female male
# Athens      0      2
# Delaware    2      0
# Franklin    1      2
# Gallia      39     29
# Jackson     32     35
# Lawrence    18     20
# Richland    0      1
# Ross        20     23 #updated this number 
# Scioto      22     27 

table(ShelterTBD$sex,ShelterTBD$tbdpos, useNA = "always")

#####updated 2.20.26####
#0  1
#Female 82 52
#Male   72 67

#old
#        N  Y
#Female 83 51
#Male   72 67

table(ShelterTBD$sex,ShelterTBD$`multtbd`, useNA = "always")
#        N   Y
#Female 110  24
#Male   105  34


table(ShelterTBD$county, ShelterTBD$`ticks`, useNA = "always") ### this one works without titles

#           N  Y <NA>
#Athens     2  0    0
#Delaware   2  0    0
#Franklin   3  0    0
#Gallia    53 14    1
#Jackson   50 17    0
#Lawrence  25 13    0
#Richland   1  0    0
#Ross      39  4    0
#Scioto    37 12    0



table(ShelterTBD$county, ShelterTBD$'tbdpos', useNA = "always")
######2.20.26 updated one ####
#0  1
#Athens    2  0
#Delaware  2  0
#Franklin  3  0
#Gallia   37 31
#Jackson  36 31
#Lawrence 18 20
#Richland  1  0
#Ross     32 11
#Scioto   23 26

#older one
#          N  Y
#Athens    2  0
#Delaware  2  0
#Franklin  3  0
#Gallia   37 31
#Jackson  37 30
#Lawrence 18 20
#Richland  1  0
#Ross     32 11
#Scioto   23 26

table(ShelterTBD$'county', ShelterTBD$'multtbd', useNA = "always")

#          0  1
#Athens    2  0
#Delaware  2  0
#Franklin  3  0
#Gallia   53 15
#Jackson  56 11
#Lawrence 26 12
#Richland  1  0
#Ross     37  6
#Scioto   35 14

table(ShelterTBD$'county', ShelterTBD$'furlength', useNA = "always")
# updated this number 
#             Long short <NA>
#Athens         0     2    0
#Delaware       2     0    0
#Franklin       0     2    1
#Gallia         2    56   10
#Jackson       13    51    3
#Lawrence       5    27    6
#Richland       0     1    0
#Ross           2    39    2
#Scioto         3    44    2
 
table(ShelterTBD$'county', ShelterTBD$size, useNA = "always")
# updated this number 
#           Large Medium Small
#Athens       2      0     0
#Delaware     0      0     2
#Franklin     0      1     2
#Gallia      29     22    17
#Jackson     24     30    13
#Lawrence    14     17     7
#Richland     1      0     0
#Ross        19     21     3
#Scioto      21     21     7


table(ShelterTBD$`ticks`, useNA = "always")
#  N    Y   <NA> 
# 212   60    1 
# updated this number 

table(ShelterTBD$`X4DX.run`, useNA = "always")  # had to change 4DX - RUn to x4dx.run idk why but thats the new name
#  N    Y  <NA> 
# 31  242    0 

table(ShelterTBD$ticks, ShelterTBD$'tbdpos', useNA = "always")
#####Updated 2.20.26
#           N tbd   Y tbd 
#N ticks     133      79
#Y ticks      21      39
#NA ticks     0        1

#old
# updated this number 
#           N tbd   Y tbd 
#N ticks     134      78
#Y ticks      21      39


table(ShelterTBD$ticks, ShelterTBD$'multtbd', useNA = "always")
# updated this number 
#          N tbd mult   Y tbd mult
#N ticks     178        34
#Y ticks      37        23
#NA ticks     0          1

table(ShelterTBD$`ticks`, ShelterTBD$`X4DX.run`, useNA = "always") 
# updated this number 
#        4DX run  N   Y
# N ticks         30  182
# Y Ticks         1   59
#NA ticks         0   1

table(ShelterTBD$`ticks`, ShelterTBD$`rickifa`, useNA = "always") 
# updated this number 
#                 negrickifa  posrickifa     NA
#     N ticks       180          30           2
#     Y ticks       30           22           8
#    NA ticks        1            0
table(ShelterTBD$`lymeseropos`, useNA = "always") 
#####updated 2.20.26####### 
# Negative   Positive  NA  
# 161        81        31

# updated this number 
# Negative   Positive  NA  
# 161        80        32

table(ShelterTBD$`anaseropos`, useNA = "always") 
#####updated 2.20.26####### 
# Negative   Positive   NA
# 238        4          31

# updated this number 
# Negative   Positive   NA
# 237        4          32

table(ShelterTBD$`ehrseropos`, useNA = "always") 
#####updated 2.20.26####### 
# Negative   Positive   NA
# 183        59         31

# updated this number 
# Negative   Positive   NA
# 182        59         32
#There 59 here and 58 for the ones in clinical signs because A46231415 was HE before further testing

table(ShelterTBD$`X4DX.hw`, useNA = "always") #renamed X4DX.hw idk why
#####updated 2.20.26####### 
# Negative   Positive   NA 
# 235        7          31

# updated this number 
# Negative   Positive   NA 
# 234        7          32

table(ShelterTBD$`rickifa`, useNA = "always") 
# updated this number 
# Negative   Positive   NA
# 211        52         10

table(ShelterTBD$`lyme.CS`, useNA = "always") 
#####updated 2.20.26####### 
# Negative   Positive   NA 
# 199        7         1 or 67?  (reran and there were 67 NA's (3.26.26) - extra negative???? matches excel sheet

# updated this number 
# Negative   Positive  NA  
# 198        7         1


table(ShelterTBD$`lyme.CS`, ShelterTBD$'lymeseropos', useNA = "always")
#####updated 2.20.26####### 
#      4DX lyme serpos    no     yes   NA  
#No cs                    127     72   0 
#yes CS                   0        7   0
#NA                       0        1   0
#updated NA 3.26.26       34       2   31

# updated this number 
#      4DX lyme serpos    no     yes   NA   
#No cs                    127     71   0  # why are there nas but not numbers? there wre 32 NAs but blanks for cs? IDK
#yes CS                   0        7   0
#NA                       0        1   0


table(ShelterTBD$`lyme.lame`, ShelterTBD$'lymeseropos', useNA = "always")  # lyme.lame new name
#####updated 2.20.26####### 
# 4DX lyme positive      no   yes  NA
#not lame              127    77  0 
#lame                   0     3   0
#  <NA>                 34    1   31

# updated this number 
# 4DX lyme positive      no   yes  NA
#not lame              127    76  0   # why are there nas but not numbers? there wre 32 NAs but blanks for cs? IDK
#lame                   0     3   0

table(ShelterTBD$`lyme.abnormal.pcv`, ShelterTBD$'lymeseropos', useNA = "always") #lyme.abnormal.pcv
#####updated 2.20.26####### 
#       4DX lyme positive  no    yes  NA  
#normal PCV                127  80   0 
#abnormal PCV              0    0     0

# updated this number 
#       4DX lyme positive  no    yes  NA  
#normal PCV                127  79    0  # why are there nas but not numbers? there wre 32 NAs but blanks for cs? IDK
#abnormal PCV              0    0     0

table(ShelterTBD$`lyme.abnormal.urine`, ShelterTBD$'lymeseropos', useNA = "always") #updated lyme.abnormal.urine
#####updated 2.20.26####### 
# 4DX lyme positive      no   yes   NA
#normal urine           127    75   0
#Abnormal urine          0      4   0 
# NA urine              34      1  31

# updated this number 
# 4DX lyme positive      no   yes   NA
#normal urine           127    74   0
#Abnormal urine          0      4   0 # why are there nas but not numbers? there wre 32 NAs but blanks for cs? IDK
# NA urine               0      1   0


table(ShelterTBD$`ehrlichia.CS`, useNA = "always") # ehrlichia.CS
# updated this number
# Ehrlichia CS  no    yes  NA 
#               194    10   1 or 69? (updated with useNA has 69)

table(ShelterTBD$'ehrlichia.CS', ShelterTBD$`ehrseropos`, useNA = "always")
# updated this number
##       4DX ehr positive    no      yes   NA
##No cs                     134       45   15
#yes CS                      6        2    2
#NA                          1        0    0
#NA updated w/ useNA 3.28.26 43        12    14   
#this is an old note but now i'm really confused ---- There 59 ehr positive and 58 for the ones in clinical signs because A46231415 was HE before further testing

table(ShelterTBD$'ehlrichia.abnormal.pcv', ShelterTBD$'ehrseropos', useNA = "always")
# updated this number
# 4dx erh positive     no   yes   NA  
# nornmal pcv         147  47    17
# yes abn pcv          0     1     0   
#NA                    1     0     0
#NA upd useNA 3.28.26  43    11   14 

table(ShelterTBD$`ehlrichia.abnormal.plt.count`, ShelterTBD$`ehrseropos`, useNA = "always")
# updated this number
#   4dx erh pos          no   yes   NA  
# Normal plt count      135   46    15
# Abn plt count 1        5     2     2
#NA                      1     0     0
#NA upd useNA 3.28.26   43    11   14 

table(ShelterTBD$`ticks`, ShelterTBD$`X4DX.run`, useNA = "always") 
# updated this number
### ###            No 4dx    Yes 4dx 
###  No ticks         30         182
### Y ticks           1           59
#NA upd w/ useNA      0            1    

table(ShelterTBD$`lymeseropos`, ShelterTBD$'county', useNA = "always") 
#####updated 2.20.26####### 
##   Athens Delaware Franklin Gallia Jackson Lawrence Richland Ross Scioto
#neg      2        0        1     42      43       15        1   25     32
#pos      0        0        0     22      21       16        0    9     13
##NA      0        2        2      4      3        7         0    9      4 

# updated this number
##   Athens Delaware Franklin Gallia Jackson Lawrence Richland Ross Scioto
#neg      2        0        1     42      43       15        1   25     32
#pos      0        0        0     22      20       16        0    9     13
##NA      0        2        2      4       4        7        0    9      4 

table(ShelterTBD$`age.months`, useNA = "always")

#Months 0.1 0.2 0.3 0.4 0.6 0.9   1 1.5   2   3   4   5   6   7   8   9  10  11  12  18  24 
#dogs     1   2   6   1   1   1   3   1   8  23   3   4  10   4   4   3  10   3  31  11  31 

#months 30  36  43  48  60  72  84  96 120 
#dogs    2  39   1  30  22   4   7   5   2 


table(ShelterTBD$'age') #age in years

#   1 y    1 year      1 yr      1 YR     1.5 m     1.5 y 1.5 years    1.5 yr      10 m     10 mo 10 months     10 yr 
#   6         2        25         1         3         1         3         8         8         1         1         2 
#11 m       2 m       2 y      2 yr    2.5 yr       3 m      3 mo  3 months       3 y      3 yr    3.6 yr       4 m 
#4         6         7        26         2        14         2         7        13        32         1         2 
#4 y      4 yr     4 yrs         5       5 m       5 y   5 years      5 yr       6 m      6 mo       6 y      6 yr 
#11        19         1         1         1        10         1        14         6         3         1         4 
#7 m       7 y      7 yr       8 m  8 months       8 y      8 yr       8yr       9 m  9 months       9 y 
#5         3         4         2         1         3         2         1         1         1         1 

### Coding the age to puppy (0-5 months), Adolescent 6-24 mo, adult 24-72 mo, senior > 72 mo ---
### Age Groups ===
table(ShelterTBD$'agestages')
# Adolescence       Adult       Puppy      Senior 
#        107          98          54          14 

table(ShelterTBD$`agestages`,ShelterTBD$tbdpos)   
#####updated 2.20.26####### 
#                 N  Y
#Adolescence     62 45
#Adult           42 56
#Puppy           47  7
#Senior           3 11

#old
#                 N  Y
#Adolescence     62 45
#Adult           43 55
#Puppy           47  7
#Senior           3 11

table(ShelterTBD$`agestages`,ShelterTBD$`multtbd`) 
#             N  Y
#Adolescence 89 18
#Adult       67 31
#Puppy       51  3
#Senior       8  6

table(ShelterTBD$'agestages', ShelterTBD$`lymeseropos`, useNA = "always") # redo this and make categorical
#####updated 2.20.26####### 
#  lyme seropos     no  yes   NA
# Adolescence       71  33     3
# Adult             59  39     0
# Puppy             23  3     28
# Senior             8  6      0

# updated this number
#  lyme seropos     no  yes   NA
# Adolescence       71  33     3
# Adult             59  38     1
# Puppy             23  3     28
# Senior             8  6      0


table(ShelterTBD$'agestages', ShelterTBD$`anaseropos`, useNA = "always")
#####updated 2.20.26####### 
#  4dx results
#               neg   pos   NA
# Adolescence   102     2    3
# Adult          97     1    0
# Puppy          26     0   28
# Senior         13     1    0

# updated this number
#  4dx results
#               neg   pos   NA
# Adolescence   102     2    3
# Adult          96     1    1
# Puppy          26     0   28
# Senior         13     1    0

table(ShelterTBD$'agestages', ShelterTBD$`ehrseropos`, useNA = "always")
#####updated 2.20.26#######
#  4dx results
#              neg  pos  NA
# Adolescence   88   16   3
# Adult         65   33   0
# Puppy         23    3  28
# Senior         7    7   0 


# updated this number
#  4dx results
#              neg  pos  NA
# Adolescence   88   16   3
# Adult         64   33   1
# Puppy         23    3  28
# Senior         7    7   0 
 
table(ShelterTBD$'agestages', ShelterTBD$`rickifa`, useNA = "always")
# updated this number
#           RICKIFA  neg  pos  NA
#Adolescence          87   18   2
#Adult                73   24   1
#Puppy                44    4   6
#Senior                7    6   1 

table(ShelterTBD$'agestages', ShelterTBD$`X4DX.run`, useNA = "always")
#  4dx run
#               N   Y
# Adolescence   3 104
# Adult         0  98
# Puppy        28  26
# Senior        0  14    

table(ShelterTBD$'agestages', ShelterTBD$county)
# County          Athens Delaware Franklin Gallia Jackson Lawrence Richland Ross Scioto
# Adolescence      0        0        1     22      36       14        0     21     13
# Adult            2        0        0     24      21       12        1     12     26
# Puppy            0        2        2     19       8       10        0      7      6
# Senior           0        0        0      3       2        2        0      3      4

table(ShelterTBD$county, ShelterTBD$`lymeseropos`, useNA = "always") 
#####updated 2.20.26#######
#      lyme seropos     neg   pos   NA  
#Athens                  2     0    0
#Delaware                0     0    2
#Franklin                1     0    2
#Gallia                 42    22    4
#Jackson                43    21    3
#Lawrence               15    16    7
#Richland                1     0    0
#Ross                   25     9    9
#Scioto                 32    13    4


# updated this number
#      lyme seropos     neg   pos   NA  
#Athens                  2     0    0
#Delaware                0     0    2
#Franklin                1     0    2
#Gallia                 42    22    4
#Jackson                43    20    4
#Lawrence               15    16    7
#Richland                1     0    0
#Ross                   25     9    9
#Scioto                 32    13    4

table(ShelterTBD$county, ShelterTBD$`anaseropos`, useNA = "always") 
###### updated this number 3.28.26#####
#    anaplassero      neg  pos  NA 
#Athens                 2  0    0
#Delaware               0  0    2
#Franklin               1  0    2
#Gallia                62  2    4
#Jackson               64  0    3  
#Lawrence              31  0    7  
#Richland               1  0    0
#Ross                  33  1    9  
#Scioto                44  1    4 

# updated this number
#    anaplassero      neg  pos  NA 
#Athens                 2  0    0
#Delaware               0  0    2
#Franklin               1  0    2
#Gallia                62  2    4
#Jackson               63  0    4  
#Lawrence              31  0    7  
#Richland               1  0    0
#Ross                  33  1    9  
#Scioto                44  1    4 

table(ShelterTBD$county, ShelterTBD$`ehrseropos`, useNA = "always") 
#####updated 3.28.26#######
#erhSerology     neg  pos  NA 
#Athens           2    0    0
#Delaware         0    0    2
#Franklin         1    0    2  
#Gallia          47   17    4  
#Jackson         51   13    3 ### increased number of NA but no other changes? 
#Lawrence        22    9    7
#Richland         1    0    0
#Ross            29    5    9  
#Scioto          30   15    4  


#####updated 2.20.26#######
#erhSerology     neg  pos  NA 
#Athens           2    0    0
#Delaware         0    0    2
#Franklin         1    0    2  
#Gallia          47   17    4  
#Jackson         51  13     0
#Lawrence        22    9    7
#Richland         1    0    0
#Ross            29    5    9  
#Scioto          30   15    4  

# updated this number
#erhSerology     neg  pos  NA 
#Athens           2    0    0
#Delaware         0    0    2
#Franklin         1    0    2  
#Gallia          47   17    4  
#Jackson         50   13    4  
#Lawrence        22    9    7
#Richland         1    0    0
#Ross            29    5    9  
#Scioto          30   15    4  



table(ShelterTBD$county, ShelterTBD$`rickifa`, useNA = "always") 
# updated numbers 
#           0  1  NA 
# Athens    2  0  0
# Delaware  2  0  0
# Franklin  2  0  1
# Gallia   52 13  3
# Jackson  53 11  3
# Lawrence 27  8  3
# Richland  1  0  0
# Ross     39  4  0
# Scioto   33 16  0

table(ShelterTBD$county, ShelterTBD$`X4DX.run`) 
#          N  Y
#Athens    0  2
#Delaware  2  0
#Franklin  2  1
#Gallia    4 64
#Jackson   3 64
#Lawrence  7 31
#Richland  0  1
#Ross      9 34
#Scioto    4 45

table(ShelterTBD$size)
#Large Medium  Small 
#110    112     51 


table(ShelterTBD$'size',ShelterTBD$'tbdpos')
#####updated 2.20.26#######
#       N  Y
#Large  51 59
#Medium 62 50
#Small  41 10

#old
#       N  Y
#Large  52 58
#Medium 62 50
#Small  41 10

table(ShelterTBD$size,ShelterTBD$`multtbd`)
#       N  Y
#Large  77 33
#Medium 92 20
#Small  46  5

table(ShelterTBD$`size`, ShelterTBD$`ticks`, useNA = "always") 
# updated numbers 3.28.26 with useNA
#         N   Y   <NA>
#Large    93  17    0
#Medium   82  29    1
#Small    37  14    0

# updated numbers 
#        N  Y   
# Large  93 17
# Medium 82 29
# Small  37 14

table(ShelterTBD$`size`, ShelterTBD$`lymeseropos`, useNA = "always") 
#####updated 2.20.26#######
#   seropos lyme      Neg  Pos  NA 
# Large                 56 45    9
# Medium                76 29    7
# Small                 29  7   15

#updated numbers 
#   seropos lyme      Neg  Pos  NA 
# Large                 56 44   10
# Medium                76 29    7
# Small                 29  7   15

table(ShelterTBD$`size`, ShelterTBD$`anaseropos`, useNA = "always") 
#####updated 2.20.26#######
#  seropos ana       neg   pos  NA
# Large               98   3    9
# Medium             104   1    7
# Small               36   0    15

#updated numbers 
#  seropos ana       neg   pos  NA
# Large               97   3    10
# Medium             104   1    7
# Small               36   0    15

table(ShelterTBD$`size`, ShelterTBD$`ehrseropos`, useNA = "always") 
#Updated 3.28.26 with use NA
#           0   1  <NA>
#large     72  29    9 #### got an extra neg large dog?
#Medium    80  25    7
#Small     31   5   15

#updated numbers 
#  ehr serpos     neg  pos  NA
# Large            71   29   10
# Medium           80   25   7
# Small            31    5   15

table(ShelterTBD$`size`, ShelterTBD$`rickifa`, useNA = "always") 
#updated numbers 
#   RRICKIFA     neg   pos  NA
# Large           83   27   0
# Medium          89   22   1
# Small           39    3   9

table(ShelterTBD$`furlength`, ShelterTBD$ticks, useNA = "always")
#updated 3.28.26 with use NA 
#           N   Y <NA>
#Long      20   7    0
#short    178  43    1
#<NA>      14  10    0

#updated numbers 
#  ticks      N   Y
# Long       20   7
# Short     178  43
#NA          14  10


table(ShelterTBD$`furlength`, ShelterTBD$tbdpos, useNA = "always")
#####updated 2.20.26#######
#   tbd     N   Y
#Long      12  15
#Short    128  94
# NA      14   10

#updated numbers 
#   tbd     N   Y
#Long      12  15
#Short    129  93
# NA      14   10

table(ShelterTBD$`furlength`, ShelterTBD$`multtbd`, useNA = "always")
#updated numbers 
# multtbd      N   Y
#Long         20   7
#Short       178  44
#NA           17   7 

table(ShelterTBD$`furlength`, ShelterTBD$`lymeseropos`, useNA = "always") 
#####updated 2.20.26#######
# lyme seropos    neg pos  NA 
#         Long    11  12    4
#         Short  138  61   23
#         NA      12   8    4 

#updated numbers 
# lyme seropos    neg pos  NA 
#         Long    11  12    4
#         Short  138  60   24
#         NA      12   8    4 

table(ShelterTBD$`furlength`, ShelterTBD$`ehrseropos`, useNA = "always") 
#####updated 2.20.26#######
# ehr sero          neg  pos   NA
#            Long   16   7     4
#           Short  156   43   23
#             NA    11    9    4


#updated numbers 
# ehr sero          neg  pos   NA
#            Long   16   7     4
#           Short  155   43   24
#             NA    11    9    4

table(ShelterTBD$`furlength`, ShelterTBD$`anaseropos`, useNA = "always") 
#####updated 2.20.26#######
# anaseropos              neg pos  NA
#                  Long   23   0    4
#                  Short 196   3    23
#                   NA    19   1    4


#updated numbers 
# anaseropos              neg pos  NA
#                  Long   23   0    4
#                  Short 195   3    24
#                   NA    19   1    4

table(ShelterTBD$`furlength`, ShelterTBD$`rickifa`, useNA = "always") 
#updated numbers 
# RICKIFA                 neg   pos  NA
#                 Long     21   4    2
#                 Short   173  45    4
#                   NA     17   3    4 

table(ShelterTBD$`sex`, ShelterTBD$`rickifa`, useNA = "always") 
#updated numbers 
#   RICKIFA  Neg  Pos NA 
# Female     106  24  4
# Male       105  28  6

table(ShelterTBD$`sex`, ShelterTBD$`lymeseropos`, useNA = "always") 
#####updated 2.20.26#######
#  lyme pos       neg   pos  NA 
#Female           85     33  16
#Male             76     48  15

#updated numbers 
#  lyme pos       neg   pos  NA 
#Female           85     32  17
#Male             76     48  15

table(ShelterTBD$`sex`, ShelterTBD$`anaseropos`, useNA = "always") 
#####updated 2.20.26#######
#    anaseropos   neg   pos  NA
#Female           117   1    16
#Male             121   3    15

#updated numbers 
#    anaseropos   neg   pos  NA
#Female           116   1    17
#Male             121   3    15


table(ShelterTBD$`sex`, ShelterTBD$`ehrseropos`, useNA = "always") 
#####updated 2.20.26#######
#  ehrserpos      neg  pos  NA 
#Female            93  25   16
#Male              90  34   1

#updated numbers 
#  ehrserpos      neg  pos  NA 
#Female            92  25   17
#Male              90  34   15


table(ShelterTBD$tbdpos, useNA = "always")
#####updated 2.20.26#######
#neg   pos 
#154   119 

#old
#neg   pos 
#155   118 

table(ShelterTBD$multtbd, useNA = "always")
#  neg   pos 
#  215    58 

######left off her 2.20.26 double checking#########
#####################################################################
# Prevalence testing  ---------------------------------------
?prop.test
#prop.test(x, n, p = NULL, alternative = c("two.sided", "less", "greater"), conf.level = 0.95, correct = TRUE)

prop.test(79,212) #prevalence of TBD in dogs with no ticks is 37.2% with 95% CI (30.4%-43.7%) The 79 is manually entered from the number of dogs with TBD and without ticks on exam, the 212 bis from adding the number for dogs without ticks (with or without tbd) together manually
#updated 3.28.26 - 78 changes to 79)
#prop.test(79,212)
#1-sample proportions test with continuity correction

#data:  79 out of 212, null probability 0.5
#X-squared = 13.25, df = 1, p-value = 0.0002726
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3081304 0.4418487
#sample estimates:
#  p 
#0.3726415 


#old  prop.test(78,212) #prevalence of TBD in dogs with no ticks is 36.79% with 95% CI (30.4%-43.7%) The 78 is manually entered from the number of dogs with TBD and without ticks on exam, the 212 bis from adding the number for dogs without ticks (with or without tbd) together manually
#1-sample proportions test with continuity correction
#data:  78 out of 212, null probability 0.5
#X-squared = 14.269, df = 1, p-value = 0.0001585
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3036665 0.4370530
#sample estimates:
#  p 
#0.3679245


prop.test(39,60)  #prevalence of TBD in dogs with ticks is 65% with 95% CI (51.5%-76.5%)
#updated numbers 
#1-sample proportions test with continuity correction
#data:  39 out of 60, null probability 0.5
#X-squared = 4.8167, df = 1, p-value = 0.02819
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.5152101 0.7655334
#sample estimates:
#  p 
#0.65 

prop.test(34,212) #prev ticks with multi tbd in dogs with no ticks is 16% with 95% CI (11.5%-21.8%)
#updated numbers
#1-sample proportions test with continuity correction
#data:  34 out of 212, null probability 0.5
#X-squared = 96.458, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1150646 0.2183468
#sample estimates:
#  p 
#0.1603774 

prop.test(23,60) #prev ticks with multi tbd in dogs with ticks is 38.3% with 95% CI (26.4%-51.8%)
#updated numbers
#1-sample proportions test with continuity correction
#data:  23 out of 60, null probability 0.5
#X-squared = 2.8167, df = 1, p-value = 0.09329
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2635284 0.5180989
#sample estimates:
#  p 
#0.3833333 

###lyme locations ====


prop.test(80, 241) #lyme prev all locations not including 32 na - Prev is 33.2% with 95% CI (27.4-39.6%)
# 1-sample proportions test with continuity correction

# data:  80 out of 241, null probability 0.5
# X-squared = 26.556, df = 1, p-value = 2.56e-07
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#  0.2735920 0.3957641
#sample estimates:
# p 
# 0.3319502 

13+32

prop.test(13,45) # scioto lyme prev not including 4 na - prev 28.9% with 95% CI 16.8-44.5%

#1-sample proportions test with continuity correction

#data:  13 out of 45, null probability 0.5
#X-squared = 7.2, df = 1, p-value = 0.00729
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1684318 0.4452073
#sample estimates:
 # p 
#0.2888889 

prop.test(0,2) #athens lyme prev
 
# 1-sample proportions test with continuity correction

# data:  0 out of 2, null probability 0.5
# X-squared = 0.5, df = 1, p-value = 0.4795
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
 # 0.0000000 0.8021325
#sample estimates:
#  p 
#0 
#Warning message:
#In prop.test(0, 2) : Chi-squared approximation may be incorrect

prop.test(0,1)  # Franklin prev lyme not including 2 NA
#1-sample proportions test with continuity correction

#data:  0 out of 1, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0000000 0.9453792
#sample estimates:
#  p 
#0 
#Warning message:
#In prop.test(0, 1) : Chi-squared approximation may be incorrect

22+42
prop.test(22,64) # gallia prev lyme not included 4 NA prev is 34.4% with 95% CI 23.2-47.4%

#1-sample proportions test with continuity correction

#data:  22 out of 64, null probability 0.5
#X-squared = 5.6406, df = 1, p-value = 0.01755
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2324524 0.4738932
#sample estimates:
#  p 
#0.34375 

43+20
prop.test(20,63) #Jackson lyme prev not inlcuding 4 NA - prev is 31.7% with 95% CI 20.9-44.8%

#1-sample proportions test with continuity correction

#data:  20 out of 63, null probability 0.5
#X-squared = 7.6825, df = 1, p-value = 0.005576
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2091041 0.4481985
#sample estimates:
#  p 
#0.3174603 

16+15
prop.test(16,31) #lawrence lyme prev no including 6 NA - prev 51.6% with 95% CI 33.4-69.4%
#1-sample proportions test with continuity correction

#data:  16 out of 31, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3339764 0.6944174
#sample estimates:
#  p 
#0.516129 

prop.test(0,1) # richland lyme prev 
# 1-sample proportions test with continuity correction

#data:  0 out of 1, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0000000 0.9453792
#sample estimates:
#  p 
#0 

9+25 # N
prop.test(9,34) #ross lyme prev not including 9 NA - prev 26.5% with 95% CI 13.5-44.7% 
# 1-sample proportions test with continuity correction

#data:  9 out of 34, null probability 0.5
#X-squared = 6.6176, df = 1, p-value = 0.0101
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1351002 0.4465129
#sample estimates:
#  p 
# 0.2647059 

table(ShelterTBD$tbdpos)
#N   Y 
#155 118 
118+155

161+80


table(ShelterTBD$`X4DX.run`, ShelterTBD$lymeseropos)


prop.test(118,273)  #prev TBD positive test out of all test results in study 43.2% with 95% CI 37.3-49.3%
#1-sample proportions test with continuity correction
#data:  118 out of 273, null probability 0.5
#X-squared = 4.7473, df = 1, p-value = 0.02935
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3730358 0.4933719
#sample estimates:
#  p 
#0.4322344 

####all counties prev ====
table(ShelterTBD$county, ShelterTBD$tbdpos)
#          N  P
#Athens    2  0
#Delaware  2  0
#Franklin  3  0
#Gallia   37 31
#Jackson  37 30
#Lawrence 18 20
#Richland  1  0
#Ross     32 11
#Scioto   23 26

prop.test(31,68) #prev gallia tbd 45.6% with 95% CI 33.6-58%
#1-sample proportions test with continuity correction
#data:  31 out of 68, null probability 0.5
#X-squared = 0.36765, df = 1, p-value = 0.5443
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3362436 0.5805243
#sample estimates:
#  p 
#0.4558824 

prop.test(30,67) #prev jackson tbd 44.8% wiht 95% CI 32.8-57.4%
#1-sample proportions test with continuity correction
#data:  30 out of 67, null probability 0.5
#X-squared = 0.53731, df = 1, p-value = 0.4635
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3278910 0.5736421
#sample estimates:
#  p 
#0.4477612 

prop.test(20,38) #prev lawrence tbd 52.6 with 95% CI 36-68.7% 
#1-sample proportions test with continuity correction
#data:  20 out of 38, null probability 0.5
#X-squared = 0.026316, df = 1, p-value = 0.8711
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3604968 0.6869209
#sample estimates:
#  p 
#0.5263158 

prop.test(11,43) # prev ross tbd 25.6% with 95% CI 12-41.5% 
#1-sample proportions test with continuity correction
#data:  11 out of 43, null probability 0.5
#X-squared = 9.3023, df = 1, p-value = 0.002289
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1403623 0.4146967
#sample estimates:
#  p 
#0.255814 

prop.test(26,49) #prev scioto tbd 53% with 95%CI 38.4-67.2%
#	1-sample proportions test with continuity correction
#data:  26 out of 49, null probability 0.5
#X-squared = 0.081633, df = 1, p-value = 0.7751
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3842160 0.6722446
#sample estimates:
#  p 
#0.5306122 

table(ShelterTBD$multtbd)
215++58

prop.test(58,273) ### all multiple tbd prev 21.2% with 95% CI 16.6-26.7%
#updated numbers
#1-sample proportions test with continuity correction
#data:  58 out of 273, null probability 0.5
#X-squared = 89.143, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1664389 0.2667476
#sample estimates:
#  p 
#0.2124542 

prop.test(81,242) ##UDPATED 3.28.26 - one extra lyme and extra test?!? one less NA - all counties lyme prev not including 31 NA - 33.5% with 95% CI 27.6-39.8%
#1-sample proportions test with continuity correction

#data:  81 out of 242, null probability 0.5
#X-squared = 25.789, df = 1, p-value = 3.808e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2763052 0.3984598
#sample estimates:
#  p 
#0.3347107 
######updated extra lyme?and extra test######
#prop.test(80,241 ) ##all counties lyme prev not including 32 NA - 33.2% with 95% CI 27.4-39.6%
#	1-sample proportions test with continuity correction
#data:  80 out of 241, null probability 0.5
#X-squared = 26.556, df = 1, p-value = 2.56e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2735920 0.3957641
#sample estimates:
#  p 
#0.3319502 

##UDPATED 3.28.26 - one extra neg test 
prop.test(4,242) #1.7% with 95% CI 0.5-4.5%  all counties ana prev not including 31 na
#1-sample proportions test with continuity correction

#data:  4 out of 242, null probability 0.5
#X-squared = 224.33, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.00530799 0.04460367
#sample estimates:
#  p 
#0.01652893 

#prop.test(4,241 ) ##all counties ana prev not including 32 NA - 1.7% with 95% CI 0.5-4.5%
#1-sample proportions test with continuity correction
#data:  4 out of 241, null probability 0.5
#X-squared = 223.34, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.00533007 0.04478525
#sample estimates:
#  p 
#0.01659751 

##UDPATED 3.28.26 - one extra neg test 
prop.test(59,242) ##all counties ehr prev not including 31 NA - 24.4% with 95% CI 19.2-30.4% 

#1-sample proportions test with continuity correction

#data:  59 out of 242, null probability 0.5
#X-squared = 62.517, df = 1, p-value = 2.642e-15
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1921010 0.3038114
#sample estimates:
#  p 
#0.2438017 

#prop.test(59,241 ) ##all counties ehr prev not including 32 NA - 24.5% with 95% CI 19.3-30.5% 
# 1-sample proportions test with continuity correction
# data:  59 out of 241, null probability 0.5
#X-squared = 61.759, df = 1, p-value = 3.881e-15
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1929205 0.3050164
#sample estimates:
#  p 
# 0.2448133 

prop.test(52,263) ##all counties rick not including 10 NA prev 19.8% with 95% CI 15.2-25.2%
#updated numbers
#1-sample proportions test with continuity correction
#data:  52 out of 263, null probability 0.5
#X-squared = 94.92, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1523827 0.2520978
#sample estimates:
#  p 
#0.1977186 


#### ehr location prev===== 

prop.test(15,45) ##scioto ehr prev not including 4 NA - 33.3% with 95CI 20.4 - 49.1%
#1-sample proportions test with continuity correction
#data:  15 out of 45, null probability 0.5
#X-squared = 4.3556, df = 1, p-value = 0.03689
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2043995 0.4905052
#sample estimates:
#  p 
#0.3333333 

prop.test(0,2) ##athens ehr prev not run

prop.test(0,1) ##franklin ehr prev not run 

prop.test(17,64) ##gallia  ehr prev not inlcuding 4 NA - 26.6% with 95CI 16.7-39.3%
#1-sample proportions test with continuity correction
#data:  17 out of 64, null probability 0.5
#X-squared = 13.141, df = 1, p-value = 0.000289
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1665366 0.3931029
#sample estimates:
#  p 
#0.265625 


#updated 3.28.26 extra negatvie than before with one less NA? jackson
prop.test(13,64)  #jackson ehr prev not inlcuidng 3 NA - 20.3% with 95ci 11.6-32.6% 
#1-sample proportions test with continuity correction 

#data:  13 out of 64, null probability 0.5
#X-squared = 21.391, df = 1, p-value = 3.746e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1166641 0.3257563
#sample estimates:
#  p 
#0.203125 

#prop.test(13,63) ##jackson ehr prev not inlcuidng 4 NA - 20.6% with 95ci 11.9-33.0% 
#1-sample proportions test with continuity correction
#data:  13 out of 63, null probability 0.5
#X-squared = 20.571, df = 1, p-value = 5.745e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1185808 0.3304352
#sample estimates:
#  p 
#0.2063492 

prop.test(9,31) ##lawrence ehr prev not inlcuding 7 NA 29.0% with 95CI 14.9-48.2%
#1-sample proportions test with continuity correction
#data:  9 out of 31, null probability 0.5
#X-squared = 4.6452, df = 1, p-value = 0.03114
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1488837 0.4823519
#sample estimates:
#  p 
#0.2903226 

prop.test(0,1) ##richland ehr prev not run

prop.test(5,34) ##ross ehr prev not including 9 NA- 14.7% with 95ci 5.5-31.8%
#1-sample proportions test with continuity correction
#data:  5 out of 34, null probability 0.5
#X-squared = 15.559, df = 1, p-value = 7.998e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.05543619 0.31834619
#sample estimates:
#  p 
#0.1470588 



#### anaplasma location prev ======

prop.test(1,45) ##scioto anaplasma prev not incuding 4 NA 2.2% with 95ci 0.1-13.2%
#1-sample proportions test with continuity correction
#data:  1 out of 45, null probability 0.5
#X-squared = 39.2, df = 1, p-value = 3.825e-10
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.001161097 0.132339248
#sample estimates:
#  p 
#0.02222222 

prop.test(0,2) ##athens anaplasma prev not run

prop.test(0,1) ##franklin anaplasma prev not run

prop.test(2,64) ##gallia  anaplasma prev not including 4 na - 3.1% with 95 ci 0.5-11.8%
#1-sample proportions test with continuity correction
#data:  2 out of 64, null probability 0.5
#X-squared = 54.391, df = 1, p-value = 1.643e-13
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.005431217 0.118140892
#sample estimates:
#  p 
#0.03125 

prop.test(0,63) ##jackson anaplasma prev 0
#1-sample proportions test with continuity correction
#data:  0 out of 63, null probability 0.5
#X-squared = 61.016, df = 1, p-value = 5.662e-15
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.00000000 0.07160284
#sample estimates:
#  p 
#0 

prop.test(0,31) ##lawrence anaplasma prev 0
#1-sample proportions test with continuity correction
#data:  0 out of 31, null probability 0.5
#X-squared = 29.032, df = 1, p-value = 7.118e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0000000 0.1372707
#sample estimates:
#  p 
#0 

prop.test(0,1) ##richland anaplasma prev not run 

prop.test(1,34) ##ross anaplasma prev notincluding 9 NA 2.9% with 95ci 0.2-17% 
#1-sample proportions test with continuity correction
#data:  1 out of 34, null probability 0.5
#X-squared = 28.265, df = 1, p-value = 1.058e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.001537215 0.170538191
#sample estimates:
#  p 
#0.02941176 

###RICK IFA ======

prop.test(16,49) ##scioto rick prev why 49 instead of 45 - pulled blood but didnt run 4dx  - 32.7% with 95 ci 20.4-47.7%
#1-sample proportions test with continuity correction
#data:  16 out of 49, null probability 0.5
#X-squared = 5.2245, df = 1, p-value = 0.02227
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2035831 0.4765784
#sample estimates:
#  p 
#0.3265306 

prop.test(0,2) ##athens rick prev not run
 
prop.test(0.2) ##franklin rick prev not run

prop.test(13,65) ##gallia  rick prev not incudling 3 NA prev 20% with 95ci 11.5-32.1%
#1-sample proportions test with continuity correction
#data:  13 out of 65, null probability 0.5
#X-squared = 22.215, df = 1, p-value = 2.437e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1148085 0.3212071
#sample estimates:
#  p 
#0.2 

prop.test(11,64) ##jackson rick prev not including 3 NA 17.2% with 95ci 9.3-29.1%
#1-sample proportions test with continuity correction
#data:  11 out of 64, null probability 0.5
#X-squared = 26.266, df = 1, p-value = 2.975e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.09293141 0.29096643
#sample estimates:
#  p 
#0.171875 


prop.test(8,35) ##lawrence rick prev not indlucing 3 NA prev 22.9% with 95ci 11.0-40.6% 
#1-sample proportions test with continuity correction
#data:  8 out of 35, null probability 0.5
#X-squared = 9.2571, df = 1, p-value = 0.002346
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1104440 0.4055202
#sample estimates:
#  p 
#0.2285714 

prop.test(0,1) ##richland rick prev not run

prop.test(4,43) ##ross rick prev 9.3% with 95ci 3.0-23.1%
#1-sample proportions test with continuity correction
#data:  4 out of 43, null probability 0.5
#X-squared = 26.884, df = 1, p-value = 2.161e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.03022409 0.23053742
#sample estimates:
#  p 
#0.09302326 

#### sex prev =====

table(ShelterTBD$sex, ShelterTBD$tbdpos)
#####updated 3.28/36 
#       0  1
#Female 82 52
#Male   72 67

#       0  1
#Female 83 51
#Male   72 67
52+82

#####updated 3.28/36 one more pos female than before
prop.test(52,134) #prev female TBD Pos 38.8% female tested pos with 95ci 30.6-47.6%
#1-sample proportions test with continuity correction

#data:  52 out of 134, null probability 0.5
#X-squared = 6.2761, df = 1, p-value = 0.01224
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3062828 0.4763539
##sample estimates:
#  p 
#0.3880597 

#updated see above prop.test(51,134) #prev female TBD Pos 38% female tested pos with 95ci 29.9-45.9%
#1-sample proportions test with continuity correction
#data:  51 out of 134, null probability 0.5
#X-squared = 7.1716, df = 1, p-value = 0.007406
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2993257 0.4688212
#sample estimates:
#  p 
#0.380597 

67+72

prop.test(67,139) #prev male TBD pos 48.2% with 95ci 39.7-56.8%
#1-sample proportions test with continuity correction
#data:  67 out of 139, null probability 0.5
#X-squared = 0.11511, df = 1, p-value = 0.7344
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3970764 0.5679611
#sample estimates:
#  p 
#0.4820144 


prop.test(48,124) #Lyme sex male not including 15 na 38.7% prev with 95ci 30.2-47.9% 
#1-sample proportions test with continuity correction
#data:  48 out of 124, null probability 0.5
#-squared = 5.879, df = 1, p-value = 0.01532
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3022360 0.4790568
#sample estimates:
#  p 
#0.3870968 


#####updated 3.28/36 one more pos female than before
prop.test(33,117) #Lyme sex female not indlucing 16 na prev is 28.2% with 95 ci 20.5-37.4%
#1-sample proportions test with continuity correction

#data:  33 out of 117, null probability 0.5
#X-squared = 21.368, df = 1, p-value = 3.791e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2046711 0.3740003
##sample estimates:
#  p 
#0.2820513 

#updated see above prop.test(32,117) #Lyme sex female not indlucing 17 na prev is 27.4% with 95 ci 19.7-36.5%
#1-sample proportions test with continuity correction
#data:  32 out of 117, null probability 0.5
#X-squared = 23.111, df = 1, p-value = 1.529e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1971425 0.3650130
#sample estimates:
#  p 
#0.2735043 

table(ShelterTBD$`lyme.CS`)

table(ShelterTBD$`anaplasma.CS`)

table(ShelterTBD$`ehrlichia.CS`)

7+1+10
#18 dogs showed cs from a tbd this could be in the same dog we did not differentiate 

table(ShelterTBD$tbdpos)
#updated 119 positive 3.28.26
#118 positive

#updated 119 positive 3.28.26
prop.test(18, 119)
#1-sample proportions test with continuity correction

#data:  18 out of 119, null probability 0.5
#X-squared = 56.504, df = 1, p-value = 5.608e-14
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.09450106 0.23121230
##sample estimates:
#  p 
#0.1512605 

# updated see avove prop.test(18,118)  #prev of 15.3% of clinical signs with positive test see two cmments above, 95ci 9.5-23.3%
#1-sample proportions test with continuity correction
#data:  18 out of 118, null probability 0.5
#X-squared = 55.602, df = 1, p-value = 8.875e-14
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.09532069 0.23306179
#sample estimates:
#  p 
#0.1525424 

prop.test(34,124) #Ehr sex m  (34 males positive with ehr out of 124 tested, 15 were na) -  #prev of 27.4% of males tested positive for ehr, 95ci 19.9-36.3%
#1-sample proportions test with continuity correction
#data:  34 out of 124, null probability 0.5
#X-squared = 24.395, df = 1, p-value = 7.847e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1997856 0.3628556
#sample estimates:
#  p 
#0.2741935 


####upated 3.28.26 - updated extra negative female, one less NA
prop.test(25, 118) ##Ehr sex f (25 females positive with ehr out of 118 tested, 16 were na) -  #prev of 21.2% of females tested positive for ehr, 95ci 14.4-29.9%
#1-sample proportions test with continuity correction

#data:  25 out of 118, null probability 0.5
#X-squared = 38.042, df = 1, p-value = 6.922e-10
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1442369 0.2986805
#sample estimates:
#  p 
#0.2118644 

#updated see above prop.test(25,117) #Ehr sex f (25 females positive with ehr out of 117 tested, 17 were na) -  #prev of 21.4% of females tested positive for ehr, 95ci 14.6-30.1%
#1-sample proportions test with continuity correction
#data:  25 out of 117, null probability 0.5
#X-squared = 37.231, df = 1, p-value = 1.049e-09
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1455087 0.3010687
#sample estimates:
#  p 
#0.2136752 

prop.test(3,124) #Ana sex m (2 males positive with ana out of 124 tested, 15 na - prevalence of 2.4% of males tested had positive titer for ana, 95% ci 0.6-7.4%)
#1-sample proportions test with continuity correction
#data:  3 out of 124, null probability 0.5
#X-squared = 110.4, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.006269046 0.074420249
#sample estimates:
#  p 
#0.02419355 

####upated 3.28.26 - updated extra negative female, one less NA
prop.test(1, 118)  #Ana sex f - 0.85% prevalence of ana in females tested, 95% CI 0.04-5.3%
#1-sample proportions test with continuity correction

#data:  1 out of 118, null probability 0.5
#X-squared = 112.08, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0004425329 0.0532093478
#sample estimates:
#  p 
#0.008474576 

#updated see above prop.test(1,117) #Ana sex f - 0.85% prevalence of ana in females tested, 95% CI 0.04-5.3%
#1-sample proportions test with continuity correction
#data:  1 out of 117, null probability 0.5
#X-squared = 111.08, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0004463166 0.0536488255
#sample estimates:
#  p 
#0.008547009 

prop.test(28,133) #Rick ifa sex m -prev of rick in tested males (6 na) was 21.1% with ci of 14.7-29.2%
#1-sample proportions test with continuity correction
#data:  28 out of 133, null probability 0.5
#X-squared = 43.429, df = 1, p-value = 4.397e-11
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1466464 0.2915220
#sample estimates:
#  p 
#0.2105263 

####updated 3.28.26 - denom incorrect 
prop.test(24, 130) # rickIFA females prev 18.5%, 95CI 12.4 -26.4%
#1-sample proportions test with continuity correction
#data:  24 out of 130, null probability 0.5
#X-squared = 50.469, df = 1, p-value = 1.21e-12
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1240954 0.2642588
#sample estimates:
#  p 
#0.1846154 

#updated see abbove prop.test(24,106) #Rick ifa sex f (updated numbers - prev of females tested for rick - 22..6% - 95CI 15.3-31.99%)
#1-sample proportions test with continuity correction
#data:  24 out of 106, null probability 0.5
#X-squared = 30.651, df = 1, p-value = 3.089e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1531418 0.3199293
#sample estimates:
#  p 
#0.2264151 

####Ages Prev =================

prop.test(45,107) #prev of adolescence with tbd (prev of tbd in adolescences is 42.1% with Ci 32.7-51.98% )
#1-sample proportions test with continuity correction
#data:  45 out of 107, null probability 0.5
#X-squared = 2.3925, df = 1, p-value = 0.1219
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3270009 0.5198970
#sample estimates:
#  p 
#0.4205607 

####updated 3.28.36 extra pos adult
prop.test(56, 98) #prev of adults with tbd is 57.1% with ci 46.8-66.96%
#1-sample proportions test with continuity correction

#data:  56 out of 98, null probability 0.5
#X-squared = 1.7245, df = 1, p-value = 0.1891
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.4675422 0.6696512
##sample estimates:
#  p 
#0.5714286 

#updated see avove prop.test(55,98) #prev of adults with tbd is 56.1% with ci 45.7-66%
#1-sample proportions test with continuity correction
#data:  55 out of 98, null probability 0.5
#X-squared = 1.2347, df = 1, p-value = 0.2665
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.4574878 0.6601072
#sample estimates:
#  p 
#0.5612245 

prop.test(7,54) # prev of puppies with tbd is 12.96% with ci 5.8-25.5
#1-sample proportions test with continuity correction
#data:  7 out of 54, null probability 0.5
#X-squared = 28.167, df = 1, p-value = 1.113e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.05801791 0.25516541
#sample estimates:
#  p 
#0.1296296 

prop.test(11,14) #prev of seniors with tbd is 78.6% 9 noted only 14 seniors included) with ci 48.8-94.3
#1-sample proportions test with continuity correction
#data:  11 out of 14, null probability 0.5
#X-squared = 3.5, df = 1, p-value = 0.06137
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.4881622 0.9429365
#sample estimates:
# p 
#0.7857143 

prop.test(18,107) #prev of adolescence with multiple tbd is 16.8% with ci 10.5-25.5%
#1-sample proportions test with continuity correction
#data:  18 out of 107, null probability 0.5
#X-squared = 45.794, df = 1, p-value = 1.313e-11
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1053751 0.2555403
#sample estimates:
#  p 
#0.1682243 

prop.test(31,98) #prev of adults with multiple tbd is 31.6%
#1-sample proportions test with continuity correction
#data:  31 out of 98, null probability 0.5
#X-squared = 12.5, df = 1, p-value = 0.000407
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2281425 0.4191179
#sample estimates:
#  p 
#0.3163265 

prop.test(3,54) #prev of puppies with multiple tbd is 5.55% 
#1-sample proportions test with continuity correction
#data:  3 out of 54, null probability 0.5
#X-squared = 40.907, df = 1, p-value = 1.596e-10
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.01445824 0.16343733
#sample estimates:
#  p 
#0.05555556 

prop.test(6,14) #prev of seniors with multiple tbd is 42.9% 
#1-sample proportions test with continuity correction
#data:  6 out of 14, null probability 0.5
#X-squared = 0.071429, df = 1, p-value = 0.7893
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1881363 0.7035185
#sample estimates:
#  p 
#0.4285714 


prop.test(3,26) #Lyme age puppy 11.5% prev in pups (28 NA)
#1-sample proportions test with continuity correction
#data:  3 out of 26, null probability 0.5
#X-squared = 13.885, df = 1, p-value = 0.0001944
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.03028373 0.31283213
#sample estimates:
#  p 
#0.1153846 

prop.test(33,104) ##lyme age adolescence  prev 31.7 with3 NA 
#1-sample proportions test with continuity correction
#data:  33 out of 104, null probability 0.5
#X-squared = 13.163, df = 1, p-value = 0.0002855
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2314428 0.4168732
#sample estimates:
#  p 
#0.3173077 

####updated 3.28.26 one extra adult postive and one less na 
prop.test(39, 98) #lyme age adult prev 39.8% with CI 30.2-50.2%
#1-sample proportions test with continuity correction

#data:  39 out of 98, null probability 0.5
#X-squared = 3.6837, df = 1, p-value = 0.05495
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3019601 0.5020535
#sample estimates:
#  p 
#0.3979592 

#updated see above prop.test(38,97) ##lyme age adult prev 39.2% with 1 NA 
#1-sample proportions test with continuity correction
#data:  38 out of 97, null probability 0.5
#X-squared = 4.1237, df = 1, p-value = 0.04229
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2957744 0.4964067
#sample estimates:
#  p 
#0.3917526 

prop.test(6,14) ##lyme age senior  prev 42. 9% with 0 NA 
#1-sample proportions test with continuity correction
#data:  6 out of 14, null probability 0.5
#X-squared = 0.071429, df = 1, p-value = 0.7893
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1881363 0.7035185
#sample estimates:
#  p 
#0.4285714 

prop.test(0,26) #ana age puppy  
#1-sample proportions test with continuity correction
#data:  0 out of 26, null probability 0.5
#X-squared = 24.038, df = 1, p-value = 9.443e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0000000 0.1602284
#sample estimates:
#  p 
#0 

prop.test(2,104) ##ana age adolescence 
#1-sample proportions test with continuity correction
#data:  2 out of 104, null probability 0.5
#X-squared = 94.24, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.003337906 0.074506291
#sample estimates:
#  p 
#0.01923077 

###updated 3.28.26 additional adult less na 
prop.test(1,98) # ana prev adult 1.02% with 95ci 0.05-6.3%
#1-sample proportions test with continuity correction

#data:  1 out of 98, null probability 0.5
#X-squared = 92.092, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0005328848 0.0636348200
#sample estimates:
#  p 
#0.01020408 

#updated see above prop.test(1,97) ##ana age adult
#1-sample proportions test with continuity correction
#data:  1 out of 97, null probability 0.5
#X-squared = 91.093, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0005383809 0.0642643849
#sample estimates:
#  p 
#0.01030928 

prop.test(1,14) ##ana age senior
#1-sample proportions test with continuity correction
#data:  1 out of 14, null probability 0.5
#X-squared = 8.6429, df = 1, p-value = 0.003283
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.003739925 0.358319133
#sample estimates:
#  p 
#0.07142857 

prop.test(3,26) #ehr age puppy
#1-sample proportions test with continuity correction
#data:  3 out of 26, null probability 0.5
#X-squared = 13.885, df = 1, p-value = 0.0001944
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.03028373 0.31283213
#sample estimates:
#  p 
#0.1153846 

prop.test(16,104) ##ehr age adolescence
#1-sample proportions test with continuity correction
#data:  16 out of 104, null probability 0.5
#X-squared = 48.471, df = 1, p-value = 3.352e-12
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.09317348 0.24083223
#sample estimates:
#  p 
#0.1538462 

#updated 3.28/26 additional adult lest na
prop.test(33,98) #ehr adult prev 33.6% with 95 ci 24.6-44.0%
#1-sample proportions test with continuity correction

#data:  33 out of 98, null probability 0.5
#X-squared = 9.8061, df = 1, p-value = 0.001739
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2463262 0.4401173
#sample estimates:
#  p 
#0.3367347 

#upd see aboveprop.test(33,97) ##ehr age adult
#1-sample proportions test with continuity correction
#data:  33 out of 97, null probability 0.5
#X-squared = 9.2784, df = 1, p-value = 0.002319
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2489959 0.4442423
#sample estimates:
#  p 
#0.3402062 

prop.test(7,14) ##ehr age senior
#1-sample proportions test without continuity correction
#data:  7 out of 14, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.267992 0.732008
#sample estimates:
#  p 
#0.5 

prop.test(4,48) #rick age puppy
#1-sample proportions test with continuity correction
#data:  4 out of 48, null probability 0.5
#X-squared = 31.688, df = 1, p-value = 1.811e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0270350 0.2087167
#sample estimates:
#  p 
#0.08333333 

prop.test(18,105) ##rick age adolescence
#1-sample proportions test with continuity correction
#data:  18 out of 105, null probability 0.5
#X-squared = 44.038, df = 1, p-value = 3.22e-11
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1074358 0.2600998
#sample estimates:
#  p 
#0.1714286 

prop.test(24,97) ##rick age adult
#1-sample proportions test with continuity correction
#data:  24 out of 97, null probability 0.5
#X-squared = 23.753, df = 1, p-value = 1.095e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1678870 0.3473274
#sample estimates:
#  p 
#0.2474227 

prop.test(6,13) ##rick age senior
#1-sample proportions test with continuity correction
#data:  6 out of 13, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2040175 0.7387967
#sample estimates:
#  p 
#0.4615385 

### size prev ====
prop.test(14,51) #small dogs with ticks prev # Updated! 27.4%
#1-sample proportions test with continuity correction
#data:  14 out of 51, null probability 0.5
#X-squared = 9.4902, df = 1, p-value = 0.002066
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1632668 0.4197601
#sample estimates:
#  p 
#0.2745098 

prop.test(29,111) #med dogs with ticks prev
#1-sample proportions test with continuity correction
#data:  29 out of 111, null probability 0.5
#X-squared = 24.36, df = 1, p-value = 7.99e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1845840 0.3547714
#sample estimates:
#  p 
#0.2612613 

prop.test(17,110) #large dogs with ticks prev
#1-sample proportions test with continuity correction
#data:  17 out of 110, null probability 0.5
#X-squared = 51.136, df = 1, p-value = 8.617e-13
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.09516327 0.23876072
#sample estimates:
#  p 
#0.1545455 

#updated 3.28.26 additional large post no na now 
prop.test(59, 110)  #preve of large with tbd 53.6% 95ci 43.9-63.1
#1-sample proportions test with continuity correction

#data:  59 out of 110, null probability 0.5
#X-squared = 0.44545, df = 1, p-value = 0.5045
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.4390768 0.6310787
#sample estimates:
#  p 
#0.5363636 

#upd see above prop.test(58,110) #prev of large with tbd
#	1-sample proportions test with continuity correction
#data:  58 out of 110, null probability 0.5
#X-squared = 0.22727, df = 1, p-value = 0.6336
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.4302045 0.6224122
#sample estimates:
#  p 
#0.5272727 

prop.test(50,112) #prev of med with tbd
#1-sample proportions test with continuity correction
#data:  50 out of 112, null probability 0.5
#X-squared = 1.0804, df = 1, p-value = 0.2986
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3534325 0.5431470
#sample estimates:
#  p 
#0.4464286 

prop.test(10,51) #prev of small with tbd
#1-sample proportions test with continuity correction
#data:  10 out of 51, null probability 0.5
#X-squared = 17.647, df = 1, p-value = 2.659e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1028918 0.3354646
#sample estimates:
#  p 
#0.1960784 

prop.test(33,110) #prev of large with multiple tbd
#1-sample proportions test with continuity correction
#data:  33 out of 110, null probability 0.5
#X-squared = 16.809, df = 1, p-value = 4.133e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2182626 0.3959370
#sample estimates:
#  p 
#0.3 

prop.test(20,112) #prev of med with multiple tbd
#1-sample proportions test with continuity correction
#data:  20 out of 112, null probability 0.5
#X-squared = 45.009, df = 1, p-value = 1.961e-11
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1150201 0.2647451
#sample estimates:
#  p 
#0.1785714 

prop.test(5,51) #prev of small with multiple tbd
#1-sample proportions test with continuity correction
#data:  5 out of 51, null probability 0.5
#X-squared = 31.373, df = 1, p-value = 2.13e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.03665948 0.22188004
#sample estimates:
#  p 
#0.09803922 

prop.test(7,36) #small dogs with lyme
#1-sample proportions test with continuity correction
#data:  7 out of 36, null probability 0.5
#X-squared = 12.25, df = 1, p-value = 0.0004653
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.08800966 0.36565772
#sample estimates:
#  p 
#0.1944444 

prop.test(29,105) #med dogs with lyme
#1-sample proportions test with continuity correction
#data:  29 out of 105, null probability 0.5
#X-squared = 20.152, df = 1, p-value = 7.151e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1955633 0.3734762
#sample estimates:
#  p 
#0.2761905 

#updated 3.28.26 additional pos large dog less na 
prop.test(45,101) # prev for large dosgs with lyme 44.6$ 95 ci 34.8-54.8%
#1-sample proportions test with continuity correction
#data:  45 out of 101, null probability 0.5
#X-squared = 0.9901, df = 1, p-value = 0.3197
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3477150 0.5475647
#sample estimates:
#  p 
#0.4455446 

#upd see abv prop.test(44,100) #lg dogs with lyme
#1-sample proportions test with continuity correction
#data:  44 out of 100, null probability 0.5
#X-squared = 1.21, df = 1, p-value = 0.2713
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3420219 0.5426412
#sample estimates:
#  p 
#0.44 

prop.test(0,36) #small dogs with ana not run

prop.test(1,105) #med dogs with ana
#1-sample proportions test with continuity correction
#data:  1 out of 105, null probability 0.5
#X-squared = 99.086, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
# 0.0004973448 0.0595510533
#sample estimates:
#  p 
#0.00952381 

#updated 3.28.26 additional large dog neg and one less na 
prop.test(3,101) #prev for large dogs with ana - 2.97% 95ci .77-9.1%
#1-sample proportions test with continuity correction

#data:  3 out of 101, null probability 0.5
#X-squared = 87.485, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.007702492 0.090645659
#sample estimates:
#  p 
#0.02970297

#upd see abv prop.test(3,100) #lg dogs with ana
#1-sample proportions test with continuity correction
#data:  3 out of 100, null probability 0.5
#X-squared = 86.49, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.007779835 0.091513087
#sample estimates:
#  p 
#0.03 

prop.test(5,36) #small dogs with ehr
#1-sample proportions test with continuity correction
#data:  5 out of 36, null probability 0.5
#X-squared = 17.361, df = 1, p-value = 3.091e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.05228497 0.30287951
#sample estimates:
#  p 
#0.1388889 

prop.test(25,105) #med dogs with ehr
#1-sample proportions test with continuity correction
#data:  25 out of 105, null probability 0.5
#X-squared = 27.771, df = 1, p-value = 1.365e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1627307 0.3330016
#sample estimates:
#  p 
#0.2380952 


#updated 3.28.26 additional large dog neg and one less na 
prop.test(29, 101) #prev for large dog with ehr 28.7% 95 ci 20.4-38.7
#1-sample proportions test with continuity correction

#data:  29 out of 101, null probability 0.5
#X-squared = 17.465, df = 1, p-value = 2.926e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2036412 0.3870714
#sample estimates:
##  p 
#0.2871287 

#upd see abv prop.test(29,100) #lg dogs with ehr
#1-sample proportions test with continuity correction
#data:  29 out of 100, null probability 0.5
#X-squared = 16.81, df = 1, p-value = 4.132e-05
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2057664 0.3906248
#sample estimates:
#  p 
#0.29 

prop.test(3,42) #small dogs with rick
#1-sample proportions test with continuity correction
#data:  3 out of 42, null probability 0.5
#X-squared = 29.167, df = 1, p-value = 6.641e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.01863051 0.20554460
#sample estimates:
#  p 
#0.07142857 

prop.test(22,111) #med dogs with rick
#1-sample proportions test with continuity correction
#data:  22 out of 111, null probability 0.5
#X-squared = 39.243, df = 1, p-value = 3.742e-10
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1309390 0.2868424
#sample estimates:
#  p 
#0.1981982 

prop.test(27,110) #lg dogs with rick
#1-sample proportions test with continuity correction
#data:  27 out of 110, null probability 0.5
#X-squared = 27.5, df = 1, p-value = 1.571e-07
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1706123 0.3384244
#sample estimates:
#  p 
#0.2454545 

####fur length =======

prop.test(15,27) #prev long hair with tbd
#1-sample proportions test with continuity correction
#data:  15 out of 27, null probability 0.5
#X-squared = 0.14815, df = 1, p-value = 0.7003
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3563864 0.7396060
#sample estimates:
#  p 
#0.5555556 

#updated additional positive short hair dog - less na 
prop.test(94, 222) #prev of short harid dogs with tbd 42.3% 95 ci 35.8-49.1
#1-sample proportions test with continuity correction

#data:  94 out of 222, null probability 0.5
#X-squared = 4.9054, df = 1, p-value = 0.02677
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3581033 0.4914381
#sample estimates:
#  p 
#0.4234234 

#upd see abvprop.test(93,222) #prev short hair with tbd UPDATED 
#1-sample proportions test with continuity correction
#data:  93 out of 222, null probability 0.5
#X-squared = 5.518, df = 1, p-value = 0.01882
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3537703 0.4869207
#sample estimates:
#  p 
#0.4189189 

prop.test(7,27) #prev long hair with multiple tbd
#1-sample proportions test with continuity correction
#data:  7 out of 27, null probability 0.5
#X-squared = 5.3333, df = 1, p-value = 0.02092
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1187294 0.4659372
#sample estimates:
#  p 
#0.2592593 

prop.test(44,222) #prev short hair with multiple tbd  UPDATED  prev 19.8% 
#1-sample proportions test with continuity correction
#data:  44 out of 222, null probability 0.5
#X-squared = 79.68, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1490926 0.2580059
#sample estimates:
#  p 
#0.1981982 

prop.test(7,27) #long hair with ticks
#1-sample proportions test with continuity correction
#data:  7 out of 27, null probability 0.5
#X-squared = 5.3333, df = 1, p-value = 0.02092
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1187294 0.4659372
#sample estimates:
#  p 
#0.2592593 

prop.test(43,221) #short hair with ticks UPDATED 
#1-sample proportions test with continuity correction
#data:  43 out of 221, null probability 0.5
#X-squared = 81.249, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1457824 0.2542408
#sample estimates:
#  p 
#0.1945701 


prop.test(12,23) #long hair with lyme
#1-sample proportions test with continuity correction
#data:  12 out of 23, null probability 0.5
#X-squared = 0, df = 1, p-value = 1
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.3108462 0.7257958
#sample estimates:
#  p 
#0.5217391 

#Updated 3.28.26  one additional positive short hair 
prop.test(61, 199) #prev of short haired dogs with lyme 30.7% 95Ci 24.4-37.6%
#1-sample proportions test with continuity correction

#data:  61 out of 199, null probability 0.5
#X-squared = 29.025, df = 1, p-value = 7.145e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2443206 0.3763568
#sample estimates:
#  p 
#0.3065327 

# upd see abv prop.test(60,198) #short hair with lyme
#1-sample proportions test with continuity correction
#data:  60 out of 198, null probability 0.5
#X-squared = 29.944, df = 1, p-value = 4.446e-08
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.2409379 0.3729126
#sample estimates:
#  p 
#0.3030303 

prop.test(7,23) #long hair with ehr
#1-sample proportions test with continuity correction
#data:  7 out of 23, null probability 0.5
#X-squared = 2.7826, df = 1, p-value = 0.09529
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1405633 0.5300578
#sample estimates:
#  p 
#0.3043478 

#updated 3.28.26 additional neg short hair
prop.test(43, 199) #short hair with ehr prev is 21.6% 95ci 16.2-28.1%
#1-sample proportions test with continuity correction

#data:  43 out of 199, null probability 0.5
#X-squared = 63.035, df = 1, p-value = 2.03e-15
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1623335 0.2810471
#sample estimates:
#  p 
#0.2160804 

#upd see abv prop.test(43,198) #short hair with ehrUPDATED 
#1-sample proportions test with continuity correction
#data:  43 out of 198, null probability 0.5
#X-squared = 62.227, df = 1, p-value = 3.06e-15
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1631757 0.2824001
#sample estimates:
#  p 
#0.2171717 

prop.test(0,23) #long hair with ana
#1-sample proportions test with continuity correction
#data:  0 out of 23, null probability 0.5
#X-squared = 21.043, df = 1, p-value = 4.49e-06
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.0000000 0.1780987
#sample estimates:
#  p 
#0 

#updated 3.28.26 additional neg short hair
prop.test(3,199) #prev of short haired dogs with ana is 1.5% 95ci 0.39-4.7%
#1-sample proportions test with continuity correction

#data:  3 out of 199, null probability 0.5
#X-squared = 185.25, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
##  0.003901455 0.046989541
#sample estimates:
#  p 
#0.01507538 


#upd see abv prop.test(3,198) #short hair with ana UPDATED 
#1-sample proportions test with continuity correction
#data:  3 out of 198, null probability 0.5
#X-squared = 184.25, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.003921201 0.047221626
#sample estimates:
#  p 
#0.01515152 


prop.test(4,25) #long hair with rick
#1-sample proportions test with continuity correction
#data:  4 out of 25, null probability 0.5
#X-squared = 10.24, df = 1, p-value = 0.001374
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.05254067 0.36916766
#sample estimates:
#  p 
#0.16

prop.test(45,218) #short hair with rick UPDATED 
#1-sample proportions test with continuity correction

#data:  45 out of 218, null probability 0.5
#X-squared = 73.986, df = 1, p-value < 2.2e-16
#alternative hypothesis: true p is not equal to 0.5
#95 percent confidence interval:
#  0.1559654 0.2674743
#sample estimates:
#  p 
#0.206422 


# Assessing risk variables ---------------------------------------
# Outcome is positive/negative for infection (tbdpos, multtbd, lymeseropos, 
# anaseropos, ehrseropos, rickifa)--> binary, dichotomous
# by variables:
#   agestages = categorical, nominal
#   size = categorical, nominal
#   furlength = dichotomous, categorical, nominal
#   ticks = dichotomous, categorical, nomimal
#   county = categorical, nominal
#   sex = dichotomous, categorical, nominal
#   lymeseropos, # anaseropos, ehrseropos, rickifa - run each of these with the outcome removed 

# Should be able to use a basic glm with family=binomial because all that matters is the outcome variable. I have multiple independent
# variables predicting a single dependent variable, the independent variables are categorical or continuous and the dependent variable 
# dichotomous, plus there are no control variables..... so I can ask the question "What is the odds probability of the dependent variable
# occuring as the values of the independent variables change?" Therefore, I want to run a logistic regression. 

# first need to convert characters to factors

levels(ShelterTBD$county)
# NULL
# this means this column is not reading as a factor
ShelterTBD$county <- factor(ShelterTBD$county)
class(ShelterTBD$county) #it worked! 


# now change the rest!
levels(ShelterTBD$`size`)
# NULL
# this means this column is not reading as a factor
ShelterTBD$`size` <- factor(ShelterTBD$`size`)
class(ShelterTBD$`size`) #it worked!

levels(ShelterTBD$`agestages`)
# NULL
# this means this column is not reading as a factor
ShelterTBD$`agestages` <- factor(ShelterTBD$`agestages`)
class(ShelterTBD$`agestages`) #it worked!

levels(ShelterTBD$`furlength`)
# NULL
# this means this column is not reading as a factor
ShelterTBD$`furlength` <- factor(ShelterTBD$`furlength`)
class(ShelterTBD$`furlength`) #it worked!

levels(ShelterTBD$`ticks`)
# NULL
# this means this column is not reading as a factor
ShelterTBD$`ticks` <- factor(ShelterTBD$`ticks`)
class(ShelterTBD$`ticks`) #it worked!

levels(ShelterTBD$`sex`)
# NULL
# this means this column is not reading as a factor
ShelterTBD$`sex` <- factor(ShelterTBD$`sex`)
class(ShelterTBD$`sex`) #it worked!

levels(ShelterTBD$lymeseropos)
ShelterTBD$lymeseropos <-factor(ShelterTBD$lymeseropos)
class(ShelterTBD$lymeseropos)

### lyme models =============================

model1<-glm(lymeseropos ~ county, ShelterTBD, family=binomial)
summary(model1)

### RESULT: County is NOT a significant predictor of Lyme prevalence. ====

#updated 3.28.26 
#Call:
#  glm(formula = lymeseropos ~ county, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate  Std. Error  z value  Pr(>|z|)
#(Intercept)    -1.557e+01  1.029e+03  -0.015    0.988
#countyFranklin -3.374e-11  1.782e+03   0.000    1.000
#countyGallia    1.492e+01  1.029e+03   0.014    0.988
#countyJackson   1.485e+01  1.029e+03   0.014    0.988
#countyLawrence  1.563e+01  1.029e+03   0.015    0.988
#countyRichland -8.922e-12  1.782e+03   0.000    1.000
#countyRoss      1.454e+01  1.029e+03   0.014    0.989
#countyScioto    1.467e+01  1.029e+03   0.014    0.989

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 308.53  on 241  degrees of freedom
#Residual deviance: 299.72  on 234  degrees of freedom
#(31 observations deleted due to missingness)
#AIC: 315.72

#Number of Fisher Scoring iterations: 14
### RESULT: County is NOT a significant predictor of Lyme prevalence. ====


#NEW RESULTS: old
#Call:
#  glm(formula = lymeseropos ~ county, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.557e+01  1.029e+03  -0.015    0.988
#countyDelaware  3.113e+01  1.455e+03   0.021    0.983
#countyFranklin  1.626e+01  1.029e+03   0.016    0.987
#countyGallia    1.509e+01  1.029e+03   0.015    0.988
#countyJackson   1.498e+01  1.029e+03   0.015    0.988
#countyLawrence  1.599e+01  1.029e+03   0.016    0.988
#countyRichland  3.610e-08  1.782e+03   0.000    1.000
#countyRoss      1.524e+01  1.029e+03   0.015    0.988
#countyScioto    1.493e+01  1.029e+03   0.015    0.988

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 354.42  on 264  degrees of freedom
#AIC: 372.42

#Number of Fisher Scoring iterations: 14
### RESULT: County is NOT a significant predictor of Lyme prevalence. ====


model2<-glm(lymeseropos ~ size, ShelterTBD, family=binomial)
summary(model2)

#updated 33.28.26#
#Call:
#  glm(formula = lymeseropos ~ size, family = binomial, data = ShelterTBD)

#Coefficients:
#             Estimate   Std. Error z value  Pr(>|z|)   
#(Intercept)  -0.2187     0.2002    -1.092   0.2747   
#sizeMedium   -0.7447     0.2962    -2.515   0.0119 * 
# sizeSmall    -1.2027     0.4663   -2.579   0.0099 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 308.53  on 241  degrees of freedom
#Residual deviance: 298.04  on 239  degrees of freedom
#(31 observations deleted due to missingness)
#AIC: 304.04

##Number of Fisher Scoring iterations: 4
### RESULT: Medium size are significant predictors of Lyme prevalence. ======  DIFFERENT FROM BEFORE included small before!!!!!!!!!!!!!!!!!

#NEW RESULTS old
#Call:
#  glm(formula = lymeseropos ~ size, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept) -0.03637    0.19072  -0.191   0.8488  
#sizeMedium  -0.71085    0.27805  -2.557   0.0106 *
#  sizeSmall   -0.23989    0.34105  -0.703   0.4818  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 362.85  on 270  degrees of freedom
#AIC: 368.85

#Number of Fisher Scoring iterations: 4

### RESULT: Medium size are significant predictors of Lyme prevalence. ======  DIFFERENT FROM BEFORE included small before!!!!!!!!!!!!!!!!!
#####stopped here 3.28.26#####

model3<-glm(lymeseropos ~ furlength, ShelterTBD, family=binomial)
summary(model3)

#NEW RESUTLS
#Call:
#  glm(formula = lymeseropos ~ furlength, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)      0.3747     0.3917   0.957    0.339  
#furlengthNA     -0.3747     0.5658  -0.662    0.508  
#furlengthshort  -0.8711     0.4154  -2.097    0.036 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 364.26  on 270  degrees of freedom
#AIC: 370.26

#Number of Fisher Scoring iterations: 4

### RESULT: Short fur is a significant predictors of Lyme prevalence.======

model4<-glm(lymeseropos ~ agestages, ShelterTBD, family=binomial)
summary(model4)


#NEW RESULTS 
#Call:
#  glm(formula = lymeseropos ~ agestages, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.6792     0.2046  -3.319 0.000902 ***
#  agestagesAdult    0.2652     0.2906   0.913 0.361494    
#agestagesPuppy    0.9777     0.3429   2.851 0.004359 ** 
#  agestagesSenior   0.3915     0.5775   0.678 0.497859    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 361.21  on 269  degrees of freedom
#AIC: 369.21

#Number of Fisher Scoring iterations: 4

#### RESULT: Age is a significant predictor of lyme prevalence. ======


model5<-glm(lymeseropos ~ sex, ShelterTBD, family=binomial)
summary(model5)


#NEW RESULTS 
#Call:
#  glm(formula = lymeseropos ~ sex, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -0.5508     0.1794  -3.071  0.00213 **
#  sexMale       0.3632     0.2474   1.468  0.14204   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 367.45  on 271  degrees of freedom
#AIC: 371.45

#Number of Fisher Scoring iterations: 4
### RESULT: Sex is a significant predictor of lyme prevalence  ===================

model6<-glm(lymeseropos ~ ticks, ShelterTBD, family=binomial)
summary(model6)


#NEW RESULTS 
#Call:
#  glm(formula = lymeseropos ~ ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -0.4410     0.1407  -3.134  0.00173 **
#  ticksY        0.3074     0.2946   1.044  0.29661   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 366.74  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 370.74

#Number of Fisher Scoring iterations: 4

###RESULT = absence???? of ticks is a significant predictor for lyme prev? =====

model7<-glm(lymeseropos ~ anaseropos, ShelterTBD, family=binomial)
summary(model7)


###===== New results show no significance 

#NEW RESULTS 
#Call:
#  glm(formula = lymeseropos ~ anaseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -0.7314     0.1387  -5.273 1.34e-07 ***
#  anaseropos1    1.8300     1.1630   1.573    0.116    
#anaseroposNA  18.2974   699.3605   0.026    0.979    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 303.36  on 270  degrees of freedom
#AIC: 309.36

#Number of Fisher Scoring iterations: 16


model8<-glm(lymeseropos ~ ehrseropos, ShelterTBD, family=binomial)
summary(model8)


####RESULT = Ehrilichia sero is predictive of lyme prev??? =====

#new results
#Call:
#  glm(formula = lymeseropos ~ ehrseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -1.4001     0.1861  -7.524 5.31e-14 ***
#  ehrseropos1     2.4762     0.3522   7.031 2.04e-12 ***
#  ehrseroposNA   19.9662  1153.0505   0.017    0.986    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 247.93  on 270  degrees of freedom
#AIC: 253.93

#Number of Fisher Scoring iterations: 17

model9<-glm(lymeseropos ~ rickifa, ShelterTBD, family=binomial)
summary(model9)


#New results
#Call:
#  glm(formula = lymeseropos ~ rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.5134     0.1422  -3.609 0.000307 ***
#  rickifa1      0.7452     0.3134   2.378 0.017409 *  
#  rickifaNA     0.1079     0.6610   0.163 0.870342    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 369.62  on 272  degrees of freedom
#Residual deviance: 363.91  on 270  degrees of freedom
#AIC: 369.91

#Number of Fisher Scoring iterations: 4

model10 <- glm(lymeseropos ~ county + size + furlength + agestages + sex + ticks + anaseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(model10)

#updated 3.28.26
#### RESULT = Medium size, Short fur length, and Ehrlichia seropositivity are significant predictors of lyme prevalence ====

#Call:
#  glm(formula = lymeseropos ~ county + size + furlength + agestages + 
#       sex + ticks + anaseropos + ehrseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                 Estimate  Std. Error z value  Pr(>|z|)    
#(Intercept)     -1.651e+01  2.797e+03  -0.006   0.9953    
#countyFranklin   1.170e+00  4.845e+03   0.000   0.9998    
#countyGallia     1.655e+01  2.797e+03   0.006   0.9953    
#countyJackson    1.642e+01  2.797e+03   0.006   0.9953    
#countyLawrence   1.747e+01  2.797e+03   0.006   0.9950    
#countyRichland   5.769e-09  4.845e+03   0.000   1.0000    
#countyRoss       1.639e+01  2.797e+03   0.006   0.9953    
#countyScioto     1.597e+01  2.797e+03   0.006   0.9954    
#sizeMedium      -1.027e+00  3.922e-01  -2.620   0.0088 ** 
#sizeSmall        6.433e-01  8.009e-01   0.803   0.4219    
#furlengthshort  -1.202e+00  5.799e-01  -2.073   0.0382 *  
#agestagesAdult  -1.950e-01  3.983e-01  -0.490   0.6244    
#agestagesPuppy  -1.714e+01  9.568e+02  -0.018   0.9857    
#agestagesSenior -1.172e+00  8.363e-01  -1.401   0.1611    
#sexMale          3.381e-01  3.682e-01   0.918   0.3585    
#ticksY           8.449e-01  4.586e-01   1.843   0.0654 .  
#anaseropos      -7.945e-01  1.365e+00  -0.582   0.5606    
#ehrseropos       2.220e+00  4.513e-01   4.920 8.66e-07 ***
#rickifa          5.055e-01  4.525e-01   1.117   0.2640    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 272.77  on 214  degrees of freedom
#Residual deviance: 193.73  on 196  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 231.73

#Number of Fisher Scoring iterations: 16

#### RESULT = Medium size, Short fur length, and Ehrlichia seropositivity are significant predictors of lyme prevalence ====

#NEW RESULTS old
#Call:
#  glm(formula = lymeseropos ~ county + size + furlength + agestages + 
#        sex + ticks + anaseropos + ehrseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.860e+01  7.604e+03  -0.002   0.9980    
#countyDelaware   7.216e+00  1.098e+04   0.001   0.9995    
#countyFranklin   5.102e+00  7.768e+03   0.001   0.9995    
#countyGallia     1.869e+01  7.604e+03   0.002   0.9980    
#countyJackson    1.822e+01  7.604e+03   0.002   0.9981    
#countyLawrence   1.919e+01  7.604e+03   0.003   0.9980    
#countyRichland  -9.452e-08  1.317e+04   0.000   1.0000    
#countyRoss       1.837e+01  7.604e+03   0.002   0.9981    
#countyScioto     1.762e+01  7.604e+03   0.002   0.9982    
#sizeMedium      -1.004e+00  3.915e-01  -2.565   0.0103 *  
#  sizeSmall        6.891e-02  7.602e-01   0.091   0.9278    
#furlengthNA     -1.015e+00  8.553e-01  -1.187   0.2353    
#furlengthshort  -1.234e+00  5.771e-01  -2.139   0.0325 *  
#  agestagesAdult  -2.548e-01  3.993e-01  -0.638   0.5234    
#agestagesPuppy  -2.072e+00  1.014e+00  -2.044   0.0409 *  
#  agestagesSenior -1.092e+00  7.440e-01  -1.468   0.1421    
#sexMale          5.254e-01  3.571e-01   1.471   0.1412    
#ticksY           7.764e-01  4.305e-01   1.804   0.0713 .  
#anaseropos1     -6.766e-01  1.305e+00  -0.519   0.6040    
#anaseroposNA     3.296e+01  2.238e+03   0.015   0.9882    
#ehrseropos1      2.608e+00  4.389e-01   5.942 2.82e-09 ***
#  ehrseroposNA            NA         NA      NA       NA    
#rickifa1         6.771e-01  4.557e-01   1.486   0.1373    
#rickifaNA       -7.851e-01  1.031e+00  -0.762   0.4462    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 212.97  on 249  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 258.97

#Number of Fisher Scoring iterations: 18

#> 

mansteplyme1<-glm(lymeseropos ~ size + furlength + agestages + sex + ticks + anaseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(mansteplyme1)
### pulled out counties because they had the highest p value 

#updated 3.28.26 
#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + sex + 
#        ticks + anaseropos + ehrseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.1310     0.6457   0.203  0.83920    
#sizeMedium       -1.0161     0.3876  -2.621  0.00876 ** 
#sizeSmall         0.6166     0.7609   0.810  0.41771    
#furlengthshort   -1.2550     0.5530  -2.270  0.02324 *  
#agestagesAdult   -0.3732     0.3822  -0.976  0.32890    
#agestagesPuppy  -17.2593   954.3218  -0.018  0.98557    
#agestagesSenior  -1.2182     0.8277  -1.472  0.14106    
#sexMale           0.2460     0.3585   0.686  0.49258    
#ticksY            0.8940     0.4464   2.003  0.04520 *  
#anaseropos       -1.0605     1.3263  -0.800  0.42394    
#ehrseropos        2.2581     0.4365   5.173  2.3e-07 ***
#rickifa           0.5174     0.4330   1.195  0.23207    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 272.77  on 214  degrees of freedom
#Residual deviance: 201.56  on 203  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 225.56

#Number of Fisher Scoring iterations: 16

#old note idk if still accurate ####per A1C comparison later in this line of code this is the best fit model making medium size, short fur, having ticks, and erh sero pos the sig=============

#NEW RESULTS
#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + sex + 
#        ticks + anaseropos + ehrseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -0.13327    0.63237  -0.211   0.8331    
#sizeMedium        -0.97526    0.38520  -2.532   0.0113 *  
#  sizeSmall         -0.02091    0.73626  -0.028   0.9773    
#furlengthNA       -0.82279    0.80189  -1.026   0.3049    
#furlengthshort    -1.21708    0.54142  -2.248   0.0246 *  
#  agestagesAdult    -0.39146    0.38244  -1.024   0.3060    
#agestagesPuppy    -1.96943    0.98002  -2.010   0.0445 *  
#  agestagesSenior   -1.11251    0.73643  -1.511   0.1309    
#sexMale            0.41684    0.34778   1.199   0.2307    
#ticksY             0.80456    0.41727   1.928   0.0538 .  
#anaseropos1       -0.74311    1.25562  -0.592   0.5540    
#anaseroposNA      21.58838 1096.47726   0.020   0.9843    
#ehrseropos1        2.53299    0.41850   6.053 1.43e-09 ***
#  ehrseroposNA            NA         NA      NA       NA    
#rickifa1           0.65722    0.43308   1.518   0.1291    
#rickifaNA         -0.50727    1.00661  -0.504   0.6143    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 222.02  on 257  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 252.02

#Number of Fisher Scoring iterations: 17


#updated 3.28.26 - sex dropped out first before ana 
mansteplyme2<-glm(lymeseropos ~ size + furlength + agestages + ticks + anaseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(mansteplyme2) #sex drops out first in this run in manstep1

#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + ticks + 
#        anaseropos + ehrseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.3011     0.5953   0.506  0.61297    
#sizeMedium       -1.0575     0.3826  -2.764  0.00571 ** 
#sizeSmall         0.5899     0.7580   0.778  0.43644    
#furlengthshort   -1.2726     0.5506  -2.311  0.02081 *  
#agestagesAdult   -0.3933     0.3811  -1.032  0.30216    
#agestagesPuppy  -17.2375   956.2069  -0.018  0.98562    
#agestagesSenior  -1.1659     0.8232  -1.416  0.15669    
#ticksY            0.8886     0.4456   1.994  0.04613 *  
#anaseropos       -1.0483     1.3233  -0.792  0.42826    
#ehrseropos        2.2820     0.4354   5.242 1.59e-07 ***
#rickifa           0.5106     0.4320   1.182  0.23721    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 272.77  on 214  degrees of freedom
#Residual deviance: 202.03  on 204  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 224.03

#Number of Fisher Scoring iterations: 16

#NEW MODEL -retired 
#mansteplyme2<-glm(lymeseropos ~ size + furlength + sex + ticks + agestages + ehrseropos + rickifa, ShelterTBD, family = binomial )
#summary(mansteplyme2)
#pulled out anaseropos as it had the highest p value and all insig, put ages back in with new model 
#Call:
#  glm(formula = lymeseropos ~ size + furlength + sex + ticks + 
#        agestages + ehrseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.330e-01  6.300e-01  -0.211   0.8328    
#sizeMedium      -9.583e-01  3.830e-01  -2.502   0.0123 *  
#  sizeSmall       -3.713e-03  7.320e-01  -0.005   0.9960    
#furlengthNA     -8.548e-01  8.009e-01  -1.067   0.2858    
#furlengthshort  -1.218e+00  5.388e-01  -2.260   0.0238 *  
#  sexMale          4.101e-01  3.468e-01   1.183   0.2369    
#ticksY           7.990e-01  4.182e-01   1.911   0.0561 .  
#agestagesAdult  -3.711e-01  3.799e-01  -0.977   0.3287    
#agestagesPuppy  -1.949e+00  9.726e-01  -2.004   0.0450 *  
#  agestagesSenior -1.121e+00  7.417e-01  -1.511   0.1308    
#ehrseropos1      2.487e+00  4.092e-01   6.079 1.21e-09 ***
#  ehrseroposNA     2.157e+01  1.098e+03   0.020   0.9843    
#rickifa1         6.163e-01  4.282e-01   1.439   0.1501    
#rickifaNA       -5.015e-01  1.003e+00  -0.500   0.6172    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 222.33  on 258  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 250.33

#Number of Fisher Scoring iterations: 17

#updated 3.28.26 - ana drops out 
mansteplyme3<-glm(lymeseropos ~ size + furlength + agestages + ticks + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(mansteplyme3) 

#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + ticks + 
#        ehrseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.2984     0.5926   0.504  0.61457    
#sizeMedium       -1.0370     0.3804  -2.726  0.00641 ** 
#sizeSmall         0.5886     0.7568   0.778  0.43675    
#furlengthshort   -1.2763     0.5472  -2.332  0.01969 *  
#agestagesAdult   -0.3656     0.3780  -0.967  0.33348    
#agestagesPuppy  -17.2240   956.7799  -0.018  0.98564    
#agestagesSenior  -1.1035     0.8132  -1.357  0.17478    
#ticksY            0.8630     0.4449   1.940  0.05239 .  
#ehrseropos        2.2069     0.4200   5.254 1.49e-07 ***
#rickifa           0.4725     0.4301   1.099  0.27196    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 272.77  on 214  degrees of freedom
#Residual deviance: 202.59  on 205  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 222.59

#Number of Fisher Scoring iterations: 16


#retired 
#mansteplyme3<-glm(lymeseropos ~ size + furlength + agestages + ticks + ehrseropos + rickifa, ShelterTBD, family = binomial )
#summary(mansteplyme3)

#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + ticks + 
#        ehrseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.15031    0.57959   0.259  0.79537    
#sizeMedium        -1.02900    0.37657  -2.733  0.00628 ** 
#  sizeSmall         -0.07072    0.73289  -0.097  0.92312    
#furlengthNA       -0.89867    0.79927  -1.124  0.26086    
#furlengthshort    -1.23277    0.53416  -2.308  0.02101 *  
#  agestagesAdult    -0.39397    0.37871  -1.040  0.29820    
#agestagesPuppy    -1.95910    0.98532  -1.988  0.04678 *  
#  agestagesSenior   -1.01896    0.73625  -1.384  0.16636    
#ticksY             0.78473    0.41701   1.882  0.05986 .  
#ehrseropos1        2.50102    0.40752   6.137  8.4e-10 ***
#  ehrseroposNA      21.56485 1092.62837   0.020  0.98425    
#rickifa1           0.59889    0.42588   1.406  0.15965    
#rickifaNA         -0.50525    1.01109  -0.500  0.61728    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 223.74  on 259  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 249.74

#Number of Fisher Scoring iterations: 17

#updated 3.28.26 - rickifa fell out of man steplyme 3 
mansteplyme4<-glm(lymeseropos ~ size + furlength + agestages + ticks + ehrseropos, ShelterTBD, family = binomial )
summary(mansteplyme4)

#Call:
#  glm(formula = lymeseropos ~ size + furlength + agestages + ticks + 
#        ehrseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.3018     0.5842   0.517  0.60543    
#sizeMedium       -0.9918     0.3743  -2.650  0.00805 ** 
#sizeSmall        -0.1186     0.6997  -0.169  0.86544    
#furlengthshort   -1.2263     0.5379  -2.280  0.02263 *  
#agestagesAdult   -0.3337     0.3721  -0.897  0.36982    
#agestagesPuppy  -17.1066   867.6938  -0.020  0.98427    
#agestagesSenior  -0.6645     0.7461  -0.891  0.37311    
#ticksY            0.9095     0.4078   2.230  0.02575 *  
#ehrseropos        2.2625     0.4160   5.439 5.36e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 278.97  on 220  degrees of freedom
#Residual deviance: 208.46  on 212  degrees of freedom
#(52 observations deleted due to missingness)
#AIC: 226.46

#Number of Fisher Scoring iterations: 16

#retired 
#mansteplyme4<-glm(lymeseropos ~ size + furlength + ticks + ehrseropos + agestages, ShelterTBD, family = binomial )
#summary(mansteplyme4)
#Call:
#  glm(formula = lymeseropos ~ size + furlength + ticks + ehrseropos + 
#        agestages, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.1771     0.5723   0.309  0.75695    
#sizeMedium        -1.0402     0.3724  -2.793  0.00522 ** 
#  sizeSmall         -0.3062     0.6946  -0.441  0.65933    
#furlengthNA       -0.8524     0.7791  -1.094  0.27393    
#furlengthshort    -1.1587     0.5248  -2.208  0.02725 *  
#  ticksY             0.8853     0.3851   2.299  0.02153 *  
#  ehrseropos1        2.5179     0.4044   6.226 4.77e-10 ***
#  ehrseroposNA      21.6499  1101.0373   0.020  0.98431    
#agestagesAdult    -0.3451     0.3725  -0.926  0.35421    
#agestagesPuppy    -2.0131     0.9921  -2.029  0.04244 *  
#  agestagesSenior   -0.9081     0.7323  -1.240  0.21493    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 367.83  on 271  degrees of freedom
#Residual deviance: 226.18  on 261  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 248.18

#Number of Fisher Scoring iterations: 17


#updated 3.28.26 - ages stages fell out of man steplyme4 = this did not happen in previous model 
mansteplyme5<-glm(lymeseropos ~ size + furlength +  ticks + ehrseropos, ShelterTBD, family = binomial )
summary(mansteplyme5)
##########Lyme sig size, fur length, ticks, ehr#############
#Call:
#  glm(formula = lymeseropos ~ size + furlength + ticks + ehrseropos, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -0.1409     0.5112  -0.276  0.78281    
#sizeMedium      -0.9665     0.3668  -2.635  0.00841 ** 
#sizeSmall       -1.2351     0.6282  -1.966  0.04928 *  
#furlengthshort  -0.9343     0.4942  -1.891  0.05868 .  
#ticksY           0.8070     0.3936   2.050  0.04035 *  
#ehrseropos       2.2013     0.3900   5.644 1.66e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 278.97  on 220  degrees of freedom
#Residual deviance: 218.78  on 215  degrees of freedom
#(52 observations deleted due to missingness)
#AIC: 230.78

#Number of Fisher Scoring iterations: 4

#updated 3.30.26
mansteplyme6<-glm(lymeseropos ~ size +  ticks + ehrseropos, ShelterTBD, family = binomial )
summary(mansteplyme6)

#Call:
#  glm(formula = lymeseropos ~ size + ticks + ehrseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.9990     0.2518  -3.968 7.25e-05 ***
#sizeMedium   -0.9755     0.3552  -2.746  0.00603 ** 
#sizeSmall    -1.2542     0.5330  -2.353  0.01862 *  
#ticksY        0.8024     0.3742   2.144  0.03202 *  
#ehrseropos    2.3637     0.3652   6.472 9.64e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 306.34  on 240  degrees of freedom
#Residual deviance: 237.79  on 236  degrees of freedom
#(32 observations deleted due to missingness)
#AIC: 247.79

#Number of Fisher Scoring iterations: 4

exp(cbind(OR = coef(mansteplyme6), confint(mansteplyme6)))
#Waiting for profiling to be done...
#               OR      2.5 %     97.5 %
#(Intercept)  0.3682299 0.22106755  0.5955102
#sizeMedium   0.3769914 0.18453509  0.7474969
#sizeSmall    0.2853101 0.09382108  0.7730867
#ticksY       2.2309060 1.06951669  4.6717454
#ehrseropos  10.6300954 5.30892725 22.3686784

#exp(coef(mansteplyme5))
#(Intercept)     sizeMedium      sizeSmall furlengthshort         ticksY     ehrseropos 
#0.8685713      0.3804008      0.2908054      0.3928476      2.2411675      9.0365063 

# retired exp(coef(mansteplyme4))
#(Intercept)      sizeMedium       sizeSmall     furlengthNA  furlengthshort          ticksY     ehrseropos1    ehrseroposNA  agestagesAdult  agestagesPuppy agestagesSenior 
#1.193788e+00    3.533895e-01    7.362370e-01    4.263836e-01    3.138850e-01    2.423629e+00    1.240191e+01    2.525878e+09    7.081210e-01    1.335687e-01    4.032878e-01 


#exp(cbind(OR = coef(mansteplyme5), confint(mansteplyme5)))

#Waiting for profiling to be done...
#                    OR      2.5 %     97.5 %
#(Intercept)       0.8685713 0.31362484  2.3798709
#sizeMedium        0.3804008 0.18176406  0.7709271
#sizeSmall         0.2908054 0.07414556  0.9129602
#furlengthshort    0.3928476 0.14748006  1.0423983
#ticksY            2.2411675 1.03309241  4.8746680
#ehrseropos        9.0365063 4.30349684 20.0157729


#retired exp(cbind(OR = coef(mansteplyme4), confint(mansteplyme4)))
#Waiting for profiling to be done...
#                       OR        2.5 %        97.5 %
#  (Intercept)     1.193788e+00 3.858610e-01  3.723344e+00
#sizeMedium      3.533895e-01 1.667345e-01  7.229323e-01
#sizeSmall       7.362370e-01 1.714344e-01  2.718272e+00
#furlengthNA     4.263836e-01 9.075296e-02  1.972287e+00
#furlengthshort  3.138850e-01 1.101954e-01  8.790754e-01
#ticksY          2.423629e+00 1.139812e+00  5.198377e+00
#ehrseropos1     1.240191e+01 5.787306e+00  2.848393e+01
#ehrseroposNA    2.525878e+09 2.718658e-10 9.449454e+174
#agestagesAdult  7.081210e-01 3.354034e-01  1.454694e+00
#agestagesPuppy  1.335687e-01 1.648031e-02  8.501606e-01
#agestagesSenior 4.032878e-01 9.152402e-02  1.652015e+00
#There were 50 or more warnings (use warnings() to see the first 50)

#warnings()
#Warning messages:
#  1: glm.fit: fitted probabilities numerically 0 or 1 occurred


step(model10)
#Start:  AIC=258.97
#lymeseropos ~ county + size + furlength + agestages + sex + ticks + 
#  anaseropos + ehrseropos + rickifa

#Df Deviance    AIC
#- county      8   222.02 252.02
#- anaseropos  1   213.22 257.22
#- rickifa     2   216.08 258.08
#- agestages   3   218.45 258.45
#<none>            212.97 258.97
#- sex         1   215.16 259.15
#- furlength   2   217.68 259.68
#- ticks       1   216.22 260.22
#- size        2   220.44 262.44
#- ehrseropos  1   256.94 300.94

#Step:  AIC=252.02
#lymeseropos ~ size + furlength + agestages + sex + ticks + anaseropos + 
 # ehrseropos + rickifa

#Df Deviance    AIC
#- anaseropos  1   222.33 250.33
#- rickifa     2   224.79 250.79
#- sex         1   223.46 251.46
#- agestages   3   227.69 251.69
#<none>            222.02 252.02
#- furlength   2   227.18 253.18
#- ticks       1   225.72 253.72
#- size        2   229.15 255.15
#- ehrseropos  1   266.52 294.52

#Step:  AIC=250.33
#lymeseropos ~ size + furlength + agestages + sex + ticks + ehrseropos + 
#  rickifa

#Df Deviance    AIC
#- rickifa     2   224.87 248.87
#- sex         1   223.74 249.74
#- agestages   3   227.94 249.94
#<none>            222.33 250.33
#- furlength   2   227.52 251.52
#- ticks       1   225.97 251.97
#- size        2   229.31 253.31
#- ehrseropos  2   334.93 358.93

#Step:  AIC=248.87
#lymeseropos ~ size + furlength + agestages + sex + ticks + ehrseropos

#Df Deviance    AIC
#- sex         1   226.18 248.18
#- agestages   3   230.20 248.20
#<none>            224.87 248.87
#- furlength   2   229.61 249.61
#- size        2   231.85 251.85
#- ticks       1   230.32 252.32
#- ehrseropos  2   341.09 361.09

#Step:  AIC=248.18
#lymeseropos ~ size + furlength + agestages + ticks + ehrseropos

#Df Deviance    AIC
#- agestages   3   231.40 247.40
#<none>            226.18 248.18
#- furlength   2   231.06 249.06
#- ticks       1   231.46 251.46
#- size        2   234.48 252.48
#- ehrseropos  2   342.07 360.07#

#Step:  AIC=247.4
#lymeseropos ~ size + furlength + ticks + ehrseropos

#Df Deviance    AIC
#- furlength   2   235.13 247.13
#<none>            231.40 247.40
#- ticks       1   236.02 250.02
#- size        2   241.19 253.19
#- ehrseropos  2   353.30 365.30

#Step:  AIC=247.13
#lymeseropos ~ size + ticks + ehrseropos#

#Df Deviance    AIC
#<none>            235.13 247.13
#- ticks       1   239.87 249.87
#- size        2   244.72 252.72
#- ehrseropos  2   358.71 366.71

#Call:  glm(formula = lymeseropos ~ size + ticks + ehrseropos, family = binomial, 
#           data = ShelterTBD)

#Coefficients:
#  (Intercept)    sizeMedium     sizeSmall        ticksY   ehrseropos1  ehrseroposNA  
#-1.0464       -0.9418       -1.2204        0.8197        2.3860       20.4394  #

#Degrees of Freedom: 271 Total (i.e. Null);  266 Residual
#(1 observation deleted due to missingness)
#Null Deviance:	    367.8 
#Residual Deviance: 235.1 	AIC: 247.1
#Warning messages:
#  1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#4: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#5: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#6: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#7: glm.fit: fitted probabilities numerically 0 or 1 occurred 
#8: glm.fit: fitted probabilities numerically 0 or 1 occurred 



#reduced.model1<-step(model10,direction='backward')

#lymmodels <-list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, finmod1, mansteplyme6, mansteplyme5, mansteplyme4, mansteplyme3, mansteplyme2, mansteplyme1)

#lymmodels.names <- c('model1', 'model2', 'model3', 'model4', 'model5', 'model6', 'model7', 'model8', 'model9', 'model10', 'finmod1', 'mansteplyme6', 'mansteplyme5', 'mansteplyme4', 'mansteplyme3', 'mansteplyme2', 'mansteplyme1')

#aictab(cand.set =lymmodels, modnames = lymmodels.names)


#Model selection based on AICc:
  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#mansteplyme1 12 223.25       0.00   0.50   0.50  -98.84
#mansteplyme4  7 224.49       1.25   0.27   0.77 -104.97
#mansteplyme3  8 226.26       3.01   0.11   0.89 -104.78
#mansteplyme5  6 227.84       4.59   0.05   0.94 -107.72
#mansteplyme2  9 228.08       4.83   0.04   0.98 -104.60
#finmod1       5 230.92       7.67   0.01   0.99 -110.32
#model10      19 231.65       8.41   0.01   1.00  -94.86
#mansteplyme6  5 244.48      21.24   0.00   1.00 -117.11
#model8        2 251.98      28.74   0.00   1.00 -123.97
#model3        2 278.08      54.84   0.00   1.00 -137.01
#model9        2 292.13      68.89   0.00   1.00 -144.04
#model6        2 300.40      77.16   0.00   1.00 -148.17
#model2        3 302.51      79.27   0.00   1.00 -148.21
#model4        4 305.74      82.49   0.00   1.00 -148.78
#model5        2 306.86      83.62   0.00   1.00 -151.41
#model7        2 307.41      84.16   0.00   1.00 -151.68
#model1        8 314.08      90.83   0.00   1.00 -148.73

#### adding in the mansteplyme models makes mansteplyme1 best? but not all significant?  Mansteplyme is all sig but not that good   ==== 

###2.26 Rerunning data #####
#modelf1<-glm(lymeseropos ~ county, ShelterTBD, family=binomial)
#summary(modelf1)
####RESULT: county not sig predictor of lyme prev #### 
#Call:
 # glm(formula = lymeseropos ~ county, family = binomial, data = ShelterTBD)

#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.557e+01  1.029e+03  -0.015    0.988
#countyFranklin -3.374e-11  1.782e+03   0.000    1.000
#countyGallia    1.492e+01  1.029e+03   0.014    0.988
#countyJackson   1.485e+01  1.029e+03   0.014    0.988
#countyLawrence  1.563e+01  1.029e+03   0.015    0.988
#countyRichland -8.922e-12  1.782e+03   0.000    1.000
#countyRoss      1.454e+01  1.029e+03   0.014    0.989
#countyScioto    1.467e+01  1.029e+03   0.014    0.989

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 308.53  on 241  degrees of freedom
#Residual deviance: 299.72  on 234  degrees of freedom
#(31 observations deleted due to missingness)
#AIC: 315.72

#Number of Fisher Scoring iterations: 14


#modelf2<-glm(lymeseropos ~ furlength, ShelterTBD, family=binomial)
#summary(modelf2)
####RESULT: furlength (short fure)  sig predictor of lyme prev #### 
#Call:
 # glm(formula = lymeseropos ~ furlength, family = binomial, data = ShelterTBD)

#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)  
#(Intercept)     0.08701    0.41742   0.208   0.8349  
#furlengthshort -0.90339    0.44484  -2.031   0.0423 *
 # ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 281.21  on 221  degrees of freedom
#Residual deviance: 277.13  on 220  degrees of freedom
#(51 observations deleted due to missingness)
#AIC: 281.13

#Number of Fisher Scoring iterations: 4

#modelf3<-glm(lymeseropos ~ agestages, ShelterTBD, family=binomial)
#summary(modelf3)
####RESULTS: age is mildly significant predictor of lyme prev - this is different! than before####

#Call:
 # glm(formula = lymeseropos ~ agestages, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.7662     0.2107  -3.637 0.000276 ***
#  agestagesAdult    0.3522     0.2949   1.194 0.232395    
#agestagesPuppy   -1.2707     0.6490  -1.958 0.050232 .  
#agestagesSenior   0.4785     0.5797   0.825 0.409140    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 308.53  on 241  degrees of freedom
#Residual deviance: 299.43  on 238  degrees of freedom
#(31 observations deleted due to missingness)
#AIC: 307.43

#Number of Fisher Scoring iterations: 4


####Ehrlichia models ===============================

#i think i have to turn ehr into a factor too, trying that here 
levels(ShelterTBD$ehrseropos)
ShelterTBD$ehrseropos <-factor(ShelterTBD$ehrseropos)
class(ShelterTBD$ehrseropos)
#This worked!!!!! otherwise you get this error > model11<-glm(ehrseropos ~ county, ShelterTBD, family=binomial) Error in eval(family$initialize) : y values must be 0 <= y <= 1 > summary(model11) Error: object 'model11' not found

model11<-glm(ehrseropos ~ county, ShelterTBD, family=binomial)
summary(model11)

###RESULT = County is not a significant predictor of ehr prev =====

#NEW RESULTS 
#Call:
#  glm(formula = ehrseropos ~ county, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.557e+01  1.029e+03  -0.015    0.988
#countyDelaware  3.113e+01  1.455e+03   0.021    0.983
#countyFranklin  1.626e+01  1.029e+03   0.016    0.987
#countyGallia    1.476e+01  1.029e+03   0.014    0.989
#countyJackson   1.449e+01  1.029e+03   0.014    0.989
#countyLawrence  1.525e+01  1.029e+03   0.015    0.988
#countyRichland -1.288e-08  1.782e+03   0.000    1.000
#countyRoss      1.484e+01  1.029e+03   0.014    0.988
#countyScioto    1.511e+01  1.029e+03   0.015    0.988

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 335.22  on 264  degrees of freedom
#AIC: 353.22

#Number of Fisher Scoring iterations: 14

model12<-glm(ehrseropos ~ size, ShelterTBD, family=binomial)
summary(model12)

####RESULT = Size is a significant predictor of ehr prev ====

#NEW RESULTS 
#Call:
#  glm(formula = ehrseropos ~ size, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -0.5991     0.1993  -3.006  0.00265 **
#  sizeMedium   -0.3172     0.2889  -1.098  0.27230   
#sizeSmall     0.1609     0.3493   0.461  0.64510   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 345.37  on 270  degrees of freedom
#AIC: 351.37

#Number of Fisher Scoring iterations: 4

model13<-glm(ehrseropos ~ furlength, ShelterTBD, family=binomial)
summary(model13)

###RESULT = Fur length is not a significant predictor of ehr prev ======

#NEW RESULTS 
#Call:
#  glm(formula = ehrseropos ~ furlength, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -0.3747     0.3917  -0.957    0.339
#furlengthNA      0.5417     0.5668   0.956    0.339
#furlengthshort  -0.4640     0.4181  -1.110    0.267

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 341.50  on 270  degrees of freedom
#AIC: 347.5

#Number of Fisher Scoring iterations: 4

model14<-glm(ehrseropos ~ agestages, ShelterTBD, family=binomial)
summary(model14)

###RESULT = Age is a significant predictor of ehr prev =====

#Call:
#glm(formula = ehrseropos ~ agestages, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.5329     0.2530  -6.060 1.37e-09 ***
#  agestagesAdult    0.9004     0.3302   2.727  0.00640 ** 
#  agestagesPuppy    1.8314     0.3738   4.899 9.62e-07 ***
#  agestagesSenior   1.5329     0.5914   2.592  0.00954 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 319.69  on 269  degrees of freedom
#AIC: 327.69

#Number of Fisher Scoring iterations: 4


model15<-glm(ehrseropos~ sex, ShelterTBD, family=binomial)
summary(model15)

###RESULT = Sex is a signficant predictor of ehr prev =======

#Call:
#  glm(formula = ehrseropos ~ sex, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.7841     0.1862  -4.211 2.55e-05 ***
#  sexMale       0.1761     0.2573   0.685    0.494    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 347.07  on 271  degrees of freedom
#AIC: 351.07

#Number of Fisher Scoring iterations: 4


model16<-glm(ehrseropos ~ ticks, ShelterTBD, family=binomial)
summary(model16)

###RESULT = Ticks ? are a significant predictor of ehr pos ======

#Call:
#  glm(formula = ehrseropos ~ ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.7287     0.1466  -4.972 6.64e-07 ***
#  ticksY        0.1097     0.3078   0.356    0.722    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 345.21  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 349.21

#Number of Fisher Scoring iterations: 4

model17<-glm(ehrseropos~ anaseropos, ShelterTBD, family=binomial)
summary(model17)

#### RESULT = not having ana is a sig pred of ehr prev =====

#new results 
#Call:
#  glm(formula = ehrseropos ~ anaseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -1.1967     0.1539  -7.777 7.42e-15 ***
#  anaseropos1    19.7627  3261.3193   0.006    0.995    
#anaseroposNA   19.7627  1153.0505   0.017    0.986    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 256.80  on 270  degrees of freedom
#AIC: 262.8

#Number of Fisher Scoring iterations: 17

model18<-glm(ehrseropos ~ lymeseropos, ShelterTBD, family=binomial)
summary(model18)

####RESULT = lyme pos is a sign pred of ehr prev =====
#New results
##Call:
#  glm(formula = ehrseropos ~ lymeseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.2756     0.2711  -8.393  < 2e-16 ***
#  lymeseropos1     2.4762     0.3522   7.031 2.04e-12 ***
#  lymeseroposNA   20.8416  1153.0505   0.018    0.986    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 209.86  on 270  degrees of freedom
#AIC: 215.86

#Number of Fisher Scoring iterations: 17

model19<-glm(ehrseropos ~ rickifa, ShelterTBD, family=binomial)
summary(model19)

###RESULT = rickifa is a significant indicator for ehr prev ======= MEEHHHHHH
#new results  
#Call:
#glm(formula = ehrseropos ~ rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.8315     0.1498  -5.553 2.81e-08 ***
#  rickifa1      0.5997     0.3168   1.893   0.0584 .  
#rickifaNA     0.4261     0.6626   0.643   0.5202    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 343.81  on 270  degrees of freedom
#AIC: 349.81

#Number of Fisher Scoring iterations: 4

model20 <- glm(ehrseropos ~ county + size + furlength + agestages + sex + ticks + anaseropos + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(model20)
#New results, scroll down for old results 
#Call:
#  glm(formula = ehrseropos ~ county + size + furlength + agestages + 
 #       sex + ticks + anaseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.148e+01  7.604e+03  -0.003  0.99775    
#countyDelaware   9.276e+00  1.098e+04   0.001  0.99933    
#countyFranklin   4.962e+00  7.768e+03   0.001  0.99949    
#countyGallia     1.749e+01  7.604e+03   0.002  0.99817    
#countyJackson    1.766e+01  7.604e+03   0.002  0.99815    
#countyLawrence   1.729e+01  7.604e+03   0.002  0.99819    
#countyRichland  -3.228e-08  1.317e+04   0.000  1.00000    
#countyRoss       1.640e+01  7.604e+03   0.002  0.99828    
#countyScioto     1.787e+01  7.604e+03   0.002  0.99812    
#sizeMedium       4.260e-01  4.383e-01   0.972  0.33104    
#sizeSmall       -1.322e+00  9.974e-01  -1.326  0.18499    
#furlengthNA      2.325e+00  9.749e-01   2.385  0.01707 *  
#  furlengthshort   3.537e-01  6.387e-01   0.554  0.57978    
#agestagesAdult   1.379e+00  4.605e-01   2.994  0.00275 ** 
#  agestagesPuppy   6.447e-01  1.213e+00   0.531  0.59509    
#agestagesSenior  2.474e+00  8.517e-01   2.904  0.00368 ** 
#  sexMale          1.813e-01  4.094e-01   0.443  0.65785    
#ticksY          -3.905e-02  4.754e-01  -0.082  0.93454    
#anaseropos1      2.089e+01  4.296e+03   0.005  0.99612    
#anaseroposNA     3.245e+01  2.236e+03   0.015  0.98842    
#lymeseropos1     2.703e+00  4.517e-01   5.984 2.18e-09 ***
#  lymeseroposNA           NA         NA      NA       NA    
#rickifa1         6.110e-02  4.785e-01   0.128  0.89840    
#rickifaNA        7.847e-01  1.607e+00   0.488  0.62545    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 172.30  on 249  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 218.3

#Number of Fisher Scoring iterations: 18

###RESULTS = Adult, Senior, and lymeseropos are significant predictors of ehrseropos ====== still the same with new data 

finmod2<-glm(ehrseropos ~ agestages + lymeseropos, ShelterTBD, family=binomial )
summary(finmod2)
#new results, scroll for old results 
#Call:
#  glm(formula = ehrseropos ~ agestages + lymeseropos, family = binomial, 
 #     data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -2.9978     0.4012  -7.472 7.88e-14 ***
#  agestagesAdult     1.1382     0.3988   2.854  0.00431 ** 
#  agestagesPuppy     0.3844     0.7497   0.513  0.60812    
#agestagesSenior    1.9676     0.7176   2.742  0.00611 ** 
#  lymeseropos1       2.5192     0.3740   6.736 1.62e-11 ***
#  lymeseroposNA     21.2010  1149.8972   0.018  0.98529    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 197.03  on 267  degrees of freedom
#AIC: 209.03

#Number of Fisher Scoring iterations: 17


step(model20)

#Start:  AIC=188.42
#ehrseropos ~ county + size + furlength + agestages + sex + ticks + 
#  anaseropos + lymeseropos + rickifa

#Df Deviance    AIC
#- county       7   156.20 180.20
#- rickifa      1   150.42 186.42
#- ticks        1   150.50 186.50
#- furlength    1   150.61 186.61
#- sex          1   151.35 187.35
#<none>             150.41 188.41
#- size         2   154.68 188.68
#- anaseropos   1   159.83 195.83
#- agestages    3   165.64 197.64
#- lymeseropos  1   181.76 217.76

#Step:  AIC=180.2
#ehrseropos ~ size + furlength + agestages + sex + ticks + anaseropos + 
#  lymeseropos + rickifa

#Df Deviance    AIC
#- rickifa      1   156.23 178.23
#- furlength    1   156.23 178.23
#- ticks        1   156.48 178.48
#- sex          1   157.34 179.34
#- size         2   159.56 179.56
#<none>             156.20 180.20
#- anaseropos   1   164.68 186.68
#- agestages    3   171.45 189.45
#- lymeseropos  1   189.83 211.83
########Error in step(model20) : ======================
#  number of rows in use has changed: remove missing values?

#####Since step model doesn't work, going to do stepwise backwards manually======  Still not working with new data as of 6.24.24 so going to do backward man stepwise


manstepehr1 <- glm(ehrseropos ~ county + size + furlength + agestages + sex + ticks + anaseropos + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepehr1)

#updated 3.28.26 
#Call:
#  glm(formula = ehrseropos ~ county + size + furlength + agestages + 
#        sex + ticks + anaseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.072e+01  4.612e+03  -0.004  0.99642    
#countyFranklin   1.736e+00  7.989e+03   0.000  0.99983    
#countyGallia     1.688e+01  4.612e+03   0.004  0.99708    
#countyJackson    1.706e+01  4.612e+03   0.004  0.99705    
#countyLawrence   1.693e+01  4.612e+03   0.004  0.99707    
#countyRichland  -8.781e-08  7.989e+03   0.000  1.00000    
#countyRoss       1.576e+01  4.612e+03   0.003  0.99727    
#countyScioto     1.730e+01  4.612e+03   0.004  0.99701    
#sizeMedium       2.379e-01  4.470e-01   0.532  0.59453    
#sizeSmall       -2.250e+00  1.344e+00  -1.674  0.09405 .  
#furlengthshort   1.825e-01  6.448e-01   0.283  0.77718    
#agestagesAdult   1.471e+00  4.763e-01   3.088  0.00202 ** 
#agestagesPuppy  -1.414e+01  1.475e+03  -0.010  0.99235    
#agestagesSenior  2.202e+00  8.932e-01   2.466  0.01368 *  
#sexMale          5.034e-01  4.334e-01   1.162  0.24543    
#ticksY           1.750e-01  5.016e-01   0.349  0.72717    
#anaseropos       2.033e+01  3.307e+03   0.006  0.99510    
#lymeseropos      2.379e+00  4.745e-01   5.013 5.35e-07 ***
#rickifa          1.527e-02  4.824e-01   0.032  0.97474    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 228.33  on 214  degrees of freedom
#Residual deviance: 152.59  on 196  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 190.59

#Number of Fisher Scoring iterations: 17


#retired new results, scroll for old
#Call:
#  glm(formula = ehrseropos ~ county + size + furlength + agestages + 
#        sex + ticks + anaseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.148e+01  7.604e+03  -0.003  0.99775    
#countyDelaware   9.276e+00  1.098e+04   0.001  0.99933    
#countyFranklin   4.962e+00  7.768e+03   0.001  0.99949    
#countyGallia     1.749e+01  7.604e+03   0.002  0.99817    
#countyJackson    1.766e+01  7.604e+03   0.002  0.99815    
#countyLawrence   1.729e+01  7.604e+03   0.002  0.99819    
#countyRichland  -3.228e-08  1.317e+04   0.000  1.00000    
#countyRoss       1.640e+01  7.604e+03   0.002  0.99828    
#countyScioto     1.787e+01  7.604e+03   0.002  0.99812    
#sizeMedium       4.260e-01  4.383e-01   0.972  0.33104    
#sizeSmall       -1.322e+00  9.974e-01  -1.326  0.18499    
#furlengthNA      2.325e+00  9.749e-01   2.385  0.01707 *  
#  furlengthshort   3.537e-01  6.387e-01   0.554  0.57978    
#agestagesAdult   1.379e+00  4.605e-01   2.994  0.00275 ** 
#  agestagesPuppy   6.447e-01  1.213e+00   0.531  0.59509    
#agestagesSenior  2.474e+00  8.517e-01   2.904  0.00368 ** 
#  sexMale          1.813e-01  4.094e-01   0.443  0.65785    
#ticksY          -3.905e-02  4.754e-01  -0.082  0.93454    
#anaseropos1      2.089e+01  4.296e+03   0.005  0.99612    
#anaseroposNA     3.245e+01  2.236e+03   0.015  0.98842    
##lymeseropos1     2.703e+00  4.517e-01   5.984 2.18e-09 ***
#  lymeseroposNA           NA         NA      NA       NA    
#rickifa1         6.110e-02  4.785e-01   0.128  0.89840    
#rickifaNA        7.847e-01  1.607e+00   0.488  0.62545    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 172.30  on 249  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 218.3

#Number of Fisher Scoring iterations: 18

manstepehr2 <- glm(ehrseropos ~ size + furlength + agestages + sex + ticks + anaseropos + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepehr2)

#counties falls out with the highest overall p values #
#upated 3.28.26
#Call:
#  glm(formula = ehrseropos ~ size + furlength + agestages + sex + 
#        ticks + anaseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.77507    0.88241  -4.278 1.88e-05 ***
#sizeMedium         0.26793    0.43705   0.613  0.53985    
#sizeSmall         -1.95430    1.32117  -1.479  0.13908    
#furlengthshort     0.05661    0.60315   0.094  0.92522    
#agestagesAdult     1.46921    0.45728   3.213  0.00131 ** 
#agestagesPuppy   -14.12558 1505.97140  -0.009  0.99252    
#agestagesSenior    2.01249    0.86658   2.322  0.02021 *  
#sexMale            0.51759    0.42112   1.229  0.21904    
#ticksY             0.26852    0.49323   0.544  0.58615    
#anaseropos        20.27664 3151.31712   0.006  0.99487    
#lymeseropos        2.36200    0.44965   5.253 1.50e-07 ***
#rickifa            0.11594    0.47301   0.245  0.80637    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 228.33  on 214  degrees of freedom
#Residual deviance: 157.99  on 203  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 181.99

#Number of Fisher Scoring iterations: 17



#retired new results 
#Call:
#  glm(formula = ehrseropos ~ size + furlength + agestages + sex + 
#        ticks + anaseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.89269    0.84894  -4.585 4.53e-06 ***
#  sizeMedium         0.39677    0.42897   0.925  0.35499    
#sizeSmall         -1.11670    0.98783  -1.130  0.25829    
#furlengthNA        2.15922    0.93329   2.314  0.02069 *  
#  furlengthshort     0.22025    0.59649   0.369  0.71195    
#agestagesAdult     1.39634    0.44091   3.167  0.00154 ** 
#  agestagesPuppy     0.56488    1.23563   0.457  0.64756    
#agestagesSenior    2.32534    0.81695   2.846  0.00442 ** 
#  sexMale            0.18754    0.39669   0.473  0.63639    
#ticksY             0.07424    0.46581   0.159  0.87338    
#anaseropos1       19.88521 2558.33629   0.008  0.99380    
#anaseroposNA      22.03039 1099.45414   0.020  0.98401    
#lymeseropos1       2.60698    0.42608   6.119 9.44e-10 ***
#  lymeseroposNA           NA         NA      NA       NA    
#rickifa1           0.13212    0.47103   0.281  0.77909    
#rickifaNA          0.59547    1.51339   0.393  0.69398    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 177.32  on 257  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 207.32

#Number of Fisher Scoring iterations: 17


manstepehr3 <- glm(ehrseropos ~ size + furlength + agestages + sex + ticks + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepehr3)

#anaseropos falls out, highest p value 
#updated 3.28.26 
#Call:
#  glm(formula = ehrseropos ~ size + furlength + agestages + sex + 
#        ticks + lymeseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.60233    0.85704  -4.203 2.63e-05 ***
#sizeMedium        0.21894    0.42520   0.515   0.6066    
#sizeSmall        -1.92992    1.31873  -1.463   0.1433    
#furlengthshort    0.09092    0.59768   0.152   0.8791    
#agestagesAdult    1.32199    0.43897   3.012   0.0026 ** 
#agestagesPuppy  -13.34710  913.21889  -0.015   0.9883    
#agestagesSenior   1.79568    0.85492   2.100   0.0357 *  
#sexMale           0.46498    0.40884   1.137   0.2554    
#ticksY            0.43900    0.47069   0.933   0.3510    
#lymeseropos       2.26611    0.43232   5.242 1.59e-07 ***
#rickifa           0.23316    0.45188   0.516   0.6059    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 228.33  on 214  degrees of freedom
#Residual deviance: 166.44  on 204  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 188.44

#Number of Fisher Scoring iterations: 16

#retire new results 

#Call:
#  glm(formula = ehrseropos ~ size + furlength + agestages + sex + 
#        ticks + lymeseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.7483     0.8291  -4.521 6.15e-06 ***
#  sizeMedium         0.3445     0.4178   0.825  0.40955    
#sizeSmall         -1.1311     0.9740  -1.161  0.24555    
#furlengthNA        2.1225     0.9176   2.313  0.02072 *  
#  furlengthshort     0.2592     0.5919   0.438  0.66139    
#agestagesAdult     1.2565     0.4258   2.951  0.00317 ** 
#  agestagesPuppy     0.4390     1.2080   0.363  0.71627    
#agestagesSenior    2.1239     0.7966   2.666  0.00767 ** 
#  sexMale            0.1614     0.3876   0.416  0.67709    
#ticksY             0.2469     0.4448   0.555  0.57888    
#lymeseropos1       2.5259     0.4111   6.144 8.02e-10 ***
#  lymeseroposNA     21.9720  1101.2670   0.020  0.98408    
#rickifa1           0.2534     0.4504   0.563  0.57364    
#rickifaNA          0.4949     1.4755   0.335  0.73729    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 186.19  on 258  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 214.19

#Number of Fisher Scoring iterations: 17


manstepehr4 <- glm(ehrseropos ~ size + agestages + sex + ticks + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepehr4)

#technically puppy is highest p value but other variables in the category are sig so droppiing short hair (NA hair is sig but its na soooooo dropping it?)
#updated 3.28.26
#Call:
#  glm(formula = ehrseropos ~ size + agestages + sex + ticks + lymeseropos + 
#        rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.3065     0.5558  -5.949 2.70e-09 ***
#sizeMedium        0.3283     0.4065   0.808  0.41934    
#sizeSmall        -0.5712     0.9259  -0.617  0.53728    
#agestagesAdult    1.1305     0.4095   2.761  0.00577 ** 
#agestagesPuppy    0.7967     1.1072   0.720  0.47177    
#agestagesSenior   1.9477     0.7802   2.496  0.01254 *  
#sexMale           0.1823     0.3774   0.483  0.62911    
#ticksY            0.3194     0.4299   0.743  0.45759    
#lymeseropos       2.3609     0.3973   5.942 2.82e-09 ***
#rickifa           0.2595     0.4342   0.598  0.55005    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 256.99  on 232  degrees of freedom
#Residual deviance: 192.15  on 223  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 212.15

#Number of Fisher Scoring iterations: 5

#retired new data results 
#Call:
# glm(formula = ehrseropos ~ size + agestages + sex + ticks + lymeseropos + 
#       rickifa, family = binomial, data = ShelterTBD)

#coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.32980    0.55763  -5.971 2.35e-09 ***
#  sizeMedium         0.31280    0.40933   0.764  0.44476    
#sizeSmall         -0.79008    0.90268  -0.875  0.38143    
#agestagesAdult     1.18035    0.41421   2.850  0.00438 ** 
#  agestagesPuppy     1.07868    1.02617   1.051  0.29318    
#agestagesSenior    2.10601    0.76972   2.736  0.00622 ** 
#  sexMale            0.09272    0.37883   0.245  0.80664    
#ticksY             0.30735    0.43277   0.710  0.47758    
#lymeseropos1       2.50265    0.39645   6.313 2.74e-10 ***
#  lymeseroposNA     21.21685 1127.59181   0.019  0.98499    
#rickifa1           0.20509    0.43920   0.467  0.64052    
#rickifaNA          0.54265    1.33733   0.406  0.68491    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 192.91  on 260  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 216.91

#Number of Fisher Scoring iterations: 17


manstepehr5 <- glm(ehrseropos ~ size + agestages + ticks + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepehr5)

#Sex fell out with the highest p value 
#upd 3.28.26 
#Call:
#  glm(formula = ehrseropos ~ size + agestages + ticks + lymeseropos + 
#        rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.1943     0.5008  -6.378 1.79e-10 ***
#sizeMedium        0.2968     0.4009   0.740  0.45915    
#sizeSmall        -0.6045     0.9202  -0.657  0.51123    
#agestagesAdult    1.1155     0.4079   2.735  0.00624 ** 
#agestagesPuppy    0.8097     1.1075   0.731  0.46468    
#agestagesSenior   1.9893     0.7767   2.561  0.01044 *  
#ticksY            0.3294     0.4290   0.768  0.44258    
#lymeseropos       2.3769     0.3960   6.002 1.95e-09 ***
#rickifa           0.2526     0.4340   0.582  0.56061    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 256.99  on 232  degrees of freedom
#Residual deviance: 192.38  on 224  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 210.38

#Number of Fisher Scoring iterations: 5


#new results 
#Call:
#  glm(formula = ehrseropos ~ size + agestages + ticks + lymeseropos + 
#        rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.2726     0.5041  -6.492 8.48e-11 ***
#  sizeMedium         0.2963     0.4035   0.734  0.46283    
#sizeSmall         -0.8040     0.9001  -0.893  0.37170    
#agestagesAdult     1.1740     0.4133   2.841  0.00450 ** 
#  agestagesPuppy     1.0769     1.0291   1.046  0.29534    
#agestagesSenior    2.1285     0.7646   2.784  0.00537 ** 
#  ticksY             0.3126     0.4322   0.723  0.46951    
#lymeseropos1       2.5108     0.3951   6.354 2.09e-10 ***
#  lymeseroposNA     21.2087  1129.0652   0.019  0.98501    
#rickifa1           0.2008     0.4390   0.457  0.64742    
#rickifaNA          0.5273     1.3387   0.394  0.69363    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 192.97  on 261  degrees of freedom
#(1 observation deleted due to missingness) ###########What doe sthis meaN?
#AIC: 214.97

#Number of Fisher Scoring iterations: 17



manstepehr6 <- glm(ehrseropos ~ size + agestages + ticks + lymeseropos, ShelterTBD, family = binomial )
summary(manstepehr6)
#RickIfa fell out with the highest p value 

#updated 3.28.26
#Call:
#  glm(formula = ehrseropos ~ size + agestages + ticks + lymeseropos, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.2555     0.5004  -6.505 7.75e-11 ***
#sizeMedium        0.3083     0.4009   0.769  0.44197    
#sizeSmall        -0.7633     0.8707  -0.877  0.38065    
#agestagesAdult    1.1588     0.4092   2.832  0.00463 ** 
#agestagesPuppy    1.1126     1.0189   1.092  0.27486    
#agestagesSenior   2.1812     0.7467   2.921  0.00349 ** 
#ticksY            0.4140     0.4061   1.019  0.30802    
#lymeseropos       2.4931     0.3914   6.370 1.89e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 265.99  on 240  degrees of freedom
#Residual deviance: 195.17  on 233  degrees of freedom
#(32 observations deleted due to missingness)
#AIC: 211.17

#Number of Fisher Scoring iterations: 5


#reitred new data 
#Call:
#  glm(formula = ehrseropos ~ size + agestages + ticks + lymeseropos, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.2576     0.5009  -6.503 7.87e-11 ***
#  sizeMedium         0.2816     0.4016   0.701  0.48320    
#sizeSmall         -0.7962     0.8731  -0.912  0.36179    
#agestagesAdult     1.1979     0.4117   2.910  0.00362 ** 
#  agestagesPuppy     1.1445     1.0223   1.119  0.26293    
#agestagesSenior    2.1917     0.7487   2.927  0.00342 ** 
#  ticksY             0.3924     0.4079   0.962  0.33606    
#lymeseropos1       2.5280     0.3932   6.430 1.28e-10 ***
#  lymeseroposNA     21.1989  1127.5473   0.019  0.98500    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 193.31  on 263  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 211.31

#Number of Fisher Scoring iterations: 17


manstepehr7 <- glm(ehrseropos ~ agestages + ticks + lymeseropos, ShelterTBD, family = binomial )
summary(manstepehr7)
#size falls out with medium having the highest p value

#updated 3.28.26  
#Call:
#  glm(formula = ehrseropos ~ agestages + ticks + lymeseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.0728     0.4190  -7.333 2.24e-13 ***
#agestagesAdult    1.1094     0.4013   2.764  0.00570 ** 
#agestagesPuppy    0.3144     0.7457   0.422  0.67331    
#agestagesSenior   1.9299     0.7183   2.687  0.00721 ** 
#ticksY            0.4497     0.3982   1.129  0.25885    
#lymeseropos       2.4086     0.3751   6.421 1.35e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 265.99  on 240  degrees of freedom
#Residual deviance: 197.00  on 235  degrees of freedom
#(32 observations deleted due to missingness)
#AIC: 209

#Number of Fisher Scoring iterations: 5


#retired new data 
#Call:
#  glm(formula = ehrseropos ~ agestages + ticks + lymeseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -3.0922     0.4212  -7.342 2.11e-13 ***
#  agestagesAdult     1.1524     0.4041   2.852  0.00434 ** 
#  agestagesPuppy     0.3319     0.7483   0.444  0.65739    
#agestagesSenior    1.9432     0.7216   2.693  0.00708 ** 
#  ticksY             0.4237     0.4003   1.059  0.28979    
#lymeseropos1       2.4491     0.3775   6.487 8.73e-11 ***
#  lymeseroposNA     21.3289  1149.5289   0.019  0.98520    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 345.33  on 271  degrees of freedom
#Residual deviance: 195.08  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 209.08

#Number of Fisher Scoring iterations: 17


manstepehr8 <- glm(ehrseropos ~ agestages + lymeseropos, ShelterTBD, family = binomial )
summary(manstepehr8)

#ticks falls out with highest p value 
#upated 3.28.26

#Call:
#  glm(formula = ehrseropos ~ agestages + lymeseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.9697     0.3980  -7.461 8.59e-14 ***
#agestagesAdult    1.0906     0.3956   2.757  0.00584 ** 
#agestagesPuppy    0.3698     0.7469   0.495  0.62053    
#agestagesSenior   1.9542     0.7137   2.738  0.00618 ** 
#lymeseropos       2.4794     0.3716   6.672 2.52e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 268.82  on 241  degrees of freedom
#Residual deviance: 199.14  on 237  degrees of freedom
#(31 observations deleted due to missingness)
#AIC: 209.14

#Number of Fisher Scoring iterations: 5

#retired new data 
#Call:
#  glm(formula = ehrseropos ~ agestages + lymeseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -2.9978     0.4012  -7.472 7.88e-14 ***
#  agestagesAdult     1.1382     0.3988   2.854  0.00431 ** 
#  agestagesPuppy     0.3844     0.7497   0.513  0.60812    
#agestagesSenior    1.9676     0.7176   2.742  0.00611 ** 
#  lymeseropos1       2.5192     0.3740   6.736 1.62e-11 ***
#  lymeseroposNA     21.2010  1149.8972   0.018  0.98529    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 347.54  on 272  degrees of freedom
#Residual deviance: 197.03  on 267  degrees of freedom
#AIC: 209.03

#Number of Fisher Scoring iterations: 17

exp(cbind(OR = coef(manstepehr8), confint(manstepehr8)))
#updated 3.28.26 
#                        OR      2.5 %    97.5 %
#(Intercept)      0.05131796 0.02222381  0.106300
#agestagesAdult   2.97599094 1.39145966  6.611644
#agestagesPuppy   1.44740858 0.28420461  5.758725
#agestagesSenior  7.05826606 1.74734373 29.258022
#lymeseropos     11.93446449 5.90586218 25.533990

#retired 
#                 OR        2.5 %        97.5 %
#  (Intercept)     4.989785e-02 2.146521e-02  1.039370e-01
#agestagesAdult  3.121287e+00 1.451707e+00  6.985401e+00
#agestagesPuppy  1.468775e+00 2.871426e-01  5.881440e+00
#agestagesSenior 7.153452e+00 1.758426e+00  2.987676e+01
#lymeseropos1    1.241901e+01 6.120289e+00  2.670675e+01
#lymeseroposNA   1.612375e+09 1.973343e-17 6.639367e+138
#There were 35 warnings (use warnings() to see them)




#ehrmodels <-list(model11, model12, model13, model14, model15, model16, model17, model18, model19, model20, finmod2, manstepehr1, manstepehr2, manstepehr3, manstepehr4, manstepehr5, manstepehr6, manstepehr7, manstepehr8
#)

#ehrmodels.names <- c('model11', 'model12', 'model13', 'model14', 'model15', 'model16', 'model17', 'model18','model19', 'model20', 'finmod2', 'manstepehr1', 'manstepehr2', 'manstepehr3', 'manstepehr4', 'manstepehr5', 'manstepehr6', 'manstepehr7', 'manstepehr8')

#aictab(cand.set =ehrmodels, modnames = ehrmodels.names)

#Model selection based on AICc:
#new results - i think this is jsut the same as the man step but reassuring that its correct? 
#Model selection based on AICc:
#  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#manstepehr2 15 209.19       0.00   0.25   0.25  -88.66
#finmod2      6 209.34       0.15   0.23   0.47  -98.51
#manstepehr8  6 209.34       0.15   0.23   0.70  -98.51
#manstepehr7  7 209.50       0.31   0.21   0.91  -97.54
#manstepehr6  9 211.99       2.80   0.06   0.97  -96.65
#manstepehr3 14 215.82       6.63   0.01   0.98  -93.09
#model18      3 215.95       6.76   0.01   0.99 -104.93
#manstepehr5 11 215.99       6.80   0.01   1.00  -96.49
#manstepehr4 12 218.12       8.93   0.00   1.00  -96.46
#model20     23 222.75      13.56   0.00   1.00  -86.15
#manstepehr1 23 222.75      13.56   0.00   1.00  -86.15
#model17      3 262.88      53.69   0.00   1.00 -128.40
#model14      4 327.84     118.65   0.00   1.00 -159.84
#model13      3 347.59     138.40   0.00   1.00 -170.75
#model16      2 349.25     140.06   0.00   1.00 -172.60
#model19      3 349.90     140.71   0.00   1.00 -171.91
#model15      2 351.11     141.92   0.00   1.00 -173.53
#model12      3 351.46     142.27   0.00   1.00 -172.68
#model11      9 353.90     144.71   0.00   1.00 -167.61

####RESULT manstepehr2 is the best fit model per the A1C score - adult, senior, lyme are significant predictors to ehrseropos===



### anaplasma models ====================================

#anaplasma
#i think i have to turn ana into a factor too, trying that here 
levels(ShelterTBD$anaseropos)
ShelterTBD$anaseropos <-factor(ShelterTBD$anaseropos)
class(ShelterTBD$anaseropos)
#This worked!!!!!



model21<-glm(anaseropos ~ county, ShelterTBD, family=binomial)
summary(model21)
#new results 
#Call:
#  glm(formula = anaseropos ~ county, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.657e+01  1.697e+03  -0.010    0.992
#countyDelaware  3.313e+01  2.400e+03   0.014    0.989
#countyFranklin  1.726e+01  1.697e+03   0.010    0.992
#countyGallia    1.423e+01  1.697e+03   0.008    0.993
#countyJackson   1.381e+01  1.697e+03   0.008    0.994
#countyLawrence  1.508e+01  1.697e+03   0.009    0.993
#countyRichland  3.023e-08  2.939e+03   0.000    1.000
#countyRoss      1.537e+01  1.697e+03   0.009    0.993
#countyScioto    1.439e+01  1.697e+03   0.008    0.993

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 189.95  on 264  degrees of freedom
#AIC: 207.95

#Number of Fisher Scoring iterations: 15


### RESULT = County is not a significant predictor of anaseropos ====

model22<-glm(anaseropos ~ size, ShelterTBD, family=binomial)
summary(model22)
#new data 
#Call:
#  glm(formula = anaseropos ~ size, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.0098     0.2954  -6.805 1.01e-11 ***
#  sizeMedium   -0.5552     0.4710  -1.179  0.23851    
#sizeSmall     1.1343     0.4262   2.661  0.00779 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 199.35  on 270  degrees of freedom
#AIC: 205.35

#Number of Fisher Scoring iterations: 5

###NEW RESULT = Size is a significant predictor of anaseropos ====


#### 
model23<-glm(anaseropos ~ furlength, ShelterTBD, family=binomial)
summary(model23)
#new data 
#Call:
#  glm(formula = anaseropos ~ furlength, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)     -1.7492     0.5417  -3.229  0.00124 **
#  furlengthNA      0.4142     0.7390   0.560  0.57514   
#furlengthshort  -0.2280     0.5793  -0.393  0.69396   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 211.56  on 270  degrees of freedom
#AIC: 217.56

#Number of Fisher Scoring iterations: 4

###NEW RESULT = Fur length is a significant predictor of anaseropos? interceptor? i think that is the long hair? ====


model24<-glm(anaseropos ~ agestages, ShelterTBD, family=binomial)
summary(model24)
#new data 
#Call:
#  glm(formula = anaseropos ~ agestages, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -3.0155     0.4580  -6.584 4.59e-11 ***
#  agestagesAdult   -0.8557     0.8486  -1.008    0.313    
#agestagesPuppy    3.0896     0.5329   5.798 6.72e-09 ***
#  agestagesSenior   0.4506     1.1343   0.397    0.691    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 141.91  on 269  degrees of freedom
#AIC: 149.91

#Number of Fisher Scoring iterations: 6
###### NEW RESULT = Age is a significant predictor of anaseropos (interceptor and puppy???????? ) ====


model25<-glm(anaseropos ~ sex, ShelterTBD, family=binomial)
summary(model25)

#new results 
#Call:
#  glm(formula = anaseropos ~ sex, family = binomial, data = ShelterTBD)#

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.8632     0.2533  -7.355 1.91e-13 ***
#  sexMale      -0.0422     0.3578  -0.118    0.906    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 212.88  on 271  degrees of freedom
#AIC: 216.88

#Number of Fisher Scoring iterations: 4

### RESULT = Sex is  a significant predictor of anaseropos (interceptor is?isthat female? ) ====


model26<-glm(anaseropos ~ ticks, ShelterTBD, family=binomial)
summary(model26)
#new results
#Call:
#  glm(formula = anaseropos ~ ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.6909     0.1894  -8.925   <2e-16 ***
#  ticksY       -1.2536     0.6219  -2.016   0.0438 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.61  on 271  degrees of freedom
#Residual deviance: 207.16  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 211.16

#Number of Fisher Scoring iterations: 5

### new RESULT = Having ticks on exam is  a significant predictor of anaseropos #(intercept is???))====


model27<-glm(anaseropos ~ ehrseropos, ShelterTBD, family=binomial)
summary(model27)
#new results 
#Call:
#  glm(formula = anaseropos ~ ehrseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -22.57    3572.54  -0.006    0.995
#ehrseropos1     19.95    3572.54   0.006    0.996
#ehrseroposNA    45.13    9238.65   0.005    0.996

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.898  on 272  degrees of freedom
#Residual deviance:  29.252  on 270  degrees of freedom
#AIC: 35.252

#Number of Fisher Scoring iterations: 21

model28<-glm(anaseropos ~ lymeseropos, ShelterTBD, family=binomial)
summary(model28)
#New results 
#Call:
#  glm(formula = anaseropos ~ lymeseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -5.075      1.003  -5.059 4.21e-07 ***
#  lymeseropos1     1.830      1.163   1.573    0.116    
#lymeseroposNA   24.641   1901.059   0.013    0.990    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.898  on 272  degrees of freedom
#Residual deviance:  37.743  on 270  degrees of freedom
#AIC: 43.743

#Number of Fisher Scoring iterations: 18

model29<-glm(anaseropos ~ rickifa, ShelterTBD, family=binomial)
summary(model29)
#new results 
#Call:
#  glm(formula = anaseropos ~ rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.91910    0.20609  -9.312   <2e-16 ***
#  rickifa1     0.05835    0.45558   0.128    0.898    
#rickifaNA    0.53280    0.81699   0.652    0.514    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.90  on 272  degrees of freedom
#Residual deviance: 212.51  on 270  degrees of freedom
#AIC: 218.51

#Number of Fisher Scoring iterations: 4

### new results RESULT = rickifa is a not significant predictor of anaseropos but interceptor is? ====


model30 <- glm(anaseropos ~ county + size + furlength + agestages + sex + ticks + lymeseropos + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(model30)

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ county + size + furlength + agestages + 
#        sex + ticks + lymeseropos + lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -3.844e+01  3.515e+04  -0.001    0.999
#countyFranklin   3.515e-01  5.903e+04   0.000    1.000
#countyGallia     1.686e+01  3.408e+04   0.000    1.000
#countyJackson   -1.460e+00  3.450e+04   0.000    1.000
#countyLawrence  -1.585e+00  3.515e+04   0.000    1.000
#countyRichland  -6.874e-07  5.903e+04   0.000    1.000
#countyRoss       1.802e+01  3.408e+04   0.001    1.000
#countyScioto     1.698e+01  3.408e+04   0.000    1.000
#sizeMedium      -9.456e-01  1.506e+00  -0.628    0.530
#sizeSmall       -1.323e+01  8.302e+03  -0.002    0.999
#furlengthshort   1.647e+01  8.615e+03   0.002    0.998
#agestagesAdult  -7.049e-01  1.528e+00  -0.461    0.645
#agestagesPuppy  -1.335e+01  9.369e+03  -0.001    0.999
#agestagesSenior -1.775e+01  1.237e+04  -0.001    0.999
#sexMale          1.108e-01  1.561e+00   0.071    0.943
#ticksY           1.611e+00  1.462e+00   1.101    0.271
#lymeseropos      8.858e-01  1.457e+00   0.608    0.543
#rickifa          1.644e+00  1.519e+00   1.082    0.279#

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 31.590  on 214  degrees of freedom
#Residual deviance: 20.877  on 197  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 56.877

#Number of Fisher Scoring iterations: 21


#retired new results scroll down for old results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -4.075e+01  5.772e+04  -0.001    0.999
#countyDelaware   6.003e+00  8.216e+04   0.000    1.000
#countyFranklin   1.770e+00  6.154e+04   0.000    1.000
#countyGallia     1.742e+01  5.619e+04   0.000    1.000
#countyJackson   -2.415e+00  5.684e+04   0.000    1.000
#countyLawrence  -1.633e+00  5.686e+04   0.000    1.000
#countyRichland   3.914e-06  9.732e+04   0.000    1.000
#countyRoss       1.837e+01  5.619e+04   0.000    1.000
#countyScioto     1.739e+01  5.619e+04   0.000    1.000
#sizeMedium      -8.096e-01  1.504e+00  -0.538    0.590
#sizeSmall       -1.732e+01  7.976e+03  -0.002    0.998
#furlengthNA      1.888e+01  1.320e+04   0.001    0.999
#furlengthshort   1.780e+01  1.320e+04   0.001    0.999
#agestagesAdult  -1.147e+00  1.556e+00  -0.737    0.461
#agestagesPuppy  -1.309e+01  1.024e+04  -0.001    0.999
#agestagesSenior  1.210e+00  2.066e+00   0.586    0.558
#sexMale          5.362e-01  1.360e+00   0.394    0.693
#ticksY           1.172e+00  1.433e+00   0.818    0.414
#lymeseropos1     1.142e+00  1.433e+00   0.797    0.425
#lymeseroposNA    8.873e+01  2.072e+04   0.004    0.997
#rickifa1         2.076e+00  1.487e+00   1.396    0.163
#rickifaNA       -1.700e+01  9.335e+03  -0.002    0.999

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.61  on 271  degrees of freedom
#Residual deviance:  23.88  on 250  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 67.88

#Number of Fisher Scoring iterations: 22



manstepana1 <- glm(anaseropos ~ county + size + furlength + agestages + sex + ticks + lymeseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana1)

#######all of the ana models say this warning : Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred 

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ county + size + furlength + agestages + 
 #       sex + ticks + lymeseropos + ehrseropos + rickifa, family = binomial, 
  #    data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -8.487e+01  2.569e+05   0.000    1.000
#countyFranklin   5.874e+01  4.367e+05   0.000    1.000
#countyGallia    -7.279e+01  2.534e+05   0.000    1.000
#countyJackson   -1.128e+02  2.553e+05   0.000    1.000
#countyLawrence  -1.136e+02  2.567e+05   0.000    1.000
#countyRichland  -2.643e-06  4.362e+05   0.000    1.000
#countyRoss      -3.204e+01  2.526e+05   0.000    1.000
#countyScioto    -5.376e+01  2.529e+05   0.000    1.000
#sizeMedium      -4.026e+01  1.567e+04  -0.003    0.998
#sizeSmall        1.362e+00  1.560e+05   0.000    1.000
#furlengthshort   3.982e+01  4.778e+04   0.001    0.999
#agestagesAdult  -2.008e+01  8.820e+03  -0.002    0.998
#agestagesPuppy   5.240e+01  1.744e+05   0.000    1.000
#agestagesSenior -2.297e+01  1.144e+05   0.000    1.000
#sexMale          3.856e+01  1.436e+04   0.003    0.998
#ticksY          -1.835e+01  8.820e+03  -0.002    0.998
#lymeseropos     -6.028e+01  2.190e+04  -0.003    0.998
#ehrseropos       1.180e+02  3.209e+04   0.004    0.997
#rickifa          4.026e+01  1.567e+04   0.003    0.998#

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 31.5901  on 214  degrees of freedom
#Residual deviance:  8.2353  on 196  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 46.235

#Number of Fisher Scoring iterations: 25

#retired new results 
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)
#(Intercept)     -8.206e+01  2.585e+05   0.000    1.000
#countyDelaware  -9.907e+01  4.433e+05   0.000    1.000
#countyFranklin  -1.741e+02  5.328e+05   0.000    1.000
#countyGallia    -7.536e+01  2.547e+05   0.000    1.000
#countyJackson   -1.316e+02  2.550e+05  -0.001    1.000
#countyLawrence  -1.144e+02  2.557e+05   0.000    1.000
#countyRichland   4.677e-07  4.362e+05   0.000    1.000
#countyRoss      -3.254e+01  2.529e+05   0.000    1.000
#countyScioto    -5.529e+01  2.533e+05   0.000    1.000
#izeMedium      -4.232e+01  2.261e+04  -0.002    0.999
#sizeSmall       -2.556e+01  5.084e+04  -0.001    1.000
#furlengthNA      4.054e+01  4.634e+04   0.001    0.999
#furlengthshort   3.698e+01  5.324e+04   0.001    0.999
#agestagesAdult  -1.966e+01  7.312e+03  -0.003    0.998
#agestagesPuppy   1.457e+01  2.617e+05   0.000    1.000
#agestagesSenior -1.422e+00  2.007e+04   0.000    1.000
#sexMale          3.818e+01  1.402e+04   0.003    0.998
#ticksY          -1.793e+01  7.312e+03  -0.002    0.998
#lymeseropos1    -6.093e+01  2.378e+04  -0.003    0.998
#lymeseroposNA    2.187e+02  6.539e+04   0.003    0.997
#ehrseropos1      1.192e+02  2.962e+04   0.004    0.997
#ehrseroposNA            NA         NA      NA       NA
#rickifa1         4.232e+01  2.261e+04   0.002    0.999
#rickifaNA        6.179e+01  1.112e+05   0.001    1.000

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.6143  on 271  degrees of freedom
#Residual deviance:   8.2353  on 249  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 54.235

#Number of Fisher Scoring iterations: 25

manstepana2 <- glm(anaseropos ~ size + furlength + agestages + sex + ticks + lymeseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana2)

#County fell out with all having the highest p value, other variables were also at 1 but the
#other variables in the category were lower

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ size + furlength + agestages + sex + 
#        ticks + lymeseropos + ehrseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)
#(Intercept)       -41.5711  8509.9132  -0.005    0.996
#sizeMedium         -1.8239     2.0851  -0.875    0.382
#sizeSmall           0.1673 15950.7041   0.000    1.000
#furlengthshort     18.8650  7769.4674   0.002    0.998
#agestagesAdult     -1.2459     1.6532  -0.754    0.451
#agestagesPuppy     -1.0405 18559.3293   0.000    1.000
#agestagesSenior   -19.6161 11838.4880  -0.002    0.999
#sexMale             1.2803     2.0404   0.627    0.530
#ticksY              1.3926     1.6719   0.833    0.405
#lymeseropos        -2.6391     2.3339  -1.131    0.258
#ehrseropos         20.9316  3471.8859   0.006    0.995
#rickifa             1.9519     1.6422   1.189    0.235

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 31.590  on 214  degrees of freedom
#Residual deviance: 15.901  on 203  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 39.901

#Number of Fisher Scoring iterations: 21


#retired new results 
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)
#(Intercept)       -43.2169  9312.5741  -0.005    0.996
#sizeMedium         -1.9689     1.6803  -1.172    0.241
#sizeSmall         -19.3348  9668.7614  -0.002    0.998
#furlengthNA        20.5614  7889.3200   0.003    0.998
#furlengthshort     18.8131  7889.3195   0.002    0.998
#agestagesAdult     -1.5000     1.4864  -1.009    0.313
#agestagesPuppy      0.7337 13645.5341   0.000    1.000
#agestagesSenior    -0.5774     2.1238  -0.272    0.786
#sexMale             1.9042     2.2215   0.857    0.391
#ticksY              1.0533     1.5054   0.700    0.484
#lymeseropos1       -2.7560     2.2346  -1.233    0.217
#lymeseroposNA      81.9308 17695.4387   0.005    0.996
#ehrseropos1        21.9978  4947.9966   0.004    0.996
#ehrseroposNA            NA         NA      NA       NA
#rickifa1            2.6408     1.6191   1.631    0.103
#rickifaNA         -16.4572 24321.1655  -0.001    0.999

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.61  on 271  degrees of freedom
#Residual deviance:  18.90  on 257  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 48.9

#Number of Fisher Scoring iterations: 22

manstepana3 <- glm(anaseropos ~ size + agestages + sex + ticks + lymeseropos + ehrseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana3)

#size small and puppy were the highest p value, however, other variables
#in the categories have lower p values so I dropped fur length
#updated 3.28.26 
#Call:
#  glm(formula = anaseropos ~ size + agestages + sex + ticks + lymeseropos + 
 #       ehrseropos + rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)  
#(Intercept)      -22.6736  3173.1127  -0.007   0.9943  
#sizeMedium        -2.2004     1.8181  -1.210   0.2262  
#sizeSmall        -18.5685  8056.0277  -0.002   0.9982  
#agestagesAdult    -1.6114     1.4075  -1.145   0.2522  
#agestagesPuppy     1.4353  9905.8738   0.000   0.9999  
#agestagesSenior    0.4412     1.6490   0.268   0.7891  
#sexMale            0.8390     1.4491   0.579   0.5626  
#ticksY             0.8739     1.4140   0.618   0.5365  
#lymeseropos       -1.7624     1.7380  -1.014   0.3106  
#ehrseropos        20.5582  3173.1125   0.006   0.9948  
#rickifa            2.4918     1.5081   1.652   0.0985 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.449  on 232  degrees of freedom
#Residual deviance: 20.300  on 222  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 42.3

#Number of Fisher Scoring iterations: 21



#new results Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)  
#(Intercept)       -23.6362  5113.0483  -0.005   0.9963  
#sizeMedium         -2.2004     1.8181  -1.210   0.2262  
#sizeSmall         -18.8844  9796.5899  -0.002   0.9985  
#agestagesAdult     -1.6114     1.4075  -1.145   0.2522  
#agestagesPuppy      1.3891 13845.8924   0.000   0.9999  
#agestagesSenior     0.4412     1.6490   0.268   0.7891  
#sexMale             0.8390     1.4491   0.579   0.5626  
#ticksY              0.8739     1.4140   0.618   0.5365  
#lymeseropos1       -1.7624     1.7380  -1.014   0.3106  
#lymeseroposNA      75.6167 19332.7902   0.004   0.9969  
#ehrseropos1        21.5208  5113.0482   0.004   0.9966  
#ehrseroposNA            NA         NA      NA       NA  
#rickifa1            2.4918     1.5081   1.652   0.0985 .
#rickifaNA         -15.4488  9763.7227  -0.002   0.9987  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.61  on 271  degrees of freedom
#Residual deviance:  20.30  on 259  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 46.3

#Number of Fisher Scoring iterations: 22



manstepana4 <- glm(anaseropos ~ size + agestages + sex + ticks + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana4)

#size small and puppy were the highest p value, however, other variables
#in the categories have lower p values so I dropped ehrseropos

#updated 3.282.6
#:
#  glm(formula = anaseropos ~ size + agestages + sex + ticks + lymeseropos + 
 #       rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       -5.4514     1.7283  -3.154  0.00161 **
#sizeMedium        -1.1947     1.3791  -0.866  0.38634   
#sizeSmall        -18.0082  4411.7908  -0.004  0.99674   
#agestagesAdult    -0.8874     1.3445  -0.660  0.50922   
#agestagesPuppy   -14.1774  5074.8126  -0.003  0.99777   
#agestagesSenior    1.2354     1.4903   0.829  0.40711   
#sexMale            0.5135     1.2889   0.398  0.69032   
#ticksY             0.7890     1.2420   0.635  0.52527   
#lymeseropos        1.1152     1.3027   0.856  0.39194   
#rickifa            1.8945     1.2592   1.504  0.13247   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.449  on 232  degrees of freedom
#Residual deviance: 29.581  on 223  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 49.581

#Number of Fisher Scoring iterations: 20


#retired
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)   
#(Intercept)        -5.4450     1.7273  -3.152  0.00162 **
#  sizeMedium         -1.1950     1.3779  -0.867  0.38580   
#sizeSmall         -18.3542  5244.2435  -0.003  0.99721   
#agestagesAdult     -0.8830     1.3433  -0.657  0.51099   
#agestagesPuppy    -14.7941  6919.9967  -0.002  0.99829   
#agestagesSenior     1.2378     1.4899   0.831  0.40607   
#sexMale             0.5084     1.2882   0.395  0.69311   
#ticksY              0.7875     1.2407   0.635  0.52559   
#lymeseropos1        1.1187     1.3027   0.859  0.39048   
#lymeseroposNA      72.3422 12256.5192   0.006  0.99529   
#rickifa1            1.8881     1.2598   1.499  0.13393   
#rickifaNA         -15.3734  5887.0652  -0.003  0.99792   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.61  on 271  degrees of freedom
#Residual deviance:  29.57  on 260  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 53.57

#Number of Fisher Scoring iterations: 21


manstepana5 <- glm(anaseropos ~ size + agestages + ticks + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana5)

#size small and puppy were the highest p value, however, other variables
#in the categories have lower p values so I dropped sex

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ size + agestages + ticks + lymeseropos + 
#        rickifa, family = binomial, data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.0993     1.4286  -3.569 0.000358 ***
#sizeMedium        -1.2602     1.3694  -0.920 0.357439    
#sizeSmall        -18.0645  4451.3899  -0.004 0.996762    
#agestagesAdult    -1.0226     1.2931  -0.791 0.429057    
#agestagesPuppy   -14.0025  5129.6507  -0.003 0.997822    
#agestagesSenior    1.3667     1.4296   0.956 0.339062    
#ticksY             0.8305     1.2287   0.676 0.499056    
#lymeseropos        1.1523     1.2891   0.894 0.371391    
#rickifa            1.8636     1.2531   1.487 0.136972    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.449  on 232  degrees of freedom
#Residual deviance: 29.747  on 224  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 47.747

#Number of Fisher Scoring iterations: 20

#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        -5.0958     1.4271  -3.571 0.000356 ***
#  sizeMedium         -1.2595     1.3679  -0.921 0.357154    
#sizeSmall         -18.4018  5271.1179  -0.003 0.997215    
#agestagesAdult     -1.0150     1.2935  -0.785 0.432612    
#agestagesPuppy    -14.6131  6966.5303  -0.002 0.998326    
#agestagesSenior     1.3677     1.4292   0.957 0.338600    
#ticksY              0.8279     1.2273   0.675 0.499948    
#lymeseropos1        1.1562     1.2891   0.897 0.369737    
#lymeseroposNA      72.3632 12332.7674   0.006 0.995318    
#rickifa1            1.8559     1.2535   1.481 0.138729    
#rickifaNA         -15.3604  5895.3904  -0.003 0.997921    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.614  on 271  degrees of freedom
#Residual deviance:  29.733  on 261  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 51.733

#Number of Fisher Scoring iterations: 21



manstepana6 <- glm(anaseropos ~ size + agestages + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana6)

#size small and puppy were the highest p value, however, other variables
#in the categories have lower p values so I dropped ticks 

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ size + agestages + lymeseropos + rickifa, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.0208     1.4193  -3.537 0.000404 ***
#sizeMedium        -0.9197     1.2394  -0.742 0.458029    
#sizeSmall        -16.5726  2731.7783  -0.006 0.995160    
#agestagesAdult    -1.1608     1.2762  -0.910 0.363055    
#agestagesPuppy   -13.4232  3094.0446  -0.004 0.996538    
#agestagesSenior    1.1521     1.3851   0.832 0.405545    
#lymeseropos        1.3283     1.2562   1.057 0.290330    
#rickifa            2.0627     1.2197   1.691 0.090821 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.483  on 233  degrees of freedom
#Residual deviance: 30.208  on 226  degrees of freedom
#(39 observations deleted due to missingness)
#AIC: 46.208

#Number of Fisher Scoring iterations: 19

#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        -5.0168     1.4176  -3.539 0.000402 ***
#  sizeMedium         -0.9212     1.2387  -0.744 0.457058    
#sizeSmall         -17.9093  5325.0640  -0.003 0.997317    
#agestagesAdult     -1.1519     1.2769  -0.902 0.367026    
#agestagesPuppy    -15.0413  6966.0217  -0.002 0.998277    
#agestagesSenior     1.1542     1.3851   0.833 0.404707    
#lymeseropos1        1.3318     1.2560   1.060 0.288999    
#lymeseroposNA      72.1217 12385.1412   0.006 0.995354    
#rickifa1            2.0533     1.2210   1.682 0.092644 .  
#rickifaNA         -14.9239  5930.9651  -0.003 0.997992    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.898  on 272  degrees of freedom
#Residual deviance:  30.192  on 263  degrees of freedom
#AIC: 50.192

#Number of Fisher Scoring iterations: 21


manstepana7 <- glm(anaseropos ~ agestages + lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana7)

#puppy was the highest p value but other var in the cat had lower p value than in size, dropped size 

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ agestages + lymeseropos + rickifa, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.4670     1.2977  -4.213 2.52e-05 ***
#agestagesAdult    -1.0717     1.2699  -0.844   0.3987    
#agestagesPuppy   -14.5192  2223.8459  -0.007   0.9948    
#agestagesSenior    0.7126     1.3514   0.527   0.5980    
#lymeseropos        1.4021     1.2006   1.168   0.2429    
#rickifa            2.1360     1.2172   1.755   0.0793 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.483  on 233  degrees of freedom
#Residual deviance: 31.623  on 228  degrees of freedom
#(39 observations deleted due to missingness)
#AIC: 43.623

#Number of Fisher Scoring iterations: 18


#retired new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.4643     1.2964  -4.215  2.5e-05 ***
#  agestagesAdult    -1.0649     1.2706  -0.838   0.4020    
#agestagesPuppy   -16.1266  4963.5949  -0.003   0.9974    
#agestagesSenior    0.7141     1.3515   0.528   0.5972    
#lymeseropos1       1.4046     1.2007   1.170   0.2421    
#lymeseroposNA     54.4457  7620.3221   0.007   0.9943    
#rickifa1           2.1290     1.2181   1.748   0.0805 .  
#rickifaNA        -14.4630  3944.6984  -0.004   0.9971    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.898  on 272  degrees of freedom
#Residual deviance:  31.611  on 265  degrees of freedom
#AIC: 47.611

#Number of Fisher Scoring iterations: 20

manstepana8 <- glm(anaseropos ~ lymeseropos + rickifa, ShelterTBD, family = binomial )
summary(manstepana8)

#updated 3.28.26
#Call:
#  glm(formula = anaseropos ~ lymeseropos + rickifa, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -5.824      1.239  -4.699 2.61e-06 ***
#lymeseropos    1.354      1.193   1.135   0.2565    
#rickifa        2.223      1.189   1.869   0.0617 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 40.483  on 233  degrees of freedom
#Residual deviance: 33.402  on 231  degrees of freedom
#(39 observations deleted due to missingness)
#AIC: 39.402

#Number of Fisher Scoring iterations: 8

#retired new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -5.816      1.238  -4.700  2.6e-06 ***
#  lymeseropos1     1.360      1.194   1.139   0.2546    
#lymeseroposNA   38.689   5942.859   0.007   0.9948    
#rickifa1         2.210      1.190   1.857   0.0634 .  
#rickifaNA      -14.450   4110.245  -0.004   0.9972    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 212.898  on 272  degrees of freedom
#Residual deviance:  33.379  on 268  degrees of freedom
#AIC: 43.379

#Number of Fisher Scoring iterations: 20


#this didn't work with new model and since nothing from the backward step was sig i didn't waste time trying to fix this code 
#anamodels <-list(model21, model22, model23, model24, model25, model26, model27, model28, model29, model30, manstepana1, manstepana2, manstepana3, manstepana4, manstepana5, manstepana6, manstepana7, manstepana8)

#anamodels.names <- c('model21', 'model22', 'model23', 'model24', 'model25', 'model26', 'model27', 'model28', 'model29', 'model30', 'manstepana1', 'manstepana2', 'manstepana3', 'manstepana4', 'manstepana5', 'manstepana6', 'manstepana7', 'manstepana8')

#aictab(cand.set =anamodels, modnames = anamodels.names)

#Model selection based on AICc:
  
#  K  AICc Delta_AICc AICcWt Cum.Wt     LL
#model27      2 33.30       0.00   0.64   0.64 -14.63
#model23      2 35.12       1.81   0.26   0.89 -15.53
#model29      2 38.93       5.63   0.04   0.93 -17.44
#manstepana8  3 39.48       6.18   0.03   0.96 -16.69
#manstepana2 12 41.46       8.16   0.01   0.97  -7.95
#model28      2 41.79       8.49   0.01   0.98 -18.87
#model26      2 43.40      10.10   0.00   0.98 -19.67
#manstepana3 11 43.50      10.20   0.00   0.99 -10.15
#model25      2 43.82      10.52   0.00   0.99 -19.89
#manstepana7  6 43.98      10.68   0.00   1.00 -15.81
#model22      3 44.35      11.05   0.00   1.00 -19.12
#model24      4 46.28      12.98   0.00   1.00 -19.06
#manstepana6  8 46.84      13.53   0.00   1.00 -15.10
#manstepana5  9 48.54      15.24   0.00   1.00 -14.87
#manstepana1 19 50.17      16.87   0.00   1.00  -4.12
#manstepana4 10 50.57      17.26   0.00   1.00 -14.79
#model21      8 53.03      19.73   0.00   1.00 -18.21
#model30     18 60.40      27.09   0.00   1.00 -10.44


#### Model 27 ehr is not sig model is the best fit model 

###RIck IFA models ==============================

#rickifa

#i think i have to turn ana into a factor too, trying that here 
levels(ShelterTBD$rickifa)
ShelterTBD$rickifa <-factor(ShelterTBD$rickifa)
class(ShelterTBD$rickifa)
#This worked!!!!!




model31<-glm(rickifa ~ county, ShelterTBD, family=binomial)
summary(model31)
#new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.657e+01  1.697e+03  -0.010    0.992
#countyDelaware -1.118e-07  2.400e+03   0.000    1.000
#countyFranklin  1.587e+01  1.697e+03   0.009    0.993
#countyGallia    1.539e+01  1.697e+03   0.009    0.993
#countyJackson   1.523e+01  1.697e+03   0.009    0.993
#countyLawrence  1.567e+01  1.697e+03   0.009    0.993
#countyRichland -1.118e-07  2.939e+03   0.000    1.000
#countyRoss      1.429e+01  1.697e+03   0.008    0.993
#countyScioto    1.584e+01  1.697e+03   0.009    0.993

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 280.95  on 264  degrees of freedom
#AIC: 298.95

#Number of Fisher Scoring iterations: 15

#old data 
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8892  -0.6681  -0.6141  -0.4419   2.1794  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.657e+01  1.697e+03  -0.010    0.992
#countyDelaware  6.798e-08  2.400e+03   0.000    1.000
#countyFranklin  6.814e-08  2.400e+03   0.000    1.000
#countyGallia    1.518e+01  1.697e+03   0.009    0.993
#countyJackson   1.499e+01  1.697e+03   0.009    0.993
#countyLawrence  1.535e+01  1.697e+03   0.009    0.993
#countyRichland  6.806e-08  2.939e+03   0.000    1.000
#countyRoss      1.429e+01  1.697e+03   0.008    0.993
#countyScioto    1.584e+01  1.697e+03   0.009    0.993

#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 261.54  on 262  degrees of freedom
#Residual deviance: 249.93  on 254  degrees of freedom
#(11 observations deleted due to missingness)
#AIC: 267.93
#Number of Fisher Scoring iterations: 15

### RESULT = County is not a significant predictor of rickifa ====


model32<-glm(rickifa ~ size, ShelterTBD, family=binomial)
summary(model32)
#new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.12300    0.22155  -5.069    4e-07 ***
#  sizeMedium  -0.23014    0.32218  -0.714    0.475    
#sizeSmall   -0.05565    0.39757  -0.140    0.889    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 291.99  on 270  degrees of freedom
#AIC: 297.99

#Number of Fisher Scoring iterations: 4

### NEW results size is NOT a signficant factor except for intercept??? ====  

model33<-glm(rickifa ~ furlength, ShelterTBD, family=binomial)
summary(model33)

#new results
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)    -1.252763   0.462910  -2.706   0.0068 **
#  furlengthNA     0.365460   0.644954   0.567   0.5710   
#furlengthshort -0.008708   0.490381  -0.018   0.9858   
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 291.93  on 270  degrees of freedom
#AIC: 297.93

#Number of Fisher Scoring iterations: 4


model34<-glm(rickifa ~ agestages, ShelterTBD, family=binomial)
summary(model34)
#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.47018    0.24798  -5.929 3.06e-09 ***
#  agestagesAdult   0.39859    0.33940   1.174   0.2402    
#agestagesPuppy  -0.01143    0.42921  -0.027   0.9788    
#agestagesSenior  1.47018    0.58924   2.495   0.0126 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 285.55  on 269  degrees of freedom
#AIC: 293.55

#Number of Fisher Scoring iterations: 4

### RESULT = Senior age is a significant predictor of rickifa ====

model35<-glm(rickifa ~ sex, ShelterTBD, family=binomial)
summary(model35)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.3312     0.2125  -6.265 3.72e-10 ***
#  sexMale       0.2036     0.2900   0.702    0.483    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)#

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 292.03  on 271  degrees of freedom
#AIC: 296.03

#Number of Fisher Scoring iterations: 4

### RESULT = Sex is not a significant predictor of rickifa (intercept?) ====

model36<-glm(rickifa ~ ticks, ShelterTBD, family=binomial)
summary(model36)

#new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.7272     0.1918  -9.004  < 2e-16 ***
#  ticksY        1.7272     0.3217   5.370 7.89e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 263.10  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 267.1

#Number of Fisher Scoring iterations: 3

### RESULT = Having ticks on exam is a significant predictor of rickifa ====

model37<-glm(rickifa ~ ehrseropos, ShelterTBD, family=binomial)
summary(model37)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.43508    0.18808  -7.630 2.34e-14 ***
#  ehrseropos1   0.84202    0.33062   2.547   0.0109 *  
#  ehrseroposNA -0.03125    0.49041  -0.064   0.9492    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 285.90  on 270  degrees of freedom
#AIC: 291.9

#Number of Fisher Scoring iterations: 4

### RESULT = Ehrseropos is a significant predictor of rickifa ====

model38<-glm(rickifa ~ lymeseropos, ShelterTBD, family=binomial)
summary(model38)
#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -1.51551    0.20508  -7.390 1.47e-13 ***
#  lymeseropos1   0.84105    0.31299   2.687  0.00721 ** 
#  lymeseroposNA  0.04917    0.49718   0.099  0.92122    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 285.03  on 270  degrees of freedom
#AIC: 291.03

#Number of Fisher Scoring iterations: 4

### RESULT = Lymeseropos is a significant predictor of rickifa ====

model39<-glm(rickifa ~ anaseropos, ShelterTBD, family=binomial)
summary(model39)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -1.2446     0.1559  -7.984 1.42e-15 ***
#  anaseropos1    2.3433     1.1652   2.011   0.0443 *  
#  anaseroposNA  -0.2217     0.4790  -0.463   0.6435    
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.52  on 272  degrees of freedom
#Residual deviance: 287.30  on 270  degrees of freedom
#AIC: 293.3

#Number of Fisher Scoring iterations: 4

### RESULT = Anaseropos is a significant predictor of rickifa ====

model40 <- glm(rickifa ~ county + size + furlength + agestages + sex + ticks + ehrseropos + lymeseropos + anaseropos, ShelterTBD, family = binomial )
summary(model40)

#updated 3.28.26
#Call:
#  glm(formula = rickifa ~ county + size + furlength + agestages + 
#        sex + ticks + ehrseropos + lymeseropos + anaseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.747e+01  1.697e+03  -0.010 0.991787    
#countyFranklin   6.700e-01  2.939e+03   0.000 0.999818    
#countyGallia     1.488e+01  1.697e+03   0.009 0.993004    
#countyJackson    1.484e+01  1.697e+03   0.009 0.993021    
#countyLawrence   1.515e+01  1.697e+03   0.009 0.992877    
#countyRichland  -1.655e-08  2.939e+03   0.000 1.000000    
#countyRoss       1.415e+01  1.697e+03   0.008 0.993347    
#countyScioto     1.549e+01  1.697e+03   0.009 0.992714    
#sizeMedium      -1.994e-01  4.093e-01  -0.487 0.626085    
#sizeSmall       -2.424e+00  1.316e+00  -1.843 0.065378 .  
#furlengthshort   4.293e-01  6.596e-01   0.651 0.515122    
#agestagesAdult   4.934e-01  4.336e-01   1.138 0.255141    
#agestagesPuppy   4.358e-01  1.297e+00   0.336 0.736838    
#agestagesSenior  1.952e+00  7.987e-01   2.444 0.014518 *  
#sexMale         -2.281e-02  3.916e-01  -0.058 0.953559    
#ticksY           1.451e+00  4.165e-01   3.483 0.000495 ***
#ehrseropos      -6.173e-02  4.761e-01  -0.130 0.896822    
#lymeseropos      6.096e-01  4.488e-01   1.358 0.174406    
#anaseropos       1.515e+00  1.327e+00   1.142 0.253506    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 220.60  on 214  degrees of freedom
#Residual deviance: 181.98  on 196  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 219.98

#Number of Fisher Scoring iterations: 15


#reitred new results 
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.711e+01  1.697e+03  -0.010   0.9920    
#countyDelaware  -1.208e-01  2.400e+03   0.000   1.0000    
#countyFranklin   1.602e+01  1.697e+03   0.009   0.9925    
#countyGallia     1.503e+01  1.697e+03   0.009   0.9929    
#countyJackson    1.486e+01  1.697e+03   0.009   0.9930    
#countyLawrence   1.508e+01  1.697e+03   0.009   0.9929    
#countyRichland   1.212e-08  2.939e+03   0.000   1.0000    
#countyRoss       1.395e+01  1.697e+03   0.008   0.9934    
#countyScioto     1.536e+01  1.697e+03   0.009   0.9928    
#sizeMedium      -3.302e-01  3.855e-01  -0.857   0.3917    
#sizeSmall        3.076e-02  5.978e-01   0.051   0.9590    
#furlengthNA     -1.567e-01  8.035e-01  -0.195   0.8454    
#furlengthshort   1.004e-01  5.839e-01   0.172   0.8635    
#agestagesAdult   4.255e-01  4.021e-01   1.058   0.2899    
#agestagesPuppy  -3.465e-01  7.330e-01  -0.473   0.6364    
#agestagesSenior  1.274e+00  7.147e-01   1.783   0.0746 .  
#sexMale          1.960e-02  3.384e-01   0.058   0.9538    
#ticksY           1.743e+00  3.701e-01   4.711 2.47e-06 ***
#  ehrseropos1      5.438e-02  4.518e-01   0.120   0.9042    
#ehrseroposNA     9.821e-01  7.092e-01   1.385   0.1661    
#lymeseropos1     4.499e-01  4.215e-01   1.067   0.2858    
#lymeseroposNA           NA         NA      NA       NA    
#anaseropos1      1.739e+00  1.238e+00   1.404   0.1602    
#anaseroposNA            NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)#

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 241.09  on 250  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 285.09

#Number of Fisher Scoring iterations: 15

### RESULT = Having ticks, senior age is a significant predictor of rickifa ====

mansteprick1 <- glm(rickifa ~ size + furlength + agestages + sex + ticks + ehrseropos + lymeseropos + anaseropos, ShelterTBD, family = binomial )
summary(mansteprick1)

#county fell out with the highest p value 

#updated 3.28.26
#Call:
#  glm(formula = rickifa ~ size + furlength + agestages + sex + 
#        ticks + ehrseropos + lymeseropos + anaseropos, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.71182    0.83913  -3.232 0.001231 ** 
#sizeMedium      -0.11824    0.40158  -0.294 0.768435    
#sizeSmall       -2.13479    1.28149  -1.666 0.095741 .  
#furlengthshort   0.48980    0.64173   0.763 0.445320    
#agestagesAdult   0.58205    0.41602   1.399 0.161789    
#agestagesPuppy   0.54466    1.29772   0.420 0.674699    
#agestagesSenior  1.90713    0.77412   2.464 0.013755 *  
#sexMale         -0.02820    0.38298  -0.074 0.941308    
#ticksY           1.49794    0.41053   3.649 0.000263 ***
#ehrseropos       0.07564    0.47018   0.161 0.872189    
#lymeseropos      0.61895    0.43728   1.415 0.156935    
#anaseropos       1.31988    1.30646   1.010 0.312364    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 220.60  on 214  degrees of freedom
#Residual deviance: 187.77  on 203  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 211.77

#Number of Fisher Scoring iterations: 6


#retired new results 
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.425972   0.709251  -3.420 0.000625 ***
#  sizeMedium      -0.283356   0.377164  -0.751 0.452484    
#sizeSmall        0.165514   0.578939   0.286 0.774961    
#furlengthNA      0.005844   0.758250   0.008 0.993851    
#furlengthshort   0.232951   0.552388   0.422 0.673232    
#agestagesAdult   0.505715   0.389740   1.298 0.194435    
#agestagesPuppy  -0.272588   0.713445  -0.382 0.702407    
#agestagesSenior  1.250903   0.688195   1.818 0.069116 .  
#sexMale          0.041930   0.329133   0.127 0.898629    
#ticksY           1.796724   0.364262   4.933 8.12e-07 ***
#  ehrseropos1      0.166939   0.447656   0.373 0.709209    
#ehrseroposNA     0.874081   0.666528   1.311 0.189725    
#lymeseropos1     0.449227   0.412775   1.088 0.276459    
#lymeseroposNA          NA         NA      NA       NA    
#anaseropos1      1.599322   1.230636   1.300 0.193742    
#anaseroposNA           NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 248.90  on 258  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 276.9

# Number of Fisher Scoring iterations: 4

#new model! 
mansteprick2 <- glm(rickifa ~ size + furlength + agestages + ehrseropos + ticks + lymeseropos + anaseropos, ShelterTBD, family = binomial )
summary(mansteprick2)

#new model - sex fell out with highest p value 
#updated 3.28.26 
#Call:
 # glm(formula = rickifa ~ size + furlength + agestages + ehrseropos + 
 #       ticks + lymeseropos + anaseropos, family = binomial, data = ShelterTBD)

#Coefficients:
  #              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.73059    0.79984  -3.414 0.000640 ***
#sizeMedium      -0.11400    0.39742  -0.287 0.774237    
#sizeSmall       -2.13258    1.28077  -1.665 0.095897 .  
#furlengthshort   0.49237    0.64093   0.768 0.442368    
#agestagesAdult   0.58531    0.41372   1.415 0.157143    
#agestagesPuppy   0.54045    1.29639   0.417 0.676761    
#agestagesSenior  1.90186    0.77073   2.468 0.013602 *  
#ehrseropos       0.07283    0.46867   0.155 0.876507    
#ticksY           1.49756    0.41053   3.648 0.000264 ***
#lymeseropos      0.61660    0.43609   1.414 0.157384    
#anaseropos       1.31983    1.30660   1.010 0.312437    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 220.60  on 214  degrees of freedom
#Residual deviance: 187.77  on 204  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 209.77

#Number of Fisher Scoring iterations: 6


#retired 
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.404477   0.688808  -3.491 0.000482 ***
#  sizeMedium      -0.288712   0.374800  -0.770 0.441116    
#sizeSmall        0.157116   0.575337   0.273 0.784787    
#furlengthNA      0.004719   0.758195   0.006 0.995034    
#furlengthshort   0.234995   0.552015   0.426 0.670323    
#agestagesAdult   0.503681   0.389367   1.294 0.195807    
#agestagesPuppy  -0.265512   0.711112  -0.373 0.708869    
#agestagesSenior  1.261293   0.683100   1.846 0.064831 .  
#ehrseropos1      0.166535   0.447605   0.372 0.709850    
#ehrseroposNA     0.869845   0.664971   1.308 0.190841    
#ticksY           1.796134   0.364160   4.932 8.13e-07 ***
#  lymeseropos1     0.455131   0.410154   1.110 0.267146    
#lymeseroposNA          NA         NA      NA       NA    
#anaseropos1      1.603283   1.230547   1.303 0.192608    
#anaseroposNA           NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 248.92  on 259  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 274.92

#Number of Fisher Scoring iterations: 4

mansteprick3 <- glm(rickifa ~ size + furlength + agestages + ticks + lymeseropos + anaseropos, ShelterTBD, family = binomial )
summary(mansteprick3)

#new model- ehrsero fell out 

#updated 3.28.26
#Call:
#  glm(formula = rickifa ~ size + furlength + agestages + ticks + 
#        lymeseropos + anaseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.7397     0.7992  -3.428 0.000608 ***
#sizeMedium       -0.1128     0.3971  -0.284 0.776456    
#sizeSmall        -2.1450     1.2786  -1.678 0.093423 .  
#furlengthshort    0.5000     0.6405   0.781 0.434970    
#agestagesAdult    0.5998     0.4030   1.488 0.136624    
#agestagesPuppy    0.5473     1.2949   0.423 0.672539    
#agestagesSenior   1.9198     0.7619   2.520 0.011742 *  
#ticksY            1.5010     0.4100   3.661 0.000251 ***
#lymeseropos       0.6452     0.3949   1.634 0.102291    
#anaseropos        1.3677     1.2699   1.077 0.281448    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 220.6  on 214  degrees of freedom
#Residual deviance: 187.8  on 205  degrees of freedom
#(58 observations deleted due to missingness)
#AIC: 207.8

#Number of Fisher Scoring iterations: 6


#retired 
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.42298    0.68900  -3.517 0.000437 ***
#  sizeMedium      -0.28022    0.37364  -0.750 0.453265    
#sizeSmall        0.14980    0.57445   0.261 0.794265    
#furlengthNA      0.04034    0.75434   0.053 0.957349    
#furlengthshort   0.24878    0.55334   0.450 0.653002    
#agestagesAdult   0.53375    0.38069   1.402 0.160894    
#agestagesPuppy  -0.25185    0.71004  -0.355 0.722814    
#agestagesSenior  1.31234    0.66883   1.962 0.049746 *  
#  ticksY           1.80042    0.36416   4.944 7.65e-07 ***
#  lymeseropos1     0.52557    0.36303   1.448 0.147689    
#lymeseroposNA    0.86192    0.66484   1.296 0.194829    
#anaseropos1      1.69024    1.20688   1.401 0.161364    
#anaseroposNA          NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 249.06  on 260  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 273.06

#Number of Fisher Scoring iterations: 4


mansteprick4 <- glm(rickifa ~ size + agestages + ticks + lymeseropos + anaseropos, ShelterTBD, family = binomial )
summary(mansteprick4)

#medium and puppy had the highest p values but other var in those cat were sig so fur length fell out with the nest highest p value 

#udpated 3.28.26
#all:
#  glm(formula = rickifa ~ size + agestages + ticks + lymeseropos + 
#        anaseropos, family = binomial, data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.3415     0.4302  -5.443 5.25e-08 ***
#sizeMedium       -0.1005     0.3885  -0.259 0.795825    
#sizeSmall        -2.7052     1.2870  -2.102 0.035558 *  
#agestagesAdult    0.6198     0.3916   1.583 0.113498    
#agestagesPuppy    0.4477     1.2933   0.346 0.729224    
#agestagesSenior   1.9693     0.7361   2.675 0.007470 ** 
#ticksY            1.5181     0.3958   3.835 0.000126 ***
#lymeseropos       0.6609     0.3806   1.736 0.082496 .  
#anaseropos        1.6401     1.2085   1.357 0.174731    
#---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 237.02  on 232  degrees of freedom
#Residual deviance: 195.21  on 224  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 213.21

#Number of Fisher Scoring iterations: 6

#retired new results 

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.1675     0.3953  -5.484 4.16e-08 ***
#  sizeMedium       -0.2892     0.3719  -0.778   0.4368    
#sizeSmall         0.1257     0.5711   0.220   0.8258    
#agestagesAdult    0.5090     0.3764   1.352   0.1763    
#agestagesPuppy   -0.3458     0.6888  -0.502   0.6157    
#agestagesSenior   1.2736     0.6645   1.917   0.0553 .  
#ticksY            1.7830     0.3597   4.957 7.17e-07 ***
#  lymeseropos1      0.4951     0.3577   1.384   0.1663    
#lymeseroposNA     0.8932     0.6634   1.347   0.1781    
#anaseropos1       1.6824     1.2023   1.399   0.1617    
#anaseroposNA          NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 249.34  on 262  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 269.34

# Number of Fisher Scoring iterations: 4

#updated 3.28.26
mansteprick5 <- glm(rickifa ~ size + agestages + ticks + lymeseropos, ShelterTBD, family = binomial )
summary(mansteprick5)
#Call:
#  glm(formula = rickifa ~ size + agestages + ticks + lymeseropos, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.2974     0.4247  -5.409 6.34e-08 ***
#sizeMedium       -0.1357     0.3856  -0.352  0.72487    
#sizeSmall        -2.8286     1.2835  -2.204  0.02754 *  
#agestagesAdult    0.5830     0.3878   1.503  0.13272    
#agestagesPuppy    0.4555     1.2904   0.353  0.72407    
#agestagesSenior   2.0407     0.7197   2.836  0.00457 ** 
#ticksY            1.5549     0.3930   3.956 7.62e-05 ***
#lymeseropos       0.7052     0.3759   1.876  0.06070 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 237.02  on 232  degrees of freedom
#Residual deviance: 197.34  on 225  degrees of freedom
#(40 observations deleted due to missingness)
#AIC: 213.34

#Number of Fisher Scoring iterations: 6


#retired new model 5
#mansteprick5 <- glm(rickifa ~ agestages + ticks + lymeseropos + anaseropos, ShelterTBD, family = binomial )
#summary(mansteprick5)

#size fell out this time instead of anaseropos - small was not sig in this model with the new data included 

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.3091     0.3393  -6.807 1.00e-11 ***
#  agestagesAdult    0.5359     0.3733   1.436   0.1511    
#agestagesPuppy   -0.1236     0.5645  -0.219   0.8267    
#agestagesSenior   1.3595     0.6613   2.056   0.0398 *  
#  ticksY            1.7396     0.3507   4.960 7.05e-07 ***
#  lymeseropos1      0.5352     0.3525   1.518   0.1290    
#lymeseroposNA     0.8520     0.6576   1.296   0.1951    
#anaseropos1       1.7086     1.2000   1.424   0.1545    
#anaseroposNA          NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 250.21  on 264  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 266.21

#Number of Fisher Scoring iterations: 4

#updated 3.29.26
mansteprick6 <- glm(rickifa ~ size + agestages + ticks, ShelterTBD, family = binomial )
summary(mansteprick6)

#retired new model 6 - anasero falls out 
#mansteprick6 <- glm(rickifa ~ agestages + ticks + lymeseropos, ShelterTBD, family = binomial )
#summary(mansteprick6)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.2849     0.3360  -6.801 1.04e-11 ***
#  agestagesAdult    0.5069     0.3700   1.370   0.1707    
#agestagesPuppy   -0.1608     0.5620  -0.286   0.7748    
#agestagesSenior   1.4428     0.6516   2.214   0.0268 *  
#  ticksY            1.7551     0.3489   5.031 4.89e-07 ***
#  lymeseropos1      0.5901     0.3476   1.698   0.0896 .  
#lymeseroposNA     0.8602     0.6568   1.310   0.1903    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 252.59  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 266.59

#Number of Fisher Scoring iterations: 4


#updated 3.29.26
mansteprick7 <- glm(rickifa ~ agestages + ticks, ShelterTBD, family = binomial )
summary(mansteprick7)

#retired 
#mansteprick7 <- glm(rickifa ~ agestages + ticks, ShelterTBD, family = binomial )
#summary(mansteprick7)

#ran one without lyme since not sig 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -2.02149    0.29506  -6.851 7.32e-12 ***
#  agestagesAdult   0.50831    0.36279   1.401   0.1612    
#agestagesPuppy   0.03518    0.45486   0.077   0.9383    
#agestagesSenior  1.43063    0.63700   2.246   0.0247 *  
#  ticksY           1.73486    0.32852   5.281 1.29e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 292.01  on 271  degrees of freedom
#Residual deviance: 256.86  on 267  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 266.86

#Number of Fisher Scoring iterations: 4

exp(cbind(OR = coef(mansteprick7), confint(mansteprick7)))

# older exp(cbind(OR = coef(mansteprick5), confint(mansteprick5)))
#                  OR       2.5 %     97.5 %
#(Intercept)     0.1005243 0.041431967  0.2207579
#sizeMedium      0.8731016 0.406659188  1.8591716
#sizeSmall       0.0590954 0.002478394  0.5455584
#agestagesAdult  1.7914144 0.844704979  3.8968210
#agestagesPuppy  1.5770306 0.066600170 16.4056902
#agestagesSenior 7.6962491 1.884142474 33.1339472
#ticksY          4.7344410 2.203662935 10.3676680
#lymeseropos     2.0241609 0.966948176  4.2510983



#retired exp(cbind(OR = coef(mansteprick7), confint(mansteprick7)))
#                 OR      2.5 %     97.5 %
#  (Intercept)     0.1324581 0.07171371  0.2291317
#agestagesAdult  1.6624866 0.82015533  3.4235928
#agestagesPuppy  1.0358074 0.41218493  2.4886440
#agestagesSenior 4.1813242 1.18141042 14.8081326
#ticksY          5.6681275 2.99131109 10.8890877

#rickmodels <-list(model31, model32, model33, model34, model35, model36, model37, model38, model39, model40, mansteprick1, mansteprick2, mansteprick3, mansteprick4, mansteprick5, mansteprick6, mansteprick7)#

#rickmodels.names <- c('model31', 'model32', 'model33', 'model34', 'model35', 'model36', 'model37', 'model38', 'model39', 'model40', 'mansteprick1', 'mansteprick2', 'mansteprick3', 'mansteprick4', 'mansteprick5', 'mansteprick6', 'mansteprick7')

#aictab(cand.set =rickmodels, modnames = rickmodels.names)

#new results 
#Model selection based on AICc:
  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#mansteprick5  8 266.76       0.00   0.27   0.27 -125.11
#mansteprick6  7 267.02       0.26   0.23   0.50 -126.30
#mansteprick7  5 267.09       0.33   0.23   0.72 -128.43
#model36       2 267.14       0.39   0.22   0.94 -131.55
#mansteprick4 10 270.18       3.42   0.05   0.99 -124.67
#mansteprick3 12 274.26       7.50   0.01   1.00 -124.53
#mansteprick2 13 276.33       9.57   0.00   1.00 -124.46
#mansteprick1 14 278.54      11.78   0.00   1.00 -124.45
#model40      22 289.16      22.40   0.00   1.00 -120.55
#model38       3 291.12      24.36   0.00   1.00 -142.52
#model37       3 291.99      25.24   0.00   1.00 -142.95
#model39       3 293.39      26.63   0.00   1.00 -143.65
#model34       4 293.70      26.94   0.00   1.00 -142.77
#model35       2 296.07      29.31   0.00   1.00 -146.01
#model33       3 298.02      31.26   0.00   1.00 -145.97
#model32       3 298.08      31.32   0.00   1.00 -145.99
#model31       9 299.64      32.88   0.00   1.00 -140.48

  ###newRESULT #model manstepric5 is the best fit model per the AIC. that model indicated senior and having ticks
  #were sig predictors of rickifapositivity. ====
  
 
#### tbdpos models ==========================================

model41<-glm(tbdpos ~ county, ShelterTBD, family=binomial)
summary(model41)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.657e+01  1.697e+03  -0.010    0.992
#countyDelaware -3.414e-08  2.400e+03   0.000    1.000
#countyFranklin -3.401e-08  2.190e+03   0.000    1.000
#countyGallia    1.639e+01  1.697e+03   0.010    0.992
#countyJackson   1.636e+01  1.697e+03   0.010    0.992
#countyLawrence  1.667e+01  1.697e+03   0.010    0.992
#countyRichland -3.408e-08  2.939e+03   0.000    1.000
#countyRoss      1.550e+01  1.697e+03   0.009    0.993
#countyScioto    1.669e+01  1.697e+03   0.010    0.992

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 373.43  on 272  degrees of freedom
#Residual deviance: 355.11  on 264  degrees of freedom
#AIC: 373.11

#Number of Fisher Scoring iterations: 15


### RESULT = County is not a significant predictor of tbdpos ====

model42<-glm(tbdpos ~ size, ShelterTBD, family=binomial)
summary(model42)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.1092     0.1910   0.572  0.56746    
#sizeMedium   -0.3243     0.2694  -1.204  0.22874    
#sizeSmall    -1.5202     0.4011  -3.790  0.00015 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 373.43  on 272  degrees of freedom
#Residual deviance: 356.62  on 270  degrees of freedom
#AIC: 362.62

#Number of Fisher Scoring iterations: 4

### RESULT = Small size is a significant predictor of tbdpos ====

model43<-glm(tbdpos ~ furlength, ShelterTBD, family=binomial)
summary(model43)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)      0.2231     0.3873   0.576    0.565
#furlengthNA     -0.5596     0.5669  -0.987    0.324
#furlengthshort  -0.5504     0.4105  -1.341    0.180

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 373.43  on 272  degrees of freedom
#Residual deviance: 371.59  on 270  degrees of freedom
#AIC: 377.59

#Number of Fisher Scoring iterations: 4

### RESULT = Furlength is not a significant predictor of tbdpos ====

model44<-glm(tbdpos ~ agestages, ShelterTBD, family=binomial)
summary(model44)
#new results 

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.3205     0.1958  -1.636 0.101747    
#agestagesAdult    0.5666     0.2825   2.006 0.044867 *  
#  agestagesPuppy   -1.5838     0.4500  -3.520 0.000432 ***
#  agestagesSenior   1.6198     0.6801   2.381 0.017243 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 373.43  on 272  degrees of freedom
#Residual deviance: 336.21  on 269  degrees of freedom
#AIC: 344.21

#Number of Fisher Scoring iterations: 4

### RESULT = Age (adult, puppy, senior) is significant predictor of tbdpos... but is it?====

model45<-glm(tbdpos ~ sex, ShelterTBD, family=binomial)
summary(model45)
#new results 

#oefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -0.4870     0.1779  -2.737   0.0062 **
#  sexMale       0.4150     0.2459   1.688   0.0915 . 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 373.43  on 272  degrees of freedom
#Residual deviance: 370.56  on 271  degrees of freedom
#AIC: 374.56

#Number of Fisher Scoring iterations: 4

### RESULT = Sex may be???? a significant predictor of tbdpos ====

model46<-glm(tbdpos ~ ticks, ShelterTBD, family=binomial)
summary(model46)
#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.5411     0.1424  -3.800 0.000145 ***
#  ticksY        1.1602     0.3058   3.793 0.000149 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 356.62  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 360.62

#Number of Fisher Scoring iterations: 4


### RESULT = Ticks on exam is a significant predictor of tbdpos (intercept too?)====

model47 <- glm(tbdpos ~ county + size + furlength + agestages + sex + ticks, ShelterTBD, family = binomial )
summary(model47)

#updated 3.28.26

#Call:
#  glm(formula = tbdpos ~ county + size + furlength + agestages + 
#        sex + ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)   
#(Intercept)     -1.697e+01  1.697e+03  -0.010  0.99202   
#countyDelaware   3.355e+00  2.400e+03   0.001  0.99888   
#countyFranklin   2.272e+00  2.313e+03   0.001  0.99922   
#countyGallia     1.711e+01  1.697e+03   0.010  0.99195   
#countyJackson    1.722e+01  1.697e+03   0.010  0.99190   
#countyLawrence   1.777e+01  1.697e+03   0.010  0.99164   
#countyRichland  -1.727e-08  2.939e+03   0.000  1.00000   
#countyRoss       1.608e+01  1.697e+03   0.009  0.99244   
#countyScioto     1.730e+01  1.697e+03   0.010  0.99187   
#sizeMedium      -5.203e-01  3.220e-01  -1.616  0.10608   
#sizeSmall       -1.293e+00  6.255e-01  -2.066  0.03879 * 
#furlengthshort  -6.291e-01  5.063e-01  -1.243  0.21401   
#agestagesAdult   5.891e-01  3.300e-01   1.785  0.07424 . 
#agestagesPuppy  -1.656e+00  6.489e-01  -2.553  0.01069 * 
#agestagesSenior  1.374e+00  7.745e-01   1.774  0.07607 . 
#sexMale          4.456e-01  3.066e-01   1.453  0.14609   
#ticksY           1.256e+00  3.845e-01   3.268  0.00108 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 339.66  on 247  degrees of freedom
#Residual deviance: 264.60  on 231  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 298.6

#Number of Fisher Scoring iterations: 15


#retired  new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.685e+01  1.697e+03  -0.010 0.992078    
#countyDelaware   2.769e+00  2.400e+03   0.001 0.999079    
#countyFranklin   2.166e+00  2.149e+03   0.001 0.999196    
#countyGallia     1.723e+01  1.697e+03   0.010 0.991897    
#countyJackson    1.692e+01  1.697e+03   0.010 0.992044    
#countyLawrence   1.737e+01  1.697e+03   0.010 0.991831    
#countyRichland   6.066e-09  2.939e+03   0.000 1.000000    
#countyRoss       1.608e+01  1.697e+03   0.009 0.992438    
#countyScioto     1.712e+01  1.697e+03   0.010 0.991951    
#sizeMedium      -3.805e-01  3.108e-01  -1.224 0.220871    
#sizeSmall       -9.735e-01  5.738e-01  -1.696 0.089800 .  
#urlengthNA     -2.340e-01  7.294e-01  -0.321 0.748394    
#furlengthshort  -7.192e-01  4.975e-01  -1.446 0.148298    
#agestagesAdult   5.341e-01  3.184e-01   1.677 0.093488 .  
#agestagesPuppy  -1.515e+00  5.846e-01  -2.591 0.009580 ** 
#  agestagesSenior  1.478e+00  7.479e-01   1.976 0.048101 *  
#  sexMale          4.658e-01  2.919e-01   1.596 0.110521    
#ticksY           1.210e+00  3.554e-01   3.404 0.000665 ***
#  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 295.84  on 254  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 331.84

#Number of Fisher Scoring iterations: 15


### RESULT = Small size, Puppy age, having ticks are significant predictors of tbdpos ====
### RESULT = Adult stage and senior age may be significant predictors of tbdpos ====


mansteppos1 <- glm(tbdpos ~ size + furlength + agestages + sex + ticks, ShelterTBD, family = binomial )
summary(mansteppos1)

#county had the highest p value in the full model above, drops out
#updated 3.28.26
#Call:
#  glm(formula = tbdpos ~ size + furlength + agestages + sex + ticks, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.1681     0.5617   0.299 0.764775    
#sizeMedium       -0.4407     0.3110  -1.417 0.156449    
#sizeSmall        -1.1318     0.6049  -1.871 0.061350 .  
#furlengthshort   -0.7652     0.4809  -1.591 0.111572    
#agestagesAdult    0.5711     0.3103   1.841 0.065683 .  
#agestagesPuppy   -1.6156     0.6419  -2.517 0.011839 *  
#agestagesSenior   1.2288     0.7372   1.667 0.095539 .  
#sexMale           0.3923     0.2947   1.331 0.183068    
#ticksY            1.3803     0.3746   3.684 0.000229 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 339.66  on 247  degrees of freedom
#Residual deviance: 281.65  on 239  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 299.65

#Number of Fisher Scoring iterations: 5


#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      0.08663    0.54683   0.158 0.874129    
#sizeMedium      -0.35136    0.30355  -1.157 0.247074    
#sizeSmall       -0.91158    0.56436  -1.615 0.106258    
#furlengthNA     -0.13655    0.68602  -0.199 0.842229    
#furlengthshort  -0.75065    0.47206  -1.590 0.111797    
#agestagesAdult   0.54924    0.30217   1.818 0.069121 .  
#agestagesPuppy  -1.46276    0.58114  -2.517 0.011834 *  
#  agestagesSenior  1.40768    0.71586   1.966 0.049249 *  
#  sexMale          0.36919    0.28159   1.311 0.189824    
#ticksY           1.34415    0.34778   3.865 0.000111 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 310.41  on 262  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 330.41

#Number of Fisher Scoring iterations: 4


mansteppos2 <- glm(tbdpos ~ size + furlength + agestages + ticks, ShelterTBD, family = binomial )
summary(mansteppos2)

#medium has the highest p value but small is close to sig so dropped the second highest p value sex 

#updated 3.28.26
#Call:
#  glm(formula = tbdpos ~ size + furlength + agestages + ticks, 
#      family = binomial, data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.4166     0.5262   0.792 0.428473    
#sizeMedium       -0.5121     0.3055  -1.676 0.093674 .  
#sizeSmall        -1.1994     0.6009  -1.996 0.045920 *  
#furlengthshort   -0.7694     0.4774  -1.612 0.107038    
#agestagesAdult    0.5483     0.3082   1.779 0.075232 .  
#agestagesPuppy   -1.6040     0.6440  -2.491 0.012743 *  
#agestagesSenior   1.3246     0.7325   1.808 0.070582 .  
#ticksY            1.3897     0.3731   3.724 0.000196 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 339.66  on 247  degrees of freedom
#Residual deviance: 283.43  on 240  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 299.43

#Number of Fisher Scoring iterations: 5

#retired new results
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.3205     0.5130   0.625   0.5321    
#sizeMedium       -0.4266     0.2972  -1.435   0.1512    
#sizeSmall        -0.9834     0.5606  -1.754   0.0794 .  
#furlengthNA      -0.1562     0.6808  -0.229   0.8185    
#furlengthshort   -0.7503     0.4682  -1.602   0.1091    
#agestagesAdult    0.5286     0.3003   1.760   0.0784 .  
#agestagesPuppy   -1.4393     0.5814  -2.475   0.0133 *  
#  agestagesSenior   1.4885     0.7122   2.090   0.0366 *  
#  ticksY            1.3498     0.3467   3.893  9.9e-05 ***
#  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 312.13  on 263  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 330.13

#Number of Fisher Scoring iterations: 4



mansteppos3 <- glm(tbdpos ~ size + agestages + ticks, ShelterTBD, family = binomial )
summary(mansteppos3)

#medium has the highest p value but small is sig so dropped the second highest p value fur length 
#updated 3.28.26
#Call:
#  glm(formula = tbdpos ~ size + agestages + ticks, family = binomial, 
 #     data = ShelterTBD)

#Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.3412     0.2688  -1.269   0.2043    
#sizeMedium       -0.4388     0.2957  -1.484   0.1378    
#sizeSmall        -0.9677     0.5385  -1.797   0.0723 .  
#agestagesAdult    0.5827     0.2983   1.954   0.0508 .  
#agestagesPuppy   -1.2342     0.5477  -2.253   0.0242 *  
#agestagesSenior   1.5551     0.7063   2.202   0.0277 *  
#ticksY            1.3755     0.3471   3.962 7.42e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 372.29  on 271  degrees of freedom
#Residual deviance: 315.09  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 329.09

#Number of Fisher Scoring iterations: 4



#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.3639     0.2689  -1.353   0.1760    
#sizeMedium       -0.4033     0.2954  -1.365   0.1722    
#sizeSmall        -0.9426     0.5392  -1.748   0.0805 .  
#agestagesAdult    0.5434     0.2980   1.823   0.0683 .  
#agestagesPuppy   -1.2374     0.5485  -2.256   0.0241 *  
#  agestagesSenior   1.5616     0.7065   2.210   0.0271 *  
#  ticksY            1.3864     0.3464   4.003 6.26e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 315.51  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 329.51

#Number of Fisher Scoring iterations: 4


mansteppos4 <- glm(tbdpos ~ agestages + ticks, ShelterTBD, family = binomial )
summary(mansteppos4)

#medium has the highest p value is dropped 

#updated 3.282.6
#Call:
#  glm(formula = tbdpos ~ agestages + ticks, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.6109     0.2161  -2.826 0.004708 ** 
#agestagesAdult    0.6506     0.2932   2.219 0.026497 *  
#agestagesPuppy   -1.6550     0.4640  -3.567 0.000362 ***
#agestagesSenior   1.5616     0.6965   2.242 0.024963 *  
#ticksY            1.2661     0.3376   3.750 0.000177 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 372.29  on 271  degrees of freedom
#Residual deviance: 319.41  on 267  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 329.41

#Number of Fisher Scoring iterations: 4

#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.6147     0.2162  -2.843 0.004471 ** 
#  agestagesAdult    0.6076     0.2930   2.074 0.038125 *  
#  agestagesPuppy   -1.6576     0.4645  -3.569 0.000359 ***
#  agestagesSenior   1.5620     0.6969   2.241 0.024998 *  
#  ticksY            1.2831     0.3371   3.806 0.000141 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 371.75  on 271  degrees of freedom
#Residual deviance: 319.44  on 267  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 329.44

#Number of Fisher Scoring iterations: 4

#####New results age, having ticks were sig indicators for TBD ==== 

exp(cbind(OR = coef(mansteppos4), confint(mansteppos4)))

#updated 3.28.26
#                   OR     2.5 %     97.5 %
#(Intercept)     0.5428554 0.3520709  0.8238716
#agestagesAdult  1.9166836 1.0830518  3.4258902
#agestagesPuppy  0.1910863 0.0714365  0.4501697
#agestagesSenior 4.7663953 1.3403750 22.4907552
#ticksY          3.5470396 1.8574010  7.0211200

#retired 
#                    OR      2.5 %     97.5 %
#  (Intercept)     0.5408116 0.35068387  0.8208588
#agestagesAdult  1.8360892 1.03751223  3.2797428
#agestagesPuppy  0.1905964 0.07119354  0.4493911
#agestagesSenior 4.7685035 1.33974837 22.5122679
#ticksY          3.6079112 1.89104864  7.1348129

#posmodels <-list(model41, model42, model43, model44, model45, model46, model47, mansteppos1, mansteppos2, mansteppos3, mansteppos4)

#posmodels.names <- c('model41', 'model42', 'model43', 'model44', 'model45', 'model46', 'model47', 'mansteppos1', 'mansteppos2', 'mansteppos3','mansteppos4')

#aictab(cand.set =posmodels, modnames = posmodels.names)

#new results 
#Model selection based on AICc:
#  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#mansteppos4  5 329.67       0.00   0.34   0.34 -159.72
#mansteppos3  7 329.94       0.27   0.29   0.63 -157.76
#mansteppos2  9 330.82       1.15   0.19   0.82 -156.07
#mansteppos1 10 331.25       1.59   0.15   0.97 -155.21
#model47     18 334.55       4.88   0.03   1.00 -147.92
#model44      4 344.36      14.69   0.00   1.00 -168.10
#model46      2 360.66      31.00   0.00   1.00 -178.31
#model42      3 362.71      33.05   0.00   1.00 -178.31
#model41      9 373.79      44.13   0.00   1.00 -177.55
#model45      2 374.61      44.94   0.00   1.00 -185.28
#model43      3 377.68      48.01   0.00   1.00 -185.80

##new RESULTS model mansteppos4 most accurate per a1c - age and ticks present most sig? ?? ====



### multitbd models  ===== 
#multtbd

model48<-glm(multtbd ~ county, ShelterTBD, family=binomial)
summary(model48)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)    -1.657e+01  1.697e+03  -0.010    0.992
#countyDelaware -1.879e-08  2.400e+03   0.000    1.000
#countyFranklin -1.890e-08  2.190e+03   0.000    1.000
#countyGallia    1.530e+01  1.697e+03   0.009    0.993
#countyJackson   1.494e+01  1.697e+03   0.009    0.993
#countyLawrence  1.579e+01  1.697e+03   0.009    0.993
#countyRichland -1.880e-08  2.939e+03   0.000    1.000
#countyRoss      1.475e+01  1.697e+03   0.009    0.993
#countyScioto    1.565e+01  1.697e+03   0.009    0.993

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 282.39  on 272  degrees of freedom
#Residual deviance: 272.38  on 264  degrees of freedom
#AIC: 290.38

#Number of Fisher Scoring iterations: 15


model49<-glm(multtbd ~ size, ShelterTBD, family=binomial)
summary(model49)

#new results 

#Call:
#  glm(formula = multtbd ~ size, family = binomial, data = ShelterTBD)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.8473     0.2081  -4.072 4.65e-05 ***
#  sizeMedium   -0.6788     0.3227  -2.103   0.0355 *  
#  sizeSmall    -1.3719     0.5148  -2.665   0.0077 ** 
#  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 282.39  on 272  degrees of freedom
#Residual deviance: 272.21  on 270  degrees of freedom
#AIC: 278.21

#Number of Fisher Scoring iterations: 4


### RESULT = Size is a significant predictor of multtbd ( small > med > intercept?)====

model50<-glm(multtbd ~ furlength, ShelterTBD, family=binomial)
summary(model50)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)     -1.0498     0.4392  -2.391   0.0168 *
#  furlengthNA      0.1625     0.6281   0.259   0.7958  
#furlengthshort  -0.3478     0.4703  -0.739   0.4596  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 282.39  on 272  degrees of freedom
#Residual deviance: 280.94  on 270  degrees of freedom
#AIC: 286.94

#Number of Fisher Scoring iterations: 4


### RESULT = intercept for fur length??? is a significant predictor of multtbd ====

model51<-glm(multtbd ~ agestages, ShelterTBD, family=binomial)
summary(model51)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.5983     0.2584  -6.184 6.24e-10 ***
#  agestagesAdult    0.8276     0.3376   2.451   0.0142 *  
#  agestagesPuppy   -1.2349     0.6479  -1.906   0.0566 .  
#agestagesSenior   1.3106     0.5987   2.189   0.0286 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 282.39  on 272  degrees of freedom
#Residual deviance: 261.57  on 269  degrees of freedom
#AIC: 269.57

#Number of Fisher Scoring iterations: 5


### RESULT = age senior and adult is a significant predictor of multtbd ====

model52<-glm(multtbd ~ sex, ShelterTBD, family=binomial)
summary(model52)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.5224     0.2253  -6.757  1.4e-11 ***
#  sexMale       0.3948     0.2995   1.318    0.187    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 282.39  on 272  degrees of freedom
#Residual deviance: 280.63  on 271  degrees of freedom
#AIC: 284.63

#Number of Fisher Scoring iterations: 4


### RESULT = Sex is not a significant predictor of multtbd but interceptro is?====

model53<-glm(multtbd ~ ticks, ShelterTBD, family=binomial)
summary(model53)

#new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.6554     0.1872  -8.845  < 2e-16 ***
#  ticksY        1.1800     0.3249   3.632 0.000281 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 266.57  on 270  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 270.57

#Number of Fisher Scoring iterations: 4


### RESULT = Having ticks on exam is a significant predictor of multtbd ====

model54 <- glm(multtbd ~ county + size + furlength + agestages + sex + ticks, ShelterTBD, family = binomial )
summary(model54) 

#updated 3.28.26 
#Warning message:
#  glm.fit: fitted probabilities numerically 0 or 1 occurred 
#Call:
#  glm(formula = multtbd ~ county + size + furlength + agestages + 
#        sex + ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.918e+01  4.612e+03  -0.004  0.99668    
#countyDelaware   1.864e+01  6.591e+03   0.003  0.99774    
#countyFranklin   3.225e+00  6.150e+03   0.001  0.99958    
#countyGallia     1.769e+01  4.612e+03   0.004  0.99694    
#countyJackson    1.770e+01  4.612e+03   0.004  0.99694    
#countyLawrence   1.880e+01  4.612e+03   0.004  0.99675    
#countyRichland  -6.059e-09  7.989e+03   0.000  1.00000    
#countyRoss       1.741e+01  4.612e+03   0.004  0.99699    
#countyScioto     1.842e+01  4.612e+03   0.004  0.99681    
#sizeMedium      -1.105e+00  4.039e-01  -2.736  0.00621 ** 
#sizeSmall       -1.604e+00  1.025e+00  -1.565  0.11760    
#furlengthshort  -5.606e-01  5.749e-01  -0.975  0.32955    
#agestagesAdult   7.809e-01  3.997e-01   1.954  0.05072 .  
#agestagesPuppy  -1.642e+01  9.452e+02  -0.017  0.98614    
#agestagesSenior  1.075e+00  7.135e-01   1.506  0.13201    
#sexMale          3.949e-01  3.797e-01   1.040  0.29832    
#ticksY           1.570e+00  4.175e-01   3.760  0.00017 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 249.30  on 247  degrees of freedom
#Residual deviance: 188.07  on 231  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 222.07

#Number of Fisher Scoring iterations: 17

#retirednew results
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     -1.717e+01  1.697e+03  -0.010 0.991927    
#countyDelaware   3.025e+00  2.400e+03   0.001 0.998994    
#countyFranklin   2.367e+00  2.172e+03   0.001 0.999131    
#countyGallia     1.593e+01  1.697e+03   0.009 0.992510    
#countyJackson    1.557e+01  1.697e+03   0.009 0.992679    
#countyLawrence   1.649e+01  1.697e+03   0.010 0.992245    
#countyRichland  -3.702e-09  2.939e+03   0.000 1.000000    
#countyRoss       1.556e+01  1.697e+03   0.009 0.992685    
#countyScioto     1.617e+01  1.697e+03   0.010 0.992394    
#sizeMedium      -8.904e-01  3.707e-01  -2.402 0.016295 *  
#  sizeSmall       -1.090e+00  7.593e-01  -1.436 0.150995    
#furlengthNA      4.919e-01  7.965e-01   0.618 0.536861    
#furlengthshort  -5.150e-01  5.450e-01  -0.945 0.344638    
#agestagesAdult   7.345e-01  3.771e-01   1.948 0.051409 .  
#agestagesPuppy  -1.332e+00  8.390e-01  -1.588 0.112324    
#agestagesSenior  9.186e-01  6.595e-01   1.393 0.163633    
#sexMale          3.829e-01  3.456e-01   1.108 0.267912    
#ticksY           1.308e+00  3.754e-01   3.486 0.000491 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 228.11  on 254  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 264.11

#Number of Fisher Scoring iterations: 15

### RESULT = Medium size, being an adult, and having ticks on exam are significant predictors of multtbd ====

manstepmult1 <- glm(multtbd ~ size + furlength + agestages + sex + ticks, ShelterTBD, family = binomial )
summary(manstepmult1) 

#counties falls out 
#Call:
#  glm(formula = multtbd ~ size + furlength + agestages + sex + 
#        ticks, family = binomial, data = ShelterTBD)

#Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.3047     0.6671  -1.956   0.0505 .  
#sizeMedium       -1.0034     0.3937  -2.549   0.0108 *  
#sizeSmall        -1.2904     0.9149  -1.410   0.1584    
#furlengthshort   -0.5626     0.5449  -1.033   0.3018    
#agestagesAdult    0.8392     0.3824   2.195   0.0282 *  
#agestagesPuppy  -16.4269   951.2551  -0.017   0.9862    
#agestagesSenior   1.0775     0.6881   1.566   0.1174    
#sexMale           0.3869     0.3683   1.050   0.2935    
#ticksY            1.6222     0.4080   3.976 7.01e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 249.30  on 247  degrees of freedom
#Residual deviance: 198.01  on 239  degrees of freedom
#(25 observations deleted due to missingness)
#AIC: 216.01

#Number of Fisher Scoring iterations: 17



#retired new results
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.3811     0.6353  -2.174 0.029720 *  
#  sizeMedium       -0.8465     0.3681  -2.300 0.021457 *  
#  sizeSmall        -1.0216     0.7316  -1.396 0.162579    
#furlengthNA       0.6528     0.7587   0.860 0.389531    
#furlengthshort   -0.4569     0.5198  -0.879 0.379425    
#agestagesAdult    0.7881     0.3647   2.161 0.030679 *  
#  agestagesPuppy   -1.2723     0.8402  -1.514 0.129929    
#agestagesSenior   0.9895     0.6435   1.538 0.124149    
#sexMale           0.3372     0.3393   0.994 0.320324    
#ticksY            1.3963     0.3684   3.790 0.000151 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)#

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 235.13  on 262  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 255.13

#Number of Fisher Scoring iterations: 5

#new model 2 
manstepmult2 <- glm(multtbd ~ size  + agestages + sex + ticks, ShelterTBD, family = binomial )
summary(manstepmult2) 

#fur length falls out not sex with new data 
#updated 3.28.26
#Call:
#  glm(formula = multtbd ~ size + agestages + sex + ticks, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.7646     0.4019  -4.391 1.13e-05 ***
#sizeMedium       -0.8410     0.3655  -2.301   0.0214 *  
#sizeSmall        -0.8472     0.6767  -1.252   0.2106    
#agestagesAdult    0.8130     0.3620   2.246   0.0247 *  
#agestagesPuppy   -0.9840     0.7761  -1.268   0.2048    
#agestagesSenior   1.1147     0.6304   1.768   0.0770 .  
#sexMale           0.3161     0.3353   0.943   0.3459    
#ticksY            1.4469     0.3642   3.973 7.10e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 238.58  on 264  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 254.58

#Number of Fisher Scoring iterations: 5

#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.7646     0.4019  -4.391 1.13e-05 ***
#  sizeMedium       -0.8410     0.3655  -2.301   0.0214 *  
#  sizeSmall        -0.8472     0.6767  -1.252   0.2106    
#agestagesAdult    0.8130     0.3620   2.246   0.0247 *  
#  agestagesPuppy   -0.9840     0.7761  -1.268   0.2048    
#agestagesSenior   1.1147     0.6304   1.768   0.0770 .  
#sexMale           0.3161     0.3353   0.943   0.3459    
#ticksY            1.4469     0.3642   3.973 7.10e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 238.58  on 264  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 254.58

#Number of Fisher Scoring iterations: 5


manstepmult3 <- glm(multtbd ~ size + agestages + ticks, ShelterTBD, family = binomial )
summary(manstepmult3) 

#sex fell out 
#updated 3.28.26
#Call:
#  glm(formula = multtbd ~ size + agestages + ticks, family = binomial, 
#      data = ShelterTBD)

#Coefficients:
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.5564     0.3295  -4.724 2.32e-06 ***
#sizeMedium       -0.8954     0.3608  -2.482   0.0131 *  
#sizeSmall        -0.9042     0.6762  -1.337   0.1812    
#agestagesAdult    0.7867     0.3597   2.187   0.0287 *  
#agestagesPuppy   -0.9798     0.7781  -1.259   0.2079    
#agestagesSenior   1.1788     0.6253   1.885   0.0594 .  
#ticksY            1.4506     0.3642   3.982 6.82e-05 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 239.48  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 253.48

#Number of Fisher Scoring iterations: 5


#retired new results 
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -1.5564     0.3295  -4.724 2.32e-06 ***
#  sizeMedium       -0.8954     0.3608  -2.482   0.0131 *  
#  sizeSmall        -0.9042     0.6762  -1.337   0.1812    
#agestagesAdult    0.7867     0.3597   2.187   0.0287 *  
#  agestagesPuppy   -0.9798     0.7781  -1.259   0.2079    
#agestagesSenior   1.1788     0.6253   1.885   0.0594 .  
#ticksY            1.4506     0.3642   3.982 6.82e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 279.27  on 271  degrees of freedom
#Residual deviance: 239.48  on 265  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 253.48

#Number of Fisher Scoring iterations: 5

exp(cbind(OR = coef(manstepmult3), confint(manstepmult3)))

#updated 3.28.26
#                   OR      2.5 %     97.5 %
#(Intercept)     0.2109028 0.10710942  0.3920723
#sizeMedium      0.4084247 0.19712197  0.8167855
#sizeSmall       0.4048823 0.09926271  1.4433670
#agestagesAdult  2.1961331 1.09536747  4.5178580
#agestagesPuppy  0.3753744 0.06874326  1.5404093
#agestagesSenior 3.2504315 0.92300590 11.1249192
#ticksY          4.2654630 2.10073393  8.8257131

#retired 
#                    OR      2.5 %     97.5 %
#  (Intercept)     0.2109028 0.10710942  0.3920723
#sizeMedium      0.4084247 0.19712197  0.8167855
#sizeSmall       0.4048823 0.09926271  1.4433670
#agestagesAdult  2.1961331 1.09536747  4.5178580
#agestagesPuppy  0.3753744 0.06874326  1.5404093
#agestagesSenior 3.2504315 0.92300590 11.1249192
#ticksY          4.2654630 2.10073393  8.8257131

#this is old but in the new model age was significant so not running this
manstepmult4 <- glm(multtbd ~ size + ticks, ShelterTBD, family = binomial )
summary(manstepmult4) 

#puppies is the highest p value but other var are sig in the cat, so age falls out

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3215  -0.7557  -0.4834  -0.3168   2.4566  

#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.1073     0.2264  -4.891 1.00e-06 ***
#sizeMedium   -0.9808     0.3514  -2.791  0.00525 ** 
#sizeSmall    -1.8599     0.5848  -3.180  0.00147 ** 
#ticksY        1.4399     0.3574   4.029 5.61e-05 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 276.13  on 270  degrees of freedom
#Residual deviance: 247.69  on 267  degrees of freedom
#(3 observations deleted due to missingness)
#AIC: 255.69

#Number of Fisher Scoring iterations: 5


multmodels <-list(model48, model49, model50, model51, model52, model53, model54, manstepmult1, manstepmult2, manstepmult3, manstepmult4)

multmodels.names <- c('model48', 'model49', 'model50', 'model51', 'model52', 'model53', 'model54', 'manstepmult1', 'manstepmult2','manstepmult3', 'manstepmult4')

aictab(cand.set =multmodels, modnames = multmodels.names)
#new results 
#Model selection based on AICc:
#  
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#manstepmult3  7 253.90       0.00   0.51   0.51 -119.74
#manstepmult2  8 255.13       1.23   0.28   0.79 -119.29
#manstepmult1 10 255.97       2.07   0.18   0.97 -117.56
#manstepmult4  4 259.45       5.55   0.03   1.00 -125.65
#model54      18 266.81      12.91   0.00   1.00 -114.05
#model51       4 269.72      15.82   0.00   1.00 -130.78
#model53       2 270.61      16.71   0.00   1.00 -133.28
#model49       3 278.30      24.40   0.00   1.00 -136.11
#model52       2 284.67      30.77   0.00   1.00 -140.31
#model50       3 287.03      33.13   0.00   1.00 -140.47
#model48       9 291.06      37.16   0.00   1.00 -136.19

###new resulst manstepmult3 is the best fit model per A1C - medium,  adult, ticks sig for multtbd with senior being just on the cutoff ====


