
library(tidyverse)
library(readxl)

#you will need to install the "rstudioapi" package if you don't have it

#get current script file path
rstudioapi::getActiveDocumentContext()$path

#get name of folder
dirname(rstudioapi::getActiveDocumentContext()$path)

#set working directory to current folder path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

read_excel("TestSheet.xlsx")

#set working directory to another folder adjacent to the script
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/data_files"))

read_excel("data_excel.xlsx")


#OR we can move up a folder by cutting out the name of the current folder
str_remove(dirname(rstudioapi::getActiveDocumentContext()$path), "/TestFolder")


