
library(readxl)
library(dplyr)
library(readr)

ds = read_excel("2_codingDataset_2coders.xlsx",sheet="DS")
jepg = read_excel("2_codingDataset_2coders.xlsx",sheet="JEPG")
jpsp = read_excel("2_codingDataset_2coders.xlsx",sheet="JPSP")
pm = read_excel("2_codingDataset_2coders.xlsx",sheet="PM")
ps = read_excel("2_codingDataset_2coders.xlsx",sheet="PS")

x = rbind(ds,jepg,jpsp,pm,ps)
x = x[x$Eligible==1 & !is.na(x$Eligible),]

write_excel_csv(x, "final-dataset-review.csv")