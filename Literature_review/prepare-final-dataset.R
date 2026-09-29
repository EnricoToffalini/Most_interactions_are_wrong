
library(readxl)
library(dplyr)
library(readr)

ds = read_excel("2_codingDataset_2coders.xlsx",sheet="DS")
jepg = read_excel("2_codingDataset_2coders.xlsx",sheet="JEPG")
jpsp = read_excel("2_codingDataset_2coders.xlsx",sheet="JPSP")
pm = read_excel("2_codingDataset_2coders.xlsx",sheet="PM")
ps = read_excel("2_codingDataset_2coders.xlsx",sheet="PS")

x = rbind(ds,jepg,jpsp,pm,ps)
x = x[!is.na(x$`id...1`),]

# Article-level decisions are taken from the screening workbook (authoritative), matched by id
scr = NULL
for (s in c("DS","JEPG","JPSP","PM","PS")) {
  scr = rbind(scr, read_excel("1_screeningEligibilityDataset_2coders.xlsx",sheet=s)[,c("id","Eligible","Tests_interactions","Tests_observed_outcome_interactions")])
}
m = match(x$`id...1`, scr$id)
stopifnot(!anyNA(m))
# workbook 2 must agree with workbook 1 on the article-level flags
stopifnot(identical(as.numeric(x$Eligible), as.numeric(scr$Eligible[m])),
          identical(as.numeric(x$Tests_interactions), as.numeric(scr$Tests_interactions[m])),
          identical(as.numeric(x$Tests_observed_outcome_interactions), as.numeric(scr$Tests_observed_outcome_interactions[m])))

# Detailed-coding sample: eligible articles testing at least one interaction on an observed outcome
keep = scr$Eligible[m] %in% 1 & scr$Tests_observed_outcome_interactions[m] %in% 1
x = x[keep,]

write_excel_csv(x, "final-dataset-review.csv")