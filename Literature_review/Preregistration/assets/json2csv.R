
library(jsonlite)
jdata  = fromJSON("DataDictionary_coded_variables_review.json")
df = data.frame(jdata$variableMeasured)

for(col in 1:ncol(df)){
  if(is.list(df[,col])){
    for(row in 1:nrow(df)){
      df[row,col] = paste(unlist(df[row,col]),collapse=", ")
    }
    df[,col] = unlist(df[,col])
  }
}

write.csv(df,
          "DataDictionary_coded_variables_review.csv",
          row.names=F)

