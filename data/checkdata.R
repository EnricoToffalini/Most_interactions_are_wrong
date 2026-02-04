
d = read.csv("codedDatasetReview.csv")
nrow(d)  

dx = d[d$Eligible==1 & !is.na(d$Eligible), ]
nrow(dx)  

dxx = dx[d$Tests_interactions==1 & !is.na(d$Tests_interactions), ]
nrow(dxx)  

sum(!is.na(dxx$Incorrect_identity_link_function))
mean(dxx$Incorrect_identity_link_function, na.rm=T)

sum(!is.na(dxx$Finds_significant_interaction))
mean(dxx$Finds_significant_interaction, na.rm=T)


