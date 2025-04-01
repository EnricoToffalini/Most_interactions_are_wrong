set.seed(0)

files = list.files("raw", pattern = "*.csv", full.names = TRUE)

dat = lapply(files, function(x) read.csv(x))
names(dat) = basename(xfun::sans_ext(files))

prepare_data = function(data, name){
  idx = sample(1:nrow(data), nrow(data))
  data = data[idx, ]
  data = data[order(data$Year, decreasing = TRUE), ]
  id = sprintf("%s_%04d", gsub("[a-z]", replacement = "", name), 1:nrow(data))
  data = data.frame(id, data)
  return(data)
}

dat = lapply(1:length(dat), function(i) prepare_data(dat[[i]], names(dat)[i]))
names(dat) = gsub("[a-z]", replacement = "", basename(xfun::sans_ext(files)))

writexl::write_xlsx(dat, "screeningDataset_start.xlsx")
