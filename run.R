
files <- list.files("scripts", pattern = "\\.R$")

for(f in files) source(paste0("scripts/",f))

