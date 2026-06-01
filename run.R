
# Restore package versions recorded in renv.lock if it exists
if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
  renv::restore(prompt = FALSE)
}

# Run all scripts 
files <- list.files("scripts", pattern = "\\.R$")
for(f in files) source(paste0("scripts/",f))

# Launch > renv::snapshot() 
# to update renv.lock with any new packages used in the scripts

