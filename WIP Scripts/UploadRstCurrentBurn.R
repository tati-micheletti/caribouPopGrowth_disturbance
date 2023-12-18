# Upload rstCurrentBurn: i.e., from landscape simulation VM to google drive to be used by 
# anthropogenic disturbances, caribou, etc.

if (!require("Require", quietly = TRUE)) {
  install.packages("Require")
  library("Require")
}
Require("googledrive")

# MODIFY THIS TO DOWNLOAD THE FILES TO THE CORRECT LOCATIONS!

for (REP in c("run01", "run02", "run03", "run04", "run05")){
  print(paste0("Uploading files to ", REP))
  FD <- paste0("~/GitHub/NT_caribou/outputs/NT1_BCR6/NT1_BCR6_CanESM5_SSP370_", REP)
  
  Y <- c(2012:2050)
  
  fls <- list.files(FD, 
                    pattern = paste(paste0("rstCurrentBurn_", Y), collapse = "|"), 
                    full.names = TRUE)
  
  if (REP == "run01")
    ID <- "1AvCZSs1jyDMsh_7rYSl5itBFW8LgXBLy"
  if (REP == "run02")
    ID <- "1mXy7XFzmKpH6I-wcsFFKigmqD0TajcLS"
  if (REP == "run03")
    ID <- "1SzyATki9WQDlwYkGRN0s1-8qAbEiFC4B"
  if (REP == "run04")
    ID <- "1fuCBzTPq5WuIWfzNoDR-SWwa744-zlRU"
  if (REP == "run05")
    ID <- "1z_M6clI4ZQuCAcAGPt9mNiWYRm3wwdJC"
  
  lapply(fls, 
         drive_upload, 
         path = as_id(ID))
}
