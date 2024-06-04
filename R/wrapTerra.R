wrapTerraList <- function(terraList, savePath, zipFiles = FALSE, uploadZip = NULL){
  Require("stringi")
  Require("qs")
  Require("zip")
  listNames <- lapply(1:length(names(terraList)), function(index1){
    obj <- lapply(1:length(names(terraList[[index1]])), function(index2){
      # message(paste0("Saving ", names(terraList[[index1]][[index2]]), "\n"))
      obj2 <- terra::wrap(terraList[[index1]][[index2]])
      fileName <- file.path(savePath, paste0(stringi::stri_rand_strings(1, 10), 
                                                ".qs"))
      qs::qsave(obj2, fileName)
      return(fileName)
    })
    names(obj) <- names(terraList[[index1]])
    return(obj)
  })
  names(listNames) <- names(terraList)
  if (zipFiles) {
    # Need to save the files together with the list
    qs::qsave(listNames, file = file.path(savePath, "theList.qs"))
    allFls <- c(file.path(savePath, "theList.qs"), 
                unlist(listNames, use.names = FALSE))
    zip(zipfile = file.path(savePath, "disturbanceList.zip"), 
        files = allFls)
    message(paste0("disturbanceList zipped to ", file.path(savePath, "disturbanceList.zip")))
    if (!is.null(uploadZip)) {
      Require("googledrive")
      drive_upload(media = file.path(savePath, "disturbanceList.zip"), 
                   path = as_id(uploadZip))
      message(paste0("disturbanceList uploaded to ", uploadZip))
    } 
  }
  return(listNames)
}

unwrapTerraList <- function(terraList, savePath = NULL){
  updatePath <- FALSE
  if (all(!is.list(terraList),
          !file.exists(file.path(savePath, "theList.qs")))) {
    message(paste0("The terraList file provided seems to be a google drive link. The contents will be",
                   " downloaded and extracted before recovering."))
    # If we pass a URL for the file instead of a list, then first we need to download
    # the file, unzip, and then we update the terraList with the unzipped file theList.qs
    drive_download(file = as_id(terraList), path = file.path(savePath, "zippedList.zip"))
    unzip(zipfile = file.path(savePath, "zippedList.zip"), 
          exdir = savePath, 
          junkpaths = TRUE)
    terraList <- qs::qread(file.path(savePath, "theList.qs"))
    updatePath <- TRUE
  } else {
    if (all(!is.list(terraList),
            file.exists(file.path(savePath, "theList.qs")))){
      terraList <- qs::qread(file.path(savePath, "theList.qs"))
      updatePath <- TRUE
    }
  }
  listNames <- lapply(1:length(names(terraList)), function(index1){
    obj <- lapply(1:length(names(terraList[[index1]])), function(index2){
      message(paste0("Recovering ", names(terraList[[index1]][[index2]])))
      if (updatePath){
        print(paste0("terraList was a link. Fixing paths for ", index2))
        pth <- dirname(terraList[[index1]][[index2]])
        terraList[[index1]][[index2]] <- gsub(x = terraList[[index1]][[index2]],
                                              pattern = pth,
                                              replacement = savePath)
      }
      obj2 <- qs::qread(terraList[[index1]][[index2]])
      return(terra::vect(obj2))
    })
    names(obj) <- names(terraList[[index1]])
    return(obj)
  })
  names(listNames) <- names(terraList)
  return(listNames)
}
