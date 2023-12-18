rstCurrentBurnListGenerator <- function(pathInputs){
  rstPath <- grepMulti(x = list.files(path = pathInputs, full.names = TRUE),
                       patterns = c("rstCurrentBurn"),
                       unwanted = c("aux", "xml"))
  if (length(rstPath) == 0)
    stop("No rstCurrentBurn file (.tif) found in pathInputs. ",
         "Please make sure these files are in the folder. ",
         "These come generallyfrom landscape simulation, and ",
         "indicate the pixels burned on a given year (raster)")
  rstCurrentBurnList <- lapply(rstPath, function(rPath){
    rasName <- paste0("Year", substrBoth(strng = tools::file_path_sans_ext(rPath),
                                         howManyCharacters = 4, fromEnd = TRUE))
    ras <- raster::raster(rPath)
    names(ras) <- rasName
    return(ras)
  })
  names(rstCurrentBurnList) <- unlist(lapply(rstCurrentBurnList, names))
  ys1 <- substrBoth(strng = tools::file_path_sans_ext(rstPath[1]),
                    howManyCharacters = 4, fromEnd = TRUE)
  ys2 <- substrBoth(strng = tools::file_path_sans_ext(rstPath[length(rstPath)]),
                    howManyCharacters = 4, fromEnd = TRUE)
  message(paste0("Files found for years: ",
                 ys1, " to ", ys2))
  return(rstCurrentBurnList)
}
