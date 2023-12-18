getDisturbanceList <- function(disturbancesFolder, 
                               disturbanceNamingPattern, 
                               currentYear){
  # This function looks for the disturbance files and puts a disturbance list together
  # The disturbance list is a 2-level list (general disturbance and specific disturbance), with 
  # mixed rasters and shapefiles of results
  allFiles <- grepMulti(list.files(path = disturbancesFolder, 
                                   full.names = TRUE), patterns = c(disturbanceNamingPattern, 
                                                                    currentYear, "tif|shp"), 
                        unwanted = ".aux.xml")
  # Return NULL if there are no disturbances
  if (length(allFiles) == 0){
    return(NULL)
  }
  allLevels <- strsplit(basename(tools::file_path_sans_ext(allFiles)), split = "_")
  level1 <- unlist(lapply(allLevels, `[[`, 2)) # "Second" level as the first is the "disturbance_"
  # howMany <- as.numeric(table(level1))
  # whichNames <- names(table(level1))
  disturbanceList <- lapply(unique(level1), function(sector){
    thisSectorFiles <- grepMulti(allFiles, patterns = sector)
    level2 <- strsplit(basename(tools::file_path_sans_ext(thisSectorFiles)), split = "_")
    level2 <- unlist(lapply(level2, `[[`, 3))
    thisSectorList <- lapply(1:length(level2), function(INDEX){
      if (tools::file_ext(thisSectorFiles[INDEX]) == "shp"){
        fl <- terra::vect(thisSectorFiles[INDEX])
      } else {
        fl <- terra::rast(thisSectorFiles[INDEX])
      }
      return(fl)
    })
    names(thisSectorList) <- level2
    return(thisSectorList)
  })
  names(disturbanceList) <- unique(level1)
  return(disturbanceList)
}