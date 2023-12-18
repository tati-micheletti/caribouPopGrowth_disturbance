createBufferedDisturbances <- function(disturbanceList, 
                                       bufferSize,
                                       rasterToMatch,
                                       studyArea){
  # Make it generic
  unlistedDL <- unlist(disturbanceList)
  # classes <- sapply(unlistedDL, class)
  # allLevels <- strsplit(names(unlistedDL), split = "\\.")
  
  allBuffered <- lapply(names(unlistedDL), function(INDEX){
    # Buffer all shapefiles
    # Convert shapefiles to rasters
    lay <- unlistedDL[[INDEX]]
    whichClass <- class(lay)
    tic(paste0("Time elapsed for buffering ", INDEX, ":"))
    if (whichClass == "SpatVector"){
      bLay <- terra::aggregate(terra::buffer(lay, width = bufferSize))
      bLaySF <- sf::st_as_sf(bLay)
      bLayRas <- fasterize::fasterize(sf = bLaySF, raster = rasterToMatch)
      names(bLayRas) <- INDEX
    } else {
      rasterToMatch
      lay[] <- lay[]
      bLayRas <- bufferCells(ras = lay, 
                            valToLookFor = 1, 
                            newValue = 1)
      names(bLayRas) <- INDEX
    }
    if (class(bLayRas) != "RasterLayer"){
      bLayRas <- raster(bLayRas)
    }
    finalRas <- postProcess(bLayRas, 
                            rasterToMatch = rasterToMatch,
                            studyArea = studyArea)
    toc()
    return(finalRas)
    })

  # Convert all pixels that are disturbed to 1, and non-disturbed to 0
  bufferedAnthropogenicDisturbance500m <- rasterToMatch
  bufferedAnthropogenicDisturbance500m[] <- bufferedAnthropogenicDisturbance500m[]
  bufferedAnthropogenicDisturbance500m[!is.na(bufferedAnthropogenicDisturbance500m[])] <- 0
  names(bufferedAnthropogenicDisturbance500m) <- "templateRas"

  for (i in 1:length(allBuffered)){
    bufferedAnthropogenicDisturbance500m[which(allBuffered[[i]][] == 1)] <- 1
  }
  
  return(bufferedAnthropogenicDisturbance500m)
}