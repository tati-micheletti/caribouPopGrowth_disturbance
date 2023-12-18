bufferCells <- function(ras, valToLookFor, newValue){
  rDT <- data.table(pixelID = 1:ncell(ras),
                    vals = as.numeric(terra::values(ras)))
  whichCells <- rDT[vals == valToLookFor, pixelID]
  ADJ <- unique(na.omit(as.numeric(adjacent(ras, cells = whichCells, 
                                            directions = "queen", 
                                            include = TRUE))))
  ras[ADJ] <- newValue
  return(ras)
}
