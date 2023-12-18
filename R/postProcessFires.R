postProcessFires <- function(rstCurrentBurnList, 
                             rasterToMatch,
                             studyArea,
                             destinationPath){
  
  rstCurrentBurnListPost <- lapply(rstCurrentBurnList, function(LAY){
    reprojLay <- reproducible::postProcess(x = LAY,
                                           rasterToMatch = rasterToMatch,
                                           destinationPath = destinationPath)
    
    maskLay <- reproducible::maskInputs(x = reprojLay, studyArea = studyArea)
    return(maskLay)
  })
  names(rstCurrentBurnListPost) <- names(rstCurrentBurnList)
  return(rstCurrentBurnListPost)
}