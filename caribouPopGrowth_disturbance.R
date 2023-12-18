## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "caribouPopGrowth_disturbance",
  description = paste0("This module will harmonize an object called `disturbanceList`",
                       "into `bufferedAnthropogenicDisturbance500m`.", "It is a connection ",
                       "module between potential resources generator and the caribou population ",
                       "growth modules"),
  keywords = "",
  authors = structure(list(list(given = "Tati", family = "Micheletti", 
                                role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", 
                                comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(caribouPopGrowth_disturbance = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "caribouPopGrowth_disturbance.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.0.10)", "ggplot2", "terra", "sf", "fasterize"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".runInterval", "numeric", 10, NA, NA,
                    "Describes the interval at which the events should occur."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("disturbancesFolder", "character", Paths[["inputPath"]], NA, NA,
                    paste0("If the anthroDisturbance_Generator module is not being run, where should ",
                           "the module look for disturbance files?")),
    defineParameter("disturbanceNamingPattern", "character", "disturbances_", NA, NA,
                    paste0("If passing specific disturbances layers, provide the naming pattern ",
                           "to be looked for. Only matters if disturbanceList is not being provided")),
    defineParameter("bufferSize", "numeric", 500, NA, NA,
                    paste0("What is the buffering size (normally in m, depending on projection) ",
                           "that the disturbances should be buffered to?")),
    defineParameter("overwriteDisturbanceLayer", "logical", TRUE, NA, NA,
                    paste0("If the bufferedAnthropogenicDisturbance500m exists and has been saved ",
                           "would you like to overwrite it (TRUE) or just load it (FALSE)?"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "disturbanceList", objectClass = NA, 
                 desc = paste0("Updated list (general category) of lists (specific ",
                               "class) of disturbances and the potential needed for ",
                               "generating disturbances. The link below provides an example."),
                 sourceURL = "https://drive.google.com/file/d/1v7MpENdhspkWxHPZMlmx9UPCGFYGbbYm/view?usp=sharing"),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "All spatial outputs will be reprojected and resampled to it",
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "bufferedAnthropogenicDisturbance500m", objectClass = "RasterLayer",
                  desc = paste0("SpatialPolygonsDataFrame with all disturbances buffered at 500m.",
                                "The object is updated when disturbanceList is available for a ",
                                "given year.")),
    createsOutput(objectName = "disturbanceList", objectClass = NA, 
                 desc = paste0("Updated list (general category) of lists (specific ",
                               "class) of disturbances and the potential needed for ",
                               "generating disturbances.")),
    createsOutput(objectName = "rstCurrentBurnList", objectClass = "list",
                 desc = paste0("List of fires by year (raster format). These ",
                               "layers are produced by simulation and ",
                               "are being outputed here in case this module is being run ",
                               "post-simulation, as it needs to match the layers used for the ",
                               "disturbance generation")),
    createsOutput(objectName = "anthroDistFilePath", objectClass = "character",
                  desc = paste0("File path of the  updated disturbance layer"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.caribouPopGrowth_disturbance = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      if (is.null(sim$disturbanceList)){
        mod$disturbanceFromSimList <- FALSE
      } else {
        mod$disturbanceFromSimList <- TRUE
      }

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "caribouPopGrowth_disturbance", "generateRstCurrentBurnList")
      sim <- scheduleEvent(sim, time(sim), "caribouPopGrowth_disturbance", "findDisturbanceList")
      sim <- scheduleEvent(sim, time(sim), "caribouPopGrowth_disturbance", "convertDisturbanceList")
      sim <- scheduleEvent(sim, time(sim), "caribouPopGrowth_disturbance", "saveBufferedDisturbances")
    },
    generateRstCurrentBurnList = {
      # I need rstCurrentBurnList for the caribou module
      # I should supply the same ones used for the disturbance generation! 
      # Already generates for all years available.
      sim$rstCurrentBurnList <- rstCurrentBurnListGenerator(Paths[["inputPath"]])
      # Test if rstCurrentBurn align with RTM
      areStackable <- tryCatch({
        invisible(raster::stack(raster::stack(sim$rstCurrentBurnList), sim$rasterToMatch))
        TRUE
      }, error = function(e){
        return(FALSE)
      })
      if (!areStackable){
        print("Simulated fires do not match the rasterToMatch. Postprocessing...")
        sim$rstCurrentBurnList <- Cache(postProcessFires, 
                                        rstCurrentBurnList = sim$rstCurrentBurnList,
                                        rasterToMatch = sim$rasterToMatch,
                                        studyArea = sim$studyArea,
                                        destinationPath = asPath(getOption("reproducible.destinationPath", 
                                                                                                  dataPath(sim)), 1))
      }
    },
    findDisturbanceList = {
      
      sim$anthroDistFilePath <- file.path(Paths[["outputPath"]],
                                          paste0("bufferedAnthDist_", 
                                                 P(sim)$bufferSize, 
                                                 "m_", time(sim), ".tif"))
      
      if (!all(file.exists(sim$anthroDistFilePath),
              !P(sim)$overwriteDisturbanceLayer)){
        # 1. Check if `disturbanceList` exists in sim.
        #    1.1. If it exists --> move to next function as is.
        #    This list is always updated in the athroDisturbance_Generator  
        if (!mod$disturbanceFromSimList){
          #    1.2. If it doesn't exist in the sim, check if *files* it exists in a folder 
          #    (the disturbance list per se will not exist as I am not saving it!):
          message(paste0("Disturbance list is not being provided by a another module. The module will",
                         " try to find the disturbance files and create it."))
          sim$disturbanceList <- getDisturbanceList(disturbancesFolder = P(sim)$disturbancesFolder,
                                                    disturbanceNamingPattern = P(sim)$disturbanceNamingPattern,
                                                    currentYear = time(sim))
          #    1.3. If it exists: load all layers exactly as in the `disturbanceList`
        }
        if (is.null(sim$disturbanceList)){
          #        1.2.1. If disturbanceList still doesn't exist there is no point in runing the module! 
          stop("disturbanceList was not found nor provided. There is no reason to run the module.")
        }
      }
    
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, "caribouPopGrowth_disturbance", "findDisturbanceList")

    },
    convertDisturbanceList = {
      if (!all(file.exists(sim$anthroDistFilePath),
               !P(sim)$overwriteDisturbanceLayer)){
        # 2. bufferedAnthropogenicDisturbance500m doesn't exist at this point, or the simulation would have stopped 
      #    2.1. Put all shapefiles together: 
      #     2.1.1. Buffer them to 500, convert to rasters 
      #     2.1.2. add the other rasters with these
      #     2.1.3. Name it bufferedAnthropogenicDisturbance500m
      sim$bufferedAnthropogenicDisturbance500m <- createBufferedDisturbances(disturbanceList = sim$disturbanceList,
                                                                             bufferSize = P(sim)$bufferSize,
                                                                             rasterToMatch = sim$rasterToMatch,
                                                                             studyArea = sim$studyArea)
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, "caribouPopGrowth_disturbance", "convertDisturbanceList")
      
    },
    saveBufferedDisturbances = {
      if (!all(file.exists(sim$anthroDistFilePath),
               !P(sim)$overwriteDisturbanceLayer)){
        writeRaster(sim$bufferedAnthropogenicDisturbance500m, 
                  filename = sim$anthroDistFilePath,
                  format = "GTiff",
                  overwrite = TRUE)
      } else {
        message(crayon::red(paste0("bufferedAnthropogenicDisturbance500m found for year ", time(sim), " and ",
                       "overwriteDisturbanceLayer is FALSE. Loading existing file.")))
        sim$bufferedAnthropogenicDisturbance500m <- raster::raster(sim$anthroDistFilePath)
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, "caribouPopGrowth_disturbance", 
                           "saveBufferedDisturbances")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (suppliedElsewhere("bufferedAnthropogenicDisturbance500m", sim)) {
      stop(paste0("bufferedAnthropogenicDisturbance500m is being supplied by another module or user. ",
                  "There is no reason to run the module."))
  }
  
  if (!suppliedElsewhere(object = "rasterToMatch", sim = sim)) {
    sim$rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/11yCDc2_Wia2iw_kz0f0jOXrLpL8of2oM/",
                               destinationPath = dPath,
                               overwrite = TRUE,
                               omitArgs = c("destinationPath", "overwrite", "filename2"))
  }
  
  return(invisible(sim))
}
