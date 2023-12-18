if(!require("Require")){
  install.packages("Require")
}
library("Require")
Require("googledrive")
Require("SpaDES.core")
# Require("sf")
# Require("data.table")
# Require("ggplot2")
# Require("tictoc")
# Require("future")
# Require("future.apply")
# Require("achubaty/amc")
# Require("LandSciTech/caribouMetrics")

REP <- "run1"

# Pass your email for authentication (used for non-interactive calls)
googledrive::drive_auth(email = "tati.micheletti@gmail.com")
options(reproducible.useTerra = FALSE) # Workaround while reproducible is not yet fully functional with terra

# If you load the project, set the directory where your modules are located 
moduleDir <- dirname(getwd())

setPaths(modulePath = moduleDir,
         cachePath = checkPath(file.path(getwd(), "cache"),
                               create = TRUE), 
         outputPath = checkPath(file.path(getwd(), "outputs", REP), 
                                create = TRUE),
         # inputPath = checkPath(file.path(getwd(), "inputs", REP), 
         #                       create = TRUE),
         inputPath = checkPath(file.path("~/projects/anthroDisturbance_Generator", 
                                         "outputs", REP), # Just to run for now, the results are in this specific folder
                               create = TRUE),
         rasterPath = checkPath(file.path(getwd(), "temp_raster"), 
                                create = TRUE))

getPaths() # shows where the 4 relevant paths are

times <- list(start = 2011, end = 2041)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  # Default values, don't need to be passed but are here as examples
  "caribouPopGrowth_disturbance" = list("disturbancesFolder" = Paths[["inputPath"]],
                                        "overwriteDisturbanceLayer" = FALSE),
  "caribouPopGrowthModel" = list(".growthInterval" = 10)
)

# Create needed objects

# Study Area -- Test study area: NT1
# Boreal_Caribou_Revised_Study_Areas_2020 <- "https://drive.google.com/file/d/1FNQKCjKhZIsr5rzWGfTvJ74K2KexN3um/view?usp=sharing"
# BIO_ENR_WFE_BorealCaribou_RangePlanRegions_2020 <- "https://drive.google.com/file/d/1x_fQEKHW2nGbqo1JvCpDwmVuTPYAavl3/view?usp=sharing"
# BIO_ENR_WFE_BorealCaribou_GNWT_NT1_range_2020 <- "https://drive.google.com/file/d/1VRSolnXMYPrkdBhNofeR81dCu_NTBSgf/view?usp=sharing"

studyArea <- prepInputs(url = "https://drive.google.com/file/d/1X-LkvBAC7-qIuCB1I408oXg3RNYODiqV/view?usp=sharing",
                        destinationPath = Paths[["inputPath"]],
                        fun = "qs::qread")
# Providing below outside of the module as GIS operations are doing weird stuff
listSACaribou <- studyArea$listSACaribou
studyArea <- postProcess(studyArea$studyArea, studyArea = listSACaribou$GNWT_NT1_range) 

# Need to add the metaherds to listSACaribou so I have something comparable to Johnson et al., 2019 for the MS
herds <- sf::st_as_sf(prepInputs(url = "https://drive.google.com/file/d/1ZaFsXPRsGOj69nyVv3SDNyT7qTHkX2kU/view?usp=sharing",
                    destinationPath = Paths[["inputPath"]],
                    studyArea = listSACaribou$GNWT_NT1_range))
# remove any province leftovers from GIS operations. Due to the
# terrible original projection (aea), we get weird little polygons from other
# provinces at borders sometimes
herds <- subset(herds, herds[["PROV_TERR"]] == "NWT")
# The original 'herds' file has the wrong projection!
herds  <- sf::st_set_crs(x = herds,
                     value = paste0("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40",
                                    " +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 ",
                                    "+datum=NAD83 +units=m no_defs"))
herds <- projectInputs(x = herds, targetCRS = raster::crs(studyArea))
listSACaribou[["Herds_NT"]] <- herds

# RTM -- Test study area: NT1
rasterToMatch <- prepInputs(url = "https://drive.google.com/file/d/1khWMH578isq1_VgUYVOpnTl4t6Z1QIj5/view?usp=sharing",
                            targetFile = "NT1_rtm.tif",
                            destinationPath = Paths[["inputPath"]],
                            studyArea = studyArea)

########### Shortcut fires for caribou module START
historicalFiresAllPath <- file.path(dirname(Paths[["outputPath"]]), "historicalFiresAll.shp")

if (!file.exists(historicalFiresAllPath)){
  historicalFiresFile <- raster::shapefile(file.path(moduleDir, "caribouPopGrowth_disturbance", 
                                                     "data", "NBAC_CAN_1986_2017_NFDB_up_to_1985.shp"))
  historicalFires <- postProcess(x = historicalFiresFile,
                                 studyArea = studyArea)
  # simplifying
  historicalFiresS <- historicalFires[, names(historicalFires) %in% c("YEAR", "DECADE")]
  historicalFiresDT <- data.table::data.table(sf::st_as_sf(historicalFiresS[, c("YEAR", "DECADE")]))
  historicalFiresDT[, geometry := NULL]
  historicalFiresDT[, decadeYear := 5 + (as.numeric(unlist(lapply(strsplit(historicalFiresDT$DECADE, split = "-"), `[[`, 1))))]
  historicalFiresDT[, fireYear := ifelse(YEAR == -9999, decadeYear, YEAR)]
  historicalFiresS$fireYear <- historicalFiresDT$fireYear
  historicalFires <- historicalFiresS[, "fireYear"]
  historicalFiresAll <- projectInputs(historicalFires, targetCRS = as.character(raster::crs(studyArea)))
  rgdal::writeOGR(obj = historicalFiresAll, dsn = dirname(historicalFiresAllPath), 
                  layer = basename(tools::file_path_sans_ext(historicalFiresAllPath)), 
                  driver = "ESRI Shapefile")
} else {
  historicalFiresAll <- raster::shapefile(historicalFiresAllPath)
}

########### Shortcut fires for caribou module END

modules <- list("caribouPopGrowth_disturbance", "caribouPopGrowthModel")
objects <- list("runName" = REP,
                "shortProvinceName" = "NT",
                "studyArea" = studyArea,
                "rasterToMatch" = rasterToMatch,
                "listSACaribou" = listSACaribou,
                "historicalFiresAll" = historicalFiresAll)
inputs <- list()
outputs <- list()

disturbanceList <- simInitAndSpades(times = times, 
                                    params = parameters, 
                                    modules = modules,
                                    objects = objects,
                                    outputs =  data.frame(objectName = c("disturbances",
                                                                         "predictedCaribou"#,
                                                                         # rep("bufferedAnthropogenicDisturbance500m",
                                                                             # times = length(seq(times$start, times$end, 
                                                                                                # by = parameters[["caribouPopGrowthModel"]][[".growthInterval"]])))
                                                                         ),
                                                          saveTime = c(rep(times$end, times = 2)#,
                                                                       # seq(times$start, times$end, 
                                                                           # by = parameters[["caribouPopGrowthModel"]][[".growthInterval"]])
                                                          ))
                                    )
