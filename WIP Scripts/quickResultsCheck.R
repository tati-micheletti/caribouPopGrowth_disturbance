# Another check of results

# One table disturbances per herd, polygon and year
DT <- rbindlist(lapply(names(disturbances_year2041), function(YEAR){
  DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]]), function(HERD){
    DT <- rbindlist(lapply(names(disturbances_year2041[[YEAR]][[HERD]]), function(POLY){
      dt <- cbind(data.table(HERD = HERD,
                             POLY = POLY,
                             YEAR = YEAR), 
                  as.data.table(disturbances_year2041[[YEAR]][[HERD]][[POLY]]))
      return(dt)
    }))
  }))
}))

# Making a disturbance map
fls <- list.files("~/projects/caribouPopGrowth_disturbance/outputs/run1/", pattern = "500m", full.names = TRUE)
fls2 <- fls[c(1,3,5,7)]
allDist <- raster::stack(lapply(rev(fls2), raster))

tempRas <- allDist[[1]]
tempRas[!is.na(tempRas)] <- 0

tempRas[allDist[[1]] == 1] <- 1
tempRas[allDist[[2]] == 1] <- 2
tempRas[allDist[[3]] == 1] <- 3
tempRas[allDist[[4]] == 1] <- 4

r <- tempRas
r <- as.factor(r)
rat <- levels(r)[[1]]
rat[["YearDisturbance"]] <- c("no disturbance","2041", "2031","2021", "up to 2011")
levels(r) <- rat

library(raster)
library(rasterVis)
levelplot(r, col.regions = c("lightgrey", "yellow", "orange", "red", "purple"), xlab="", ylab="")

# % Disturbance across the whole area
Dist <- data.table(table(r[]))
Dist[, totalPix := sum(N)]
Dist[, Year := c(0, 2041, 2031, 2021, 2011)]
Dist[, newPercDist := (N/totalPix)*100]
Dist2 <- Dist[2:5, ]
Dist2[, V1 := NULL]
names(Dist2)[names(Dist2) == "N"] <- "disturbedPix"
setkey(Dist2, "Year")
Dist2[, cummDisturbance := cumsum(newPercDist)]

# Extract disturbances and changes across polygons

library("googledrive")
fls <- list.files(path = "~/projects/caribouPopGrowth_disturbance/outputs/run1/", pattern = ".tif", full.names = T)
fls2 <- list.files(path = "~/projects/caribouPopGrowth_disturbance/outputs/run1/", pattern = ".rds", full.names = T)
fls <- c(fls, fls2)
lapply(fls, drive_upload, path = as_id("1C68BZU6eMF1FaXCOsyVnO8BVBqEsbElB"))