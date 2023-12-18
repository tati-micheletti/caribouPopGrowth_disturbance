# For caribouPopGrowth, create bufferedAnthropogenicDisturbance500m every time step 
# If there are no files corresponding to the specific point in time, just use the previous one.
# Steps:
# 1. Check if `disturbanceList` exists in sim. Maybe createModObject?
#    1.1. If it exists --> move to next function as is.  
#    1.2. If it doesn't exist in the sim, check if it exists in a folder:
#         1.2.1. If it doesn't exist: return the same default than caribouPopGrowth layer --> Make bufferedAnthropogenicDisturbance500m
#         1.2.2. If it exists: load all layers exactly as `disturbanceList`
# 2. Check if bufferedAnthropogenicDisturbance500m exists
#    2.1. If it doesn't yet exist: Put all shapefiles together: 
#        2.1.1. Buffer them to 500, convert to rasters 
#        2.1.2. add the other rasters with these
#        2.1.3. Name it bufferedAnthropogenicDisturbance500m
#    2.2. If it exists, skip this function
#    
#    THE END

