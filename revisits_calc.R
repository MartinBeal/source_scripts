## Functions to count number of revisitations to sites, where sites are buffer-areas around tracking points
# Useful in lieu of recurse:getRecursions when using e.g. only one point per trip 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wkvisits_pb: idnetifies sites usnig buffer-areas around tracking points and is
# useful when only one point per trip/track

# calculate dist matrix among all points
# identify which points were revisited first, and count number of revisits
# no sites should overlap (all overlaps are counted as revisits to the point visited first in the DT sequence)

wkvisits_pb <- function(dests, wks){
  
  wksvisitslist <- lapply(seq_along(wks), function(k){ # split by weeks
                            
    oneWK <- subset(dests, dests$rel_wk == wks[k])
    oneWK <- st_transform(oneWK, crs="+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    
    oneWK$site   <- rep(TRUE)
    oneWK$siteID <- rep(0)
    
    oneWKsplit <- split(oneWK, oneWK$ID)
    
    pts_buf_list <- do.call(rbind, 
                            lapply(seq_along(oneWKsplit), function(x){ # split by IDs
      
      one <- oneWKsplit[[x]]
      
      one$site   <- rep(TRUE)
      one$siteID <- rep(0)
      
      pts_buf <- sf::st_buffer(one, 500)
      coords  <- sf::st_coordinates(one)
      
      int <- sf::st_intersects(pts_buf, one)
      
      for(i in seq_along(int)){
        # identify which points are sites and which are revisits 
        pnts <- int[[i]]
        
        one$siteID[pnts] <- pnts[[1]]
        
        if(length(pnts > 1)){
          one$site[pnts[-1]] <- FALSE
        }
      }
      
      one$siteID <- as.numeric(as.factor(one$siteID))
      
      one <- one %>% group_by(siteID) %>% mutate(
        returntime = difftime(DateTime, first(DateTime), units = "hours"),
        returntime = ifelse(returntime == 0, NA, returntime)
      )
      
    }) )
  }) 
  return(wksvisitslist)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wkvisits_rcrs: idnetifies sites using recurse:getRecursions and therefore 
# useful at a higher resolution, when time spent within buffer/areas can be 
# calculated and is of interest

wkvisits_rcrs <- function(TD, wks){
  
  wkspropslist <- lapply(seq_along(wks), function(k){
    oneWK <- subset(TD, TD$rel_wk == wks[k])
    oneWK <- st_transform(oneWK, crs="+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
    # one <- st_transform(one, crs="+proj=utm +zone=34 +ellps=GRS80 +units=m +no_defs")
    
    oneWK <- oneWK %>% ungroup() %>% 
      rename(Latitude = X, Longitude = Y) %>% 
      bind_cols(as.data.frame(st_coordinates(oneWK))) %>% 
      dplyr::select(X, Y, DateTime, ID) %>% 
      st_drop_geometry() %>% as.data.frame()
    
    # Choose 'site' size (radius around points)
    visits <- getRecursions(oneWK, radius=500, threshold=2)  # all trip points

    return(visits$revisitStats)
    
  })
}

