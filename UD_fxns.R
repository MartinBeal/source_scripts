#### Custom functions for handling Utilization Distributions ####

## function for converting estUDm objects to RasterStacks
estUD2raster <- function(KDE){raster::stack(lapply(KDE, function(x) {
  raster::raster(as(x, "SpatialPixelsDataFrame"), values=TRUE)
} ))}


### fxn deriving contour area from Utilization Distribution (Raster*)
## for a RasterStack, fxn needs to run for each Layer in the Raster
## simple (T/F) indicates whether cells inside should be 1 or the % value
# outVal sets the value for cells outside the desired contour
ud2iso <- function(KDE, levelUD=100, simple, outVal=NA){
  
  KDElev <- KDE
  pixArea <- raster::res(KDElev)[1]
  
  if(simple==TRUE){
    # return cells w/in levelUD as 1 (equivalent to TRUE)
    df <- data.frame(UD = raster::getValues(KDElev)) %>%
      mutate(rowname = seq_len(length(raster::getValues(KDElev)))) %>%
      mutate(usage = .data$UD * (pixArea^2)) %>%
      arrange(desc(.data$usage)) %>%
      mutate(cumulUD = cumsum(.data$usage*100)) %>%
      mutate(INSIDE = ifelse(.data$cumulUD < (levelUD), 1, outVal)) %>%
      arrange(.data$rowname) %>%
      dplyr::select(.data$INSIDE)
    
  } else {
    # return % probability values within contour area (0-levelUD/NA)
    df <- data.frame(UD = raster::getValues(KDElev)) %>%
      mutate(rowname = seq_len(length(raster::getValues(KDElev)))) %>%
      mutate(usage = .data$UD * (pixArea^2)) %>%
      arrange(desc(.data$usage)) %>%
      mutate(cumulUD = cumsum(.data$usage*100)) %>%
      mutate(INSIDE = ifelse(.data$cumulUD < (levelUD), .data$cumulUD, outVal)) %>%
      arrange(.data$rowname) %>%
      dplyr::select(.data$INSIDE)
    
  }

  KDElev[] <- df$INSIDE
  return(KDElev)
}
