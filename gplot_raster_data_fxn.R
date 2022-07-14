## Function that converts Raster* data to tibble for use in ggplot2 using geom_tile()
## Improves on gplot() by allowing for stacking rasters and polygons (sf)
# Written by Sebastien Rochette (github: https://github.com/statnmap/cartomisc,
# found on SO: https://stackoverflow.com/questions/47116217/overlay-raster-layer-on-map-in-ggplot2-in-r)

gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x))) 
  names(dat) <- c('value', 'variable')
  
  dat <- tibble::as_tibble(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]], 
                            by = c("value" = "ID"))
  }
  dat
}
