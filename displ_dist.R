#### Calculate displacement distance from first tracking locations for 
### multiple tracked individuals

## TD is tracking dataset. Must have columns:
# 'latitude', 'longitude', 'timestamp', 'id'

displ_dist <- function(TD){
  if(is.na(dist)){stop("What displacement distance should be used for filter?")}
  
  TD <- dplyr::arrange(TD, id, timestamp)
  
  trax_sf <- sf::st_as_sf(TD, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant")
  
  ## get appropriate projection
  proj <- crsuggest::suggest_crs(trax_sf, limit = 1, drop_na = T)
  
  ## convert for amt 
  trax_amt <- TD %>% 
    amt::make_track(.x=longitude, .y=latitude, .t=timestamp, 
               id = id, 
               crs = 4326, 
               all_cols = T) %>% 
    dplyr::arrange(id, t_)

  ## project data
  trax_amt_proj <- amt::transform_coords(
    trax_amt, crs_to=as.integer(proj$crs_code))
  
  ## loop net squared-displacement calculation
  alist <- list()
  for(i in 1:dplyr::n_distinct(trax_amt_proj$id)){
    print(i)
    one_proj <- subset(trax_amt_proj, id == unique(trax_amt_proj$id)[i])
    one <- subset(trax_amt, id == unique(trax_amt$id)[i])
    one$nsd <- amt::nsd(one_proj)
    alist[[i]] <- one 
  }
  
  trax_amt <- do.call(rbind, alist)
  
  TD$disp_km <- sqrt(trax_amt$nsd) / 1000 # convert to km
  return(TD)
}


