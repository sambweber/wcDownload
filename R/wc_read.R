# ------------------------------------------------------------------------------------------------------------
# wc_read_fastGPS
# ------------------------------------------------------------------------------------------------------------

#' Function for reading FastLoc GPS files downloaded from Wildlife Computers Portal

# -------------------------------------------------------------------------------------------------------------


wc_read_fastGPS = function(file=NULL,directory) {
  
  if(is.null(file)){
  file = list.files(directory,pattern = "\\-[0-9]-FastGPS.csv",full.names = T) %>% fs::path_wd
  }
  
  read.csv(file) %>%
  mutate(Date = parse_date_time(paste(Time,Day), c("HMS dbY", "HMOS dbY"),tz='GMT')) %>%
  subset(!is.na(Longitude)&!is.na(Latitude)) %>%
  arrange(Date) %>%
  st_as_sf(coords = c('Longitude','Latitude'),crs=4326)
  
}  


# ------------------------------------------------------------------------------------------------------------
# wc_read_locs
# ------------------------------------------------------------------------------------------------------------

#' Function for reading merged Argos/FastGPS location files downloaded from Wildlife Computers Portal

# -------------------------------------------------------------------------------------------------------------

wc_read_locs = function(file=NULL,directory) {
  
  if(is.null(file)){
  file = list.files(directory,pattern = "\\-[0-9]-Locations.csv",full.names = T) %>% fs::path_wd
  }
  
  read.csv(file) %>%
  mutate(Date = parse_date_time(Date, c("HMS dbY", "HMOS dbY"),tz='GMT')) %>%
  arrange(Date) %>%
  st_as_sf(coords = c('Longitude','Latitude'),crs=4326)
  
}  
