# ------------------------------------------------------------------------------------------------------------
# wc_read_fastGPS
# ------------------------------------------------------------------------------------------------------------

#' Function for reading FastLoc GPS files downloaded from Wildlife Computers Portal

# -------------------------------------------------------------------------------------------------------------


wc_read_fastGPS = function(file=NULL,directory,max.residual = 35) {
  
  if(is.null(file)){
  file = list.files(directory,pattern = "\\-[0-9]-FastGPS.csv",full.names = T) 
  }
  
  read.csv(file) %>%
  mutate(Date = parse_date_time(paste(Time,Day), c("HMS dbY", "HMOS dbY", "HMS dby", "HMOS dby"),tz='GMT')) %>%
  subset(!is.na(Longitude)&!is.na(Latitude)&Residual<max.residual) %>%
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
  file = list.files(directory,pattern = "\\-[0-9]-Locations.csv",full.names = T)
  }
  
  read.csv(file) %>%
  mutate(Date = parse_date_time(Date, c("HMS dbY", "HMOS dbY"),tz='GMT')) %>%
  arrange(Date) %>%
  st_as_sf(coords = c('Longitude','Latitude'),crs=4326)
  
}  

# ------------------------------------------------------------------------------------------------------------
# wc_read_all
# -------------------------------------------------------------------------------------------------------------

wc_read_all = function(directory) {
  
  locs_file = wc_read_locs(directory) %>%
              subset(type!='FastGPS')
  
  gps_file = wc_read_fastGPS(directory) %>%
             mutate(Type = 'FastGPS',Ptt = unique(locs_file$Ptt),Instr = unique(locs_file$Instr)) %>%
             dplyr::select(DeployID = Name, Type, Quality = Satellites, Latitude, Longitude)
  
  bind_rows(locs_file,gps_file) %>%
  arrange(Date)
  
}  

# ------------------------------------------------------------------------------------------
# wc_read_DDN
# ------------------------------------------------------------------------------------------

wc_read_DDN = function(directory) {

  f = list.files(directory,pattern = "\\DDN.csv",full.names = T)
  
  if(length(f)){
   read.csv(f) %>% tibble() %>% mutate(Date = as.POSIXct(Date,format = '%H:%M:%S %d-%b-%Y',tz='GMT'))
  } else stop('Cannot find DDN.csv file in the directory provided')

}

# ------------------------------------------------------------------------------------------
# wc_haulouts
# ------------------------------------------------------------------------------------------
devtools::source_url("https://github.com/sambweber/track_tools/blob/main/R/daynight.R?raw=TRUE")

wc_haulouts = function(directory, event.gap, daynight = T){

   ddn = wc_read_DDN(directory) %>% subset(Disposition == 'dry',select='Date')
   gps = wc_read_fastGPS(directory=directory) %>% subset(Hauled.Out == 1,select='Date')
  
   haulouts = 
      mutate(gps,GPS=TRUE,dry=floor_date(Date,unit = 'hours') %in% ddn$Date) %>% bind_rows(
      subset(ddn,!Date %in% floor_date(gps$Date,unit = 'hours')) %>% mutate(dry=TRUE,GPS=FALSE)
      ) %>% arrange(Date)
  
  if(!missing(event.gap)){
  haulouts = mutate(haulouts,lag = Date - lag(Date)) %>%
             mutate(event = cumsum(lag > duration(event.gap) | is.na(lag))) %>%
             dplyr::select(-lag)
  }
  
  if(daynight){
     empty = st_is_empty(haulouts) # Replace empty geoms from DDN with centroids of gps haulouts
     st_geometry(haulouts[empty,]) <- rep(st_centroid(st_combine(haulouts[!empty,])),sum(empty))
     haulouts = daynight(haulouts,'Date')
   }
  
  return(haulouts)
  
}
