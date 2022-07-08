# This script prepares the haul-level data by adding in covariate data,
# error checking, adding projected coordinates, and removing the 
# At-Sea Tribal fishery data. 
#
{
   # Inputs: 
   #  - 'data/from_Kate/ashop_haul_info.rds'
   #  - 'data/from_Kate/observed_shoreside_hake_haul_info.rds'
   #  - 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9.tif'
   #      Downloaded from: https://www.gebco.net/
   #  - 'data/GIS/sst/...'
   #      SST data for all applicable years from: 
   #      https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html#detail
   #      saved with original file names into 'data/GIS/sst' folder
   #  - 'data/covariates/CUTI_daily.csv'
   #       Downloaded from: http://mjacox.com/upwelling-indices/
   #  - 
   
   
   # Outputs: 
   #  - 'data/ashop.rds'
   #       reorganized ASHOP data.
   #  - 'data/ashop_sf.RDS'
   #       same as 'data/ashop.rds', but converted to a "simple feature" object.
   #  - 'data/wcgop.rds'
   #       reorganized WCGOP data
   #  - 'data/wcgop_sf.RDS'
   #       same as 'data/wcgop.rds', but converted to a "simple feature" object.
   #  - 'data/hauls.rds'
   #       combination of 'data/ashop.rds' and 'data/wcgop.rds'
   #  - 'data/hauls_sf.rds'
   #       same as 'data/hauls.rds', but converted to a "simple feature" object.
   #  - 'data/ne_states.rds'
   #       polygon outlines of the states of North America from Natural Earth. 
   #  - 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9_slope.tif'
   #       slope calculated from the corresponding gebco bathymetry data.
   #  - 'data/GIS/noaa_crm_bathymetry_vol7.nc'
   #  - 'data/GIS/noaa_crm_bathymetry_vol8.nc'
   #      Both CRM bathymetry files downloaded from: 
   #      https://www.ngdc.noaa.gov/mgg/coastal/crm.html
   #  - 'data/GIS/crm_bathymetry_orig_extent.tif'
   #      Merged raster combining the 2 CRM bathymetry rasters
   #  - 'data/GIS/crm_bathymetry.tif'
   #      Merged raster combining the 2 CRM bathymetry rasters 
   #  - 'data/GIS/crm_bathymetry_slope.tif'
   #      Slope calculated from the CRM bathymetry dataset
   #  - 'data/fdat.rds'
   #      intermediate dataset. Basically hauls.rds with covariates added in,
   #      but before most error checking. 
   #  - 'data/data_prepped.rds'
   #      This is the primary output of this script. Columns include: 
   #		- haul_id: (factor) unique identifying number
   #		- sector: (factor) fishing sector
   #		- lat: (numeric) projected & transformed latitude (units = km)
   #		- lon: (numeric) projected & transformed longitude (units = km)
   #       NOTE! I use USA Contiguous Equidistant Conic (ESRI:102005; EPSG:102005) 
   #    for projecting coordinates. In our part of the world, latitude ends up
   #    being largely horizontal rather than vertical. 
   #		- latitude: (numeric) unprojected latitude as reported by boats
   #		- longitude: (numeric) unprojected longitude as reported by boats
   #		- lat_proj: (numeric) latitude projected to USA Contiguous Equidistant Conic
   #		- lon_proj: (numeric) longitude projected to USA Contiguous Equidistant Conic
   #		- datetime: (POSIXct) haul date and time
   #		- date: (Date) haul date
   #		- year: (integer) haul year
   #		- year_f: (factor) haul year
   #		- month: (integer) haul month
   #		- doy: (integer) haul day of year
   #		- ToD: (numeric) haul time of day (hours from midnight)
   #		- duration: (numeric) haul duration in minutes
   #		- hake_mt: (numeric) metric tons of hake
   #		- chinook_n: (numeric) number of Chinook bycatch
   #		- chinook_kg: (numeric) kg of Chinook bycatch
   #		- source: (factor) data source (either ASHOP or WCGOP)
   #		- sst: (numeric) Sea surface temperature (degrees C)
   #		- sst_lag_7: (numeric) Sea surface temperature (degrees C) 7 days before haul
   #		- sst_lag_365: (numeric) Sea surface temperature (degrees C) 365 days before haul
   #		- sst_anomaly: (numeric) Sea surface temperature anomaly (degrees C)
   #		- sst_anom_lag_7: (numeric) Sea surface temperature anomaly (degrees C) 7 days before haul
   #		- sst_anom_lag_365: (numeric) Sea surface temperature anomaly (degrees C)365 days before haul
   #		- cuti: (numeric) Coastal Upwelling Transport Index (units = volume of vertical transport per second per meter of coastline)
   #		- cuti_lag_7: (numeric) Coastal Upwelling Transport Index (units = volume of vertical transport per second per meter of coastline) 7 days before haul
   #		- cuti_lag_365: (numeric) Coastal Upwelling Transport Index (units = volume of vertical transport per second per meter of coastline) 365 days before haul
   #		- depth_fishing: (numeric) fishing depth reported by boats (units = meters)
   #		- depth_bottom: (numeric) bottom depth  reported by boats (from CRM for boats not reporting depths) (units = meters)
   #		- slope_bottom: (numeric) bottom slope calculated from CRM (from GEBCO for (1?) haul that does not have CRM depths)
   #  - 'data/data_prepped_sf.rds'
   #       same as 'data/data_prepped.rds', but converted to a "simple feature" object.
   #  - 'data/prediction_grid_list.rds'
   #       contains 2 dataframes with "newdata" for predicting models to make maps of predictions. 
   #       1) grid1 has covariate values from a particular date
   #           - Particular values:
   #       	    - sector == 'Catcher-Processor' (most hauls of any sector)
   #          	 	- year == 2015 (just picked a year)
   #          		- doy == 141  (most common day)
   #           	- ToD == 7 am (most common time of day)
   #       	- Average values for: 
   #           	- duration
   #           	- depth_fishing (but must be < depth_bottom)
   #       	- Date &/or location-specific covariate values
   #           	- depth_bottom
   #           	- slope_bottom
   #           	- sst_anomaly / sst_anom_lag_7 / sst_anom_lag_365
   #           	- cuti / cuti_lag_7 / cuti_lag_365     
   #    	2) Yearly grids of covariate values to map (only) spatial component of the model
   #       	 - all covariates held at mean values (even when that's unrealistic (e.g. bottom depth isn't really constant through space))
   #          		- sector == 'Catcher-Processor' (most hauls of any sector)
   #           	- year == 2002:2019
   #           	- doy == 141  (most common day)
   #           	- ToD == 7 am (most common time of day)
   #           	- duration == average value
   #           	- depth_fishing == average value
   #           	- depth_bottom  == average value
   #           	- slope_bottom  == average value
   #           	- sst_anomaly / sst_anom_lag_7 / sst_anom_lag_365 == average value
   #           	- cuti / cuti_lag_7 / cuti_lag_365  = average value
}

# library(tidyverse)
# library(sf)
# library(rnaturalearth)
# library(raster)
# library(janitor)
# library(DT)
# library(leaflet)
# library(leafem)
# library(pbapply)
# library(skimr)
# library(htmltools)
# library(sp)
# library(rgdal)
# library(ncdf4)
# other libraries that must be installed
#     sf, rnaturalearth, raster, janitor, DT, leaflet, leafem, pbapply, skimr, janitor
# other packages that are called, but are probably installed with above packages
#    htmltools, SP

#Location where data is stored

in_drive <- "X:/Input/Richerson/salmon_bycatch_project/"

# Load data
{
   # Load ASHOP data
   {
      # load ASHOP data: 
      ashop0 <- readRDS(file = paste0(in_drive, 'data/from_Kate/ashop_haul_info_20200507.rds')) %>% 
         dplyr::ungroup()
      
      # look at the structure
      # str(ashop0)
      
      # make sure that the hauls are unique
      dplyr::n_distinct(ashop0$unique_haul_id)
      nrow(ashop0)
      
      # with_tz CONVERTS the time to a different timezone
      min( lubridate::with_tz(ashop0$deployment_date, tzone = 'America/Los_Angeles'))
      # force_tz keeps the same hour of the day and just says that it belongs in a different timezone.
      min( lubridate::force_tz(ashop0$deployment_date, tzone = 'America/Los_Angeles'))
      # This makes it look like I should just be converting the timezone, not forcing it. 
      #   (unless the fishing season really opens at 8am, not at midnight)
      #   But if I convert rather than force, most hauls end up occurring between 
      #   11pm and 2 pm, with a decrease from 2 pm to 11 pm. It's basically 7 hours
      #   off. (7 hours is the difference between UTC and PDT)
      #   So I'll use force_tz so that the bulk of the hauls are in the middle of
      #   the day. 
      
      # rename and organize a little differently
      ashop <- ashop0 %>% 
         dplyr::arrange(deployment_date) %>% 
         dplyr::mutate(haul_id = unique_haul_id,
                       sector  = factor( stringr::str_to_title(as.character(sector)) ),
                       lat     = avg_lat,
                       lon     = avg_long,
                       # datetime= deployment_date,
                       # force datetime to the PST8PDT timezone
                       datetime= lubridate::force_tz(time = deployment_date, 
                                                     tzone = 'America/Los_Angeles'),
                       date    = as.Date(datetime, tz = 'America/Los_Angeles'), 
                       # Need to specify the timezone again (https://lukemiller.org/index.php/2019/05/r-as-date-and-time-zones/) could also use: as.Date(format(datetime, format = '%Y-%m-%d'))
                       # even though "Date" objects do *not* store timezone info; it's just used for converting POSIXct to date format b/c as.Date *IGNORES TIMEZONE INFORMATION STORED IN THE POSIXCT OBJECT!*
                       year    = as.integer(format(datetime, '%Y')),
                       month   = as.integer(format(datetime, '%m')),
                       doy     = as.integer(format(datetime, '%j')),
                       ToD     = as.numeric(difftime(datetime,  as.character(date) , units = 'hours')),
                       # because Date objects do NOT store timezone info, they are treated as always belonging to UTC. 
                       # To change the timezone of a Date object, I need to convert it to character (and then optionally to PosixCT)
                       duration = duration_in_min,
                       fishing_depth_f = fishing_depth_fathoms,
                       fishing_depth_m = fishing_depth_f * 1.8288,
                       bottom_depth_f  = bottom_depth_fathoms,
                       bottom_depth_m  = bottom_depth_f * 1.8288,
                       hake_mt    = retained_hake_mt,
                       chinook_n  = chinook_count,
                       chinook_kg = chinook_weight_kg) %>% 
         dplyr::select(haul_id,
                       sector,
                       lat,
                       lon,
                       datetime,
                       date,
                       year,
                       month,
                       doy,
                       ToD,
                       duration,
                       fishing_depth_f,
                       fishing_depth_m,
                       bottom_depth_f,
                       bottom_depth_m,
                       hake_mt,
                       chinook_n,
                       chinook_kg)
      
      # min(ashop$datetime)
      # min(ashop$date)
      # min(ashop$ToD)
      # 
      # ggplot(ashop, aes(x = ToD)) + geom_histogram(bins = 48)
      # ggplot(ashop %>% filter(year > 2010), aes(x = ToD)) + geom_histogram(bins = 48) #  + scale_x_datetime(date_labels = "%H:%M", expand = c(0,0))
      
      # Save the re-organized ashop data
      saveRDS(object = ashop, file = paste0(in_drive, 'data/ashop.rds'))
      
      # also convert to a "simple feature" dataset for mapping
      ashop_sf <- sf::st_as_sf(ashop, 
                               coords = c('lon', 'lat'), 
                               crs = 4326, # this is the standard CRS for GPS data
                               remove = F) # this prevents the removal of the "lon" and "lat" columns
      # save it to file
      saveRDS(obj = ashop_sf, file = paste0(in_drive, 'data/ashop_sf.RDS'))
   }
   
   # Load WCGOP data
   {
      # Load WCGOP data: 
      wcgop0 <- readRDS(file = paste0(in_drive, 'data/from_Kate/observed_shoreside_hake_haul_info_20210507.rds')) %>% 
         dplyr::ungroup() # make sure it's NOT grouped anymore
      
      # look at the structure of the data
      str(wcgop0)
      
      # make sure that the hauls are unique
      dplyr::n_distinct(wcgop0$unique_haul_id)
      nrow(wcgop0)
      sum(duplicated(wcgop0$unique_haul_id))
      # "duplicated" doesn't count the first instance as a duplicate!
      nrow(wcgop0) - dplyr::n_distinct(wcgop0$unique_haul_id)
      sum(is.na(wcgop0$unique_haul_id))
      # which ones are duplicated?
      dups1 <- wcgop0 %>%
         dplyr::add_count(unique_haul_id) %>%
         dplyr::filter(n > 1)
      # this gives 64 rows instead of 63 b/c it includes the first instance of NA
      dups2 <- wcgop0[duplicated(wcgop0$unique_haul_id),]
      
      # it seems that the duplicates are all NA's
      wcgop0 %>%
         dplyr::add_count(unique_haul_id) %>%
         dplyr::filter(n > 1) %>%
         dplyr::summarize(
            haul_duration_hrs = paste(unique(haul_duration_hrs)),
            fishing_depth_fathoms = paste(unique(fishing_depth_fathoms)),
            deployment_date   = paste(unique(deployment_date)),
            chinook_count     = paste(unique(chinook_count)),
            chinook_weight_kg = paste(unique(chinook_weight_kg)),
            retained_hake_mt = paste(unique(retained_hake_mt)))
      
      # rename and organize a little differently
      wcgop <- wcgop0 %>% 
         dplyr::ungroup() %>% 
         dplyr::filter(!is.na(unique_haul_id)) %>% 
         dplyr::arrange(deployment_date) %>% 
         dplyr::mutate(haul_id = unique_haul_id,
                       sector  = factor('Shoreside'), # "Shoreside Hake" and "Midwater Hake" are the same, so I'll just use one name
                       lat     = avg_lat,
                       lon     = avg_long,
                       # datetime= deployment_date,
                       # force datetime to the PST8PDT timezone
                       datetime= lubridate::force_tz(time = deployment_date, 
                                                     tzone = 'America/Los_Angeles'),
                       date    = as.Date(datetime, tz = 'America/Los_Angeles'), 
                       # Need to specify the timezone again (https://lukemiller.org/index.php/2019/05/r-as-date-and-time-zones/) could also use: as.Date(format(datetime, format = '%Y-%m-%d'))
                       # even though "Date" objects do *not* store timezone info; it's just used for converting POSIXct to date format b/c as.Date *IGNORES TIMEZONE INFORMATION STORED IN THE POSIXCT OBJECT!*
                       year    = as.integer(format(datetime, '%Y')),
                       month   = as.integer(format(datetime, '%m')),
                       doy     = as.integer(format(datetime, '%j')),
                       ToD     = as.numeric(difftime(datetime,  as.character(date) , units = 'hours')),
                       # because Date objects do NOT store timezone info, they are treated as always belonging to UTC. 
                       # To change the timezone of a Date object, I need to convert it to character (and then optionally to PosixCT)
                       duration = haul_duration_hrs * 60,
                       fishing_depth_f = fishing_depth_fathoms,
                       fishing_depth_m = fishing_depth_f * 1.8288,
                       bottom_depth_f  = NA_real_,
                       bottom_depth_m  = NA_real_,
                       hake_mt    = retained_hake_mt,
                       chinook_n  = chinook_count,
                       chinook_kg = chinook_weight_kg) %>% 
         dplyr::select(haul_id,
                       sector,
                       lat,
                       lon,
                       datetime,
                       date,
                       year,
                       month,
                       doy,
                       ToD,
                       duration,
                       fishing_depth_f,
                       fishing_depth_m,
                       bottom_depth_f,
                       bottom_depth_m,
                       hake_mt,
                       chinook_n,
                       chinook_kg)
      
      # ggplot(wcgop, aes(x = ToD)) + geom_histogram(bins = 48) + scale_x_datetime(date_labels = "%H:%M", expand = c(0,0))
      
      # save the WCGOP dataset
      saveRDS(object = wcgop, file = paste0(in_drive, 'data/wcgop.rds'))
      
      # also convert to a "simple feature" dataset for mapping
      wcgop_sf <- sf::st_as_sf(wcgop,
                               coords = c('lon', 'lat'), 
                               crs = 4326, # this is the standard CRS for GPS data
                               remove = F) # this prevents the removal of the "lon" and "lat" columns
      
      # save it to file
      saveRDS(obj = wcgop_sf, file = paste0(in_drive, 'data/wcgop_sf.RDS'))
   }
   
   # Combine ASHOP & WCGOP datasets
   {
      # combine the datasets
      dat <- rbind(ashop %>% 
                      dplyr::mutate(source = 'ASHOP'), 
                   wcgop %>% 
                      dplyr::mutate(source = 'WCGOP')) %>% 
         dplyr::mutate(haul_id = factor(haul_id),
                       source = factor(source))
      
      # plot when they occur
      # ggplot(dat, aes(x = ToD)) + geom_histogram(bins = 48) + scale_x_datetime(date_labels = "%H:%M", expand = c(0,0))
      
      # save to file
      saveRDS(object = dat, file = paste0(in_drive, 'data/hauls.rds'))
      
      # also convert to a "simple feature" dataset for mapping
      dat_sf <- sf::st_as_sf(dat,
                             coords = c('lon', 'lat'), 
                             crs = 4326, # this is the standard CRS for GPS data
                             remove = F) # this prevents the removal of the "lon" and "lat" columns
      saveRDS(object = dat_sf, file = paste0(in_drive, 'data/hauls_sf.rds'))
   }
   
   # Find hauls over land
   {
      # rnaturalearth has something going on. If you've already loaded raster or 
      # perhaps some other package, it won't work. If you've only loaded tidyverse
      # it will work. If the downloads below don't work, just restart R and try again.
      # see: https://github.com/ropensci/rnaturalearth/issues/29
      
      # download data on coastline from natural earth
      # get the states of North America (download it if I haven't saved it already)
      if(file.exists(paste0(in_drive, 'data/ne_states.rds'))){
         states <- readRDS(file = paste0(in_drive, 'data/ne_states.rds'))
      } else {
         states  <- rnaturalearth::ne_download(scale = 'large',
                                               type = 'states', 
                                               category = 'cultural', 
                                               returnclass = 'sf') %>% 
            subset(., admin %in% c('United States of America', 'Canada', 'Mexico'))  %>% 
            sf::st_transform(x = ., crs = 4326) # %>% sf::st_shift_longitude()
         
         saveRDS(object = states, file = paste0(in_drive, 'data/ne_states.rds'))
      }
      
      # check to see which hauls are over land: 
      if( !('over_land' %in% names(dat_sf))){
         dat_sf <- sf::st_join(dat_sf, states) %>% 
            dplyr::mutate(over_land = dplyr::if_else(condition = is.na(postal), 
                                                     true = 'ocean', 
                                                     false = paste0('land (', postal, ')'))) %>% 
            dplyr::select(names(dat_sf), over_land)
      }
      
      # summary of over-landness
      dat_sf %>% 
         janitor::tabyl(over_land) %>% 
         janitor::adorn_pct_formatting(digits = 4) %>%
         DT::datatable(., 
                       options = list(
                          columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      
      # plot of those over land
      ggplot() + 
         theme_classic() + 
         geom_sf(data = states) + 
         geom_sf(data = dat_sf) + 
         geom_sf(data = dat_sf %>% dplyr::filter(over_land != 'ocean'), 
                 size = 2, 
                 color = 'red') + 
         coord_sf(xlim = range(dat_sf$lon), 
                  ylim = range(dat_sf$lat))
      
      # What would happen if I moved the hauls over land??
      {
         # ID the hauls to move
         removed <- dat_sf %>% 
            dplyr::mutate(x_over_land = if_else( over_land != 'ocean',
                                                 T,
                                                 F),
                          # add in 3 hauls that are way into the Straight of Juan de Fuca
                          x_over_land = if_else( lon > -124.3 & lat > 48.1, #  haul_id %in% c('911629241112004', '911629041112004'), 
                                                 T, 
                                                 x_over_land),
                          # add in a haul that is INSIDE Grey's Harbor
                          x_over_land = if_else( haul_id %in% c('100827'), #  haul_id %in% c('911629241112004', '911629041112004'), 
                                                 T, 
                                                 x_over_land),
                          bycatch = if_else(chinook_n > 0, 'bycatch', 'none')) %>% 
            dplyr::filter(x_over_land)
         
         # haul 98125538352005 needs to go SOUTH, not WEST
         # both old and new
         moved <- rbind(removed %>% 
                           dplyr::mutate(location = 'original'),
                        removed %>% 
                           as_tibble() %>% 
                           dplyr::mutate(lon = if_else(haul_id == '98125538352005',
                                                       lon,
                                                       lon - 1),
                                         lat = if_else(haul_id == '98125538352005',
                                                       lat - 1,
                                                       lat),
                                         location = 'altered') %>% 
                           sf::st_as_sf(.,
                                        coords = c('lon', 'lat'), 
                                        crs = 4326, 
                                        remove = F) %>% 
                           dplyr::select(c(names(removed), 'location')))
         
         # create lines to show where hauls are moving from/to
         lapply(X = 1:nrow(removed), FUN = function(i){
            sf::st_linestring(x = matrix(data = c(removed[i,] %>% pull('lon'),
                                                  removed[i,] %>% pull('lat'),
                                                  ifelse(removed[i,] %>% pull('haul_id') == '98125538352005',
                                                         removed[i,] %>% pull('lon'),
                                                         removed[i,] %>% pull('lon')-1),
                                                  ifelse(removed[i,] %>% pull('haul_id') == '98125538352005',
                                                         removed[i,] %>% pull('lat')-1,
                                                         removed[i,] %>% pull('lat'))), 
                                         nrow = 2, 
                                         ncol = 2, 
                                         byrow = T))
         }) %>% 
            sf::st_sfc(.) %>% 
            sf::st_sf() -> lines
         
         ##stopped
         
         # define a color palatte for the map
         palexm <- leaflet::colorFactor(scales::hue_pal()(length(unique(removed$haul_id))),
                                        domain = unique(removed$haul_id))
         
         # create a map showing where hauls are moved from & to
         lfexm <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%  # "preferCanvas = TRUE" is necessary for lots of datapoints
            # background data in grey
            leaflet::addCircleMarkers(data = dat_sf,
                                      color = 'grey',
                                      stroke = FALSE,
                                      fillOpacity = .1,
                                      group = 'All Hauls',
                                      popup = ~htmltools::htmlEscape(haul_id)) %>%
            
            # lines connecting original and altered locations
            leaflet::addPolylines(data = lines,
                                  color = 'black') %>% 
            # original locations
            leaflet::addCircleMarkers(data = moved %>% 
                                         dplyr::filter(location == 'original'),
                                      fillColor = ~palexm(haul_id),
                                      fillOpacity = .5,
                                      stroke = FALSE,
                                      group = 'Original location',
                                      popup = ~htmltools::htmlEscape(haul_id)) %>% 
            # altered locations
            leaflet::addCircleMarkers(data = moved %>% 
                                         dplyr::filter(location == 'altered'),
                                      fillColor = ~palexm(haul_id),
                                      fillOpacity = .9,
                                      stroke = FALSE,
                                      group = 'Altered location',
                                      popup = ~htmltools::htmlEscape(haul_id)) %>% 
            # Layers control
            leaflet::addLayersControl(
               overlayGroups = c( #'All Hauls',
                  'Bycatch',
                  'No Bycatch'),
               options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>% 
            leafem::addMouseCoordinates() %>% 
            leaflet::addLegend(position = 'topright', 
                               title  = 'Haul ID', # need HTML coding for line breaks, not R
                               pal    = palexm,
                               values = unique(removed$haul_id)) %>% 
            leaflet::setView(lng = -124.8, lat = 47.5, zoom = 8) %>% 
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) # https://stackoverflow.com/a/64094357/3174566 
         
         # view the map
         lfexm
         }
   }
   
   # move some hauls that are over land
   {
      # I'll move all the points over land to the west 1 degree. 
      # I'll move the 1 haul inside Grays Harbor west 1 degree. (haul_id == 100827)
      # I'll move the 3 hauls way into the Strait of Juan de Fuca west 1 degree.
      # I'll REmove the haul completely that's in Canada. 
      # I'll also use the average fishing depth for the 4 hauls that have fishing depths of 0. 
      #      None of those 4 hauls caught any hake or any Chinook, but they did have durations, so presumably they were trying. 
      
      # Some other suspicious hauls by location: 
      # 
      # Very close to shore: 
      #     77337129432002
      #     83806033962003
      #     96630
      #     138103
      #     100827
      #     95163
      #     118934
      #     
      # Far from other hauls
      #     214364 (further South)
      #     20552108840552015  (Southwest)
      #     20552108740552015  (Southwest)
      #     20552109940552015  (West)
      #     20551110436812015  (West)
      
      # what do the measured fishing & bottom depths look like on those hauls? (Is there evidence the locations are in the wrong place?)
      very_close <- c('77337129432002', '83806033962003', '96630', '138103', '100827', '95163', '118934')
      very_far <- c('214364', '20552108840552015', '20552108740552015', '20552109940552015', '20551110436812015')
      
      dat_sf %>% 
         dplyr::mutate(suspicious = if_else(haul_id %in% very_close, 'very close', 
                                            if_else(haul_id %in% very_far, 'very far', 'normal'))) %>% 
         dplyr::filter(haul_id %in% c(very_close, very_far)) %>% 
         dplyr::select(sector, suspicious, duration, fishing_depth_m, bottom_depth_m, hake_mt, chinook_n)
      
      
      
      # final alterations to the data
      fdat <- dat_sf %>% 
         as_tibble() %>% # remove the sf geometry info
         # Move points over land 1 degree to the west
         dplyr::mutate(
            # remove the point in Canada
            remove = dplyr::if_else(haul_id == '98125538352005',
                                    T, F),
            remove_reason = dplyr::if_else(haul_id == '98125538352005',
                                           'In Canada', 
                                           NA_character_),
            # indicator column to ID hauls to be moved
            move_west = dplyr::if_else( (over_land != 'ocean') | 
                                           (lon > -124.3 & lat > 48.1) | 
                                           (haul_id %in% c('100827')),
                                          T,F),
            # x_no_sst       = if_else( is.na(sst),
            #                           T,F),
            moved = if_else( (move_west) & !remove, 
                             T, F),
            lon_original = lon,
            # actually move them west.
            lon = if_else(moved,
                          lon - 1,
                          lon)) %>% 
         # add in the sf geometry info again
         sf::st_as_sf(.,
                      coords = c('lon', 'lat'), 
                      crs = 4326, 
                      remove = F)
   }
}

#save(fdat, file = "fdat.Rdata")

# Add in covariates (this can take a while to run)
{
   # covariates to add in:
   # Bathymetry (from GECBO (https://www.gebco.net/) and NOAA CRM (https://www.ngdc.noaa.gov/mgg/coastal/crm.html))
   # - bottom_depth_m_GEBCO
   # - bottom_slope_GEBCO
   # - bottom_depth_m_CRM
   # - bottom_slope_CRM
   # Sea Surface Temperature (from https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html#detail)
   # - sst
   # - sst_lag_7
   # - sst_lag_365
   # - sst_anomaly
   # - sst_anomaly_lag_7
   # - sst_anomaly_lag_365
   # Upwelling index (from http://mjacox.com/upwelling-indices/)
   # - cuti
   
   # Bathymetry & Slope
   if( !( 'bottom_depth_m_CRM' %in% names(fdat)) ){
      # GEBCO
      {
         # read in the bathymetry data
         gb_r <- raster::raster(x = paste0(in_drive, 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9.tif'))
         # gb_r
         # plot(gb_r)
         
         # extract point values using bilinear interpolation
         fdat$bottom_depth_m_GEBCO <- -1* raster::extract(x = gb_r,
                                                          y = fdat, 
                                                          method = 'bilinear')
         # Multiply by -1 b/c GEBCO represents depths as negative values, but 
         # the boat data represents depths as positive values. I'll stick with
         # positive values for all depths. 
         
         # Calculate the slope 
         # "The elevation data should be in map units (typically meter) for projected 
         #  (planar) raster data. They should be in meters when the coordinate 
         #  reference system (CRS) is longitude/latitude."
         # GEBCO elevations are in meters, so this should work as-is
         raster::terrain(x = gb_r, 
                         opt = 'slope', 
                         filename = paste0(in_drive, 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9_slope.tif'), 
                         overwrite = T)
         
         gbslope_r <- raster::raster(x = paste0(in_drive, 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9_slope.tif'))
         # raster::plot(gbslope_r)
         
         # extract values from the raster
         fdat$bottom_slope_GEBCO <- raster::extract(x = gbslope_r, 
                                                      y = fdat, 
                                                      method = 'bilinear')
      }
      
      # NOAA Coastal Relief Model
      {
         # Prepare the CRM data
         {
            # download the files from: https://www.ngdc.noaa.gov/mgg/coastal/crm.html
            # file name of the NOAA Coastal Relief Model bathymetry data for Vol 7 (Central Pacific)
            crm7_filename <- paste0(in_drive, 'data/GIS/noaa_crm_bathymetry_vol7.nc')
            if( !file.exists(crm7_filename) ){
               download.file(url = 'https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol7.nc', 
                             destfile = crm7_filename, 
                             mode = 'wb') # need to make sure to write the file in binary, or it won't open
            }
            crm8_filename <- paste0(in_drive, 'data/GIS/noaa_crm_bathymetry_vol8.nc')
            if( !file.exists(crm8_filename) ){
               download.file(url = 'https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol8.nc', 
                             destfile = crm8_filename,
                             mode = 'wb') # need to make sure to write the file in binary, or it won't open
            }
            
            # read in the netCDF files
            # bath7.cdf <- ncdf4::nc_open(crm7_filename)
            # bath8.cdf <- ncdf4::nc_open(crm8_filename)
            
            # read the NetCDF files directly into a raster
            bath7.r <- raster::raster(x = crm7_filename)
            bath7.r
            bath8.r <- raster::raster(x = crm8_filename)
            bath8.r
            
            # do the rasters intersect? 
            raster::intersect(raster::extent(bath7.r), 
                              raster::extent(bath8.r))
            
            # not enough to matter. 
            # Where they overlap, I'll just take the values from the first raster 
            #  (which is the default behaviour of raster::merge)
            #bath0 <- raster::merge(x = bath7.r, bath8.r)
            #KR note: When I plot the raster generated here, it looks like something went wrong, and I'm not sure why. Instead, I generated the same raster using the gstg code and save it, then loading it here. 
            
            bath0 <- readRDS("X:/Output/Richerson reports/2019/Green sturgeon/bath0.rds")
            
            # plot the new raster
            # raster::plot(bath0)
            
            # set the raster projection
            # https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ngdc.mgg.dem:348/html#
            # gives the CRS as: "urn:ogc:def:crs:EPSG::4269urn:ogc:def:crs:EPSG::5715"
            # https://spatialreference.org/ref/epsg/4269/
            # 4269 = horizontal CRS; proj4 = "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
            # https://spatialreference.org/ref/epsg/5715/
            # 5715 = vertical CRS; proj4 = ""
            raster::crs(bath0) <- sp::CRS('+init=EPSG:4269')
            
            # save the raster to file
            raster::writeRaster(x = bath0, 
                                filename = paste0(in_drive, 'data/GIS/crm_bathymetry_orig_extent.tif'), 
                                overwrite=T)
            
            # Read in the GEBCO Raster, which I'll use to make sure they have the same CRS
            # and then use it to crop the CRM raster to an appropriate extent
            cropper <- raster::raster(paste0(in_drive, 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9.tif'))
            cropper
            
            # make sure the rasters have the same CRS
            if( !isTRUE(all.equal(raster::crs(cropper), raster::crs(bath0))) ) {
               bath <- raster::projectRaster(from = bath0, 
                                             crs = raster::crs(cropper))
               # don't use the "to" argument b/c that will copy the extent AND RESOLUTION 
               # from the "to" raster, and I don't want to down-sample to the GEBCO resolution
            }
            raster::crs(bath0)
            raster::crs(bath)
            
            # The resulting CRM dataset is huge, but most of it is over land, 
            # so I'll set positive values (above sea level) to NA
            bath <- raster::reclassify(x = bath, 
                                       rcl = cbind('from'    = 0, 
                                                   'to'      = Inf, 
                                                   'becomes' = NA), 
                                       right = TRUE) # open on the left (so 0 is NOT converted to NA), but closed on the right (so Inf IS converted to NA)
            bath
            
            # crop the bathymetry dataset using the GEBCO dataset
            bath_c <- raster::crop(x = bath, y = cropper)
            bath_c
            
            # save cropped raster to file
            raster::writeRaster(x = bath_c, 
                                filename = paste0(in_drive, 'data/GIS/crm_bathymetry.tif'), 
                                overwrite = T)
            
            # I can't make the interactive plot b/c there's not enough memory for R to do that
            {
               # # plot an interactive map of the rasters to make sure they're doing what I want
               # # leaflet uses epgs:3857, so I need to project to that first 
               # # (leaflet will do it automatically, but it can screw up the color palate, so I'll do it first)
               # # to get rid of errors see: https://github.com/rstudio/leaflet/issues/224
               # bath_l <- leaflet::projectRasterForLeaflet(bath_c, method = 'bilinear')
               # # the warnings actually come from raster package: raster::projectRaster(from = bath, crs = sp::CRS('+init=EPSG:3857'))
               # 
               # # first create a color palatte function
               # rpal1 <- leaflet::colorNumeric(palette  = 'viridis', 
               #                                domain   = range(na.omit(raster::values(bath_l))), 
               #                                na.color = "transparent")
               # 
               # # then create the plot
               # plot_height_pix <- 900
               # lf01 <- leaflet::leaflet(height = plot_height_pix) %>% 
               #    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% # leaflet::addTiles() %>% 
               #    leaflet::addRasterImage(x = bath_l, 
               #                            maxBytes = 50000000, # up to (nearly) 50 mb
               #                            colors  = rpal1, 
               #                            project = FALSE, 
               #                            opacity = 0.9,
               #                            group = "Bath",
               #                            layerId = "Bath") %>% # it's already projected
               #    leafem::addImageQuery(x = bath_l, 
               #                          digits = 4,
               #                          position = 'topleft',
               #                          project = TRUE,
               #                          layerId = "Bath") %>%
               #    leafem::addMouseCoordinates() %>%
               #    leaflet::addLegend(position = 'topright', 
               #                       title = 'Bathymetry (m)', # need HTML coding for line breaks, not R
               #                       pal = rpal1, 
               #                       values = raster::values(bath_l))
               # lf01
            }
            
            # Calculate the slope 
            raster::terrain(x = bath_c, 
                            opt = 'slope', 
                            filename = paste0(in_drive, 'data/GIS/crm_bathymetry_slope.tif'), 
                            overwrite = T)
            # raster::plot(raster::raster('data/GIS/crm_bathymetry_slope.tif'))
            # compare to GEBCO slope: 
            # GEBCO_slope <- raster::raster('data/GIS/slope_01_2020_n501_s399_w-1281_e-1229.tif')
            # range(raster::getValues(GEBCO_slope), na.rm = T)
            
            # Calculate the Terrain Ruggedness Index: 
            #   "The terrain indices are according to Wilson et al. (2007), as in gdaldem. 
            #    TRI (Terrain Ruggedness Index) is the mean of the absolute differences between 
            #    the value of a cell and the value of its 8 surrounding cells."
            
            # raster::terrain(x = bath_c, 
            #                 opt = 'TRI', 
            #                 filename = 'data/GIS/crm_bathymetry_TRI.tif', 
            #                 overwrite = T)
         }
         
         # read in the NOAA CRM bathymetry data
         cb_r <- raster::raster(x = paste0(in_drive,'data/GIS/crm_bathymetry.tif'))
         # cb_r
         # raster::plot(cb_r)
         
         # extract point values using bilinear interpolation
         fdat$bottom_depth_m_CRM <- (-1)*raster::extract(x = cb_r,
                                                           y = fdat, 
                                                           method = 'bilinear')
         # multiply by -1 b/c CRM (like GEBCO) represents depths as negative 
         # values, but the boat data represents depths as positive values. 
         
         # read in slope data
         cbslope_r <- raster::raster(x = paste0(in_drive, 'data/GIS/crm_bathymetry_slope.tif'))
         # cbslope_r
         # plot(cbslope_r)
         
         # extract values from the raster
         fdat$bottom_slope_CRM <- raster::extract(x = cbslope_r, 
                                                    y = fdat, 
                                                    method = 'bilinear')
      }
      
      # compare GEBCO & CRM data
      if(FALSE){
         # GEBCO vs CRM bathymetry
         ggplot(data = fdat %>% dplyr::mutate(depth_diff = bottom_depth_m_GEBCO - bottom_depth_m_CRM),
                mapping = aes(x = depth_diff)) +
            theme_bw() +
            geom_histogram(bins = 100)
         # GEBCO vs on-boat data
         ggplot(data = fdat %>% dplyr::mutate(depth_diff = bottom_depth_m_GEBCO - bottom_depth_m),
                mapping = aes(x = depth_diff)) +
            theme_bw() +
            geom_histogram(bins = 100)
         # CRM vs on-boat data
         ggplot(data = fdat %>% dplyr::mutate(depth_diff = bottom_depth_m_CRM - bottom_depth_m),
                mapping = aes(x = depth_diff)) +
            theme_bw() +
            geom_histogram(bins = 100)

         # Average difference from on-boat data
         fdat %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(gdif = bottom_depth_m_GEBCO - bottom_depth_m,
                          cdif = bottom_depth_m_CRM - bottom_depth_m) %>%
            dplyr::summarize(GEBCO_mean = mean(gdif, na.rm = T),
                             CRM_mean   = mean(cdif, na.rm = T),
                             GEBCO_median = median(gdif, na.rm = T),
                             CRM_median   = median(cdif, na.rm = T))

         # GEBCO vs CRM slope
         ggplot(data = fdat %>% dplyr::mutate(depth_diff = bottom_slope_GEBCO - bottom_slope_CRM),
                mapping = aes(x = depth_diff)) +
            theme_bw() +
            geom_histogram(bins = 100)
      }
   }
   
   # SST
   if( !( 'sst' %in% names(fdat)) ){
      # make sure all necessary netCDF files are downloaded from 
      # https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html#detail
      
      # function to extract data from netCDF file using raster package
      # requires our dataset as input, so this function isn't very generic
      # output = vector of SST values. 
      get_SST <- function(dataset, 
                          lag_days = 0,
                          sst_type = 'mean', # "mean" or "anom"
                          sst_folder = paste0(in_drive, 'data/GIS/sst'), # folder of netCDF files from NOAA
                          show.progress = FALSE){
         # sst_type can be 'mean' or 'anom'. This is based on the netcdf file names from NOAA.
         # this function assumes that "dataset" has column "datetime"
         
         # original data order
         dataset$ID <- 1:nrow(dataset)
         
         # lag date
         dataset$datetime_lag <- dataset$datetime - (lag_days * 3600*24) # default datetime units are seconds, so they need to be converted to days
         dataset$year_lag <- as.numeric(format(dataset$datetime_lag, format = '%Y'))
         dataset$doy_lag  <- as.numeric(format(dataset$datetime_lag, format = '%j'))
         # nc SST longitude values are from 0 to 360
         dataset <- dataset %>%  
            dplyr::mutate(x = (lon + 360)%%360, # mod isn't actually necessary since our study area is only negative longitudes
                          y = lat) %>%
            # I change the longitude manually instead of using sf::st_shift_longitude(), 
            # because sf::st_shift_longitude() is slow and b/c "dataset" loses the "sf" 
            # class when assigning values the way that I do, which prevents the sf:: 
            # functions from actually working
            dplyr::as_tibble() # make sure it's not an sf object
         
         # arrange data by year
         dataset <- dataset %>% dplyr::arrange(datetime)
         
         # Column for SSTs
         colindex <- ifelse(test = sst_type %in% names(dataset),
                            yes = match(x = sst_type, table = names(dataset)), 
                            no = ncol(dataset) + 1)
         dataset[, colindex] <- NA
         column_names <- names(dataset)
         column_names[colindex] <- sst_type #this name is just temporary. 
         
         # create progress bar
         if(show.progress){
            counter <- 1
            pb <- pbapply::startpb(0, nrow(dataset))
            on.exit(pbapply::closepb(pb))
         }
         
         # loop over years 
         #    this is to speed things up by decreasing the number of times that 
         #    each SST dataset is read into R
         for(this.yr in unique(dataset$year_lag)){
            # print(this.yr)
            
            # subset the data
            daty <- dataset %>% 
               dplyr::filter(year_lag == this.yr)
            
            # get the netcdf files with mean or anomaly
            # as a raster brick
            nc <- raster::brick(x = paste0(#getwd(),
                                           #"/",
                                           sst_folder,
                                           "/sst.day.", 
                                           sst_type, 
                                           ".", 
                                           this.yr, 
                                           ".nc"))
            
            # loop over unique Day of Year
            for(this.doy in unique(daty$doy_lag)){ # for each row i (i.e. each haul)
               # print(this.doy)
               
               # subset the data
               datd <- daty %>% 
                  dplyr::filter(doy_lag == this.doy)
               
               # interpolate the SST values for all hauls on a particular day
               datd$temp <- raster::extract(x = nc[[this.doy]],     # subset the relevant doy
                                            y = datd[,c('x', 'y')], # coordinates of hauls
                                            method = 'bilinear')
               
               # add the sst back into the dataset
               dataset[match(x     = datd$ID, 
                             table = dataset$ID), 
                       sst_type ] <- datd$temp
               
               # update the pbapply timer
               if(show.progress){
                  pbapply::setpb(pb, counter)
                  counter <- counter + nrow(datd)
               }
            } # end for loop over day of year
         } # end of loop over years
         
         rdata <- dataset %>% dplyr::arrange(ID)
         return(rdata %>% dplyr::pull(!!as.symbol(sst_type)))
      } # end function get_SST
      
      # add in some lagged SST measurements
      print('calculating SST')
      fdat$sst         <- get_SST(dataset = fdat, lag_days = 0,   sst_type = 'mean', show.progress = T)
      # fdat %>% filter(is.na(sst)) %>% DT::datatable()
      # # ggplot() + geom_sf(data = fdat) + geom_sf(data = fdat %>% filter(is.na(sst)), size = 2, color = 'red')
      print('calculating SST lag 7 days')
      fdat$sst_lag_7   <- get_SST(dataset = fdat, lag_days = 7,   sst_type = 'mean', show.progress = T)
      # # print('calculating SST lag 30 days')
      # fdat$sst_lag_30  <- get_SST(dataset = fdat, lag_days = 30,  sst_type = 'mean', show.progress = T)
      # print('calculating SST lag 60 days')
      # fdat$sst_lag_60  <- get_SST(dataset = fdat, lag_days = 60,  sst_type = 'mean', show.progress = T)
      # print('calculating SST lag 90 days')
      # fdat$sst_lag_90  <- get_SST(dataset = fdat, lag_days = 90,  sst_type = 'mean', show.progress = T)
      # print('calculating SST lag 180 days')
      # fdat$sst_lag_180 <- get_SST(dataset = fdat, lag_days = 180, sst_type = 'mean', show.progress = T)
      print('calculating SST lag 365 days')
      fdat$sst_lag_365 <- get_SST(dataset = fdat, lag_days = 356, sst_type = 'mean', show.progress = T)
      
      # add in some lagged SST anomaly measurements
      print('calculating SST anomaly')
      fdat$sst_anom         <- get_SST(dataset = fdat, lag_days = 0,   sst_type = 'anom', show.progress = T)
      print('calculating SST anomaly lag 7 days')
      fdat$sst_anom_lag_7   <- get_SST(dataset = fdat, lag_days = 7,   sst_type = 'anom', show.progress = T)
      # print('calculating SST anomaly lag 30 days')
      # fdat$sst_anom_lag_30  <- get_SST(dataset = fdat, lag_days = 30,  sst_type = 'anom', show.progress = T)
      # print('calculating SST anomaly lag 60 days')
      # fdat$sst_anom_lag_60  <- get_SST(dataset = fdat, lag_days = 60,  sst_type = 'anom', show.progress = T)
      # print('calculating SST anomaly lag 90 days')
      # fdat$sst_anom_lag_90  <- get_SST(dataset = fdat, lag_days = 90,  sst_type = 'anom', show.progress = T)
      # print('calculating SST anomaly lag 180 days')
      # fdat$sst_anom_lag_180 <- get_SST(dataset = fdat, lag_days = 180, sst_type = 'anom', show.progress = T)
      print('calculating SST anomaly lag 365 days')
      fdat$sst_anom_lag_365 <- get_SST(dataset = fdat, lag_days = 365, sst_type = 'anom', show.progress = T)
   }
   
   # CUTI (Coastal Upwelling Transport Index)
   if( !( 'cuti' %in% names(fdat)) ){
      # cuti data downloaded from: http://mjacox.com/upwelling-indices/
      # on 2020-12-07
      cuti0 <- read.csv(file = 'data/covariates/CUTI_daily.csv', header = T)
      # str(cuti0)
      
      # reorganize the data
      cuti <- cuti0 %>% 
         dplyr::mutate(date = as.Date(x = paste(year, month, day, sep = '-'))) %>% 
         tidyr::pivot_longer(data = ., 
                             cols = c(-year, -month, -day, -date), 
                             names_to = 'lat', 
                             names_prefix = 'X',
                             values_to = 'cuti') %>% 
         dplyr::mutate(lat = sub('N', '', lat),
                       lat = as.numeric(lat))
      
      # function for bilinear interpolation of the data to each point
      get_cuti <- function(dataset, 
                           lag_days,
                           show.progress = TRUE){
         
         # lag date
         dataset$datetime_lag <- dataset$datetime - (lag_days * 3600*24)
         dataset$date_lag <- as.Date(dataset$datetime_lag)
         
         # original data order
         dataset$ID <- 1:nrow(dataset)
         
         # empty column for cuti
         colindex <- ifelse(test = 'cuti_lag' %in% names(dataset),
                            yes = match(x = 'cuti_lag', table = names(dataset)), 
                            no = ncol(dataset) + 1)
         dataset[, colindex] <- NA_real_
         column_names <- names(dataset)
         column_names[colindex] <- 'cuti_lag'
         
         # create progress bar
         if(show.progress){
            counter <- 1
            pb <- pbapply::startpb(0, nrow(dataset))
            on.exit(pbapply::closepb(pb))
         }
         
         # for loop over years
         for(i in 1:nrow(dataset)){
            
            this.lat <- dataset[i,] %>% dplyr::pull(lat) %>% as.vector()
            this.day <- dataset[i,] %>% dplyr::pull(date_lag) %>% as.vector()
            
            # get the 2 nearest values for interpolation
            #lower value
            clat <- sort(unique(cuti$lat))
            
            # get the CUTI at the lower latitude
            c00 <- if( this.lat < min(clat) ){
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == min(clat) ) %>% 
                  dplyr::pull(cuti)
            } else if( this.lat > max(clat)){
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == max(clat) ) %>% 
                  dplyr::pull(cuti)
            } else {
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == floor(this.lat) ) %>% 
                  dplyr::pull(cuti)
            }
            
            # get the CUTI at the higher latitude
            c01 <- if( this.lat < min(clat) ){
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == min(clat)) %>% 
                  dplyr::pull(cuti)
            } else if( this.lat > max(clat)){
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == max(clat)) %>% 
                  dplyr::pull(cuti)
            } else {
               cuti %>% 
                  dplyr::filter( date == this.day & 
                                    lat == ceiling(this.lat)) %>% 
                  dplyr::pull(cuti)
            }
            
            # Interpolate values (this still works if the lat is out of the cuti range)
            cuti.interp <- (ceiling(this.lat) - this.lat) * c00 + 
               (this.lat - floor(this.lat)) * c01
            
            # add the upwelling index (cuti) back into the dataset
            dataset[match(x     = dataset$ID[i], 
                          table = dataset$ID), 
                    'cuti_lag' ] <- round(cuti.interp, digits = 5)
            
            # update the pbapply timer
            if(show.progress){
               pbapply::setpb(pb, counter)
               counter <- counter + 1
            }
         } # end for loop over haul points
         
         rdata <- dataset %>% dplyr::arrange(ID)
         return(rdata %>% dplyr::pull(cuti_lag))
      }
      
      fdat$cuti         <- get_cuti(fdat, lag_days = 0)
      fdat$cuti_lag_7   <- get_cuti(fdat, lag_days = 7)
      fdat$cuti_lag_365 <- get_cuti(fdat, lag_days = 365)
      
      
      # double-check cuti
      fdat %>% 
         dplyr::filter(lat > 48) %>% 
         dplyr::group_by(date) %>% 
         summarize(n_unique = n_distinct(cuti),
                   cutis = paste(unique(cuti), collapse = ', '),
                   range = diff(range(cuti))) %>% 
         ungroup() %>% 
         dplyr::filter(n_unique > 1) %>% 
         dplyr::arrange(dplyr::desc(range))
   }
}

saveRDS(object = fdat, file = 'data/fdat.rds')
# fdat <- readRDS(file = 'data/fdat.rds')
# error-checking
if(TRUE){
   
   # Investigate (but don't change) potential problems
   {
      # Look at the whole dataset
      fdat %>% 
         dplyr::as_tibble() %>% 
         skimr::skim()
      
      # 31 hauls are missing a duration
      # 7914 are missing bottom depths
      # 1 haul is missing a CRM bottom depth & slope
      
      # Date & Time
      {
         fdat %>% 
            dplyr::group_by(sector) %>% 
            summarise('Missing' = sum(is.na(datetime)),
                      'Earliest Date' = min(date),
                      'Latest Date'   = max(date),
                      'Earlist ToD'   = min(ToD), # strftime(x = min(ToD), format = '%H:%M:%S'),
                      'Latest ToD'    = max(ToD), # strftime(x = max(ToD), format = '%H:%M:%S'),
                      'Earliest DoY'  = min(doy),
                      'Latest DoY'    = max(doy)) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         
         # The dates looks ok from this, but why do the times cover 24 hrs per day?
         fdat %>% 
            ggplot(data = ., aes(x = ToD)) + 
            theme_bw() +
            geom_histogram(bins = 96) + # bin for every 15 minutes
            xlab(label = 'Time of Day') # + scale_x_datetime(expand = c(0,0),breaks = seq(min(fdat$ToD),max(fdat$ToD) + 3600, by = "3 hours"),labels = scales::date_format("%H:%M", tz = 'America/Los_Angeles'))
         
         # Function to adjust bandwidth of density plot
         # Source: http://stackoverflow.com/a/24986121/496488
         bw = function(b,x) b/bw.nrd0(x)
         dplyr::bind_rows( 
            # yesterday
            fdat %>% 
               dplyr::mutate(
                  # subtract 1 day
                  ToD = ToD - 24),
            # today
            fdat,
            # tomorrow
            fdat %>% 
               dplyr::mutate(
                  # add 1 day
                  ToD = ToD + 24)
         ) %>% 
            ggplot(., aes(x = ToD, 
                          color = sector)) + 
            theme_bw() +
            geom_density(adjust = bw(b = 1000, 
                                     x = dat %>% pull(ToD)),
                         size = 2) +
            xlab(label = 'Time of Day') +
            coord_cartesian(xlim = c(min(fdat$ToD),
                                     max(fdat$ToD)), 
                            expand = 0) # + scale_x_datetime(labels = scales::date_format("%H:%M", tz = 'America/Los_Angeles'), breaks = scales::date_breaks("3 hours"))
         
         # there are also a few hauls overnight for other sectors, too
         fdat %>% 
            dplyr::filter( sector != 'Catcher-Processor' ) %>% 
            ggplot(., aes(x = ToD)) + 
            theme_bw() +
            geom_histogram(bins = 96) + # bin for every 15 minutes
            xlab(label = 'Time of Day') # + scale_x_datetime(expand = c(0,0),breaks = seq(min(fdat$ToD),max(fdat$ToD) + 3600, by = "3 hours"),labels = scales::date_format("%H:%M", tz = 'America/Los_Angeles'))
         # and those still show up after the 2011 rules went into effect
         fdat %>% 
            dplyr::filter( sector != 'Catcher-Processor' & 
                              year > 2010) %>% 
            ggplot(., aes(x = ToD)) + 
            theme_bw() +
            geom_histogram(bins = 96) + # bin for every 15 minutes
            xlab(label = 'Time of Day') # + scale_x_datetime(expand = c(0,0),breaks = seq(min(fdat$ToD),max(fdat$ToD) + 3600, by = "3 hours"),labels = scales::date_format("%H:%M", tz = 'America/Los_Angeles'))
         
         # there are 7 hauls with ToD > 24. I'll ignore that...
         fdat %>% dplyr::filter(ToD >= 24)
         }
      
      # Haul duration 
      {
         # Duration is missing 
         sum(is.na(fdat$duration))
         # values. Another 
         sum( fdat$duration < 10 , na.rm = T)
         # values are < 10 minutes. 
         
         # Short Hauls
         short_hauls <- fdat %>% 
            dplyr::filter(duration < 30)
         
         short_hauls %>%
            ggplot(mapping = aes(x = duration)) + 
            theme_bw() + 
            geom_histogram() + 
            xlab(label = 'Haul Duration (minutes)')
         
         # Very Short Hauls
         vshort_hauls <- fdat %>% 
            dplyr::filter(duration < 10)
         
         vshort_hauls %>%
            ggplot(mapping = aes(x = duration)) + 
            theme_bw() + 
            geom_histogram() + 
            xlab(label = 'Haul Duration (minutes)')
         # but the very short hauls still have some catch
         vshort_hauls %>% 
            dplyr::mutate(len = if_else(duration <=5, '0 to 5 min', '5+ to 10')) %>% 
            dplyr::group_by(len) %>% 
            dplyr::summarise(
               n_hauls = length(hake_mt),
               n_empty = sum( hake_mt == 0 & chinook_n == 0),
               n_hake = sum(hake_mt != 0),
               n_chinook = sum(chinook_n != 0),
               n_hake_chin = sum(hake_mt != 0 & chinook_n != 0)) %>% 
            DT::datatable()
         
         # 89% of hauls < 10 minutes still have catch...
         1 - (sum( vshort_hauls$hake_mt == 0 & vshort_hauls$chinook_n == 0) / nrow(vshort_hauls))
         
         # Most of the very short (< 10 minute) hauls are around 5 minutes, but 
         fdat %>% dplyr::filter(duration == 0) %>% nrow() 
         # hauls are of length 0 minutes!
         
         # Very Short Hauls by Sector
         vshort_hauls %>% 
            janitor::tabyl(sector) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', 
                                                    targets = '_all')))) %>% 
            DT::formatPercentage(c("percent"), 1)
         
         
         # Very Short Hauls by Time
         vsh1 <- vshort_hauls %>% 
            ggplot(mapping = aes(x = year)) + 
            theme_bw() + 
            geom_histogram(bins = length(unique(vshort_hauls$year)))
         
         vsh2 <- vshort_hauls %>% 
            ggplot(mapping = aes(x = month)) + 
            theme_bw() + 
            geom_histogram(bins = length(unique(vshort_hauls$month)))
         
         vsh3 <- vshort_hauls %>% 
            ggplot(mapping = aes(x = doy)) + 
            theme_bw() + 
            geom_histogram()
         
         cowplot::plot_grid(vsh1, vsh2, vsh3, ncol = 3)
         # The very short hauls happen mostly right at the beginning of the fishing season.
         
         # haul length by day of year
         fdat %>% 
            ggplot(mapping = aes(x = doy, 
                                 y = duration+1)) + 
            theme_bw() + 
            geom_point(alpha = 0.2) + 
            geom_smooth() + 
            facet_wrap(facets = ~sector) +
            xlab(label = 'Day of the year') +
            ylab(label = 'Duration of haul (minutes)')+
            scale_y_log10()
      }
      
      # Haul bottom Depths
      {
         # There are 
         sum(is.na(fdat$bottom_depth_m))
         # hauls with no (boat measured) bottom depth. An additional
         sum( fdat$bottom_depth_m < 10, na.rm = T )
         # haul has a bottom depth < 10 meters. 
         
         # Missing depths by Sector
         fdat %>% 
            dplyr::filter( is.na(bottom_depth_m )) %>% 
            janitor::tabyl(sector) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         # I'll add in depths for hauls that are missing depth information using GEBCO or CRM bathymetry data. 
         
         
         # Missing CRM depths by Sector
         fdat %>% 
            dplyr::filter( is.na(bottom_depth_m_CRM )) %>% 
            janitor::tabyl(sector) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         fdat %>% 
            dplyr::filter( is.na(bottom_depth_m_CRM )) %>% 
            dplyr::select(haul_id, sector, duration, fishing_depth_m, bottom_depth_m)
         # is it in a weird location? (Yes, but the fishing depth & bottom depth suggest it was in deep water)
         {
            lfmap <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%  # necessary for lots of datapoints
               # background data in grey
               leaflet::addCircleMarkers(data = fdat,
                                         color = 'grey',
                                         stroke = FALSE,
                                         fillOpacity = .5,
                                         group = 'All Hauls',
                                         popup = ~htmltools::htmlEscape(haul_id)) %>%
               # 1 location missing CRM depth
               leaflet::addCircleMarkers(data = fdat %>% dplyr::filter(is.na(bottom_depth_m_CRM)),
                                         color = 'red',
                                         stroke = FALSE,
                                         fillOpacity = 1,
                                         group = 'Missing CRM',
                                         popup = ~htmltools::htmlEscape(haul_id)) %>%
               
               # Layers control
               leaflet::addLayersControl(
                  overlayGroups = c( #'All Hauls',
                     'All Hauls',
                     'Missing CRM'),
                  options = leaflet::layersControlOptions(collapsed = FALSE)
               ) %>% 
               leafem::addMouseCoordinates() %>% 
               leaflet::setView(lng = -124.8, lat = 47.5, zoom = 8) %>% 
               leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) # https://stackoverflow.com/a/64094357/3174566 
            lfmap
            }
         
         # how do the hauls previously identified as being very close or far from shore compare?
         fdat %>% 
            dplyr::mutate(suspicious = if_else(haul_id %in% very_close, 'very close', 
                                               if_else(haul_id %in% very_far, 'very far', 'normal'))) %>% 
            dplyr::filter(haul_id %in% c(very_close, very_far)) %>% 
            dplyr::select(sector, suspicious, duration, fishing_depth_m, bottom_depth_m, bottom_depth_m_GEBCO, bottom_depth_m_CRM, hake_mt, chinook_n)
         
         # how many hauls end up in very shallow water according to GEBCO/CRM?
         fdat %>% 
            dplyr::filter(sector != 'Atsea Tribal') %>% 
            dplyr::filter(bottom_depth_m_GEBCO < 100 |
                             bottom_depth_m_CRM < 100 | 
                             (bottom_depth_m < 100 & bottom_depth_m > 0)) %>% 
            dplyr::select(sector, duration, fishing_depth_m, bottom_depth_m, bottom_depth_m_GEBCO, bottom_depth_m_CRM, hake_mt, chinook_n) %>% 
            ggplot(aes(x = bottom_depth_m_CRM, y = bottom_depth_m)) + 
            theme_bw() + 
            geom_point() + 
            # geom_vline(xintercept = fdat %>% 
            #               dplyr::filter(bottom_depth_m_GEBCO < 100 |
            #                                bottom_depth_m_CRM < 100) %>% pull(bottom_depth_m) %>% min(., na.rm = T), 
            #            color = 'red', linetype = 2) + 
            geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 2) + 
            # coord_fixed() + 
            coord_cartesian(ylim = c(0,1000)) + 
            ylab(label = 'Boat Reported Depth (m)') + 
            xlab(label = 'NOAA CRM Depth (m)')
         
         fdat %>% filter(sector != 'Atsea Tribal') %>% arrange(bottom_depth_f) %>% pull(bottom_depth_f) %>% '['(1:25) * 1.8288
         nrow(fdat %>% dplyr::filter(sector != 'Atsea Tribal' & bottom_depth_m_CRM < 69))
         fdat %>% dplyr::filter(bottom_depth_m_CRM < 69) %>% janitor::tabyl(sector)
      }
      
      # Haul bottom Depths
      {
         sum(is.na(fdat$bottom_slope_CRM))
         sum(is.na(fdat$bottom_slope_GEBCO))
      }
      
      # Fishing Depths
      {
         fdat %>% 
            ggplot(mapping = aes(x = fishing_depth_m, 
                                 color = sector)) + 
            theme_bw() +
            geom_density(size = 1) +
            xlab(label = 'Fishing Depth (m)') + 
            coord_cartesian(xlim = c(0,600))
         
         # Fishing depths of 0
         fdat %>% 
            dplyr::mutate(fishing_depth = factor(
               dplyr::if_else(condition = fishing_depth_f > 0,
                              true = 'positive', 
                              false = dplyr::if_else(condition = is.na(fishing_depth_f), 
                                                     true = 'NA', 
                                                     dplyr::if_else(condition = fishing_depth_f < 0, 
                                                                    true = 'negative', 
                                                                    false = 'zero'))),
               levels = c('NA', 'negative', 'zero', 'positive')))%>% 
            janitor::tabyl(fishing_depth) %>% 
            janitor::adorn_pct_formatting(digits = 4) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         
         # Fishing beneath ocean bottom (according to measured depths)
         fdat %>% 
            dplyr::mutate(fishing_depth = factor(
               dplyr::if_else(condition = is.na(bottom_depth_f), 
                              true = 'No bottom depth',
                              false = dplyr::if_else(condition = fishing_depth_f > bottom_depth_f,
                                                     true = 'Below Bottom', 
                                                     false = dplyr::if_else(condition = is.na(fishing_depth_f), 
                                                                            true = 'NA', 
                                                                            dplyr::if_else(condition = fishing_depth_f > (bottom_depth_f - (10/1.8288)), 
                                                                                           true = 'Fishing within 10m of bottom', 
                                                                                           false = 'Fishing above bottom')))),
               levels = c('NA', 'Below Bottom', 'Fishing within 10m of bottom', 'Fishing above bottom', 'No bottom depth')))%>% 
            janitor::tabyl(fishing_depth) %>% 
            janitor::adorn_pct_formatting(digits = 4) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         # fishing beneath ocean bottom (using CRM depths)
         fdat %>% 
            dplyr::mutate(fishing_depth = factor(
               dplyr::if_else(condition = is.na(bottom_depth_m_CRM), 
                              true = 'No bottom depth',
                              false = dplyr::if_else(condition = fishing_depth_m > bottom_depth_m_CRM,
                                                     true = 'Below Bottom', 
                                                     false = dplyr::if_else(condition = is.na(fishing_depth_m), 
                                                                            true = 'NA', 
                                                                            dplyr::if_else(condition = fishing_depth_m > (bottom_depth_m_CRM - (10)), 
                                                                                           true = 'Fishing within 10m of bottom', 
                                                                                           false = 'Fishing above bottom')))),
               levels = c('NA', 'Below Bottom', 'Fishing within 10m of bottom', 'Fishing above bottom', 'No bottom depth')))%>% 
            janitor::tabyl(fishing_depth) %>% 
            janitor::adorn_pct_formatting(digits = 4) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         
         # fishing beneath ocean bottom (using CRM depths)
         # Shoreside ONLY
         fdat %>% 
            dplyr::filter(sector == 'Shoreside') %>% 
            dplyr::mutate(fishing_depth = factor(
               dplyr::if_else(condition = is.na(bottom_depth_m_CRM), 
                              true = 'No bottom depth',
                              false = dplyr::if_else(condition = fishing_depth_m > bottom_depth_m_CRM,
                                                     true = 'Below Bottom', 
                                                     false = dplyr::if_else(condition = is.na(fishing_depth_m), 
                                                                            true = 'NA', 
                                                                            dplyr::if_else(condition = fishing_depth_m > (bottom_depth_m_CRM - (10)), 
                                                                                           true = 'Fishing within 10m of bottom', 
                                                                                           false = 'Fishing above bottom')))),
               levels = c('NA', 'Below Bottom', 'Fishing within 10m of bottom', 'Fishing above bottom', 'No bottom depth')))%>% 
            janitor::tabyl(fishing_depth) %>% 
            janitor::adorn_pct_formatting(digits = 4) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      }
      
      # SSTs
      {
         fdat %>% 
            dplyr::as_tibble() %>% 
            dplyr::select(sst, sst_lag_7, sst_lag_365, 
                          sst_anom, sst_anom_lag_7, sst_anom_lag_365) %>% 
            skimr::skim()
         
         
         # Histogram of SSTs
         (hist_sst <- ggplot(fdat, aes(x = sst)) + 
               theme_bw() + 
               geom_histogram(bins = 100) + 
               xlab(label = expression('Sea Surface Temperature (' * degree * 'C)')))
         
         # Histogram of SST anomalies
         (hist_ssta <- ggplot(fdat, aes(x = sst_anom)) + 
               theme_bw() + 
               geom_histogram(bins = 100) + 
               xlab(label = expression('SST Anomaly (' * degree * 'C)')))
         # combined
         cowplot::plot_grid(hist_sst, hist_ssta)
         
         (hs1 <- ggplot(fdat) + 
               theme_bw() + 
               geom_point(aes(x = sst, y = sst_lag_365), color = 'black', alpha = 0.2) + 
               geom_smooth(aes(x = sst, y = sst_lag_365), method = 'gam', color = 'black')+
               geom_smooth(aes(x = sst, y = sst_lag_365), method = 'lm', color = 'black')+
               xlab(label = expression('Sea Surface Temperature (' * degree * 'C)')) + 
               coord_equal())
         (hs2 <- ggplot(fdat) + 
               theme_bw() + 
               geom_point(aes(x = sst_anom, y = sst_anom_lag_365), color = 'blue', alpha = 0.2) + 
               geom_smooth(aes(x = sst_anom, y = sst_anom_lag_365), color = 'blue') +
               geom_smooth(aes(x = sst_anom, y = sst_anom_lag_365), method = 'lm', color = 'blue') +
               xlab(label = expression('SST Anomaly (' * degree * 'C)')) + 
               coord_equal())
         # combined
         cowplot::plot_grid(hs1, hs2)
      }
   }

   # things to fix: 
   # 1. hauls missing duration or very (unrealistically ?) short duration
   #     - I'll just make every haul at least 10 minutes in length
   # 2. hauls missing bottom depths &/or slopes
   # 3. hauls missing fishing depths
   # 4. hauls with fishing depths > bottom depths
   #     - there aren't any if using boat-reported bottom depths, but shoreside
   #       doesn't report bottom depths. After filling in shoreside depths with 
   #       CRM depths, there are 1817 hauls with fishing depths below the bottom depth. 
   
   # Fix problems 
   #  (note: this code uses some columns created in the investigation code above)
   {
      # haul duration 
      {
         if( !('duration_orig' %in% names(fdat))){
            fdat <- fdat %>% 
               dplyr::rowwise() %>% 
               # make all durations at least 10 minutes in length (including 31 NAs)
               dplyr::mutate(
                  duration_orig= duration,
                  duration     = ifelse(test = (is.na(duration_orig) | duration_orig < 10), 
                                        yes = 10, 
                                        no = duration_orig)) %>% 
               dplyr::ungroup()
         }
      }
      
      # missing bottom depths and slopes
      {
         if( !('depth_adjusted_for_fishing_depth' %in% names(fdat))){
            fdat <- fdat %>% 
               dplyr::rowwise() %>% 
               # make all durations at least 10 minutes in length (including 31 NAs)
               dplyr::mutate(
                  # fill in missing bottom depths with CRM (formerly with GEBCO) data
                  depth = dplyr::if_else(condition = is.na(bottom_depth_m) | bottom_depth_m <= 0, 
                                         true  = bottom_depth_m_CRM,  # CRM & GEBCO depths were originally negative, but I already changed them to match boat depths, which are positive
                                         false = bottom_depth_m),
                  depth_adjusted_for_fishing_depth = dplyr::if_else(condition = fishing_depth_m > depth, 
                                                                    true  = fishing_depth_m - 10,  # CRM & GEBCO depths were originally negative, but I already changed them to match boat depths, which are positive
                                                                    false = depth),
                  slope = dplyr::if_else(condition = is.na(bottom_slope_CRM), 
                                         true  = bottom_slope_GEBCO,  # CRM & GEBCO depths were originally negative, but I already changed them to match boat depths, which are positive
                                         false = bottom_slope_CRM),
               ) %>% 
               dplyr::ungroup()
         }
      }
      
      # fill in the 4 missing fishing depths
      {
         # Fishing depths of 0
         fdat %>% 
            dplyr::mutate(fishing_depth = factor(
               dplyr::if_else(condition = fishing_depth_f > 0,
                              true = 'positive', 
                              false = dplyr::if_else(condition = is.na(fishing_depth_f), 
                                                     true = 'NA', 
                                                     dplyr::if_else(condition = fishing_depth_f < 0, 
                                                                    true = 'negative', 
                                                                    false = 'zero'))),
               levels = c('NA', 'negative', 'zero', 'positive')))%>% 
            janitor::tabyl(fishing_depth) %>% 
            janitor::adorn_pct_formatting(digits = 4) %>% 
            DT::datatable(., 
                          options = list(
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))))
         
         # get the mean fishing depth
         (mean.fishing.depth = fdat %>% 
               dplyr::filter(fishing_depth_f > 0) %>% 
               pull(fishing_depth_f) %>% 
               mean())
         
         # fill in the missing values
         if( !('fishing_depth_f_original' %in% names(fdat))){
            fdat <- fdat %>% 
               dplyr::rowwise() %>% 
               dplyr::mutate(fishing_depth_f_original = fishing_depth_f,
                             # fill in missing fishing depths with the mean value (note: this might be below the bottom depth at the location)
                             fishing_depth_f = ifelse(fishing_depth_f <= 0,
                                                      mean.fishing.depth,
                                                      fishing_depth_f),
                             fishing_depth_m = fishing_depth_f * 1.8288) %>% 
               dplyr::ungroup()
         }
      }
      
      # haul durations
      fdat %>% dplyr::pull(duration) %>% min()
      fdat %>% dplyr::filter(duration < 60) %>% ggplot(aes(x = duration)) + geom_histogram(bins = 50)
      # Any fishing depths > bottom depths?
      fdat %>% dplyr::filter(fishing_depth_m > depth) %>% nrow()
      fdat %>% dplyr::filter(fishing_depth_m > depth_adjusted_for_fishing_depth) %>% nrow()
      # fishing depths
      fdat %>% dplyr::filter(is.na(fishing_depth_m)) %>% nrow()
      fdat %>% dplyr::pull(fishing_depth_m) %>% min()
   }
}

# Save data for analyses 
{
   # use this projection 
   # projecting to UTM will introduce some distortion since the data spread across multiple UTM zones, but probably not much distortion. See: https://gis.stackexchange.com/questions/31701/calculating-areal-distortion-outside-utm-zone/31711#31711
   # alternatives: 
   #   - US National Atlas Equal Area EPSG: 2163
   #   - TWO-POINT EQUIDISTANT, EPSG: 54031
   #   - ESRI:102003
   #   - I'll go with USA Contiguous Equidistant Conic, ESRI:102005   https://spatialreference.org/ref/esri/usa-contiguous-equidistant-conic/
   #     [units are in meters]
   # or pick a different one here: http://downloads2.esri.com/support/documentation/ao_/710Understanding_Map_Projections.pdf
   # USA Contiguous Equidistant Conic, ESRI:102005 (should also be EPSG:102005)  https://spatialreference.org/ref/esri/usa-contiguous-equidistant-conic/
   proj_crs <- '+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
   # If I were starting over, I think I would just use UTM instead (zone 10 EPSG = 32610)
   # proj_crs <- '+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
   
   # add in projected coordinates
   coords_proj <- fdat %>% 
      dplyr::filter(sector != 'Atsea Tribal') %>% 
      sf::st_transform(crs = proj_crs) %>% 
      # copied-and-pasted from https://spatialreference.org/ref/esri/usa-contiguous-equidistant-conic/proj4/
      sf::st_coordinates()
   # head(coords_proj)
   
   #### rearrange data #### 
   dat_full <- fdat %>% 
      # exclude atsea tribal sector (only haul with "remove == TRUE" was in atsea tribal))
      dplyr::filter(sector != 'Atsea Tribal') %>% 
      dplyr::mutate(sector = factor(sector),
                    year_f = factor(year),
                    lon_proj = as.vector(coords_proj[,'X']),
                    lat_proj = as.vector(coords_proj[,'Y']),
                    proj_crs_proj4string = proj_crs,
                    depth_bottom  = depth,
                    depth_fishing = fishing_depth_m,
                    slope_bottom  = slope,
                    # use projected coordinates for model fitting instead of lat/lon
                    # but divide by 1000 to make numbers smaller (and scale = km instead of meters)
                    sst_anomaly = sst_anom,
                    latitude  = lat, # unprojected coordinates
                    longitude = lon, # unprojected coordinates
                    lat = (lat_proj - mean(lat_proj)) / 1000,
                    lon = (lon_proj - mean(lon_proj)) / 1000) %>% 
      dplyr::select(haul_id,
                    sector, 
                    lat, lon, 
                    latitude, longitude,
                    lat_proj, lon_proj,
                    datetime,
                    date, year, year_f, month, doy, ToD,
                    duration,
                    hake_mt,
                    chinook_n,
                    chinook_kg,
                    source,
                    geometry,
                    sst, sst_lag_7, sst_lag_365, 
                    sst_anomaly, sst_anom_lag_7, sst_anom_lag_365, 
                    cuti, cuti_lag_7, cuti_lag_365,
                    depth_fishing,
                    depth_bottom, 
                    slope_bottom)
   saveRDS(object = dat_full, file = 'data/data_prepped_sf.rds')
   
   # library(sf)
   # dat_full <- readRDS('data/data_prepped_sf.rds')
   
   # save as a dataframe
   saveRDS(dat_full %>% 
              as.data.frame() %>% 
              dplyr::select(-geometry),
      file = 'data/data_prepped.rds')
}


# prepare dataset for mapping model predictions
{
   # 1) grid of covariate values on a particular date
   #    - Particular values:
   #        - sector == 'Catcher-Processor' (most hauls of any sector)
   #        - year == 2015 (just picked a year)
   #        - doy == 141  (most common day)
   #        - ToD == 7 am (most common time of day)
   #    - Average values for: 
   #        - duration
   #        - depth_fishing (but must be < depth_bottom)
   #    - Date &/or location-specific covariate values
   #        - depth_bottom
   #        - slope_bottom
   #        - sst_anomaly / sst_anom_lag_7 / sst_anom_lag_365
   #        - cuti / cuti_lag_7 / cuti_lag_365

   # 2) Yearly grids of covariate values to map (only) spatial component of the model
   #     - all covariates held at mean values (even when that's unrealistic (e.g. bottom depth isn't really constant through space))
   #        - sector == 'Catcher-Processor' (most hauls of any sector)
   #        - year == 2002:2019
   #        - doy == 141  (most common day)
   #        - ToD == 7 am (most common time of day)
   #        - duration == average haul duration
   #        - depth_fishing == average fishing depth
   #        - depth_bottom  == average bottom depth
   #        - slope_bottom  == average bottom slope
   #        - sst_anomaly / sst_anom_lag_7 / sst_anom_lag_365 == average value
   #        - cuti / cuti_lag_7 / cuti_lag_365  = average value
   
   # first create a grid of locations to make the predictions
   lat_min <- 40.5    # min(dat_full$latitude)
   lat_max <- 48.5    # max(dat_full$latitude)
   lon_min <- -127.5  # min(dat_full$longitude)
   lon_max <- -124    # max(dat_full$longitude)
   grid_resolution <- 0.02
   latitude_vector <- seq(lat_min, lat_max, grid_resolution)
   longitude_vector <- seq(lon_min, lon_max, grid_resolution)
   
   grid0 <- expand.grid(lat = latitude_vector,
                        lon = longitude_vector) %>% 
      # convert to a spatial "sf" object
      sf::st_as_sf(., 
                   coords = c('lon', 'lat'), 
                   crs = 4326, # this is the standard CRS for GPS data
                   remove = F) # this prevents the removal of the "lon" and "lat" columns
   
   # add in the projected coordinates
   grid_coords_proj <- grid0 %>% 
      sf::st_transform(crs = proj_crs) %>% 
      # copied-and-pasted from https://spatialreference.org/ref/esri/usa-contiguous-equidistant-conic/proj4/
      sf::st_coordinates()
   if( !('lat_proj' %in% names(grid0))) {
      grid0 <- grid0 %>% 
         mutate(
            lon_proj = as.vector(grid_coords_proj[,'X']),
            lat_proj = as.vector(grid_coords_proj[,'Y']),
         )
   }
   
   # Grid 1
   {
      # add in covariate values
      grid1 <- grid0 %>% 
         dplyr::mutate(
            sector = factor('Catcher-Processor', levels = levels(dat_full$sector)),
            year_f = factor('2015', levels = levels(dat_full$year_f)),
            doy    = as.numeric(names(sort(table(dat_full$doy), decreasing = T))[1]), # day 142 is the most common
            # need datetime to extract SST & CUTI
            datetime = as.POSIXct(x = paste0(as.character(year_f), '-', doy), format = '%Y-%j'),
            ToD    = 7, # as.POSIXct( format( as.POSIXct(x = 50400, origin = as.Date(dat_full$ToD[1])), format = '%H:%M:%S'),format = '%H:%M:%S', tz = 'America/Los_Angeles'),
            duration = mean(dat_full$duration), # 163 minutes
            depth_fishing = mean(dat_full$depth_fishing))  # 258.7; need to update after getting depths at these locations
      
      # extract covariates from rasters
      {
         # Bathymetry & Slope
         if( !( 'depth_bottom' %in% names(grid1)) ){
            # GEBCO (only GEBCO b/c CRM doesn't have full coverage)
            # read in the bathymetry data
            gb_r      <- raster::raster(x = 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9.tif')
            gbslope_r <- raster::raster(x = 'data/GIS/gebco_2020_n50.1_s39.9_w-128.1_e-122.9_slope.tif')
            # gb_r
            # plot(gb_r)
            
            # extract point values using bilinear interpolation
            grid1$depth_bottom <- -1* raster::extract(x = gb_r,
                                                      y = grid1 %>% as.data.frame() %>% dplyr::select(lon, lat) %>% as.matrix(), 
                                                      method = 'bilinear')
            # take negative value in the raster b/c GEBCO represents depths as negative values, but the boat data represents depths as positive values. 
            
            # extract values from the raster
            grid1$slope_bottom <- raster::extract(x = gbslope_r, 
                                                  y = grid1 %>% as.data.frame() %>% dplyr::select(lon, lat) %>% as.matrix(), 
                                                  method = 'bilinear')
         }
         
         # SST
         if( !( 'sst' %in% names(grid1)) ){
            # make sure all necessary netCDF files are downloaded from https://www.psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html#detail
            
            # add in some lagged SST measurements
            print('calculating SST')
            grid1$sst         <- get_SST(dataset = grid1, lag_days = 0,   sst_type = 'mean', show.progress = T)
            print('calculating SST lag 7 days')
            grid1$sst_lag_7   <- get_SST(dataset = grid1, lag_days = 7,   sst_type = 'mean', show.progress = T)
            print('calculating SST lag 365 days')
            grid1$sst_lag_365 <- get_SST(dataset = grid1, lag_days = 365, sst_type = 'mean', show.progress = T)
            
            # add in some lagged SST anomaly measurements
            print('calculating SST anomaly')
            grid1$sst_anomaly      <- get_SST(dataset = grid1, lag_days = 0,   sst_type = 'anom', show.progress = T)
            print('calculating SST anomaly lag 7 days')
            grid1$sst_anom_lag_7   <- get_SST(dataset = grid1, lag_days = 7,   sst_type = 'anom', show.progress = T)
            print('calculating SST anomaly lag 365 days')
            grid1$sst_anom_lag_365 <- get_SST(dataset = grid1, lag_days = 365, sst_type = 'anom', show.progress = T)
         }
         
         # CUTI (Coastal Upwelling Transport Index)
         if( !( 'cuti' %in% names(grid1)) ){
            # cuti data downloaded from: http://mjacox.com/upwelling-indices/
            # on 2020-12-07
            cuti0 <- read.csv(file = 'data/covariates/CUTI_daily.csv', header = T)
            # str(cuti0)
            
            cuti <- cuti0 %>% 
               dplyr::mutate(date = as.Date(x = paste(year, month, day, sep = '-'))) %>% 
               tidyr::pivot_longer(data = ., 
                                   cols = c(-year, -month, -day, -date), 
                                   names_to = 'lat', 
                                   names_prefix = 'X',
                                   values_to = 'cuti') %>% 
               dplyr::mutate(lat = sub('N', '', lat),
                             lat = as.numeric(lat))
            
            # extract cuti values
            grid1$cuti         <- get_cuti(grid1, lag_days = 0)
            grid1$cuti_lag_7   <- get_cuti(grid1, lag_days = 7)
            grid1$cuti_lag_365 <- get_cuti(grid1, lag_days = 365)
         }
      }
      
      # make sure that fishing depth < bottom depth
      {
         # distance off the bottom for boats in shallow water
         # 80% of the bottom depth
         
         grid1 <- grid1 %>%
            dplyr::rowwise() %>% 
            dplyr::mutate(
               # Can't fish BELOW the bottom, so make sure bottom > fishing depth
               depth_fishing  = ifelse( depth_fishing > (depth_bottom * 0.8),
                                        max(10, depth_bottom * 0.8), # make sure it's not a negative value, either
                                        # the problem I was having earlier was that I was using the vectorized "dplyr::if_else", which included the entire vector of all depth_bottom_m, so when I took the max, it was only using the maximum depth value over all locations, not the depth value at each specific location. 
                                        depth_fishing)) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
               lon_unproj = lon, 
               lat_unproj = lat, 
               lat = (lat_proj - mean(dat_full$lat_proj)) / 1000,
               lon = (lon_proj - mean(dat_full$lon_proj)) / 1000
            )
      }
   }
   
   # Grid 2
   {
      # create copies of grid 0 for each year to predict
      grid2 <- do.call(what = rbind, args = lapply(X = 2002:2019, FUN = function(x) grid0 %>% dplyr::mutate(year_f = x))) %>% 
         # then add in covariate values
         dplyr::mutate(
            sector = factor('Catcher-Processor', levels = levels(dat_full$sector)),
            year_f = factor(year_f, levels = levels(dat_full$year_f)),
            doy    = as.numeric(names(sort(table(dat_full$doy), decreasing = T))[1]), # day 142 is the most common
            # need datetime to extract SST & CUTI
            datetime = as.POSIXct(x = paste0(as.character(year_f), '-', doy), format = '%Y-%j'),
            ToD    = 7, # 7am # as.POSIXct( # 7 amformat( as.POSIXct(x = 50400, origin = as.Date(dat_full$ToD[1])), format = '%H:%M:%S'),format = '%H:%M:%S', tz = 'America/Los_Angeles'),
            duration = mean(dat_full$duration), # 163 minutes
            depth_fishing = mean(dat_full$depth_fishing),# 258 m
            depth_bottom  = mean(dat_full$depth_bottom), # 512 m
            slope_bottom  = mean(dat_full$slope_bottom), # 0.0606
            sst_anomaly      = mean(dat_full$sst_anomaly),
            sst_anom_lag_7   = mean(dat_full$sst_anom_lag_7),
            sst_anom_lag_365 = mean(dat_full$sst_anom_lag_365),
            cuti         = mean(dat_full$cuti),
            cuti_lag_7   = mean(dat_full$cuti_lag_7),
            cuti_lag_365 = mean(dat_full$cuti_lag_365)
         ) %>%
         dplyr::mutate(
            lon_unproj = lon, 
            lat_unproj = lat, 
            lat = (lat_proj - mean(dat_full$lat_proj)) / 1000,
            lon = (lon_proj - mean(dat_full$lon_proj)) / 1000
         )
   }
   
   # add in polynomials for models that use them
   {
      # names of continuous covariates to be transformed
      covariate_names <- covar <- c(# 'sector',
                                    # 'year_f',
                                    'doy',
                                    'ToD',
                                    'duration', 
                                    'depth_fishing',
                                    'depth_bottom',
                                    'slope_bottom',
                                    'sst_anomaly',
                                    'sst_anom_lag_7',
                                    'sst_anom_lag_365',
                                    'cuti',
                                    'cuti_lag_7', 
                                    'cuti_lag_365',
                                    'lat',  # lat = projected & transformed latitude
                                    'lon')  # lon = projected & transformed longitude
      
      # create a function to calculate the mean, sd, and 2nd order polynomial for each covariate
      calculate_transformation_info <- function(covariate_name, 
                                                dataset,
                                                polynomial_degree = 2){
         
         cn <- covariate_name
         c_s <- scale(x = dataset[,covariate_name])
         cmean <- attributes(c_s)$`scaled:center`
         csd   <- attributes(c_s)$`scaled:scale`
         cpolynomial <- poly(x = dataset[,covariate_name], degree = polynomial_degree)
         
         list(
            'mean' = cmean,
            'sd' = csd,
            'polynomial' = cpolynomial
         )
      }
      
      # calculate the transformation info
      tl <- lapply(X = covariate_names, 
                   FUN = calculate_transformation_info, 
                   dataset = dat_full %>% as.data.frame())
      names(tl) <- covariate_names
   }
   
   # save the projection grids
   saveRDS(object = list(
      'grid1' = grid1,
      'grid2' = grid2,
      'transformations' = tl
   ), file = 'data/prediction_grid_list.rds')
}
