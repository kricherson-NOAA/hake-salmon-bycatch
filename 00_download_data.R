##%######################################################%##
#                                                          #
####            Create directories if needed            ####
#                                                          #
##%######################################################%##

dirs_needed <- paste0(getwd(), 
                      c("/data/", "/data/GIS/", "/data/GIS/sst/", "/data/covariates/", "/data/from_Kate/"))

lapply(dirs_needed, function(dirnames){
  if(!dir.exists(dirnames))
  {
    dir.create(dirnames)
  }
})

##%######################################################%##
#                                                          #
####                 Download SST data                  ####
#                                                          #
##%######################################################%##

#What years do we need data for?
years <- c(2002:2020)

#SST file names to download
files <- paste0("sst.day.mean.",years,".nc")

#Download SST data for selected years. This will take a while! 
lapply(files, function(filenames){
  download.file(paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/",filenames), destfile =  paste0(download_to, filenames))
})

##%######################################################%##
#                                                          #
####                   Download CUTI                    ####
#                                                          #
##%######################################################%##

download.file("http://www.mjacox.com/wp-content/uploads/2022/03/CUTI_daily.csv", destfile = paste0(getwd(), "/data/covariates/", "CUTI_daily.csv"))

##%######################################################%##
#                                                          #
####              Download NOAA bathymetry              ####
#                                                          #
##%######################################################%##

#Volume 7 (Central Pacific)
download.file("https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol7.nc", destfile = paste0(getwd(), "/data/GIS/", "crm_vol7.nc"))

#Volume 8 (NW Pacific)
download.file("https://www.ngdc.noaa.gov/thredds/fileServer/crm/crm_vol8.nc", destfile = paste0(getwd(), "/data/GIS/", "crm_vol8.nc"))

##%######################################################%##
#                                                          #
####              Download GEBCO bathmetry              ####
#                                                          #
##%######################################################%##
 
#Not clear to me if this is possible to do within R, so do manually by visiting https://download.gebco.net/ and entering the boundaries 50.1 (north), -122.9 (east), 39.9 (south), and -128.1 (west). Format is GeoTIFF grid, grid is GEBCO 2020.



