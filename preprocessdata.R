library(rgdal)
library(dplyr)

# read in shapefiles (points) and change to .RDS
VPDES <- readOGR('data','VPDES_wgs84')
VPDESco <- VPDES@coords
VPDES <- VPDES@data%>%
  select(FACILITY_N,PERMIT_NO,MAJOR__MIN,MUNICIPAL_,RECEIVING_)
coordinates(VPDES) <- VPDESco
proj4string(VPDES) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(VPDES,layer='EMMA_VPDES','data',driver='ESRI Shapefile')
# Still need to rename columns in r



dams <- readOGR('data','dam_wgs84')
damco <- dams@coords
dams <- dams@data%>%
  select(DAM_NAME,NID_ID,PRM_PURPOS,NID_DAMTYP,RIVER,DAM_HEIGHT)
coordinates(dams) <- damco
proj4string(dams) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(dams,layer='EMMA_dams','data',driver='ESRI Shapefile')
# Still need to rename columns in r

mon <- readOGR('data','All_Monitoring_05222017')
monco <- mon@coords
mon <- mon@data%>%
  select(STATION,DESCRIP,STRNAME,PARENTB,STRALERORD,SPEC_STD)
coordinates(mon) <- monco
proj4string(mon) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(mon,layer='EMMA_monitoringstations','data',driver='ESRI Shapefile')
# Still need to rename columns in r



hazwaste <- readOGR('data','RC_Temp_HW_Sites')
hazwasteco <- hazwaste@coords
hazwaste <- hazwaste@data%>%
  select(FACILITY_N,Match_addr,EPA_ID)
coordinates(hazwaste) <- hazwasteco
proj4string(hazwaste) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(hazwaste,layer='EMMA_hazwaste','data',driver='ESRI Shapefile')
# Still need to rename columns in r


tankfacilities <- readOGR('data','Reg_TankFacilities_Clipped')
tankfacilitiesco <- tankfacilities@coords
tankfacilities <- tankfacilities@data
coordinates(tankfacilities) <- tankfacilitiesco
proj4string(tankfacilities) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(tankfacilities,layer='EMMA_tankfacilities','data',driver='ESRI Shapefile')


boatramps <- readOGR('data','DGIF_Boating_Access_Sites_841')
saveRDS(boatramps@data,'data/GIF_Boating_Access_Sites_84.RDS')
# Still need to rename columns in r
