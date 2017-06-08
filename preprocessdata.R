library(rgdal)
library(dplyr)

# read in shapefiles (points) and change to .RDS
VPDES <- readOGR('data','VPDES_wgs84')
saveRDS(VPDES@data,'data/VPDES.RDS')


dams <- readOGR('data','dam_wgs84')
damco <- dams@coords
dams <- dams@data%>%
  select(DAM_NAME,NID_ID,PRM_PURPOS,NID_DAMTYP,RIVER,DAM_HEIGHT)
coordinates(dams) <- damco
proj4string(dams) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(dams,layer='damsEMMA','data',driver='ESRI Shapefile')
# Still need to rename columns in r

mon <- readOGR('data','All_Monitoring_05222017')
monco <- mon@coords
mon <- mon@data%>%
  select(STATION,DESCRIP,STRNAME,PARENTB,STRALERORD,SPEC_STD)
coordinates(mon) <- monco
proj4string(mon) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
writeOGR(mon,layer='monitoringstationsEMMA','data',driver='ESRI Shapefile')


saveRDS(mon@data,'data/monitoringstations.RDS')
hazwaste <- readOGR('data','RC_Temp_HW_Sites')
saveRDS(hazwaste@data,'data/RC_Temp_HW_Sites.RDS')
tankfacilities <- readOGR('data','Reg_TankFacilities_Clipped')
dat <- tankfacilities@data  
co <- data.frame(Lng=tankfacilities@coords[,1],Lat=tankfacilities@coords[,2]) 
tankfacilities1 <- cbind(dat,co)
saveRDS(tankfacilities1,'S_FLD_HAZ_AR_rmvblank.RDS')
boatramps <- readOGR('data','DGIF_Boating_Access_Sites_841')
saveRDS(boatramps@data,'data/GIF_Boating_Access_Sites_84.RDS')
