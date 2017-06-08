source('global.R')
# Change back to style=basic

# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
# Polys
bounds <- readOGR('data','RoanokeBoundary')
#floodplain <- readOGR('data','S_FLD_HAZ_AR_rmvblank')
vdhcontact <- readOGR('data','VHD_ODW_Clipped')
vdemcontact <- readOGR('data','VDEM_EmergencyMgmt_Localities_Clipped')
#sinkholes <- readOGR('data','VADMME_Sinkholes_Clipped')
epacontact <- readOGR('data','EPA_OSCs_Clipped')
dgifcontact <- readOGR('data','DGIF_Bounds_Clipped')

#Lines
#centerline <- readOGR('data','VAroadcenterline2017_84')
#wqs <- readOGR('data','wqs_riverine_id305b_2013_84')

# Points, rename point fields so show up pretty in popupTable
#VPDES <- readRDS('data/VPDES.RDS')

dams <- readOGR('data','damsEMMA')
names(dams@data) <- c('Dam Name','NID','Purpose','Type','Stream Name','Height (ft)')

monstations <- readOGR('data','monitoringstationsEMMA')
names(monstations@data) <- c('StationID','Description','Stream Name','Parent Basin','Strahler Order','Special Standards')

#hazwaste <- readRDS('data/RC_Temp_HW_Sites.RDS')
#tankfacilities <- readRDS('S_FLD_HAZ_AR_rmvblank.RDS')
boatramps <- readRDS('data/GIF_Boating_Access_Sites_84.RDS')


#Ecoregions@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



shinyServer(function(input, output, session) {
  # -----------------------------------------------------------------------------------------------------
  ## Water Tab ##
  #-------------------------------------------------------------------------------------------------------
  #activeDot <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='blue',fillColor = 'yellow',
  #                                                fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'Selected')}
  
  ## Map ## 
  output$waterMap <- renderLeaflet({
    damicon <- icons(iconUrl = 'www/Dam-48_background.png' ,iconWidth = 20,iconHeight = 20)
    
      leaflet()%>%
        addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
        addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
        addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
        addProviderTiles(providers$OpenTopoMap,group='Open Topo Map')%>%
        addCircleMarkers(data=monstations,radius=2,color='green',group="Monitoring Stations",
                         popup=popupTable(monstations))%>%hideGroup('Monitoring Stations')%>%
        addMarkers(data=dams,icon=damicon, group="Dams", popup=popupTable(dams))%>%hideGroup('Dams')%>%
        addCircleMarkers(data=boatramps,~Long_,~Lat,radius=2,color='orange',
                       opacity = 1,group='DGIF Boat Ramps',
                       popup=paste(sep='<br/>',
                                   paste('DGIF Boat Ramp:',boatramps$SITENAME),
                                   paste('Waterbody:',boatramps$WATERBODY),
                                   paste('No of Ramps:',boatramps$NO_OFRAMPS),
                                   paste('Location:',boatramps$LOCATION),
                                   paste('Latitude:',boatramps$Lat),'Longitude:',boatramps$Long_))%>%hideGroup('DGIF Boat Ramps')%>%
        addPolygons(data=bounds,color='gray',fill=0.1,stroke=0.2,group="Municipalities",
                    popup=paste("Municipality: ",bounds@data$DESCRIPT,sep=""))%>%hideGroup('Municipalities')%>%
        
      #addPolylines(data=wqs, color='blue', group="Streams",popup=paste(sep='<br/>',
      #                        paste('Stream Name: ',wqs@data$WATER_NAME),
      #                        paste('Basin: ',wqs@data$BASIN),
      #                        paste('WQS Class: ',wqs@data$WQS_CLASS),
      #                        paste('Trout Stream: ',wqs@data$WQS_TROUT)))%>%hideGroup('Streams')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Monitoring Stations','Dams','DGIF Boat Ramps'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()%>%#style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      addHomeButton(extent(bounds), "Pilot Project  Boundary")%>%setView(-80.043,37.274,zoom=9)
     
  })

  ## Plot Incident on map ##
  observeEvent(input$plotIncident,{
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    # make a spatial object from lat/long
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    # highlight catchment
    incidentCatchment <- 
    
    leafletProxy('waterMap')  %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='userIncident',layerId='Incident',popup='Incident')
    
  })
  
  #sum(findInterval(incident()@coords[2],bbox(catchments)[2,]),
  #    findInterval(incident()@coords[1],bbox(catchments)[1,]))<=1
  
  # make a spatial object from user lat/long
  incident <- reactive({
    if(as.numeric(input$incidentLat)=="NA" & as.numeric(input$incidentLng)=="NA")
      return(NULL)
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")       
    return(point)
  })
  
  
  # -----------------------------------------------------------------------------------------------------
  ## Notifications Tab ##
  #-------------------------------------------------------------------------------------------------------
  output$vdhTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <-vdhcontact[incident(),]@data%>%select(VDH_ODW,Field_Dire,Phone_Num,Email_Add,Office_Add)
    names(x) <- c('VDH Office','Field Director','Phone Number','Email Address','Office Address')
    return(x)})
  
  output$vdemTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- vdemcontact[incident(),]@data%>%select(NAME,VDEMregion)
    names(x) <- c('DEM Region','DEM Region Number')
    return(x)})
  
  output$epaTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- epacontact[incident(),]@data%>%select(EPA_VARegi,Primary_Co,Secondary_,Tertiary_C)
    names(x) <- c('EPA Region','Primary Contact','Secondary Contact','Tertiary Contact')
    return(x)})
  
  output$dgifTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- dgifcontact[incident(),]@data%>%select(DGIFreg3,Phone_Num ,RO_Address,AfterHours,AH_Email)
    names(x) <- c('DGIF Region','Phone Number','Regional Office Address','After Hours Phone Number','After Hours Email Address')
    return(x)})
  
  # -----------------------------------------------------------------------------------------------------
  ## Resources Tab ##
  #-------------------------------------------------------------------------------------------------------
  
  # Do all calculations in map?
  ## Map ## 
  output$resourcesMap <- renderLeaflet({
    damicon <- icons(iconUrl = 'www/Dam-48_background.png' ,iconWidth = 20,iconHeight = 20)
    
    leaflet()%>%
      addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$OpenTopoMap,group='Open Topo Map')%>%
      addCircleMarkers(data=monstations,radius=2,color='green',group="Monitoring Stations",
                       popup=popupTable(monstations))%>%hideGroup('Monitoring Stations')%>%
      addMarkers(data=dams,icon=damicon, group="Dams", popup=popupTable(dams))%>%hideGroup('Dams')%>%
      addCircleMarkers(data=boatramps,~Long_,~Lat,radius=2,color='orange',
                       opacity = 1,group='DGIF Boat Ramps',
                       popup=paste(sep='<br/>',
                                   paste('DGIF Boat Ramp:',boatramps$SITENAME),
                                   paste('Waterbody:',boatramps$WATERBODY),
                                   paste('No of Ramps:',boatramps$NO_OFRAMPS),
                                   paste('Location:',boatramps$LOCATION),
                                   paste('Latitude:',boatramps$Lat),'Longitude:',boatramps$Long_))%>%hideGroup('DGIF Boat Ramps')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Monitoring Stations','Dams','DGIF Boat Ramps'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()%>%#style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      addHomeButton(extent(bounds), "Pilot Project  Boundary")%>%setView(-80.043,37.274,zoom=9)
    
  })
  
  ## Plot Incident on map ##
  observeEvent(input$plotIncident,{
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    # make a spatial object from lat/long
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    
    leafletProxy('resourcesMap')  %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='userIncident',layerId='Incident',popup='Incident')
    
  })
  
  
  output$test <- renderPrint({
    #if(class(incident())!='SpatialPointsDataFrame')
    #  print('doesnt')
    #print('does')
    #print(as.numeric(input$incidentLat)=="NA")
    #print(as.numeric(input$incidentLng)=="NA")
    #print(sum(as.numeric(findInterval(incident()@coords[2]),bbox(catchments)[2,]),
    #        findInterval(as.numeric(incident()@coords[1]),bbox(catchments)[1,])))
  })
  
  
  
})

#addCircleMarkers(data=monstations,~MDECLONG,~MDEC_LAT,radius=2,color='green',group="Monitoring Stations",
#                 popup=paste(sep='<br/>',
#                             paste('StationID:',monstations$STATION),
#                             paste('Description:',monstations$DESCRIP),
#                             paste('Stream Name:',monstations$STRNAME),
#                             paste('Parent Basin',monstations$PARENTB),
#                             paste('Strahler Order:',monstations$STRALERORD),
#                             paste('Special Standards:',monstations$SPEC_STD),
#                             paste('Latitude:',monstations$MDEC_LAT,'Longitude:',monstations$MDECLONG)))%>%hideGroup('Monitoring Stations')%>%

# addMarkers(data=dams,~LONGITUD_X,~LATITUDE_Y,icon=damicon, group="Dams",
#popup=paste(sep="<br/>",
#              paste("Dam Name: ",dams@data$DAM_NAME," NID: ",dams@data$NID_ID),
#              paste("Purpose: ",dams$PRM_PURPOS," Type: ",dams$NID_DAMTYP),
#              paste("Stream Name: ",dams$RIVER),
#              paste("Dam Height: ",dams$DAM_HEIGHT," ft")))%>%hideGroup('Dams')%>%



## Move map view to adjust with marker click ##
#observeEvent(input$waterMap_marker_click,{
#  click <- input$waterMap_marker_click
#  proxy <- leafletProxy("waterMap")
#  if(click$id=="Selected"){
#    proxy%>%removeMarker(layerId='Selected')
#  }else{
#    proxy %>% setView(lng=click$lng,
#                      lat=ifelse(input$waterMap_zoom<10,click$lat+(3/input$waterMap_zoom),click$lat),
#                      input$waterMap_zoom)%>%
#      activeDot(click$lng,click$lat)
#  }
#})
