source('global.R')
# Change back to style=basic

# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
# Polys
bounds <- readOGR('data','RoanokeBoundary')
#floodplain <- readOGR('data','S_FLD_HAZ_AR_rmvblank')
vdhcontact <- readOGR('data','VHD_ODW_Clipped')
vdemcontact <- readOGR('data','VDEM_EmergencyMgmt_Localities_Clipped')
sinkholes <- readOGR('data','VADMME_Sinkholes_Clipped')
epacontact <- readOGR('data','EPA_OSCs_Clipped')
dgifcontact <- readOGR('data','DGIF_Bounds_Clipped')
roanokecohealthcontact <- readOGR('data','RoanokeCountyHealthDistict')
swIntakesPolys <- readOGR('data','swIntakes_84')
catchments <- readOGR('data','Catchments')

#Lines
wqs <- readOGR('data','wqs_riverine_id305b_2013_84')
centerline <- readOGR('data','VAroadcenterline2017_84')

# Points, rename point fields so show up pretty in popupTable
VPDES <- readOGR('data','EMMA_VPDES')
names(VPDES@data) <- c('Facility Name','Permit Number','Major/Minor','Municipal/Industrial','Receiving Stream')
dams <- readOGR('data','EMMA_dams')
names(dams@data) <- c('Dam Name','NID','Purpose','Type','Stream Name','Height (ft)')
monstations <- readOGR('data','EMMA_monitoringstations')
names(monstations@data) <- c('StationID','Description','Stream Name','Parent Basin','Strahler Order','Special Standards')
hazwaste <- readOGR('data','EMMA_hazwaste')
names(hazwaste@data) <- c('Facility Name','Address','EPA ID')
tankfacilities <- readOGR('data','EMMA_tankfacilities')
boatramps <- readRDS('data/GIF_Boating_Access_Sites_84.RDS')
surfaceIntakes <- readRDS('data/EMMAswIntakes.RDS')
# Stream gage data in global.R



shinyServer(function(input, output, session) {
  # -----------------------------------------------------------------------------------------------------
  ## Water Tab ##
  #-------------------------------------------------------------------------------------------------------
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
        addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                         fillOpacity=0.5,group='Stream Gages',
                         popup=popupTable(gageInfo, zcol = c('Gage Number','Description',
                                                             'Drainage Area (sq mi)','Web Address')))%>%hideGroup('Stream Gages')%>%
        addPolygons(data=bounds,color='gray',fill=0.1,stroke=0.2,group="Municipalities",
                    popup=paste("Municipality: ",bounds@data$DESCRIPT,sep=""))%>%hideGroup('Municipalities')%>%
        addPolygons(data=sinkholes,color='yellow',fill=0.1,stroke=0.1,group="Sinkholes",
                    popup=paste(sep='<br/>',"Sinkholes",
                                paste('Sq Feet',sinkholes@data$SqFeet),
                                paste('Sq Meters',sinkholes@data$SqMeters),
                                paste('Acres',sinkholes@data$Acres)))%>%hideGroup('Sinkholes')%>%
        addCircleMarkers(data=surfaceIntakes,~intakeLng,~intakeLat,radius=2,color='deeppink',
                         opacity = 1,group='Surface Water Intakes',
                         popup=paste(sep='<br/>',
                                     paste('Intake:',surfaceIntakes$Intake),
                                     paste('Phone:',surfaceIntakes$Phone)))%>%hideGroup('Surface Water Intakes')%>%
      addPolylines(data=wqs, color='navy', group="Streams",popup=paste(sep='<br/>',
                              paste('Stream Name: ',wqs@data$WATER_NAME),
                              paste('Basin: ',wqs@data$BASIN),
                              paste('WQS Class: ',wqs@data$WQS_CLASS),
                              paste('Trout Stream: ',wqs@data$WQS_TROUT)))%>%hideGroup('Streams')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Monitoring Stations','Dams','Stream Gages',
                                       'DGIF Boat Ramps','Surface Water Intakes','Streams','Sinkholes'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()%>%#style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      addHomeButton(extent(bounds), "Pilot Project  Boundary")%>%setView(-80.043,37.274,zoom=9)%>%
      addMeasure(activeColor='#3D535D',completedColor='#7D4479')
  })
  
  observe({
    shinyjs::toggleState('plotIncident', input$incidentLat !="" && input$incidentLng !="" )#&&
    #findInterval(input$incidentLat,bbox(catchments)[2,])==1)#,
    # findInterval(incident()@coords[1],bbox(catchments)[1,]))>1)
  })
  observe({
    shinyjs::toggleState('plotSRxings', input$incidentLat !="" && input$incidentLng !="")
  })
  
  ## Plot Incident on map ##
  observeEvent(input$plotIncident,{
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    # make a spatial object from lat/long
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    # Highlight catchment incident falls into
    incidentCatchment <- catchments[incident(),]
    
    # Add incident location and highlight catcment to map
    leafletProxy('waterMap')  %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addPolygons(data=incidentCatchment,color='blue',fill=0.02,stroke=0.1,group="Catchment",
                  popup="Incident Catchment")%>%#%>%hideGroup('Catchment')
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='userIncident',layerId='Incident',popup='Incident')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Monitoring Stations','Dams','Stream Gages',
                                       'DGIF Boat Ramps','Surface Water Intakes','Streams','Sinkholes','Catchment'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')
    })
  
  
  

  # make a spatial object from user lat/long
  incident <- reactive({
    req(input$plotIncident)
    
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")       
    return(point)
  })
  
  observeEvent(input$plotSRxings,{
    
    # highlight catchment
    incidentCatchment <- catchments[incident(),]
    
    # Find Stream/Road intersections within catchment
    streamsincatchment <- raster::intersect(wqs,incidentCatchment)
    roadsincatchment <- raster::intersect(centerline,incidentCatchment)
    streamXroad <- gIntersection(streamsincatchment,roadsincatchment)
    # Crazy data manipulation to just get lat/longs out right
    z <- as.table(coordinates(streamXroad))
    row.names(z) <- 1:nrow(coordinates(streamXroad))
    z1 <- as.data.frame(t(z))%>%
      spread(Var2,Freq)
    
    finalDF <- data.frame(t(z1[2,-1]),t(z1[1,-1]),'Stream/Road Intersection',t(z1[2,-1]),t(z1[1,-1]))
    names(finalDF) <- c('lat','lng','Type','Latitude','Longitude')
    coordinates(finalDF) <- ~lng+lat
    proj4string(finalDF) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")       
    
    leafletProxy('waterMap')  %>% clearControls() %>%
      #addPolygons(data=incidentCatchment,color='blue',fill=0.02,stroke=0.1,group="Catchment",
      #            popup="Incident Catchment")%>%#%>%hideGroup('Catchment')
      #addCircleMarkers(data=incident(),radius=8,
      #                 color=~'red',stroke=F,fillOpacity=0.5,
      #                 group='userIncident',layerId='Incident',popup='Incident')%>%
      addCircleMarkers(data=finalDF,radius=4,color=~'black',stroke=F,fillOpacity=0.5,
                       group='Stream/Road Crossings',
                       popup=paste(sep='<br/>','Stream/Road Crossing',
                                   paste(finalDF@data$Latitude,finalDF@data$Longitude,sep=", ")))%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Monitoring Stations','Dams','Stream Gages',
                                       'DGIF Boat Ramps','Surface Water Intakes','Streams','Sinkholes','Catchment','Stream/Road Crossings'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')
    })
    
  
  

  # -----------------------------------------------------------------------------------------------------
  ## Regulated Sources Tab ##
  #-------------------------------------------------------------------------------------------------------
  
  # Do all calculations in map?
  ## Map ## 
  output$regulatedSourcesMap <- renderLeaflet({
    hazicon <- icons(iconUrl = 'www/alert-circled.png',iconWidth = 20,iconHeight = 20)
    tankicon <- icons(iconUrl = 'www/tank2.png',iconWidth = 30,iconHeight = 30)
    permiticon <- icons(iconUrl = 'www/android-note.png',iconWidth = 20,iconHeight = 20)
    
    leaflet()%>%
      addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$OpenTopoMap,group='Open Topo Map')%>%
      addMarkers(data=VPDES,icon=permiticon, group="VPDES Permits", popup=popupTable(VPDES))%>%hideGroup('VPDES Permits')%>%
      addMarkers(data=hazwaste,icon=hazicon, group="Hazardous Waste Sites", popup=popupTable(hazwaste))%>%hideGroup('Hazardous Waste Sites')%>%
      addMarkers(data=tankfacilities,icon=tankicon, group="Tank Facilities", popup='Tank Facility')%>%hideGroup('Tank Facilities')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('VPDES Permits','Hazardous Waste Sites',
                                       'Tank Facilities'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()%>%#style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      addHomeButton(extent(bounds), "Pilot Project  Boundary")%>%setView(-80.043,37.274,zoom=9)%>%
      addMeasure(activeColor='#3D535D',completedColor='#7D4479')
    
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
    incidentCatchment <- catchments[incident(),]
    
    leafletProxy('regulatedSourcesMap') %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addPolygons(data=incidentCatchment,color='blue',fill=0.02,stroke=0.1,group="Catchment",
                  popup="Incident Catchment")%>%#%>%hideGroup('Catchment')
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='userIncident',layerId='Incident',popup='Incident')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('VPDES Permits','Hazardous Waste Sites',
                                       'Tank Facilities','Catchment'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')
    
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
  
  
  
  
  # -----------------------------------------------------------------------------------------------------
  ## Notifications Tab ##
  #-------------------------------------------------------------------------------------------------------
  output$swIntakesTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- swIntakesPolys[incident(),]@data%>%select(Intake,FieldOffic,Phone,Fax,area)
    if(nrow(x)<1){
      return(data.frame(Intake='Not in surface water intake watershed'))}else{
        names(x) <- c('Surface Water Intake','Field Office','Phone','Fax','Intake Watershed Area (sq mi)')
        return(x)}
  })
  
  output$vdhTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <-vdhcontact[incident(),]@data%>%select(VDH_ODW,Field_Dire,Phone_Num,Email_Add,Office_Add)
    names(x) <- c('VDH Office','Field Director','Phone Number','Email Address','Office Address')
    return(x)})
  
  output$countyTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <-roanokecohealthcontact[incident(),]@data
    names(x) <- c('Heath District','Director of Office','Phone Number')
    return(x)})
  
  output$vdemTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- vdemcontact[incident(),]@data%>%select(NAME,VDEMregion)%>%
      mutate(Contact=c('Brian Thurman'),Phone=c('540-986-6982'),
             Email=c('brian.thurman@vdem.virginia.gov'))
    x <- rbind(x,data.frame(NAME='Roanoke City',VDEMregion='6',Contact='Ray Earp',Phone='804-513-5726',Email='ray.earp@vdem.virginia.gov'))
    names(x) <- c('DEM Region','DEM Region Number','Regional Hazmat Officer','Phone Number','Email Address')
    return(x)})
  
  
  output$regionalDGIFTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- dgifcontact[incident(),]@data%>%select(DGIFreg3,Phone_Num ,RO_Address,AfterHours,AH_Email)
    names(x) <- c('DGIF Region','Phone Number','Regional Office Address','After Hours Phone Number','After Hours Email Address')
    return(x)})
  
  output$epaTable <- renderTable({
    if(is.null(incident()))
      return(NULL)
    x <- epacontact[incident(),]@data%>%select(EPA_VARegi,Primary_Co,Secondary_,Tertiary_C)
    names(x) <- c('EPA Region','Primary Contact','Secondary Contact','Tertiary Contact')
    return(x)})
  
  output$statewideDGIFTable <- renderTable({
    x <- data.frame(Contact='Ernie Aschenbach',Phone='804-367-2733',Email='ernie.aschenbach@dgif.virginia.gov')
    names(x) <- c('Statewide Contact','Phone Number','Email Address')
    return(x)
  })
  
  output$USFWStable <- renderTable({
    x <- data.frame(Contact='Susan Lingenfelser',Phone='804-824-2415',Email='susan_lingenfelser@fws.gov')
    names(x) <- c('Contact','Phone Number','Email Address')
    return(x)})
  
  output$NRCtable <- renderTable({
    return(data.frame(Hotline='800-424-8802'))})
  
  output$USCGtable <- renderTable({
    x <- data.frame(Hampton='757-483-8567')
    names(x) <- 'Hampton Roads Command Center'
    return(x)})
  
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
