source('global.R')
# Change back to style=basic

# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
bounds <- readOGR('data','RoanokeBoundary')
centerline <- readOGR('data','VAroadcenterline2017_84')
wqs <- readOGR('data','wqs_riverine_id305b_2013_84')
#Ecoregions@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



shinyServer(function(input, output, session) {
  # -----------------------------------------------------------------------------------------------------
  ## Incident Location Tab ##
  #-------------------------------------------------------------------------------------------------------
  activeDot <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='blue',fillColor = 'yellow',
                                                  fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'Selected')}
  
  ## Map ## 
  output$incidentMap <- renderLeaflet({
    #if(input$targetlocation==""){
      leaflet()%>%
      addProviderTiles(providers$Thunderforest.Landscape,group='Thunderforest Landscape')%>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery')%>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map')%>%
      addProviderTiles(providers$OpenTopoMap,group='Open Topo Map')%>%
      addPolygons(data=bounds,color='gray',fill=0.1,stroke=0.2,group="Municipalities",
                  popup=paste("Municipality: ",bounds@data$DESCRIPT,sep=""))%>%hideGroup('Municipalities')%>%
      addPolylines(data=wqs, color='blue', group="Streams",popup=paste(sep='<br/>',
                              paste('Stream Name: ',wqs@data$WATER_NAME),
                              paste('Basin: ',wqs@data$BASIN),
                              paste('WQS Class: ',wqs@data$WQS_CLASS),
                              paste('Trout Stream: ',wqs@data$WQS_TROUT)))%>%hideGroup('Streams')%>%
      addLayersControl(baseGroups=c('Thunderforest Landscape','Esri World Imagery',
                                    'Open Street Map','Open Topo Map'),
                       overlayGroups=c('Municipalities','Streams'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')%>%
      addMouseCoordinates()%>%#style='basic')%>%
      addMiniMap(toggleDisplay=T)%>%
      addHomeButton(extent(bounds), "Pilot Project  Boundary")%>%setView(-80.043,37.274,zoom=9)
     #   }
    #else{
    #  target_pos = geocode(input$targetlocation)
      
    #  leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%#style='basic')%>%
    #    setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)#%>%
        #addHomeButton(extent(gageInfo), "Virginia Gages")%>%
    #    }
  })
  
  ## Move map view to adjust with marker click ##
  observeEvent(input$incidentMap_marker_click,{
    click <- input$incidentMap_marker_click
    proxy <- leafletProxy("incidentMap")
    if(click$id=="Selected"){
      proxy%>%removeMarker(layerId='Selected')
    }else{
      proxy %>% setView(lng=click$lng,
                        lat=ifelse(input$incidentMap_zoom<10,click$lat+(3/input$incidentMap_zoom),click$lat),
                        input$incidentMap_zoom)%>%
        activeDot(click$lng,click$lat)
    }
  })
  
  ## Plot Incident on map ##
  observeEvent(input$plotIncident,{
    lat <- as.numeric(input$incidentLat)
    lng <- as.numeric(input$incidentLng)
    # make a spatial object from lat/long
    point <- data.frame(name='incident',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    leafletProxy('incidentMap') %>% clearMarkers() %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=12)%>%
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='userIncident',layerId='Incident',popup='Incident')
    
  })
  
  ## Add polygons ##
  #observe({if(input$supaBshape==T){
  #  leafletProxy('unweightedMap')%>%
  #    addPolygons(data=Superbasins,color='blue',fill=0.9,stroke=0.1,group="Superbasins_",
  #                popup=paste("Superbasin: ",Superbasins@data$NAME,sep=""))}else(leafletProxy('unweightedMap')%>%clearGroup("Superbasins_"))})
})