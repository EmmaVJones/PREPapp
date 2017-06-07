shinyUI(fluidPage(theme = "yeti.css", 
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))),
                  navbarPage('VDEQ PREP Response Tool',
                             tabPanel("Incident Location",
                                      #bootstrapPage(div(class="outer",
                                      #                  tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                      #                  leafletOutput("unweightedMap"),
                                      h4("Please input the latitude and longitude of the incident below. Then click the 'Locate Incident' button
                                         to plot it on the map."),
                                      fluidRow(
                                        column(4,textInput('incidentLat','Latitude:',placeholder='e.g. 37.27854')),
                                        column(4,textInput('incidentLng','Longitude:',placeholder='e.g. -80.01876')),
                                        column(4,actionButton('plotIncident',"Locate Incident"))),
                                      leafletOutput("incidentMap",width="100%",height=600)
                             )
                  )
))
                                                 