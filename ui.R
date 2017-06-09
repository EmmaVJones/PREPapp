shinyUI(fluidPage(theme = "yeti.css", 
                  tagList(
                    singleton(tags$head(tags$script(src='//cdn.datatables.net/fixedheader/2.1.2/js/dataTables.fixedHeader.min.js',type='text/javascript'))),
                    singleton(tags$head(tags$link(href='//cdn.datatables.net/fixedheader/2.1.2/css/dataTables.fixedHeader.css',rel='stylesheet',type='text/css')))),
                  navbarPage('VDEQ PREP Response Tool',
                             tabPanel("Incident Location",
                                      #bootstrapPage(div(class="outer",
                                      #                  tags$style(type ="text/css",".outer {position: fixed; top: 75px; left: 0; right: 0; bottom: 0; overflow-y: scroll; padding: 0}"),
                                      #                  leafletOutput("unweightedMap"),
                                      fluidRow(
                                        column(4,textInput('incidentLat',strong('Incident Latitude:'),placeholder='e.g. 37.27854')),
                                        column(4,textInput('incidentLng',strong('Incident Longitude:'),placeholder='e.g. -80.01876')),
                                        column(4,actionButton('plotIncident',"Locate Incident"),
                                               actionButton('plotSRxings','Locate Stream/Road Crossings'))),
                                      tabsetPanel(
                                        tabPanel("Water",
                                                 leafletOutput("waterMap",width="100%",height=600)),
                                        tabPanel("Notifications",
                                                 wellPanel(h5('Virginia Department of Heath Contact Information'),
                                                           tableOutput('vdhTable')),
                                                 wellPanel(h5('County Department of Health Contact Information'),
                                                           tableOutput('countyTable')),
                                                 wellPanel(h5('Virginia Department of Emergency Management Contact Information'),
                                                           tableOutput('vdemTable')),
                                                 wellPanel(h5('Environmental Protection Agency Contact Information'),
                                                           tableOutput('epaTable')),
                                                 wellPanel(h5('Virginia Department of Game and Inland Fisheries Contact Information'),
                                                           tableOutput('dgifTable'))),
                                                 #verbatimTextOutput('test')),
                                        tabPanel("Resources"),
                                        tabPanel("Regulated Sources",
                                                 leafletOutput("regulatedSourcesMap",width="100%",height=600)))
                             ),
                             tabPanel('Table Information')
                  )
))
                                                 