shinyServer(function(input, output, clientData, session){
 # setting ship icons for marker on leaflet map
  
   icons <- awesomeIcons(
    icon        = "ship",
    iconColor   = "black",
    library     = 'fa',
    markerColor = "teal"
  )
   #call module for second dropdown
   
  newentry <- callModule(ShipSelectServer, "UIone")
  #splashscreen modal
  
  output$welcomeModal <- renderUI({
    create_modal(modal(
      id = "simple-modal",
      title = "Important message",
      header = h2(class = "ui header", icon("ship"), div(class = "content", "Ship Sprint")),
      content = grid(
        grid_template = grid_template(
          default = list(
            areas = rbind(c("photo", "text")),
            cols_width = c("50%", "50%")
          )
        ),
        container_style = "grid-gap: 20px",
        area_styles = list(text = "padding-right: 20px"),
        photo = tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/MAERSK_MC_KINNEY_M%C3%96LLER_%26_MARSEILLE_MAERSK_%2848694054418%29.jpg/800px-MAERSK_MC_KINNEY_M%C3%96LLER_%26_MARSEILLE_MAERSK_%2848694054418%29.jpg", style = "width: 100%", alt = "MAERSK MC KINNEY"),
        text = HTML(
          paste0(introspiel
            )
          )
        )
      )
    )
  })
  # ui server for ship details
  
  output$cardforship <- renderUI({
    
   
    # grab all details
    #browser()
    shipFlag <- Shipdetail()$FLAG[[1]] 
    ShipName  <- Shipdetail()$SHIPNAME[[1]] 
    ShipType  <- Shipdetail()$ship_type[[1]]
    ShipID  <- as.character(Shipdetail()$SHIP_ID[[1]])
    ShipCurPort  <- toupper(Shipdetail()$port[[1]])
    ShipDestPort  <- toupper(Shipdetail()$DESTINATION[[1]])
    ShipLen  <- as.character(Shipdetail()$LENGTH[[1]])
    ShipWidth  <- as.character(Shipdetail()$WIDTH[[1]])
    ShipDwt  <- as.character(Shipdetail()$DWT[[1]])
    #browser()
    tagList(
      div(class= "ui centered column", style=" height = 500px;",
          div(class= "centered column",
              div(class = "centered row",
              
              h4("Ship Details")),
              container_style = "grid-gap: 20px",
             
              card(
              div( class= "ui centered segments",
                   card(
                     div(class = "ui segment",
                         div(class = "ui two column stackable center aligned grid",
                             div(class = "ui vertical divider",
                                 "TO"),
                             
                             div(class = "middle aligned row",
                                 div(class = "column",
                                     ShipCurPort ),
                                 div(class = "column",
                                     ShipDestPort)
                             )))),
                   card(div(class = "ui segment",
                       div(class = " row",
                           shiny::span(class = "left floated header ",tags$b(paste0("Ship flag :"))),
                           shiny::span(class = "right floated ",tags$i(class = paste(tolower(shipFlag), " flag"))))  
                       )),
                 
                   card(
                     div(class = "ui segment",style="align: right;",
                         div(class = "middle aligned row",
                             shiny::span(class = "left floated ",tags$b(paste0("Ship Name :"))),
                             shiny::span(class = "right floated ",tags$b(ShipName)))  
                     )),
                   
                   card(
                     div(class = "ui segment",
                         div(class = "middle aligned row",
                             shiny::span(class = "left floated ",tags$b(paste0("Ship Type :"))),
                             shiny::span(class = "right floated ",tags$b(ShipType)))  
                     )),
                   card(
                     div(class = "ui segment",
                         div(class = "middle aligned row",
                             shiny::span(class = "left floated ",tags$b(paste0("Ship ID :"))),
                             shiny::span(class = "right floated ",tags$b(ShipID)))  
                     )),
                   card(
                   div(class = "ui segment",
                       div(class = "middle aligned row",
                           shiny::span(class = "left floated ",tags$b(paste0("Ships Length( in m) :"))),
                           shiny::span(class = "right floated ",tags$b(ShipLen)))  
                   )),
                   card(
                   div(class = "ui segment",
                       div(class = "middle aligned row",
                           shiny::span(class = "left floated ",tags$b(paste0("Ships's Weight(in tons) :"))),
                           shiny::span(class = "right floated ",tags$b(ShipWidth)))  
                   ))
                   ))
              
              )

    ))
    
  })
  #reactive for filtering main data using inputs from callmodule
  
  Shipdetail <- reactive({
    
    shipname_from_module <- newentry()[[1]]
    shiptype_from_module <- newentry()[[2]]
    
    
    validate(
      need(!is.na(shipname_from_module) && shipname_from_module != "",'Select ship type and name'),
      need(!is.na(shiptype_from_module) && shiptype_from_module != "",'Select ship type and name'))
     
   df2 %>%
      filter(SHIPNAME  == shipname_from_module  & ship_type == shiptype_from_module)
    
  })
  
  output$mymap1 <- renderLeaflet({
    validate(
      need(!is.na(Shipdetail()),'Select ship type and name')
      )
    # grab extents to add padding using fitbounds
   lat1 <- min(Shipdetail()$LAT)
   lat2 <- max(Shipdetail()$LAT)
   lng1 <- min(Shipdetail()$LON)
   lng2 <- max(Shipdetail()$LON)
 #  browser()
   # render leaflet map
    leaflet(Shipdetail()) %>% 
      addTiles() %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      fitBounds(lng1, lat1, lng2, lat2,options = list(padding = c(400,400))) %>%
      addPolylines( ~LON,~LAT, 
                   label = paste0("Distance covered is ",round(Shipdetail()$distance,0),' meters in ',
                                  difftime(Shipdetail()$newdatetime[[2]],Shipdetail()$newdatetime[[1]],units = "secs" ), " seconds"), 
                   labelOptions = labelOptions(noHide = TRUE)) %>%
      addAwesomeMarkers(lng = ~LON, lat = ~LAT,icon = icons,
                                                         popup =paste0(Shipdetail()$ship_type," ship ",
                                                                       Shipdetail()$SHIPNAME," heading towards ",
                                                                       Shipdetail()$DESTINATION," covered a max distance of ",
                                                                       round(Shipdetail()$distance,1)," meters"," on ",
                                                                       format(Shipdetail()$newdatetime,format =" %m/%d/%Y")))
    
  })
  #  options
  leafletOutput("mymap1", width="100%", height="100%")
 
})

