semanticPage(
  
 
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  ),
  uiOutput("welcomeModal"),
  grid(
    grid_template = grid_template(
      default = list(
        areas = rbind(
          c("title", "map","map"),
          c("ui1", "map","map"),
          c("shipcard", "map","map")
        ),
        cols_width = c("400px", "1fr"),
        rows_height = c("80px", "auto", "550px")
      )),
    area_styles = list(title = "margin: 20px;", ui1 = "margin: 20px;", shipcard = "margin: 20px;"),
    title = h1(class = "ui header", icon("ship"), div(class = "content", "Ships Ahoy!!")),
  ui1 = ShipSelectUI("UIone"),
 shipcard = 
   card(style = "border-radius: 0; width: 100%; background: #efefef",
     uiOutput("cardforship")),
 
    
   map =segment(
    leafletOutput("mymap1")
  )
)

)