ShipSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
   
    dropdown_input(ns("ship_type"),
                   type = "search selection single",
                   default_text =labelMandatory("Select Ship type"),
                    choices =unique(df2$ship_type),
                   value =unique(df2$ship_type)[[1]])
    ,
    br(),
    br(),
  
    uiOutput(ns("ShipNameDropdown")))
    
    
  
}

ShipSelectServer <- function(input, output, session) {
    
    
    
    output$ShipNameDropdown <- renderUI(
      {  
        validate(
          need(!is.null(input$ship_type),'Select ship type and name'))
        ns <- session$ns
        
        tagList(
          dropdown_input(ns("ship_name"),
                         default_text =labelMandatory("Select Ship name"), 
                         choices =ship_name_choices(), 
                         type = "search selection single",
                         
                           ))
          
   
     
    
      })
    
    ship_name_choices <- reactive({

      if(!is.null(input$ship_type) && input$ship_type != "")
      {
         
        cdf<- df2 %>%
          filter(ship_type  == input$ship_type) %>%
          select(SHIPNAME) %>%
          distinct()%>%as.data.frame()
         
        cdf[[1]]
      }
      else{
        NULL
      }
     
    })

    returneddata <- reactive({
      
      list(input$ship_name,input$ship_type)
    })
    
    
    return(returneddata)
    
    }
