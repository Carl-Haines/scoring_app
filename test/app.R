library(shiny)
###https://stackoverflow.com/questions/38083286/r-shiny-how-to-dynamically-append-arbitrary-number-of-input-widgets###

LHSchoices <- c("X1", "X2", "X3", "X4")


#------------------------------------------------------------------------------#

# MODULE UI ----
namesUI <- function(id, number) {
    
    ns <- NS(id)
    
    tagList(
        fluidRow(
            column(6,
                   textInput(ns("name"), label=NULL, placeholder="Enter Name"),
                   
          
            ),
            
            column(6,
                  
                       numericInput(paste0(ns("value.name")), label=NULL, value=T)#,
                   
           
            )
        )
    )
    
}

#------------------------------------------------------------------------------#

# MODULE SERVER ----

names <- function(input, output, session, name.number){
    reactive({
        
        req(input$name, input$value.name)
        #req(input$value.name)
        
       # Create Pair: variable and its value
        df <- data.frame(
            "name.number" = name.number,
            "name" = input$name,
            "value" = input$value.name,
           
            stringsAsFactors = FALSE
        )
        
        return(df)
        
    })
    #reactive({
    #    
    #    req(input$name, input$value.name)
    #    #req(input$value.name)
    #    
    #    # Create Pair: variable and its value
    #    
    #    rv$df <- data.frame(
    #        "name.number" = name.number,
    #        "name" = input$name,
    #        "value" = input$value.name,
    #        
    #        stringsAsFactors = FALSE
    #    )
        
     #   return(df)
        
    #})
}
#------------------------------------------------------------------------------#

# Shiny UI ----

ui <- fixedPage(
    #verbatimTextOutput("test1"),
    
    namesUI("var1", 1),
    h5(""),
    actionButton("insertBtn", "Add another line"),
    actionButton("submit", "Submit"),
    #tableOutput("test2"),
    
    verbatimTextOutput(paste0("test3")),
    verbatimTextOutput(paste0("test4")),
    #verbatimTextOutput(paste0("test5")),
    #verbatimTextOutput(paste0("test6"))
    tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  ")

)

# Shiny Server ----

server <- function(input, output, session) {
    
    total <- reactiveValues(tot = c())
    
    
    add.name <- reactiveValues()
    add.name$df <- data.frame("name.number" = numeric(0),
                                  "name" = character(0),
                                 "value" = numeric(0),
                              "total" = numeric(0),
                                 stringsAsFactors = FALSE)
    
    var1 <- callModule(names, paste0("var", 1), 1)
    
    observe(add.name$df[1, ] <- var1())
    
    
    observeEvent(input$submit, {
        
        print(add.name$df[1,])
        for (i in 1:nrow(add.name$df)){
          total$tot[i] <- sum(total$tot[i], add.name$df[i,3], na.rm=T)
          ns<- NS(paste0("var",i))
          updateNumericInput(session, ns("value.name"), value = 0)
        }
        
        
    })
        
        
        #output$test1 <- renderPrint({
       #     print(add.name$df)
        #})
        
        #output$test2 <- renderTable({
        #    add.name$df
        #})
        output$test3 <- renderText({
            #add.name$df$value[2]
            total$tot
            
            
        })
        #ns<- NS("var1")
        #updateNumericInput(session, ns("value.name"), value = NA)
        #add.name$df$value[1] <- 0
        output$test4 <- renderText({
            #
            add.name$df$value[1]
            #input$ns("value.name")
            
        })
        
        #total$val <- sum(total$val, total$new, na.rm=T) 
        
       
        
        
        #output$num_players <- renderText({input$num_players})
        #total_1$val <- sum(total_1$val, input$score1, na.rm=T) 
        
        #total_2$val <- sum(total_2$val, input$score2, na.rm=T)
        #updateNumericInput(session, "score1", value = NA)
        #updateNumericInput(session, "score2", value = NA)
    
    
    observeEvent(input$insertBtn, {
        
        btn <- sum(input$insertBtn, 1)
        
        insertUI(
            selector = "h5",
            where = "beforeEnd",
            ui = tagList(
                namesUI(paste0("var", btn), btn)
            )
        )
        
        newline <- callModule(names, paste0("var", btn), btn)
        
        observeEvent(newline(), {
            add.name$df[btn, ] <- newline()
        })
        
    })
    
    output$test1 <- renderPrint({
        print(add.name$df[1,])
    })
    
}

#------------------------------------------------------------------------------#

shinyApp(ui, server)