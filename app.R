#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

ui <- shinyUI(fluidPage(
  tags$head(tags$script(HTML(jscode))),
  #actionButton("btn", "Click me to print the value in the text field"),
  #div("Or press Enter when the text field is focused to \"press\" the button"),
  #tagAppendAttributes(
  #  textInput("text", NULL, "foo"),
   # `data-proxy-click` = "btn"),
    titlePanel("title"),
    sidebarLayout(
        sidebarPanel(
            #checkboxInput("var1", label = "Some question 1", value = TRUE),
            #checkboxInput("var2", label = "Some question 2", value = TRUE), 
            numericInput("num_players", label="Number of players",value=2,  min=2, max=12),
            actionButton(inputId = "submit", label = "Submit", icon = NULL, width = NULL),
            #tagAppendAttributes(
            #  textInput("text", NULL, "bar"),
            #  `data-proxy-click` = "submit"),
        ),
        mainPanel(
            
            
            fluidRow(
                column(width=3, offset=1,
            textInput(paste0("name",1), label=NULL, placeholder="Enter Name"),
            tagAppendAttributes(
              numericInput(paste0("score",1), label=NULL, value=T),
              `data-proxy-click` = "submit"),
            
            verbatimTextOutput(paste0("total",1))
            ),
            column(3,
            textInput(paste0("name",2), label=NULL, placeholder="Enter Name"),
            tagAppendAttributes(
              numericInput(paste0("score",2), label=NULL, value=T),
              `data-proxy-click` = "submit"),
            verbatimTextOutput(paste0("score", 2)),
            verbatimTextOutput(paste0("total2"))
            )
            )
            
        ))))
server <- function(input, output, session) {
    #players <- input$num_players
    #input$num_players <-2
    total_1 <- reactiveValues(val=0)
    total_2 <- reactiveValues(val=0)
    
    observeEvent(input$submit, {
        total_1$val <- sum(total_1$val, input$score1, na.rm=T) 
                                                              
        total_2$val <- sum(total_2$val, input$score2, na.rm=T)
        updateNumericInput(session, "score1", value = NA)
        updateNumericInput(session, "score2", value = NA)
        
        })
    
    output$num_players <- renderText({input$num_players})
    calc_score <- reactive({
        input$submit
        isolate(sum(c(input$score1, input$score2)))
    })
    
    output$tot1 <- renderText({
        calc_score()
    })
    #output$tot1a <- renderText({
    #    output$total_1()
    #})
    output$total1 <- renderText({
         #counter$countervalue
        total_1$val# print the latest value stored in the reactiveValues object
    })
    output$total2 <- renderText({
      total_2$val
    })
    observeEvent(input$btn, {
      cat(input$text, "\n")
    })
}
shinyApp(ui = ui, server = server)


