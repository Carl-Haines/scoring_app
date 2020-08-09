library(shiny)
library(ggplot2)
library(shinythemes)
###https://stackoverflow.com/questions/38083286/r-shiny-how-to-dynamically-append-arbitrary-number-of-input-widgets###

#LHSchoices <- c("X1", "X2", "X3", "X4")
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

#------------------------------------------------------------------------------#

# MODULE UI ----
namesUI <- function(id, number) {
    
    ns <- NS(id)
    
    tagList(
        fluidRow(
            column(6,
                   textInput(ns("name"), label=NULL, placeholder="Enter Name"),
                   
          
            ),
            
            
            column(5,
                   tagAppendAttributes(
                    #numericInput(paste0(ns("value.name")), label=NULL, value=T)#,
                   textInput(ns("value.name"), label=NULL, placeholder=NULL, value="0"),
                   `data-proxy-click` = "submit")
                   
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

ui <- navbarPage("The Scoreboard App",
                 theme = shinytheme("cosmo"),
                 
                tabPanel("Molkky",
                         tags$head(tags$script(HTML(jscode))),
    #verbatimTextOutput("test1"),
                        fluidRow(
                            column(4,
                                   
                                   
                                    namesUI("var1", 1),
                                    h5(""),
                                    actionButton("insertBtn", "New player", class = "btn-secondary"),
                                    actionButton("submit", "Submit", class = "btn-primary")
                          ),
                          #tableOutput("test2"),
                          
                          
                          #verbatimTextOutput(paste0("test4")),
                            column(8,
                              plotOutput("plot"),
                              #verbatimTextOutput(paste0("test3")),
                            )
                        )
                          #verbatimTextOutput(paste0("test5")),
                          #verbatimTextOutput(paste0("test6"))
                          ##tags$script("
                          #Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                          ##  Shiny.onInputChange(variableName, null);
                          #});
                        #")
                      
                      )#,
#tabPanel("Standard")
)

# Shiny Server ----

server <- function(input, output, session) {
    
    total <- reactiveValues(tot = c())
    x <- reactiveValues(tot = c())
    
    
    add.name <- reactiveValues()
    add.name$df <- data.frame("name.number" = numeric(0),
                                  "name" = character(0),
                                 "value" = numeric(0),
                              #"total" = numeric(0),
                                 stringsAsFactors = FALSE)
    
    var1 <- callModule(names, paste0("var", 1), 1)
    
    observe(add.name$df[1, ] <- var1())
    
    
    observeEvent(input$submit, {
      try({
      df = isolate(add.name$df)
      print(df)
        
        for (i in 1:nrow(add.name$df)){
          if (!is.na(as.numeric(df[i,3])) & as.numeric(df[i,3])!=0 ) {
            
            total$tot[i] <- sum(total$tot[i],as.numeric( df[i,3]), na.rm=T)
            x$tot[i] <- 0
          }
          else if (df[i,3] == "x" | df[i,3] == "X") {
            x$tot[i] <- sum(x$tot[i], 1, na.rm=T) 
            if (x$tot[i]>3) x$tot[i]=3
            
          }
          else if (df[i,3] == "-x" | df[i,3] == "-X" | df[i,3] == "- x" | df[i,3] == "- X") {
            x$tot[i] <- sum(x$tot[i], -1, na.rm=T)
            if (x$tot[i]<0) x$tot[i]=0
          }
            
          ns<- NS(paste0("var",i))
          updateNumericInput(session, ns("value.name"), value = 0)
        }
        
      
        output$plot <- renderPlot({
        #input$newplot
        # Add a little noise to the cars data
        #cars2 <- 1:total$tot[1]
        df = isolate(add.name$df)
        total$tot[is.na(total$tot)] = 0
        n_players = nrow(df)
        if (length(total$tot) == n_players) df$total = total$tot
        else if (length(total$tot) > 0){
          new_tot <- rep(0, n_players)
          new_tot[1:length(total$tot)] <- total$tot
          df$total <- new_tot
        }
        else if ((length(total$tot) > 0)==F) total$tot = c(0) 
        
        excess <- df$total - 50
        excess_players <- (excess>0)
        df$total[excess_players] <- 25
        total$tot[excess_players] <- 25
        print(df$total)
        winner = (excess==0)
        print(excess)
        
        df$x = x$tot
        df$x[is.na(df$x)] = 0 
        df$x[winner] = 100
        df$x <- factor(df$x, levels=c(0,1,2,3,100))
        #names(df)[5] <- total
        print(df)
        
        df$name <- factor(df$name, levels = df$name)
        
        ggplot(data=df, aes(x=name, y=total))+
          geom_segment(aes(x=name, xend=name, y=0, yend=total), color="grey")+
          geom_point(aes(color=x), size=10)+
          geom_text(aes(label=total), position=position_dodge(width=0.9), vjust=0.4, colour = "white", fontface = "bold", size = 5)+
          #scale_fill_continuous(values=c("deepskyblue1", "yellow1"))+
          scale_color_manual(values=c("#e6d700", "#E69F00","#e63600",  "#999999", "#00a616" ),
                             breaks = c("0", "1", "2", "3", "100"),
                             labels = c("", "X", "XX", "XXX", "Winner"),
                             drop = FALSE)+
          theme_light() +
          theme(panel.grid.major.x=element_blank(),
                panel.border = element_blank(),
                axis.ticks.x = element_blank()
                )+
          #scale_x_discrete(breaks=1:nrow(add.name$df),
          #                 labels=add.name$df[,2])+
          xlab("")+
          ylab("Total")+
          ylim(0,50)+
          theme(axis.text=element_text(size=14, face = "bold"),
                axis.title=element_text(size=18, face = "bold"),
                legend.text=element_text(size=12, face = "bold"),
                legend.title=element_blank()
                )
        
                 
        
      })
      })
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
        
        output$test4 <- renderText({
            
            add.name$df$value[1]
            
            
        })
        
        
    
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