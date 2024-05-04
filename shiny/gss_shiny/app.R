#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny)
library(dplyr)
library(ggplot2)

import_tbl <- readRDS("import.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("PSY 8712 Final Shiny Project"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("sexselect",
                   label="Which sex would you like to see?",
                   choices=c("Male",
                             "Female",
                             "All"),
                   selected="All"),
      radioButtons("partyidselect",
                   label="Which party identification do you want to see?",
                   choices=c("Strong Democrat",
                             "Independent", 
                             "Strong Republican", 
                             "All"),
                   selected="All"),
      selectInput("ageselect",
                   label="Which age do you want to see?",
                   choices = c("18","19","20","21","22","23","24", "25","26","27","28", "29", "30",
                              "31","32","33","34","35","36","37","38","39","40","41","42","43","44",
                              "45","46","47","48","49","50","51", "52","53","54","55","56","57","58",
                              "59", "60","61","62","63","64","65","66","67","68","69","70",
                              "71","72","73","74","75","76","77","78","79","80","81", "82",
                              "83","84","85","86","87","88","89", "All"),
                   selected = "All"),
      radioButtons("raceselect",
                    label = "Which race do you want to view?",
                    choices = c("White", 
                               "Black", 
                               "Other",
                               "All"),
                    selected = "All"),
      radioButtons("relstrengthselect",
                   label = "How often of strength connection to faith/spirituality do you want to see?",
                   choices = c("Many times a day", 
                              "Every day", 
                              "Most days", 
                              "Some days", 
                              "Once in a while", 
                              "Never/Almost Never", 
                              "All"),
                   selected = "All")),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
server <-  function(input, output) {
  output$distPlot <- renderPlot({ #render plot for main panel
    if (input$sexselect != "All"){ #if the All option is not selected 
      filtered_tbl <- filter(import_tbl, sex == input$sexselect) #then filter by selected gender
    }
    else if (input$sexselect == "All"){ #if all option is selected, do not filter
      filtered_tbl <- import_tbl
    }
    
    if(input$partyidselect != "All"){ #if All option is not selected 
      filtered_tbl <- filter(import_tbl, partyid == input$partyidselect)#then filter by selected pid
    }
    else if (input$partyidselect == "All"){ #if All is selected 
      filtered_tbl <- import_tbl #then do not filter
    }
    
    if(input$ageselect != "All"){ # if both is not selected
      filtered_tbl <- filter(import_tbl, age == input$ageselect) #then filter by selected president choice
    }
    else if (input$ageselect == "All"){ #if both is selected
      filtered_tbl <- import_tbl #do not filter
    }
    
    if(input$raceselect != "All"){ #if All is not selected
      filtered_tbl <- filter(import_tbl, race == input$raceselect) #filter by selected income level
    }
    else if (input$raceselect == "All"){ #if All is selected
      filtered_tbl <- import_tbl #do not filter
    }
    if(input$relstrengthselect != "All"){ #if all is not selected
      filtered_tbl <- filter(import_tbl, rel_strength == input$relstrengthselect) 
    }
    else if (input$relstrengthselect == "All"){ #if all is selected
      filtered_tbl <- import_tbl #do not filter
    }
    
    ggplot(filtered_tbl, #create a plot using the filtered tibble
           aes(x= avg_empathy, y=avg_probehav)) + 
      geom_jitter() + #jitter plot because it makes the graph better to look at.
      geom_smooth(method="lm", 
                  color="purple") +
      labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Empathy and Prosocial Behavior")
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
