#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(shiny) #using the shiny library to run the shiny app
library(dplyr) #using the dplyr library for easier data filtering 
library(ggplot2) #using the ggplot2 library to create the graphs to present on the app

import_tbl <- readRDS("import.RDS") #import the rds file to create the app based on the variables from the final.R used script

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  titlePanel("PSY 8712 Final Shiny Project"), #creating the title to be displayed
  sidebarLayout(
    sidebarPanel(
      radioButtons("sexselect", #using radiobuttons to provide the option and labeling for the sex group
                   label="Which sex would you like to see?", #providing the question that will be asked to the viewer
                   choices=c("Male",
                             "Female",
                             "All"), #providing the selection of the groups and an option that shows all groups
                   selected="All"), #providing the default as the all option in which both male and female will be shown on the graph
      radioButtons("partyidselect",  #using radiobuttons to provide the option and labeling for the party identification group
                   label="Which party identification do you want to see?", #providing the question that will be asked to the viewer
                   choices=c("Strong Democrat",
                             "Independent", 
                             "Strong Republican", 
                             "All"), #providing the selection of the groups and an option that shows all groups
                   selected="All"), #providing the default as the all option in which all three groups will be shown on the graph
      selectInput("ageselect",  #using radiobuttons to provide the option and labeling for the ages
                   label="Which age do you want to see?", #providing the question that will be asked to the viewer
                   choices = c("18","19","20","21","22","23","24", "25","26","27","28", "29", "30",
                              "31","32","33","34","35","36","37","38","39","40","41","42","43","44",
                              "45","46","47","48","49","50","51", "52","53","54","55","56","57","58",
                              "59", "60","61","62","63","64","65","66","67","68","69","70",
                              "71","72","73","74","75","76","77","78","79","80","81", "82",
                              "83","84","85","86","87","88","89", "All"), #providing the selection of the groups and an option that shows all groups
                   selected = "All"), #providing the default as the all option in which all ages will be shown on the graph
      radioButtons("raceselect",  #using radiobuttons to provide the option and labeling for the race groups
                    label = "Which race do you want to view?", #providing the question that will be asked to the viewer
                    choices = c("White", 
                               "Black", 
                               "Other",
                               "All"), #providing the selection of the groups and an option that shows all groups
                    selected = "All"), #providing the default as the all option in which all three groups will be shown on the graph
      radioButtons("relstrengthselect",  #using radiobuttons to provide the option and labeling for the religious/spiritual strength groups
                   label = "How often of strength connection to faith/spirituality do you want to see?", #providing the question that will be asked to the viewer
                   choices = c("Many times a day", 
                              "Every day", 
                              "Most days", 
                              "Some days", 
                              "Once in a while", 
                              "Never/Almost Never", 
                              "All"), #providing the selection of the groups and an option that shows all groups
                   selected = "All")), #providing the default as the all option in which all groups will be shown on the graph
    mainPanel( #the main panel
      plotOutput("distPlot") #displaying the distribution plot created in the server output
    )
  )
)
server <-  function(input, output) {
  output$distPlot <- renderPlot({ #render plot for main panel
    if (input$sexselect != "All"){ #if the All option is not selected 
      filtered_tbl <- filter(import_tbl, sex == input$sexselect) #filter by selected sex
    }
    else if (input$sexselect == "All"){ #if all option is selected, do not filter
      filtered_tbl <- import_tbl #provide plot as is
    }
    
    if(input$partyidselect != "All"){ #if All option is not selected 
      filtered_tbl <- filter(import_tbl, partyid == input$partyidselect) #filter by selected party identification
    }
    else if (input$partyidselect == "All"){ #if All is selected 
      filtered_tbl <- import_tbl #then do not filter and provide plot as is
    }
    
    if(input$ageselect != "All"){ # if all is not selected
      filtered_tbl <- filter(import_tbl, age == input$ageselect) # filter by selected age 
    }
    else if (input$ageselect == "All"){ #if all is selected
      filtered_tbl <- import_tbl #do not filter and provide plot as is
    }
    
    if(input$raceselect != "All"){ #if All is not selected
      filtered_tbl <- filter(import_tbl, race == input$raceselect) #filter by racial group
    }
    else if (input$raceselect == "All"){ #if All is selected
      filtered_tbl <- import_tbl #do not filter and provide plot as is
    }
    if(input$relstrengthselect != "All"){ #if all is not selected
      filtered_tbl <- filter(import_tbl, rel_strength == input$relstrengthselect) #filter by strength to religion and spirituality
    }
    else if (input$relstrengthselect == "All"){ #if all is selected
      filtered_tbl <- import_tbl #do not filter and provide plot as is
    }
    
    ggplot(filtered_tbl, #create a plot using the filtered tibble
           aes(x= avg_empathy, y=avg_probehav)) + #exploring avg_empathy and avg_probehav for the x and y axis
      geom_jitter() + #using geom_jitter because it spreads the points out horizontally and/or vertically, which makes it easier to see the distribution 
      geom_smooth(method="lm", 
                  color="purple") + #using linear regression line and the color purple to add
      labs(x = "Empathy Toward Others", y = "Prosocial Behavior", title = "Scatterplot of Empathy and Prosocial Behavior") #providing title for the scatterplot to make it easily interpretable
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
