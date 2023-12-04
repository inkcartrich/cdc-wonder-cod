#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(cowplot)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cyborg"),
    
    # Application title
    titlePanel("CDC WONDER Underlying Cause of Death"),
 
    sidebarLayout(
        sidebarPanel(
            helpText("Dataset for mortality as a result of mental and behavioral disorders from 1999 - 2017 
                     in the United States."),
            selectInput("state", "State", ""),
            selectInput("cod", "Cause of Death", ""),
            
            radioButtons("vis", "Visualization", c("Mortality" = "Deaths",
                                                   "Crude Rate" = "Crude.Rate"),
                         selected = "Deaths"),
            
            helpText("Showing top five counties:"),
            
            fluidRow(
                tableOutput("contents")
                )
    ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    

)

server <- function(input, output, session) {
    

    myData <- reactive({
        read.delim("cod_county_2017.txt")
    })
    
    observe({
        updateSelectInput(session, "state",
                          label = "State",
                          choices = c("All", unique(as.character(myData()[["State"]]))),
                          selected = "All")
        
        updateSelectInput(session, "cod",
                          label = "Cause of Death",
                          choices = c("All", unique(as.character(myData()[["Cause.of.death"]]))),
                          selected = "All")
    })
    
    myDataFilt <- reactive({
                    
                    selection <- input[["vis"]]
                    
                    myData() %>% 
                        filter(
                            (if(input[["state"]] != "All"){State == input[["state"]]}
                             else{State == State}),
                            (if(input[["cod"]] != "All"){Cause.of.death == input[["cod"]]}
                             else{Cause.of.death == Cause.of.death})
                        ) %>% 
                    select(fips = County.Code, value = selection) %>% 
                    filter(!is.na(fips), value != "Unreliable") %>% 
                    group_by(fips) %>% 
                    summarize(tot = sum(as.numeric(value))) %>% 
                    as.data.frame()
    })
    
    output[["distPlot"]] <- renderPlot({
        x    <- myDataFilt()
        state <- if(input[["state"]] != "All"){input[["state"]]}
                    else{}
        state_title <- if(input[["state"]] != "All"){input[["state"]]}
                        else{"the United States"}
        cod_title <- if(input[["cod"]] != "All"){tolower(paste("from", input[["cod"]]))}
                        else{"due to mental and behavioral disorders"}
        select_title <- if(input[["vis"]] == "Deaths"){"Mortality"}else{"Crude death rate"}
            
        usmap::plot_usmap(regions = "counties", data = x, values = "tot", color = NA, include = as.character(state)) +
            scale_fill_continuous(
                low = "navy", high = "orange", name = "Total"
            ) +
            labs(#title = paste(select_title, "in",  state_title, cod_title, "(1999 - 2017)"),
                 #subtitle = "All sub-national mortality and population figures less than 10 persons are suppressed (shown in gray)",
                 caption = "CDC WONDER Underlying Cause of Death") +
            theme(
                text = element_text(color = "white"),
                legend.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
            )
    }, width = 100%, height = 100%, bg = "black")
    
    output[["contents"]] <- renderTable({
        
        selection <- input[["vis"]]
        
        d <- myData() %>% 
            filter(
                (if(input[["state"]] != "All"){State == input[["state"]]}
                 else{State == State}),
                (if(input[["cod"]] != "All"){Cause.of.death == input[["cod"]]}
                 else{Cause.of.death == Cause.of.death})
            ) %>% 
            select(fips = County, value = selection) %>% 
            filter(!is.na(fips), value != "Unreliable") %>% 
            group_by(fips) %>% 
            summarize(tot = sum(as.integer(value))) %>% 
            arrange(desc(tot)) %>% 
            head(5)
        
        colnames(d) <- c("County, State",
                         "Value")
        d
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
