#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Chicago Inspection Predictor"),
    h4("Created By Rongbin Ye for JHU Data Product Development course."),
    h5("This model requires time for loading and runing after submitting."),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("lat",
                        "Latitude:",
                        min = 41.6481,
                        max = 42.0209,
                        step = .001,
                        value = 41.7000
            ),
            sliderInput("lng",
                        "Longtitude:",
                        min = -87.9144,
                        max = -87.5251,
                        step = .001,
                        value = -87.7500
            ),
            selectInput("FType","Choose a Facility Type:",
                        list('restaurant', 'school', 'bakery', 'catering')
            ),
            selectInput("RType", "Choose a Risk Type:", list("risk 1 (high)","risk 2 (medium)","risk 3 (low)")
            ),
            checkboxInput("Showp","Show the Summary", TRUE
            ),
            submitButton("Submit")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            h3("The Location of ideal location"),
            leafletOutput("Map_Locator"),
            h3("The Model Summary"),
            textOutput("Show_summary"),
            h3("The Predicted Result"),
            textOutput("Predicted_Result")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session){
    
    ## Loading Data
    inspection_data <- data.table::fread('https://data.cityofchicago.org/api/views/4ijn-s7e5/rows.csv?accessType=DOWNLOAD')
    
    ## Data Cleaning
    main_data <- inspection_data %>% 
        filter(inspection_data$Results == c('Pass', 'Fail', 'Pass w/ Conditions') 
               & inspection_data$`Facility Type` == c('Restaurant', 'School', 'Bakery', 'Catering')
               & inspection_data$`Inspection Type` == 'Canvass')%>%
        select(`Inspection ID`,`Facility Type`, Results, Risk, Latitude, Longitude, Location)
    
    ## Drop all NAs
    main_data <- drop_na(main_data)
    
    ### Convert all lower class
    main_data$`Facility Type` <- tolower(main_data$`Facility Type`)
    main_data$Results <- tolower(main_data$Results)
    main_data$Risk <- tolower(main_data$Risk)
    
    ### Target for Pass or Fail
    main_data$Results <- ifelse(main_data$Results == "fail", "fail", "pass")
    
    ### Convert to factors
    main_data$Results <- as.factor(main_data$Results)
    main_data$Risk <- as.factor(main_data$Risk)
    main_data$`Facility Type` <- as.factor(main_data$`Facility Type`)
    
    ### Round Up lat & Lon
    main_data$Latitude <- round(main_data$Latitude, 4)
    main_data$Longitude <- round(main_data$Longitude, 4)
    
    ### main_data
    main_data<- main_data %>% mutate(Ftype = `Facility Type`) %>%
        select(-`Inspection ID`, -Location,- `Facility Type`)
    
    
    ## Model Trainning
    model_Go <- glm(formula = Results ~ Ftype + Risk + Latitude + Longitude, 
                    data = main_data,
                    family = binomial)
    
    ## Tab 1: Major Location 
    output$Map_Locator <- renderLeaflet({
        Ftype <- as.character(input$FType)
        Risk <- as.character(input$RType) 
        lat0 <- as.numeric(input$lat)
        lng0 <- as.numeric(input$lng)
        leaflet(data = df) %>% 
            addTiles() %>%
            addMarkers(
                lat = lat0,
                lng = lng0,
            )
    })
    
    ## Tab 2: Summary
    #    function(input, output, session){
    ## Reactive Data
    
    output$Show_summary <- renderText({
        Ftype <- as.character(input$FType)
        Risk <- as.character(input$RType) 
        lat0 <- as.numeric(input$lat)
        lng0 <- as.numeric(input$lng)
        summary_all <- paste("The Imagined condition is ", Ftype, 
                             ", at risk level of ",Risk, 
                             ". The location is at (", lat0,
                             ",", lng0, ").")
        
        Sum_Msg <- ifelse(input$Showp, summary_all, "Information Abbreviated")
        
        Sum_Msg
    })
    
    #    }
    
    ## Tab 3: Present_Final
    
    ## Reactive Data 
    test_df <- reactive({
        df <- data.frame(
            Ftype = input$FType, 
            Risk = input$RType,
            Latitude = input$lat, 
            Longitude = input$lng)
    })
    
    modeloutput <- reactive({
        final_predict <- predict(model_Go, newdata = test_df(), type = "response")
    })
    
    {   
        output$Predicted_Result <- renderText({"<font color=\"#FF0000\">"
            ## Creating the record
            probability <- round(modeloutput(),4)
            outcome <- ifelse(probability > 0.6, 
                              "The project asked to predict might pass the canvass inspection.",
                              "The project asked to predict might fail the canvass inspection.")
            outcome <- paste(outcome, "The raw probability is ", probability)
            outcome
        })
    }
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
