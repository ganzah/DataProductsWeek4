#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

# Read data from file and preprocess it. Only some columns are selected 
# (neighbourhood, neighbourhood_group, room_type, price, number_of_reviews, reviews_per_month)
# and data outliers are filtered (remove listings with prices equal to 0 and greater than the .9 quantile)
set.seed(20200224)
data <- read.csv("AB_NYC_2019.csv")
data <- data %>% 
    select(neighbourhood, neighbourhood_group, room_type, price, number_of_reviews, reviews_per_month) %>% 
    replace(., is.na(.), 0) %>% filter(price > 0) %>% filter(price <= quantile(data$price, .9))


ui <- fluidPage(

    titlePanel("AirBnB data for NYC"),
    
    # Instructions
    ##############
    hr(),
    p(
        "This shiny app shows an example visualization of data from airbnb listings and metrics in NYC.
        The dataset can be found on ", 
        a(href = "https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data", "Kaggle"),
        ". Only some columns are selected (neighbourhood, neighbourhood_group, room_type, price, 
        number_of_reviews, reviews_per_month) and data outliers are filtered (remove listings with 
        prices equal to 0 and greater than the .9 quantile). Also, NA's are replaced by 0 "
    ),
    p(
        "The app shows two plots. The first plot is a boxplot of prices by neighbourhood group and 
        it is a reactive plot. You can select a neighbourhood in the checkboxed on the left panel 
        see the plot update immediately."
    ),
    p(
        "The second plot is a boxplot that separates data also by 
        room type. This second plot is not reactive: you need to click the button 'Show 2nd plot!' 
        to visualize. The server will generate the plot in response to the button click. You need to 
        click the button everytime you want to update this second plot."
    ),
    hr(),
    ##############
    
    sidebarLayout(
        sidebarPanel(
            #Available neighbourhood groups
            checkboxGroupInput("group", "Neighbourhood groups:",
               c("Bronx" = "Bronx",
                 "Brooklyn" = "Brooklyn",
                 "Manhattan" = "Manhattan",
                 "Queens" = "Queens",
                 "Staten Island" = "Staten Island"),
               selected = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
               ),
        ),

        mainPanel(
            # First plot, reactive
            plotOutput("firstPlot"),
            
            #Action button and second plot, non-reactive
            actionButton("createSecondPlot", "Show 2nd plot!"),
            plotOutput("secondPlot")
    )
    )
)

server <- function(input, output) {

    # Reactively generates new data when user changes neghbourhood selection
    newData <- reactive({data %>% filter(neighbourhood_group %in% input$group)})
    
    # Creates first plot
    output$firstPlot <- renderPlot({
        ggplot(data = newData(), aes(x = neighbourhood_group, y = price, fill=neighbourhood_group)) + 
        geom_boxplot() + 
        labs(title = "Prices by Neighbourhood groups", x = "Neighbourhood groups", y = "Price")
    })
    
    # Creates second plot on action button event
    priceByRoomType <- eventReactive(input$createSecondPlot, {
        ggplot(newData(), aes(x = room_type, y = price, fill=neighbourhood_group)) + geom_boxplot() +
        labs(title = "Prices by Neighbourhood groups and Room types", x = "Room types", y = "Price")
    })
    output$secondPlot <- renderPlot({
        priceByRoomType()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
