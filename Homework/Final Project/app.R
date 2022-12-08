#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)

dest <- read_delim("dest.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  select(srch_destination_id, starts_with("popular_food"))

data <- read.csv("new_data.csv") %>% 
  filter(is_booking == 1) %>%
  inner_join(dest, by = "srch_destination_id") %>%
  pivot_longer(col = starts_with("popular_food"), names_to = "popular", values_to = "probability") %>%
  select(popular, probability)

names <- data %>%
  distinct(popular)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Compare two Popular Scores"),

    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "popular1", 
                label = "Popular Score 1", 
                choices = names), 
    
    selectInput(inputId = "popular2", 
                label = "Popular Score 2", 
                choices = names), 
    
    selectInput(inputId = "popular3", 
                label = "Popular Score 3", 
                choices = names), 

        # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(outputId = "boxplot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$boxplot <- renderPlotly({
      data %>%
        filter(popular == input$popular1 | popular == input$popular2 | popular == input$popular3) %>%
        plot_ly(x = ~popular, y = ~probability, type = "box", color = ~popular, boxpoints = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
