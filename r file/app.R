library(shiny)
library(tidyverse) 
library(shinythemes)

sinatra <- read_rds("sinatra.rds") 
ewf <- read_rds("ewf.rds") 

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("superhero"),
    navbarPage("About Games", 
               tabPanel("About", 
                        h1("About the data"), 
                        p("So far, I have collected a variety of data sets for my project. 
The graphs on this app represent two of them. I made a Frank Sinatra plot using a data set that catalogued the most popular songs on holiday seasons. 
I know, at least generally, that I may want to chart the popularity of songs or genres over time, so I thought this was a good start. 
I narrowed down that tibble to Frank Sinatra holiday songs just as an excercise. 
Test
The Earth, Wind & Fire plot was made with a different data set that has information on more than 160,000 songs. I filtered it for every EWF Song 
and tried to model their most active years with the release_data column and the summarise() function. These are not necessarily graphs I'll need for the 
final project, but they are examples of what I can do with the data.")), 
               tabPanel("Analysis", 
                        mainPanel(  
                            titlePanel("Graphs"),
                            br(),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Sinatra", plotOutput("sinatra_plot")),
                                        tabPanel("EWF",  plotOutput("ewf_plot"))
                            )
                            
                            
                            
                            
                        )        
                        
                        
                        
                        ))
        # Show a plot of the generated distribution 
    
        
    )  


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sinatra_plot <- renderPlot({ 
        sinatra %>% 
            ggplot(aes(x = title, y = total)) + 
            geom_col() + 
            labs(title = "Frank Sinatra Holiday Songs in Top Charts Internationally", 
                 x = "Song Title", 
                 y = "Number of Countries Where Songs are in Top Charts") + 
            theme_bw()
        
    }) 
    
    output$ewf_plot <- renderPlot  ({ 
        ewf %>% 
            ggplot(aes(x = release_year, y = total)) + 
            geom_col() + 
            labs(title = "Number of Earth, Wind & Fire Songs per Year", 
                 subtitle = "Do You Remember? The 21st Night of September?", 
                 x = "Release Year", 
                 y = "Total Number of Songs") + 
            theme_bw()
    })  
    
    output$about <- renderText({"So far, I have collected a variety of data sets for my project. 
The graphs on this app represent two of them. I made a Frank Sinatra plot using a data set that catalogued the most popular songs on holiday seasons. 
I know, at least generally, that I may want to chart the popularity of songs or genres over time, so I thought this was a good start. 
I narrowed down that tibble to Frank Sinatra holiday songs just as an excercise. 

The Earth, Wind & Fire plot was made with a different data set that has information on more than 160,000 songs. I filtered it for every EWF Song 
and tried to model their most active years with the release_data column and the summarise() function. These are not necessarily graphs I'll need for the 
final project, but they are examples of what I can do with the data."})
} 

# Run the application 
shinyApp(ui = ui, server = server)
