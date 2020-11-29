library(shiny)
library(tidyverse) 
library(shinythemes)
library(spotifyr)
library(shinyWidgets)

sinatra <- read_rds("sinatra.rds") 
ewf <- read_rds("ewf.rds") 
kanye <- get_artist_audio_features('kanye west')

Sys.setenv(SPOTIFY_CLIENT_ID = '8a8f9963e2b84ea485694d8a99919904')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1f4676b66f6c49389bfb5093ebfa0dee')

access_token <- get_spotify_access_token()

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage("kanye omaRi west", 
               tabPanel("About", 
                        h1("Introduction"), 
                        p("To say the least, Kanye Omari West is an interesting guy. An artist, producer, fashion designer, celebrity, and cultural icon, West has established himself as both a pop culture icon and a divisive national figure. 
                           However, while some might consider West to be unpredictable, maybe there are some trends or patterns we can observe in his music and activity. For example, how does the music vary between West's albums? 
                           Are there more commonalities between his hits than we might think? Beyond his music, what about his presidential run this year? Did it affect West's streaming or Twitter engagement? 
                           What can this tell us about presidential runs and their effect on the businesses of political outsiders? 
                           In this project, I seek out to confront all of these questions - not necessarily to draw a definitive answer - but to perhaps shed some light on the mystery and fascination behind West's career."), 
                        h5("The source code for this Shiny App can be found at my GitHub", a("HERE", href="https://github.com/dashchin/spotify_data_test"))), 
               tabPanel("Analysis", 
                        tabsetPanel(type = "tabs",
                                    tabPanel("Album Analysis",   
                                             sidebarPanel( 
                                                 textInput(inputId = "entered_text",                      # a name for the value you choose here
                                                           label = "Place your title text here:",         # a label above the text box
                                                           value = "Example"),
                                                 sliderInput(inputId = "selected_year",                   # a name for the value you choose here
                                                             label = "Choose a year:",  # the label to display above the slider
                                                             min = 1958, max = 2019, value = 1958, animate = TRUE)
                                             ),          
                                             mainPanel(),
                                    ), 
                                    tabPanel("Track Analysis ",
                                             mainPanel()
                                    ),   
                                    tabPanel("Sample Analysis ",
                                             mainPanel()
                                    ),
                                    tabPanel("Tempo Analysis ",
                                             mainPanel()
                                    ), 
                                    tabPanel("Key analysis ", 
                                             mainPanel(
                                                 titlePanel("Kanye's Keys"),
                                                 titlePanel("Kanye's Favorite Keys (Overall)"),
                                                 br(),
                                                 plotOutput("kanye_keys_plot"), 
                                                 br(),
                                                 shinyWidgets::sliderTextInput(inputId = "selected_album", 
                                                                               label = "Pick an Album (Most Recent First):", 
                                                                               choices = c(unique(kanye$album_name))
                                                 ),  
                                                 titlePanel("Kanye's Favorite Keys (Per Album)"),
                                                 plotOutput("kanye_keys_plot2"), 
                                                 titlePanel("Distribution of Kanye's Keys"), 
                                                 plotOutput("kanye_keys_plot3")
                                             )
                                    ), 
                                    tabPanel("Presidential Run ",   
                                             mainPanel(  
                                                 titlePanel("Graphs")
                                             )
                                    ),
                                    tabPanel("Danceability",   
                                             mainPanel(  
                                                 titlePanel("Graphs"),
                                                 br(),
                                                 plotOutput("my_plot")
                                             )
                                    )
                        )
               )
    )
) 


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Kanye's Favorite Keys (Overall) 
    
    output$kanye_keys_plot <- renderPlot  ({    
        kanye %>% 
            count(key_mode, sort = TRUE) %>% 
            head(20) %>%  
            ggplot(aes(x = fct_reorder(key_mode, n), y = n)) + 
            geom_col(fill = "orange") + 
            labs(x = "Key", 
                 y = "Total number of songs written in key", 
                 title = "Kanye's Favorite Keys",
                 subtitle = "What key will he use next?") + 
            theme_bw() 
        
    }) 
    
    # Kanye's Favorite Keys (Per Album)
    
    output$kanye_keys_plot2 <- renderPlot  ({    
        kanye %>% 
            filter(album_name == input$selected_album) %>% 
            count(key_mode, sort = TRUE) %>% 
            head(20) %>%  
            ggplot(aes(x = fct_reorder(key_mode, n), y = n)) + 
            geom_col(fill = "orange") + 
            labs(x = "Key", 
                 y = "Total number of songs written in key", 
                 title = "Kanye's Favorite Keys",
                 subtitle = "What key will he use next?") + 
            theme_bw() 
        
    }) 
    
    # Distribution of Kanye's Keys
    
    output$kanye_keys_plot3 <- renderPlot ({ 
        kanye %>% 
            distinct(track_name, album_name, album_release_date, key_mode, mode_name) %>%  
            mutate(year = substr(album_release_date, 1, 4)) %>%  
            select(mode_name, album_name, year, album_release_date) %>%  
            group_by(mode_name, year, album_release_date) %>%  
            summarize(total = n()) %>%  
            arrange(year) %>%  
            filter(mode_name == "minor") %>%  
            group_by(year) %>%  
            summarize(total = sum(total))%>%  
            ggplot(aes(x = year, y = total)) + 
            geom_point() + 
            labs(title = "Minor Keys per Album Release Year", 
                 x = "Year", 
                 y = "Number of Songs in Minor") 
        
    })
    
    
    
} 

# Run the application 
shinyApp(ui = ui, server = server)
