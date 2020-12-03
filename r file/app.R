
# Required libraries for the app

library(ggplot2) 
library(shiny)
library(tidyverse) 

# Overall tone of the app

library(shinythemes)

# Obtaining data from Spotify via its API 

library(spotifyr)
Sys.setenv(SPOTIFY_CLIENT_ID = '8a8f9963e2b84ea485694d8a99919904')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '1f4676b66f6c49389bfb5093ebfa0dee')
access_token <- get_spotify_access_token()

# Creating tools for the App

library(shinyWidgets)

# Used for creating linear models and regressions.

library(rstanarm)

# Creating tables for predictive models

library(gt)
library(gtsummary)
library(broom.mixed)
library(gapminder)

# Using functions to make animated plots

library(plotly)
library(gganimate)

# Used for counting collabs

library(stringr)

# For creating the  worldcloud

library(wordcloud2) 



# Getting a large data set from the spotifyr library 

kanye <- get_artist_audio_features('kanye west')

# Importing a csv of Kanye's streaming statistics on Spotify

Kanye_streams <- read_csv("Kanye streams.csv")

# Joining together the streams and spotify data 

full_set <- Kanye_streams %>% 
    mutate(collabs = str_count(With, ",") + 1) %>%  
    select(Track, Streams, With, collabs) %>%  
    rename(track_name = Track) %>%  
    full_join(kanye, by = "track_name") %>%  
    distinct(track_name, .keep_all = TRUE) 

# A tibble representing an album released in 2000 

new_obs <- tibble(album_release_year = 2020)

# A table of the loudness model 

gt_tbl <- stan_glm(data = kanye, refresh = 0, 
                   loudness ~ album_release_date) %>%
    tbl_regression(intercept = FALSE) %>%
    as_gt() %>%
    tab_header(title = "Regression of Loudness by Release Date",
               subtitle = "The Effect of Time on Loudness") 

# Creating a model of loudness as a function of album release year

model1 <- stan_glm(data = kanye, refresh = 0, loudness ~ album_release_year) 

# Modified version of the kanye_streams object that counts the number of
# features.

streamsf_plot <- Kanye_streams %>% 
    mutate(collabs = str_count(With, ",") + 1) %>%  
    select(Streams, With, collabs, Track) %>% 
    remove_missing(na.rm = TRUE) %>% 
    ggplot(aes(x = collabs, y = Streams, text = paste(
        Track 
    ))) + 
    geom_point(alpha = 0.5, colour = "red") + 
    labs(x = "Number of Features", 
         y = "Number of Streams", 
         title = "Streams as a function of features", 
         subtitle = "Including singles.")


 # Knit options

knitr::opts_chunk$set(echo = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage("kanye omaRi west", 
               tabPanel("About the Project", 
                        h1("Introduction"), 
                        p("To say the least, Kanye Omari West is an interesting 
                        guy. An artist, producer, fashion desggigner, celebrity, 
                        and cultural icon, West has established himself as both 
                        a pop culture icon and a divisive national figure.
                        However, while some might consider West to be 
                        unpredictable, maybe there are some trends or patterns 
                        we can observe in his music and activity. For example, 
                        how does the music vary between West's albums? 
                        Are there more commonalities between his hits than we 
                        might think? Beyond his music, what about his 
                        presidential run this year? Did it affect West's 
                        streaming or Twitter engagement? 
                        What can this tell us about presidential runs and their 
                        effect on the businesses of political outsiders? In this 
                          project, I seek out to confront all of these questions 
                          - not necessarily to draw a definitive answer - but to 
                          perhaps shed some light on the mystery and fascination 
                          behind West's career."), 
                        h5("The source code for this Shiny App can be found at 
                           my GitHub", 
                           a("HERE", 
                             href="https://github.com/dashchin/spotify_data_test"
                             )
                           ), 
                        imageOutput("myImage"),
                        img(src="outfile.gif", align = "left"), 
                        p("Image taken from", 
                           a("this pinterest post", 
                             href="https://br.pinterest.com/pin/4214384776339037
                             54/?amp_client_id=CLIENT_ID(_)&mweb_unauth_id={
                             {default.session}}&amp_url=https%3A%2F%2Fbr.
                             pinterest.com%2Famp%2Fpin%2F837669599427200817%2F")
                           )
                        ),
                        
               tabPanel("Analysis", 
                        tabsetPanel(type = "tabs",
                                    
                                    # Streams analysis page
                                    
                                    tabPanel("Popularity Analysis ",
                                             mainPanel(
                                                 titlePanel("A Stastical 
                                                 Analysis
                                                            of Streams"), 
                                                 br(), 
                                                 br(),
                                                 p("One interesting 
                                                 (and pertinent) area of 
                                                 statistics in the music 
                                                 industry is stream count. With 
                                                 Spotify being one of the major 
                                                 streaming platforms of the day, 
                                                 and Kanye West being one of the 
                                                 most prominent figures in the 
                                                 music industry, analyses of 
                                                 Kanye's music can give insight 
                                                 into greater trends of 
                                                 entertainment. How well do 
                                                 these stream numbers map onto 
                                                 other artists?Using temperance, 
                                                 we can say that we should be 
                                                 cautious. One thing to keep in 
                                                 mind is that Kanye holds his 
                                                 own creative direction and his 
                                                 own team of producers, so these 
                                                 numbers and predictions may not
                                                 map on perfectly to other 
                                                 artists. That being said, as 
                                                 Kanye is someone with a lot of 
                                                 influence, maybe we can explain 
                                                 some trends through the 
                                                 patterns we see in his music."), 
                                                 em("Hover over a title to view 
                                                    each song's cumulative 
                                                    streams"),
                                                 wordcloud2Output('wordcloud', 
                                                                  height="600px", 
                                                                  width = "100%"
                                                                  ),
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 titlePanel("Popularity 
                                                            and Features"),
                                                 p("Here's another consideration 
                                                 when talking about popularity: 
                                                 who else is on the song? In the
                                                 past few years, the conventional
                                                 thought has been that the number
                                                 of artists feature on pop songs
                                                 is increasing. From a market 
                                                 and streaming standpoint, this 
                                                 does seem to make intuitive 
                                                 sense. When there are more 
                                                 artists in a given track, each 
                                                 artist has a chance to increase 
                                                 their fanbase by explaning into 
                                                 other ones.Moreover, upon 
                                                 release, the audience of the 
                                                 song is expected to increase 
                                                 dramatically. Below, we can see 
                                                 the number of artists featured 
                                                 in a given track."), 
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 em("Hover over each 
                                                            point to view the song 
                                                            title and number of 
                                                            respective featured 
                                                            artists."),
                                                 plotlyOutput("streamsfeatures"),
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 br(), 
                                                 p("This graph seems to provide 
                                                 some fascinating implications. 
                                                 While 'Champions' is by far the
                                                 song with the most features, it
                                                 is not even close to the 
                                                 collaborative track with the 
                                                 greatest number of hits. As 
                                                 shown by the data, 'I Love it 
                                                 With' (featuring Lil Pump) 
                                                 seems to reign supreme. 
                                                 Moreover, other songs with only
                                                 one feature, like 
                                                 'Mixed Personalities', seem to 
                                                 do better than songs with more 
                                                 features? Perhaps one of the 
                                                 greater points we can draw from 
                                                 this graph is that there is not 
                                                 a high correlation between the 
                                                 number of collaborators and the 
                                                 number of streams. A lot of 
                                                 things go into making a popular 
                                                 song, and the number of 
                                                 features is just a small part. 
                                                 But with that point in mind, 
                                                 what about variables besides
                                                 the number of collaborators? 
                                                 What if Champions was released 
                                                 in 2020 instead of 2016? Is Lil 
                                                 Pump the key to a smash hit? In 
                                                 the next section, we will look 
                                                 at the influence of more 
                                                 variables."),
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 titlePanel("Does Danceable = 
                                                            Replayable?"), 
                                                 p("Many would agree that Kanye 
                                                 doesn't quite make 'dance' pop 
                                                 (Think artists like Lady Gaga, 
                                                 Zedd, etc.). However, if you're 
                                                 at a rager, chances are you 
                                                 will hear a Kanye song. While
                                                 Kanye West isn't necessarily 
                                                 a dance act, he is certainly 
                                                 associated with electronic 
                                                 producers and sampled artists 
                                                 (ex. Daft Punk, Hudson Mohawke) 
                                                 Spotify scores danceability 
                                                 'based on a combination of 
                                                 musical elements including 
                                                 tempo, rhythm stability, beat 
                                                 strength, and overall 
                                                 regularity. A value of 0.0 is
                                                 least danceable and 1.0 is most 
                                                 danceable.' Below, we can see 
                                                 the danceability as the 
                                                 independent variable and 
                                                 Streams as the outcome."),
                                                 br(),
                                                 br(),
                                                 imageOutput("streamdance"),     
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 br(), 
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 br(), 
                                                 br(), 
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 )
                                    ),   
                                    
                                    # Loudness page
                                    
                                    tabPanel("The Loudness War?",
                                             mainPanel(
                                                 titlePanel("Listens from 
                                                             Loudness?"),
                                                 p("Another thing we can
                                                   observe with Spotify data
                                                   is the effect of the 
                                                   'loudness war'. For broad 
                                                   context, the 'loudness war' 
                                                   is shorthand for the trend of
                                                   greater overall loudness in 
                                                   songs in recent years. 
                                                   Spotify measures loudness 
                                                   in dB LUFS, or perceived
                                                   average loudness throughout
                                                   a song. The increasing 
                                                   volume of music is a 
                                                   controversial topic among 
                                                   listeners and sound engineers.
                                                   While volume can contribute
                                                   to the energy of the song, 
                                                   many argue that over-
                                                   compression (a common
                                                   technique to achieve greater
                                                   loudness) ruins song quality 
                                                   and songwriting by reducing
                                                   dynamic range. One producer
                                                   blamed for this trend is 
                                                   Rick Rubin, a legendary MC
                                                   and producer who frequently
                                                   collaborates with Kanye.
                                                   Below, we can see a graph 
                                                   of the 
                                                   
                                                   "),
                                                 imageOutput("myImage4"),
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 titlePanel("Are Songs Getting
                                                             Louder?"),
                                                 p("If we assume that the
                                                   loudness war is a real or
                                                   intentional phenomenon,
                                                   is Kanye participating in
                                                   the loudness war? This graph
                                                   shows the loudness of Kanye's
                                                   music over time.
                                                   "),
                                                 plotOutput("kanye_loud"), 
                                                 br(), 
                                                 p("From the graph, it seems
                                                   as though there might be a
                                                   slight upward trend!"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 titlePanel("Release Date as
                                                             Coefficients for
                                                             Loudness"),
                                                 p("The following table gives
                                                   some insight as to how the 
                                                   year of release might affect
                                                   the loudness of a released
                                                   song."),
                                                 tableOutput("table"), 
                                                 br(), 
                                                 p("The table seems to imply
                                                   a solidly positive increase
                                                   as time goes on. Negative
                                                   values seem to disappear."),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(), 
                                                 titlePanel("Predicting the
                                                             Future of Loud"),
                                                 p("While Kanye hasn't 
                                                    released much music this 
                                                    year, how loud could a Kanye
                                                    song be if it were released
                                                    this year? Below, I've 
                                                    generated some predictions."),
                                                 plotOutput("loud_pred"), 
                                                 br(),
                                                 br(),
                                                 br(), 
                                                 titlePanel("Using Wisdom"), 
                                                 br(), 
                                                 p("So how well does this data
                                                   predict or explain loudness
                                                   trends in Kanye's music?
                                                   At first glance, it seems as 
                                                   though there is a slight 
                                                   trend upwards in recent
                                                   years. But another thing to 
                                                   keep in mind is that Spotify
                                                   normalizes volume to -14dB 
                                                   LUFS by default, though this
                                                   can be disabled. This does
                                                   not mean all songs are
                                                   produced with this in mind, 
                                                   though. Songs are now either
                                                   turned up or down by Spotify,
                                                   to get a better read, we 
                                                   might want to look at a plat-
                                                   form that does not equalize
                                                   volume, like Soundcloud
                                                   before we make definitive
                                                   statements about Kanye's 
                                                   loudness"), 
                                                 
                                             )
                                    ), 
                                    
                                    # Key analysis page 
                                    
                                    tabPanel("Key analysis ", 
                                             mainPanel(
                                                 titlePanel("Kanye's Keys"),
                                                 headerPanel("Kanye's Favorite 
                                                             Keys (Overall)"),
                                                 br(),
                                                 p("Here, we can see the total
                                                   number of songs written
                                                   in each key in Kanye's 
                                                   discography (top 20)."),
                                                 br(),
                                                 plotOutput("kanye_keys_plot"), 
                                                 br(),
                                                 p("There seems to be a clear
                                                   preference for C#/Db Major"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 titlePanel("Kanye's Favorite 
                                                            Keys (Per Album)"),
                                                 br(),
                                                 p("Using the slider, we can
                                                   visualize the distribution 
                                                   of keys in each album. There
                                                   is a stark difference 
                                                   between albums."),
                                                 br(),
                                                 shinyWidgets::sliderTextInput(
                                                     inputId = "selected_album", 
                                                     label = "Pick an Album 
                                                     (Most Recent First):", 
                                                     choices = c(unique(
                                                         kanye$album_name)
                                                         )
                                                 ),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 plotOutput("kanye_keys_plot2"),
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 titlePanel("Modes of Kanye West 
                                                             songs over time"),
                                                 br(), 
                                                 p("There's a classic saying
                                                   that minor is sad and major
                                                   is happy. While I don't 
                                                   believe that to be true, 
                                                   these graphs show how the
                                                   mode of Kanye songs has 
                                                   changed over time. Overall,
                                                   there doesn't seem to be a
                                                   massive change in either 
                                                   direction."),
                                                 br(),
                                                 headerPanel("Minor"),
                                                 plotOutput("kanye_keys_plot3"), 
                                                 br(), 
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 headerPanel("Major"),
                                                 plotOutput("kanye_keys_plot4")
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
        
        Kanye_streams <- read_csv("Kanye streams.csv")
        kanye <- get_artist_audio_features('kanye west')
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
            geom_col(fill = case_when(input$selected_album == 
                                          "JESUS IS KING" ~ "blue",
                                      input$selected_album == 
                                          "KIDS SEE GHOSTS" ~ "brown2", 
                                      input$selected_album == 
                                          "ye" ~ "cadetblue3", 
                                      input$selected_album == 
                                          "The Life Of Pablo" ~ "darkorange", 
                                      input$selected_album == 
                                          "Yeezus" ~ "darkgray", 
                                      input$selected_album == 
                                          "Kanye West Presents Good Music 
                                      Cruel Summer" ~ "antiquewhite3", 
                                      input$selected_album == 
                                          "Watch The Throne" ~ "goldenrod", 
                                      input$selected_album == 
                                          "My Beautiful Dark Twisted Fantasy" ~ 
                                          "firebrick1", 
                                      input$selected_album == 
                                          "808s & Heartbreak" ~ 
                                          "mediumturquoise", 
                                      input$selected_album == 
                                          "Graduation" ~ "mediumpurple4", 
                                      input$selected_album == 
                                          "Graduation (Alternative 
                                      Business Partners)" ~ "mediumpurple4", 
                                      input$selected_album == 
                                          "Late Orchestration" ~ "lightsalmon4", 
                                      input$selected_album == 
                                          "Late Registration" ~ "orangered4", 
                                      TRUE ~ "darkgoldenrod1")) + 
            labs(x = "Key", 
                 y = "Total number of songs written in key", 
                 title = "Kanye's Favorite Keys",
                 subtitle = "What key will he use next?") + 
            theme_bw() 
        
    }) 
    
    # Rendering the plot for distribution of Kanye's Keys in minor
    
    output$kanye_keys_plot3 <- renderPlot ({ 
        kanye %>% 
            distinct(track_name, album_name, album_release_date, key_mode, 
                     mode_name) %>%  
            mutate(year = substr(album_release_date, 1, 4)) %>%  
            select(mode_name, album_name, year, album_release_date) %>%  
            group_by(mode_name, year, album_release_date) %>%  
            summarize(total = n(), .groups = "drop") %>%  
            arrange(year) %>%  
            filter(mode_name == "minor") %>%  
            group_by(year) %>%  
            summarize(total = sum(total), .groups = "drop")%>%  
            ggplot(aes(x = year, y = total)) + 
            geom_point() + 
            labs(title = "Minor Keys per Album Release Year", 
                 x = "Year", 
                 y = "Number of Songs in Minor")  + 
            geom_path(aes(group = 1)) + 
            theme_bw()
        
    }) 
    
    #  Rendering the plot for major keys. 
    
    output$kanye_keys_plot4 <- renderPlot ({ 
        kanye %>% 
            distinct(track_name, album_name, album_release_date, key_mode, 
                     mode_name) %>%  
            mutate(year = substr(album_release_date, 1, 4)) %>%  
            select(mode_name, album_name, year, album_release_date) %>%  
            group_by(mode_name, year, album_release_date) %>%  
            summarize(total = n(), .groups = "drop") %>%  
            arrange(year) %>%  
            filter(mode_name == "major") %>%  
            group_by(year) %>%  
            summarize(total = sum(total), .groups = "drop")%>%  
            ggplot(aes(x = year, y = total)) + 
            geom_point() + 
            labs(title = "Major Keys per Album Release Year", 
                 x = "Year", 
                 y = "Number of Songs in Major") + 
            geom_path(aes(group = 1)) + 
            theme_bw()
        
    }) 
    
    # Rendering photo of albums on pinterest. 
    
    output$myImage <- renderImage({
        list(src = "myImage2.png",
             width = 480,
             height = 480,
             alt = "This is alternate text")
    }, deleteFile = FALSE) 
    
    # Danceability vs. streams plot in a .gif format. 
    
    output$streamdance <- renderImage({
        list(src = "streamdance.gif",
             width = 480,
             height = 480,
             alt = "This is alternate text")
    }, deleteFile = FALSE)
    
    # Creating the loudness over time plot. 
    
    output$kanye_loud <- renderPlot ({ 
        kanye %>%  
            select(album_release_date, loudness, track_name, album_name) %>%  
            ggplot(aes(x = album_release_date, y = loudness, 
                       color = "lightsalmon1")) + 
            geom_jitter() +
            theme_bw() + 
            labs(title = "Loudness of Kanye West Songs over Time", 
                 subtitle = "Is the Loudness War Here?", 
                 x = "Song Release Date", 
                 y = "Loudness in dB") 
        
    }) 
    
    # Creating the gt table for the loudness coefficients. 
    
    output$table <-render_gt(
            expr = gt_tbl,
            height = px(600),
            width = px(600)
        ) 
    
    # Creating a predictive mdoel with posterior epred. 
    # The data was read in as a tibble. 
    # The result was piped into a ggplot and modeled.
    
    output$loud_pred <- renderPlot( 
        posterior_epred(model1, newdata = new_obs) %>%  
            as_tibble() %>%  
            mutate_all(as.numeric) %>%  
            ggplot(aes(x = `1`, color = "darkorange1")) + 
            geom_histogram(aes(y = after_stat(count/sum(count))), 
                           bins = 100) + 
            labs(title = "Posterior for loudness of songs in 2020", 
                 x = "Loudness (dB", 
                 y = "Percentage", 
                 subtitle = "Loudness as a function of time. 
                 How Loud Would a Kanye Song be Today?") + 
            theme_bw() +
            scale_y_continuous(labels = scales::percent_format()) 
        
        
    )
    
    # Creating a loudness over time plot
    
    output$myImage4 <- renderImage({
        list(src = "outfile.gif",
             width = 480,
             height = 480,
             alt = "This is alternate text")
    }, deleteFile = FALSE) 
    
    # Creating a plotly object with hovering labels
    
    output$streamsfeatures <- renderPlotly(
        
        ggplotly(streamsf_plot, tooltip = "text")
    ) 
    
    # Creating wordcloud with word cloud 2 
    
    output$wordcloud <- renderWordcloud2({
        
        # Tibble of stream count per album for the word cloud
        
        wc_tibble<-full_set %>%  
            subset(!is.na(Streams)) %>%    
            select(track_name, Streams) 
        
        # Creating the word cloud and setting the colors
                                                
        wordcloud2(data = wc_tibble, 
                   fontFamily = "sans-serif", 
                   color = rep_len(c("plum","orange", "lightblue", "orange"), 
                                   nrow(wc_tibble)))
    
})

}

# Run the application 
shinyApp(ui = ui, server = server)
