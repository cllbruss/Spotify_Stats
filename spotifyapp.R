library(shiny)
library(fpp3)
library(ggplot2)

spotify <- read.csv("Spotify 2010 - 2019 Top 100.csv")
names(spotify)[1] <- "title"
names(spotify)[3] <- "genre"
spotify$added <- NULL
spotify <- spotify[spotify$pop > 50,]
spotify$pop <- as.numeric(spotify$pop)
spotify$genre <- factor(spotify$genre)
spotify$dnce = NULL
spotify$dB = NULL
spotify$id = NULL
spotify$val = NULL
spotify$acous = NULL
spotify$spch = NULL
spotify$live = NULL
spotify$dur = NULL



ui <- fluidPage(
  titlePanel("Spotify Top Charts"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_genre",
        label = "Select Genre",
        choices = spotify$genre
      ),
      selectInput(
        inputId = "selected_year",
        label = "select year",
        choices = spotify$top.year,
        multiple = TRUE
      ),
      sliderInput(
        inputId = "selected_bpm", 
        label = "Choose bpm range",
        min = min(spotify$bpm),
        max = max(spotify$bpm), 
        value = c(65, 206)
       ),
      submitButton(
        text = "Submit Filters",
        icon = icon("music")
      )
    ),
   mainPanel(
     h4("App Instructions: This app looks at Spotify top charts across the years and compares songs popularity to their bpms."),
     h6("*If plot and table are blank, the selected genre might not have songs for selected year(s), change the parameters!*"),
     plotOutput("plot"),
     tableOutput("table")
   )
  )
)
server <- function(input, output, session) {

  output$table <- renderTable({
      
      spotify_filter <- spotify[spotify$genre == input$selected_genre & spotify$top.year %in% input$selected_year & 
                                  spotify$bpm >= input$selected_bpm[1] & spotify$bpm <= input$selected_bpm[2],]

      spotify_filter
      
  })
  
  output$plot <- renderPlot({
    
    spotify_filter <- spotify[spotify$genre == input$selected_genre & spotify$top.year %in% input$selected_year & 
                                 spotify$bpm >= input$selected_bpm[1] & spotify$bpm <= input$selected_bpm[2],]
    
    ggplot(spotify_filter, aes(x=bpm, y=pop)) + 
      geom_point() + geom_smooth(method = lm, se = FALSE) + labs(title = "Scatter plot of bpm and song popularity for selected genre and year(s)")
    
  })
  
}

shinyApp(ui, server)

