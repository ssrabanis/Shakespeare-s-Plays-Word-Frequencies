# Load packages -----------------------------------------------------
library(shiny)
library(shinydashboard)
library(wordcloud)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(plotly)
library(dplyr)
library(quanteda)
library(rsconnect)


# Load data ---------------------------------------------------------

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


# task4: add in getFreq function for pre-processing

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

## UI ##########################################################################
ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("Shakespeare's Plays Word Frequencies"), 
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "book", label = "Choose a Book",
                  choices = books,
                  selected = "A Mid Summer Night's Dream"),
      
      ## add checkbox to stop words -------------------
      
      checkboxInput(inputId = "stop_words", label = "Stop words",
                    value = TRUE),
      
      ## add action button to run the app -------------
      actionButton(inputId = "Run_button", 
                   label = "Run"),
      
      ## seperate sections ---------
      br(),hr(),
      
      # Subtitle
      h3("Word Cloud Settings"),
 
      # task2: add in the inputs in the sidebarPanel
      
      ## set slider for number of words ----------------
      sliderInput(inputId = "word_level", label = "Max # of Words:",
                  min = 10, max = 200, value = 100, step = 10),
      
      ## set slider for Size of largest words ----------------
      sliderInput(inputId = "lword_level", label = "Size of largest words:",
                  min = 1, max = 8, value = 4),
      
      ## set slider for Size of smallest words ----------------
      sliderInput(inputId = "sword_level", label = "Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      
      ## seperate sections ---------
      br(),hr(),
      
      # Subtitle
      h3("Word Count Settings"),
      
      ## set slider for Minimum words for counts chart ----------------
      sliderInput(inputId = "word_level_ch", label = "Minimum words for counts chart:",
                  min = 10, max = 100, value = 25),
      
      ## set slider for Word size for Counts Chart ----------------
      sliderInput(inputId = "size_ch", label = "Word size for Counts Chart:",
                  min = 8, max = 30, value = 14)
      
    ),
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel((title = "Word Cloud"),
                           plotOutput(outputId = "cloud",height = "600px", width="100%")),
                  tabPanel((title = "Word Counts"),
                           plotOutput(outputId = "freq",height = "600px", width="100%"))

                           
                  )
      
 
      )

    )
  )
  

server <- function(input, output) {
  
  # reactive expression
  freq <- eventReactive( input$Run_button, {
    
    withProgress({
      setProgress(message = "Processing corpus input$book, input$stop_words")
      getFreq(input$book,input$stop_words) 
    })
    
  })
  
  ## create plotOutput-----------------------------------------
    output$cloud <- renderPlot({
      
      v <- freq()
      pal <- brewer.pal(8,"Dark2")
      
      v %>% 
        with(
          wordcloud(
            word, 
            n, 
            scale = c(input$lword_level,input$sword_level),
            random.order = FALSE, 
            max.words = input$word_level, 
            colors=pal))
      
  })
    ## create horizontal bar chart  ----------------------------------
    
  output$freq <- renderPlot({
  v <- freq()
  v %>% filter(n > input$word_level_ch) %>% ggplot(aes(n,reorder(word,n))) + geom_bar(stat = "identity") + 
                                            theme(text = element_text(size = input$size_ch),
                                            axis.title = element_blank(), axis.text.x = element_text(),
  )                                                                                                       
         
})

}

shinyApp(ui = ui, server = server)
