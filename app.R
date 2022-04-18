library(shiny)
#install.packages(c("wordcloud", "shinythemes"))
library(wordcloud)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
# install.packages("tidytext")
library(tidytext)
#install.packages('rsconnect')
library(rsconnect)

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

# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

ui <- fluidPage(
  theme = shinytheme("yeti"), # change shinytheme
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "book_select", label = "Select Book", choices = books),
      checkboxInput(inputId = "stopword", label = "Remove Stopwords", value = TRUE),
      actionButton(inputId = "run_app", label = "Update"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId = "maxwords", label = "Max Number of Words", 
                  min = 10, max = 200, value = 100, step = 10),
      sliderInput(inputId = "largest_word", label = "Largest Word Size",
                  min = 1, max = 8, value = 4),
      sliderInput(inputId = "smallest_word", label = "Smallest Word Size",
                  min = .1, max = 4, value = .5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId = "min_word_counts", label = "Minimum Word Counts",
                  min = 10, max = 100, value = 25),
      sliderInput(inputId = "font_size", label = "Font Size",
                  min = 8, max = 30, value = 14)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloud", height = "600px")),
        tabPanel("Frequency", plotOutput("freq", height = "600px"))
      )
    )
  ),
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$run_app, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book_select, input$stopword) # ... = replace with the two inputs from Task 2
    })
  })
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$smallest_word, input$largest_word),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    ggplot(
      filter(v, n > input$min_word_counts),
      aes(
        x = reorder(word, n),
        y = n
      )
    ) +
      geom_col() +
      coord_flip() +
      theme(
        text = element_text(size = input$font_size),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)
