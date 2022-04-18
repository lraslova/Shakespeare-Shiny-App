library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(dplyr)


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

# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
                
                sidebarPanel(
                  selectInput(inputId = "books", label = "Choose a book:",
                              choices = books,
                              selected = "summer"),
                  
                  checkboxInput(inputId = "stopwords", label = "Stop Words",
                                value = TRUE),
                  
                  actionButton(inputId = "rerun",
                               label = "Rerun"),
                  
                  hr(h3("Word Cloud Settings")),
                  
                  sliderInput(inputId = "maxword", label = "Max # of Words:",
                              min = 10, max = 200, value = 100, step = 10),
                  
                  sliderInput(inputId = "largword", label = "Size of largest Words:",
                              min = 1, max = 8, value = 4),
                  
                  sliderInput(inputId = "smallword", label = "Size of smallest Words:",
                              min = 0.1, max = 4, value = 0.5),
                  
                  hr(h3("Word Count Settings")),
                  
                  sliderInput(inputId = "mincount", label = "Minimum Words for Counts Chart:",
                              min = 10, max = 100, value = 25),
                  
                  sliderInput(inputId = "sizecount", label = "Words Size for Counts Chart:",
                              min = 8, max = 30, value = 14),
                  
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Word Cloud", plotOutput("cloud", height = "600px")), 
                    tabPanel("Word Counts", plotOutput(outputId = "freq", height = "600px"))
                  )
                )
    )
  )

  
  # task6: and modify your figure heights


server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(
    input$rerun,{
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$books, input$stopwords) # ... = replace with the two inputs from Task 2
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
        scale = c(input$largword, input$smallword),
        random.order = FALSE, 
        max.words = input$maxword, 
        colors=pal))
  
})

output$freq <- renderPlot({
  v <- freq()
  pal <- brewer.pal(8,"Dark2")
  
  v %>% 
    filter(input$mincount < n) %>%
    ggplot(aes(x=reorder(word, n), y=n)) + 
    geom_col() +
    coord_flip() + 
    theme(text=element_text(size=input$sizecount), axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
})  

}

shinyApp(ui = ui, server = server)
