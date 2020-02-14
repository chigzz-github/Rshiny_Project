library(colourpicker)

ui <- fluidPage(
  h1("Word Cloud"),
  # Add a numeric input for the number of words
  numericInput(inputId = "num", label = "Maximum number of words",
               value = 100, min = 5),
  # Add a color input for the background color
  colourInput("col","Background color","white"),
  wordcloud2Output("cloud")
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    # Use the values from the two inputs as
    # parameters to the word cloud
    create_wordcloud(artofwar,
                     num_words = input$num, background = input$col)
  })
}

shinyApp(ui = ui, server = server)