
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  # Define a reactive variable named `input_file`
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Use the reactive variable as the word cloud data source
    create_wordcloud(data = input_file(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)