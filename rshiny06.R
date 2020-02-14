ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      # Add radio buttons input
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          # First choice is "book", with "Art of War" displaying
          "Art of War" = "book",
          # Second choice is "own", with "Use your own words" displaying
          "Use your own words" = "own",
          # Third choice is "file", with "Upload a file" displaying
          "Upload a file" = "file"
        )
      ),
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
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(input_file(), num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)



