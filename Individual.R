library("shiny")
library("plyr")
library("ggplot2")
library("scales")
library('dplyr')
library('lattice')
library('tidyr')

Data <-read.csv("House_Price_data.csv") # load the dataframe
#Initializing UI for The Application
ui <- fluidPage(
  # Title of The Application
  titlePanel("Housing Price Data"),
  
  #Input for the Plots
  mainPanel(
    width = 12,
      
      #Checkbox Inputs
      checkboxGroupInput(
        inputId = "checkboxes",
        label = "The General Zoning Classification:",
        choices = unique(Data$MSZoning),
        inline = TRUE,
        selected = "RH"
      ),
      # List Inputs
      selectInput(
        size = 1,
        selectize = FALSE,
        inputId = "List",
        label =  "Select a Year Built:",
        choices = unique(Data$YearBuilt),
        selected = c("1928", "1949","1992")
      ),
    #Creating the Main Panel
      mainPanel(
        tabsetPanel( 
          tabPanel("YearBuilt", plotOutput("Plot1")),
          tabPanel("YrSold", plotOutput("Plot2")),
          tabPanel("GrLivArea",plotOutput("Plot3")),
          tabPanel("SaleCondition",plotOutput("Plot4")),
          tabPanel("LotArea",plotOutput("Plot5")),
          tabPanel("Condition1",plotOutput("Plot6"))
        ),
      #Checkbox Inputs 2
      selectInput(
        size = 1,
        selectize = FALSE,
        inputId = "checkboxes1",
        label =  "Select Year Sold:",
        choices = unique(Data$YrSold),
        selected = c()
    ),
      )
),
)

# Initializing the server 
server <- function(input, output) {
  output$Plot1 <- renderPlot({
   #Barplot 
    Data<-Data[Data[,'YearBuilt']== input$List, ]
    Bar_plot<-ggplot(data=Data,aes(x=HouseStyle,y=SalePrice,fill=HouseStyle))+
    scale_y_continuous(labels = scales::comma)+
    ggtitle("SalePrice depending on the Style of Dwelling")+
    xlab("Style of dwelling")+
    ylab("Sale Price ")+
    geom_bar(stat="identity")+theme_minimal()
    Bar_plot
  })
  output$Plot2 <- renderPlot({
    
    #Scatter Plot 
    rownames(Data)
    Data<-Data[Data[,'YrSold']==input$checkboxes1, ]
    Scatter_plot<-ggplot(data=Data,aes(x=SalePrice,y=YearBuilt,label))+
    scale_x_continuous(labels = scales::comma)+
    ggtitle("SalePrice depending upon Year Build and Year Sold")+
    xlab("Property's sale price in dollars ")+
    ylab("Original construction year")+
    geom_point(color="Coral")
    Scatter_plot
    })
    output$Plot3 <- renderPlot({
    
    #LineGraph 
    Data<-Data[Data[,'MSZoning']== input$checkboxes, ]
    Line_graph <- ggplot(data=Data,aes(x=GrLivArea,y=YearBuilt,group=1))+
      ggtitle("Above grade living area in square feet vs Year of Construction")+
      xlab("Above grade (ground) living area square feet")+
      ylab("Original construction year")+
      geom_line(color="Deeppink")+
      geom_point(color="Blue");
    Line_graph
    
  })
  output$Plot4 <- renderPlot({
    
    #BoxPlot
    #Data<-Data[Data[,'SaleCondition']== input$checkboxes1, ]
    box_plot <- ggplot(data=Data,aes(x=SaleCondition,y=YrSold,fill=SaleCondition)) +
      ggtitle("Year Sold Depending on the Condition of Sale")+
      xlab("Condition of sale")+
      ylab("Year Sold")+
      geom_boxplot( outlier.shape=16,outlier.size=2, notch=FALSE)+theme_minimal()
    box_plot 
 
     })
  output$Plot5 <- renderPlot({
    
    #Data<-Data[Data[,'LotArea']==input$checkboxes1, ]
    Histogram<-ggplot(data=Data,aes(x=Condition1,y=LotArea,fill=LotArea)) +
      scale_y_continuous(labels = scales::comma)+
      ggtitle("Proximity to main road or railroad vs Lot Area")+
      xlab("Proximity to main road or railroad")+
      ylab("Lot size")+
      geom_histogram(stat="identity")+theme_minimal()
    Histogram
  })
  output$Plot6 <- renderPlot({
    
    #Data<-Data[Data[,'Condition1']==input$checkboxes1, ]
    Histogram<-ggplot(data=Data,aes(x=Condition1,y=SalePrice,fill=Condition1)) +
      scale_y_continuous(labels = scales::comma)+
      ggtitle("Proximity to main road or railroad vs Sale Price")+
      xlab("Proximity to main road or railroad")+
      ylab("Sale Price")+
      geom_histogram(stat="identity")+theme_minimal()
    Histogram
  })
}

# Running the Rshiny Application
shinyApp(ui = ui, server = server)

