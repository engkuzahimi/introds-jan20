library(arules)
library(gsubfn)
library(dplyr)
library(shiny)
library(psych)

ui <- fluidPage(
  titlePanel('Game Sales'),
  splitLayout(
    sidebarPanel(
      selectInput(
        inputId = 'dataset',
        label = 'Choose the data to be viewed:',
        choices = c('Name', 'Platform', 'Year', 'Genre', 'Publisher', 'SalesNorthAmerica', 'SalesEurope', 'SalesJapan', 'SalesOther', 'SalesGlobal', 'Developer')
      ),
      sliderInput(
        inputId = 'obs',
        label = 'Number of observations: ',
        min = 0,
        max = 16720,
        value = 100
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  #tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("view"))
      )
    )
  )
)

server <- function(input, output) {
  # Reading the dataset into data and then omit rows with empty cells
  data <- na.omit(read.csv('s.csv', stringsAsFactors = FALSE, na.strings=c("N/A")))
  
  #
  # # Removing rows with any empty cells inside it
  data <- data[!(is.na(data$Name) | data$Name==""), ]
  data <- data[!(is.na(data$Platform) | data$Platform==""), ]
  data <- data[!(is.na(data$Year) | data$Year==""), ]
  data <- data[!(is.na(data$Genre) | data$Genre==""), ]
  data <- data[!(is.na(data$Publisher) | data$Publisher==""), ]
  data <- data[!(is.na(data$NA_Sales) | data$NA_Sales==""), ]
  data <- data[!(is.na(data$EU_Sales) | data$EU_Sales=="") , ]
  data <- data[!(is.na(data$JP_Sales) | data$JP_Sales==""), ]
  data <- data[!(is.na(data$Other_Sales) | data$Other_Sales==""), ]
  data <- data[!(is.na(data$Global_Sales) | data$Global_Sales==""), ]
  data <- data[!(is.na(data$Developer) | data$Developer==""), ]

  write.csv(data,"C:\\Users\\Jim\\Desktop\\CleanedData.csv", row.names = TRUE)
  
  # Getting the input from user on which column of table to display
   dataInput <- reactive({
     switch(input$dataset,
            'Name' = data$Name,
            'Platform' = data$Platform,
            'Year' = data$Year,
            'Genre' = data$Genre,
            'Publisher' = data$Publisher,
            'SalesNorthAmerica' = data$NA_Sales,
            'SalesEurope' = data$EU_Sales,
            'SalesJapan' = data$JP_Sales,
            'SalesOther' = data$Other_Sales,
            'SalesGlobal' = data$Global_Sales,
            'Developer' = data$Developer
     )
   })
 
  output$summary <- renderPrint({
    dataColumn <- dataInput()
    summary(dataColumn)
  })

  output$view <- renderTable({
    dataColumn <- dataInput()
    head(dataColumn, n = input$obs)
  })
  pairs(data[c("Year","NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")])
  pairs.panels(data[c("Year","NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")])

}

shinyApp(ui = ui, server = server)


# # Histogram for GameProducedbyYear
hist(data$Year,
     main="Game Produced by Year",
     xlab="Year",
     border="red",
     col="black",
     xlim=c(1985,2020),
     breaks=55)

top10 <- head(data, 10)
barplot(table(head(data$Genre,10)),cex.names=0.5, main="Genres based On Top 10 Games Sold",
        names.arg=c("Misc","Platform", "Racing", "Sports"))

table(data$Platform)
barplot(table(data$Platform),cex.names=0.5, main="Total Number of Games build by Platform", horiz=TRUE,
 mes.arg=c("3DS", "DC", "GBA", "DS","GBA","GC","PC","PS","PS2","PS3","PS4","PS Vita","Wii","Wii U","X360","XBox","Xbox1"))

summary(data$Year)
summary(data$NA_Sales)
summary(data$EU_Sales)
summary(data$JP_Sales)
summary(data$Global_Sales)

