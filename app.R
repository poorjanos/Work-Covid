library(shiny)
library(dplyr)
library(lubridate)


covid_agg_imp <- read.csv(here::here("Data", "covid_agg_imp"),
                            stringsAsFactors = FALSE)

# User interface ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Kethavi hatralekos idosor dekompozicio"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ProductInput", "Termek",
                  sort(unique(covid_agg_imp$TERMEKCSOPORT)),
                  selected = "LAKAS")
    ),
    mainPanel(
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    )
  )
)


# Server ------------------------------------------------------------------------
server <- function(input, output) {
  
  filtered <- reactive({
    if (is.null(input$ProductInput)) {
      return(NULL)
    }   
    
    covid_agg_imp %>%
      filter(TERMEKCSOPORT == input$ProductInput)
  })
  

  output$coolplot <- renderPlot({
    df <- filtered()
    vals <- df$HATRALAKOS_ARANY
    ts_obj <- ts(vals, frequency = 12, start =  c(2018, 1))
    ts_stl <-  stl(ts_obj, s.window = 'periodic')
    plot(ts_stl)
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui, server)