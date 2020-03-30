library(shiny)

fluidPage(
  titlePanel("Covid-19 Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Country",
                  choices = c("Germany", "Spain")),
      selectInput("modelInput", "Model",
                  choices = c("SEIR", "SIR")),
      sliderInput("severeCasesInput", "Severe cases (%)", 0, 50, 20),
      sliderInput("intensiveCareInput", "Intensive care (%)", 0, 20, 6),
      sliderInput("fatalityRateInput", "Fatality rate (%)", 0, 3, 1, 0.5)
      , width = 2
    ),
    mainPanel(
      headerPanel("Cumulated cases"),
      plotOutput("Covid_plot"),
      br(),
      br(),
      br(),
      br(),
      headerPanel("Model"),
      tableOutput("Cases_table"),
      plotOutput("Covid_predictions"),
      width = 10
    )
  )
)