library(shiny)

shinyUI(fluidPage(
  titlePanel("Lending Club Loan Data"),
  sidebarLayout(
    sidebarPanel(h2("Inputs"),
                 br(),
                 textInput("file", "File:"),
                 actionButton("browse", "Browse"),
                 actionButton("upload", "Upload Data"),
                 selectInput("var", 
                             label = "Choose the algorithm",
                             choices = c("Logistic Regression","Decision Trees","GBM","Random Forest","XGBoost"))
                 ),
    mainPanel(h1("Model Output"),
              h3(textOutput("nrows")),
              tableOutput('summary'))
  )
))

