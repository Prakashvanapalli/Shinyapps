library(shiny)

shinyUI(fluidPage(
  titlePanel("Lending Club Loan Data"),
  sidebarLayout(
    sidebarPanel(h2("Inputs"),
                 br(),
                 textInput("file", "File:"),
                 actionButton("browse", "Browse"),
                 actionButton("upload", "Upload Data"),
                 radioButtons("sub_group","Sub_Group",c("A3","B3","C3","D3")),
                 selectInput("var", 
                             label = "Choose the algorithm",
                             choices = c("Logistic Regression","Decision Trees","GBM","Random Forest"))
                 ),
    mainPanel(h1("Model Output"),
              h3(textOutput("nrows")),
              tableOutput('summary'))
  )
))

