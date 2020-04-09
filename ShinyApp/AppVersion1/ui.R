library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    navbarPage("BSG Insurance",
               tabPanel("Explore Training Data",
                        tabsetPanel(
                            tabPanel("Plots"),
                            tabPanel("Data Table")
                        )),
               tabPanel("Policy Portfolio Management"),
               tabPanel("Premium Calculator"),
               tabPanel("Additional Explanations"))
   
))
