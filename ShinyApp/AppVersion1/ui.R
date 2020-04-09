library(shiny)



# Define UI for application
shinyUI(fluidPage(
    navbarPage("BSG Insurance",
               # First Main-Tab
               tabPanel("Explore Training Data",
                        tabsetPanel(
                            # Fist Sub-Tab
                            tabPanel("Plots",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                             )
                                         )
                                     )),
                            
                            # Second Sub-Tab
                            tabPanel("Data Table")
                        )),
               
               # Second Main-Tab
               tabPanel("Policy Portfolio Management"),
               
               # Third Main-Tab
               tabPanel("Premium Calculator"),
               
               # Fourth Main-Tab
               tabPanel("Additional Explanations"))
   
))
