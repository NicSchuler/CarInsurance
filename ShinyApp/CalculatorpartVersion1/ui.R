
library(shiny)


shinyUI(fluidPage(
    navbarPage("BSGN Insurance",
               # Third Main-Tab-------------
               tabPanel("Premium Calculator", "Calculate your personalized premium!!!"), br(),
               tabsetPanel(
                   # First Sub-Tab, Insert Personal Information---------
                   tabPanel("Insert Personal Information",
                            br(),
                            "Please Insert Personal Information:",
                            br(),
                            verticalLayout(
                                br(),
                                wellPanel(textInput("Name_drv", "Insert the Name:", "Max Muster")),
                                wellPanel(textInput("Brand_drv", "Insert the Automobile Brand:", "BMW")),
                                wellPanel(radioButtons("Information_pol", "Information about Policy-Types", choices = c("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_pol == 'Available'",
                                              flowLayout(
                                                  selectInput("Information_pol_coverage", "Insert policy coverage:", c("Mini", "Median1", "Median2", "Maxi"), selected=c("Mini")),
                                                  selectInput("Information_pol_usage", "Insert policy usage", c("WorkPrivate", "Professional", "Retired", "AllTrips"), selected=c("WorkPrivate"))
                                              )
                                          )
                                ),
                                wellPanel(radioButtons("Information_drv", "Information about Drivers / Policy-Holders", choices = c("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_drv == 'Available'",
                                              flowLayout(
                                                  sliderInput("Information_drv_age1", "Please insert the Driver's age:", min=18, max=105, value=45),
                                                  checkboxGroupInput("Information_drv_sex1M", "Please insert the Driver's sex:", c("M", "W", "Other")),
                                                  numericInput("Information_drv_age_lic1", "Please insert the age of the Driver's licence:", value=20, min=0, max=87)
                                              )
                                          )
                                )
                            )
                   ),
                   # Second Sub-Tab, Insert Vehicle Information---------
                   tabPanel("Please Insert Vehicle Information",
                            br(),
                            "Please Insert Vehicle Information:",
                            br(),
                            verticalLayout(
                                br(),
                                wellPanel(radioButtons("Information_vh", "Information about Vehicle", choices = c("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_vh == 'Available'",
                                              flowLayout(
                                                  numericInput("Information_vh_speed", "Please insert the Vehicle's top speed in kmh:", value=240, min=0, max=500),
                                                  numericInput("Information_vh_value", "Please insert the Vehicle's value in CHF:", value=20000, min=0, max=5000000),
                                                  numericInput("Information_vh_weight", "Please insert the Vehicle's weight in kg:", value=1000, min=0, max=5000)
                                              )
                                          )
                                )
                            ), actionButton("safe", "Safe values!")   
                   ),
                   # Third Sub-Tab, Calculation---------
                   tabPanel("Calculation",
                            br(),
                            "Dear", "", 
                            br(),
                            "The insurance premium for your", "", "will amount up to:",
                            br(),
                            dataTableOutput('displayDf')
                   )
                   
               )
    )
))