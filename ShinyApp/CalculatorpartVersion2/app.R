library(shiny)


ui<-shinyUI(fluidPage(
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
                                wellPanel(radioButtons("Information_pol", "Information about Policy-Types", choices = list("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_pol == 'Available'",
                                              flowLayout(
                                                  selectInput("Information_pol_coverage", "Insert policy coverage:", c("Mini", "Median1", "Median2", "Maxi"), selected=c("Mini")),
                                                  selectInput("Information_pol_usage", "Insert policy usage", c("WorkPrivate", "Professional", "Retired", "AllTrips"), selected=c("WorkPrivate"))
                                              )
                                          )
                                ),
                                wellPanel(radioButtons("Information_drv", "Information about Drivers / Policy-Holders", choices = list("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_drv == 'Available'",
                                              flowLayout(
                                                  sliderInput("Information_drv_age1", "Please insert the Driver's age:", min=18, max=105, value=45),
                                                  checkboxGroupInput("Information_drv_sex1", "Please insert the Driver's sex:", c("M", "W", "Other")),
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
                                wellPanel(radioButtons("Information_vh", "Information about Vehicle", choices = list("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_vh == 'Available'",
                                              flowLayout(
                                                  numericInput("Information_vh_speed", "Please insert the Vehicle's top speed in kmh:", value=240, min=0, max=500),
                                                  numericInput("Information_vh_value", "Please insert the Vehicle's value in CHF:", value=20000, min=0, max=5000000),
                                                  numericInput("Information_vh_weight", "Please insert the Vehicle's weight in kg:", value=1000, min=0, max=5000),
                                                  numericInput("Information_vh_din", "Please insert the Vehicle's motor power (in PS?):", value=100, min=5, max=150)
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

server<- function(input,output,session){
    rf_tuned <- load("Data/rf_tuned_shiny.RData")
    observeEvent(input$safe, {
        # fetching the input
        pol_coverage <- as.factor(input$Information_pol_coverage)
        pol_usage <- as.factor(input$Information_pol_usage)
        drv_sex1 <- as.factor(input$Information_drv_sex1)
        drv_age_lic1 <- as.integer(input$Information_drv_age_lic1)
        drv_age1 <- as.integer(input$Information_drv_age1)
        vh_din <- as.integer(input$Information_vh_din)
        vh_speed <- as.integer(input$Information_vh_speed)
        vh_value <- as.integer(input$Information_vh_value)
        vh_weight <- as.integer(input$Information_vh_weight)
        
        info <- as.data.frame(cbind(pol_coverage, pol_usage, drv_sex1, drv_age_lic1, drv_age1, vh_din, vh_speed, vh_value, vh_weight))
        print(info)
        # info$pred <- as.factor("")
        # saving with the action button
        # observeEvent({input$safe}, { # && input$ready?
        pred <- predict(rf_tuned, newdata = as.data.frame.array(info))
        # output$Pred <- renderPrint(pred())
    })
}



shinyApp(ui=ui, server=server)


