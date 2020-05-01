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
                                # numericInput("Information_pol_insee_code", "In which deprartment do you life:", value=83, min=0, max=96)
                                wellPanel(radioButtons("Information_drv", "Information about Drivers / Policy-Holders", choices = list("Available", "Not Available"), selected = "Not Available", inline = TRUE),
                                          conditionalPanel(
                                              condition = "input.Information_drv == 'Available'",
                                              flowLayout(
                                                  sliderInput("Information_drv_age1", "Please insert the Driver's age:", min=18, max=105, value=45),
                                                  radioButtons("Information_drv_sex1", "Please insert the Driver's sex:", c("M", "W", "Other"),selected="M"),
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
                            ),actionButton("safe", "Safe values!")
                   ),
                   # Third Sub-Tab, Calculation---------
                   tabPanel("Calculation",
                            br(),
                            "Dear", "",
                            br(),
                            "The insurance premium for your", "", "will amount up to:",
                            br(),
                            verbatimTextOutput("Prediction")
                   )
                   
               )
    )
))

server<- function(input,output){
    rf_tuned <- readRDS("../../Data/rf_tuned_shiny.rds")
    lm_tuned <- readRDS("../../Data/lm_tuned_only_pos.rds")
    premium <- reactiveVal(0)
    observeEvent(input$safe, {
        
        # fetching the input
        pol_coverage <- input$Information_pol_coverage
        pol_usage <- input$Information_pol_usage
        # pol_insee_code <- input$Information_pol_insee_code
        drv_sex1 <- input$Information_drv_sex1
        drv_age_lic1 <- as.integer(input$Information_drv_age_lic1)
        drv_age1 <- as.integer(input$Information_drv_age1)
        vh_din <- as.integer(input$Information_vh_din)
        vh_speed <- as.integer(input$Information_vh_speed)
        vh_value <- as.integer(input$Information_vh_value)
        vh_weight <- as.integer(input$Information_vh_weight)
        
        
        # binding to data frame
        info <- data.frame(pol_coverage, pol_usage, drv_age1, drv_sex1, drv_age_lic1, vh_din, vh_speed, vh_value, vh_weight)
        # pol_insee_code,
        
        # preparing for prediction
        info$pol_coverage <- as.factor(info$pol_coverage)
        info$pol_usage <- as.factor(info$pol_usage)
        info$drv_sex1 <- as.factor(info$drv_sex1)
        # info$pol_insee_code <- as.factor(info$pol_insee_code)
        
        
        
        pos_or_neg <- predict(rf_tuned, newdata = info)
        info$pred <- pos_or_neg
        pred <- predict(lm_tuned, newdata = info)
        
        
        premium(pred)
    })    
    
    output$Prediction <- renderPrint({
        premium()
    })
}



shinyApp(ui=ui, server=server)


