# Packages---------
library(shiny)
library(tidyverse)

# Drop-Down Lists-----------
Trainnumcols = c("pol_bonus", "pol_duration", "drv_age1", "drv_age2", "drv_age_lic1", "drv_age_lic2", "vh_age", "vh_cyl", "vh_din", "vh_speed", "vh_value", "vh_weight", "Sum_claim_amount")

Trainfactorcols = c("NULL", "pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2", "drv_sex1", "drv_sex2", "vh_fuel", "vh_make", "vh_type", "CountDistinct_id_claim")

Testnumcols = c("drv_age_lic1", "vh_din", "vh_speed", "vh_value", "vh_weight")

Testfactorcols = c("pol_coverage", "pol_usage", "drv_drv2", "drv_sex1")

MapVars = c("insured_Cars", "Avg_pol_bonus", "Avg_claim_count", "Avg_claim_amount")

# Define UI for application-----------

shinyUI(fluidPage(
    navbarPage("BSGN Insurance",
               
               # First Main-Tab-------------
               tabPanel("Premium Calculator", "Calculate your personalized premium!!!", br(),
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
                                          radioButtons("Information_drv_sex1", "Please insert the Driver's sex:", c("M", "F"),selected="M"),
                                          numericInput("Information_drv_age_lic1", "Please insert the age of the Driver's licence:", value=20, min=0, max=87),
                                          radioButtons("Information_drv_drv2", "Are there two drivers?", c("No", "Yes"),selected="No")
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
                          ),actionButton("safe", "Calculate Premium")
                 ),
                 # Third Sub-Tab, Calculation---------
                 tabPanel("Calculation",
                          br(),
                          "Dear", "",
                          br(),
                          "The insurance premium for your", "", "will amount up to:",
                          br(),
                          textOutput("Prediction"),
                          actionButton("SendRequest", "Send Request")
                 )
                 
               )),
               
               # Second Main-Tab Contract Control------------
               tabPanel("Contract Control",
                        br(),
                        fluidPage(
                          # First Row, Intro and Number of unchecked requests
                          fluidRow(
                            column(6,
                                   p(strong("This page allows the person in charge to check the requests one by one.")),
                                   hr()),
                            column(2,
                                   actionButton("CheckNextContract", "Next Contract"),
                                   hr()),
                            column(3, style = "background-color: green;color:white",
                                   p("Number of unchecked requests"),
                                   hr()),
                            column(1, style = "background-color: green;color:white",
                                   textOutput("CheckNumber"),
                                   hr())
                          ),
                          # Second Row, Name Car and RF-Prediction
                          fluidRow(
                            column(2,
                                   p("Name")),
                            column(2,
                                   textOutput("CheckName")),
                            column(2,
                                   p("Car manufacturer")),
                            column(2,
                                   textOutput("Checkvh_make")),
                            column(2,
                                   p("Claims expected")),
                            column(2,
                                   textOutput("Checkpred"))
                            
                          ),
                          # Third Row, Coverage, Usage and Car-Value
                          fluidRow(
                            column(2,
                                   p("Policy Coverage")),
                            column(2,
                                   textOutput("Checkpol_coverage")),
                            column(2,
                                   p("Policy Usage")),
                            column(2,
                                   textOutput("Checkpol_usage")),
                            column(2,
                                   p("Value of the Car [Euro]")),
                            column(2,
                                   textOutput("Checkvh_value"))
                          ),
                          # Fourth Row: Number of Drivers, Gender, age-lic
                          fluidRow(
                            column(2,
                                   p("Number of Drivers")),
                            column(2,
                                   textOutput("Checkdrv_drv2")),
                            column(2,
                                   p("Gender of the Driver")),
                            column(2,
                                   textOutput("Checkdrv_sex1")),
                            column(2,
                                   p("Years with the License")),
                            column(2,
                                   textOutput("Checkdrv_age_lic1"))
                          ),
                          # Fifth Row: Car din, speed, weight
                          fluidRow(
                            column(2,
                                   p("Power [bhp]")),
                            column(2,
                                   textOutput("Checkvh_din")),
                            column(2,
                                   p("Maximum Speed")),
                            column(2,
                                   textOutput("Checkvh_speed")),
                            column(2,
                                   p("Car weight")),
                            column(2,
                                   textOutput("Checkvh_weight"))
                          ),
                          # Sixth Row: Expected Claims, Premium, Buttons
                          fluidRow(
                            column(2,
                                   p("Exp. Claim amount [Euro]")),
                            column(2,
                                   textOutput("CheckfairPremium")),
                            column(2,
                                   p("Offered Premium [Euro]")),
                            column(2,
                                   textOutput("CheckPremium")),
                            column(2,
                                   actionButton("AcceptContract_Go", "Accept Contract")),
                            column(2,
                                   actionButton("DeclineContract_Go", "Decline Contract"))
                          )
                        )
               ),
               
               # Third Main-Tab Filters---------
               tabPanel("Filters",
                        br(),
                        p(strong("These filters are applied to all Plots and the data.table of the training data")),
                        br(),
                        
                        # Filter policies
                        radioButtons("Filter_pol", "Filter Policy-Types", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                        conditionalPanel(
                          condition = "input.Filter_pol == 'Yes'",
                          flowLayout(
                            sliderInput("Filter_pol_bonus", "Filter pol_bonus", min=0.5, max=3.5, value=c(0.5, 3.5)),
                            checkboxGroupInput("Filter_pol_coverage", "Filter pol_coverage", c("Mini", "Median1", "Median2", "Maxi"), selected=c("Mini", "Median1", "Median2", "Maxi"), inline=TRUE),
                            checkboxGroupInput("Filter_pol_usage", "Filter pol_usage", c("Retired", "WorkPrivate", "Professional", "AllTrips"), selected=c("Retired", "WorkPrivate", "Professional", "AllTrips"), inline=TRUE)
                          )
                        ),
                        br(),
                        
                        # Filter Drivers / Policy-Holders
                        radioButtons("Filter_drv", "Filter Drivers / Policy-Holders", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                        conditionalPanel(
                          condition = "input.Filter_drv == 'Yes'",
                          flowLayout(
                            checkboxGroupInput("Filter_drv_drv2","Filter drv_drv2", c("1 Driver", "2 Drivers"), selected=c("1 Driver", "2 Drivers"), inline=TRUE),
                            sliderInput("Filter_drv_age1", "Filter drv_age1", min=18, max=105, value=c(18,105)),
                            sliderInput("Filter_drv_age2", "Filter drv_age2", min=18, max=105, value=c(18,105)),
                            checkboxGroupInput("Filter_drv_sex1", "Filter drv_sex1", c("M","F"), selected=c("M","F"), inline=TRUE),
                            checkboxGroupInput("Filter_drv_sex2", "Filter drv_sex2", c("M","F"), selected=c("M","F"), inline=TRUE),
                            sliderInput("Filter_drv_age_lic1", "Filter drv_age_lic1", min=0, max=115, value=c(0,115)),
                            sliderInput("Filter_drv_age_lic2", "Filter drv_age_lic2", min=0, max=115, value=c(0,115))
                          )
                        ),
                        br(),
                        
                        # Filter insured cars
                        radioButtons("Filter_vh", "Filter insured cars", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                        conditionalPanel(
                          condition = "input.Filter_vh == 'Yes'",
                          flowLayout(
                            sliderInput("Filter_vh_age", "Filter vh_age", min=0, max=70, value=c(0,70)),
                            sliderInput("Filter_vh_cyl", "Filter vh_cyl", min=0, max=7000, value=c(0,7000)),
                            sliderInput("Filter_vh_din", "Filter vh_din", min=0, max=600, value=c(0,600)),
                            sliderInput("Filter_vh_speed", "Filter vh_speed", min=20, max=320, value=c(20,320)),
                            checkboxGroupInput("Filter_vh_type", "Filter vh_type", c("Tourism","Commercial"), selected=c("Tourism","Commercial"), inline=TRUE),
                            sliderInput("Filter_vh_value", "Filter vh_value", min=0, max=160000, value=c(0,160000)),
                            sliderInput("Filter_vh_weight", "Filter vh_weight", min=0, max=8000, value=c(0,8000))
                            
                          )),
                        br(),
                        
                        # Filter insured cars
                        radioButtons("Filter_claims", "Filter the Claims", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                        conditionalPanel(
                          condition = "input.Filter_claims == 'Yes'",
                          flowLayout(
                            checkboxGroupInput("Filter_CountDistinct_id_claim", "Filter CountDistinct_id_claim", c(0:9), selected=c(0:9), inline=TRUE),
                            sliderInput("Filter_Sum_claim_amount", "Filter Sum_claim_amount", min=-10000, max=150000, value=c(-10000,150000))
                          )),
                        
                        actionButton("Filter_Go", "Apply Filter")
               ),
               
               # Fourth Main-Tab Policy Protfolio Management-------------
               tabPanel("Policy Portfolio Management",
                        tabsetPanel(
                          
                          # First Sub-Tab, Densities---------
                          tabPanel("Densities",
                                   br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       p(strong("Choose the Variable you want to compare")),
                                       br(),
                                       selectInput("CompDensity_Var", "Variable to compare", choices = Testnumcols, selected = "vh_speed"),
                                       br(),
                                       actionButton("ComDensity_Go", "Create Plot")
                                     ),
                                     mainPanel(
                                       p(strong("Compare densites of training data with actual policies")),
                                       p("Both data sets are filtered according to the filter tab."),
                                       br(),
                                       plotOutput("ComDensPlot"),
                                       br(),
                                       downloadButton("downloadDensity", "Download Plot")
                                     )
                                   )),
                          
                          # Second Sub-Tab, Categories---------
                          tabPanel("Mass",
                                   br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       p(strong("Choose the Variable you want to compare")),
                                       br(),
                                       selectInput("CompMass_Var", "Variable to compare", choices = Testfactorcols, selected = "pol_coverage"),
                                       br(),
                                       actionButton("ComMass_Go", "Create Plot")
                                     ),
                                     mainPanel(
                                       p(strong("Compare densites of training data with actual policies")),
                                       p("Both data sets are filtered according to the filter tab."),
                                       br(),
                                       plotOutput("ComMassPlot"),
                                       br(),
                                       downloadButton("downloadMass", "Download Plot")
                                     )
                                   )),
                          
                          # Third Sub-Tab, Expected Profit calculator---------
                          tabPanel("Expected Profit",
                                   br(),
                                   p(strong("Estimate profit in general and based on filters")),
                                   br(),
                                   p("This page allows you to estimate the profits for the current year, so that you do not have bad surprises when you see the financial report."),
                                   fluidPage(
                                     # Plot-Row
                                     fluidRow(
                                       column(6,
                                              plotOutput("GenProfitPlot")),
                                       column(6,
                                              plotOutput("FilProfitPlot"))
                                     ),
                                     # Download-Button Row
                                     fluidRow(
                                       column(2, offset=2,
                                              downloadButton("downloadGenProfitPlot", "Download General Profit Plot")),
                                       column(2, offset=4,
                                              downloadButton("downloadFilProfitPlot", "Download filtered Profit Plot"))
                                     ),
                                     # KPI-Title Row
                                     fluidRow(
                                       column(6,
                                              hr(),
                                              p(strong("Some KPI's about the unfiltered, hold policies"))),
                                       column(6,
                                              hr(),
                                              p(strong("Some KPI's about the filtered, hold policies")))
                                     ),
                                     # Number of Contracts
                                     fluidRow(
                                       column(4,
                                              hr(),
                                              p("Number of contracts hold (total)")),
                                       column(1,
                                              hr(),
                                              textOutput("NmbContrTot")),
                                       column(4, offset=1,
                                              hr(),
                                              p("Number of contracts hold (filtered)")),
                                       column(1,
                                              hr(),
                                              textOutput("NmbContrFil"))
                                     ),
                                     # Return on Sales
                                     fluidRow(
                                       column(4,
                                              p("Return on Sales (total)")),
                                       column(1,
                                              textOutput("ROSTot")),
                                       column(4, offset=1,
                                              p("Return on Sales (filtered)")),
                                       column(1,
                                              textOutput("ROSFil"))
                                     ),
                                     # Return per Contract
                                     fluidRow(
                                       column(4,
                                              p("Average profit per contract (total)")),
                                       column(1,
                                              textOutput("AvgProfTot")),
                                       column(4, offset=1,
                                              p("Average profit per contract (filtered)")),
                                       column(1,
                                              textOutput("AvgProfFil"))
                                     ),
                                     
                                     
                                     
                                   )
                          ),
                          
                          # Fourth Sub-Tab, Contracts Data Table------------
                          tabPanel("Held Contracts",
                                   br(),
                                   mainPanel(
                                     dataTableOutput("contractsdatatable")
                                   )
                          )
                          
                        )),
               
               # Fifth Main-Tab Trainingdata------------
               tabPanel(" Explore Training Data",
                        tabsetPanel(
                            # First Sub-Tab, Scatterplot-------------
                            tabPanel("Scatterplot",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             p(strong("Variables for the Plot")),
                                             selectInput("Scatter_X_Axis", "Variable for X-Axis", choices = Trainnumcols, selected = "vh_speed"),
                                             selectInput("Scatter_Y_Axis", "Variable for Y-Axis", choices = Trainnumcols, selected = "Sum_claim_amount"),
                                             selectInput("Scatter_Color", "Variable for color", choices = Trainfactorcols, selected = "NULL"),
                                             selectInput("Scatter_Shape", "Variable for shape", choices = Trainfactorcols, selected = "NULL"),
                                             actionButton("Scatter_Go", "Create Plot"),
                                             br(), br(),
                                             radioButtons("Scatter_Smooth", "Add Regression", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                                             conditionalPanel(
                                               condition = "input.Scatter_Smooth == 'Yes'",
                                               selectInput("Scatter_Method", "Select Regression Method", choices = c("auto", "lm", "glm", "gam"), selected = "auto")
                                               
                                             ),
                                             
                                             ),
                                         mainPanel(
                                           p(strong("Scatterplot filtered according to Filter-Page")),
                                           br(),
                                           plotOutput("Scatterplot"),
                                           br(),
                                           downloadButton("downloadScatter", "Download Plot")
                                         )
                                         )
                                     ),
                            
                            # Second Sub-Tab, Map----------
                            tabPanel("Map",
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         p(strong("Plot a variable to the french map (pretty slow)")),
                                         selectInput("Map_Variable", "Variable for the map", choices = MapVars, selected="Avg_claim_amount"),
                                         br(),
                                         actionButton("Map_Go", "Create Map")
                                       ),
                                       
                                       mainPanel(
                                         plotOutput("MapPlot"),
                                         br(),
                                         downloadButton("downloadMap", "Download Map")
                                     )
                                     )
                            ),
                            
                            # Third Sub-Tab, Data Table------------
                            tabPanel("Data Table",
                                     br(),
                                     mainPanel(
                                        dataTableOutput("traindatatable")
                                        )
                                     )
                        
                        )),
               
               
               
               
               
               
               
               
               # Fifth Main-Tab Additional Explanations----------
               tabPanel("Additional Explanations")
               )
   
))
