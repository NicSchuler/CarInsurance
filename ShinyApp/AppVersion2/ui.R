# Packages---------
library(shiny)
library(tidyverse)

# Drop-Down Lists-----------
numcols = c("pol_bonus", "pol_duration", "drv_age1", "drv_age2", "drv_age_lic1", "drv_age_lic2", "vh_age", "vh_cyl", "vh_din", "vh_speed", "vh_value", "vh_weight", "Sum_claim_amount")

factorcols = c("NULL", "pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2", "drv_sex1", "drv_sex2", "vh_fuel", "vh_make", "vh_type", "CountDistinct_id_claim")

factorcol = c("pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2", "drv_sex1", "drv_sex2", "vh_fuel", "vh_make", "vh_type", "CountDistinct_id_claim")

MapVars = c("insured_Cars", "Avg_pol_bonus", "Avg_claim_count", "Avg_claim_amount")

# Define UI for application-----------

shinyUI(fluidPage(
    navbarPage("BSGN Insurance",
               # First Main-Tab Filters---------
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
               # Second Main-Tab Trainingdata------------
               tabPanel(" Explore Training Data",
                        tabsetPanel(
                            # First Sub-Tab, Scatterplot-------------
                            tabPanel("Scatterplot",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             p(strong("Variables for the Plot")),
                                             selectInput("Scatter_X_Axis", "Variable for X-Axis", choices = numcols, selected = "vh_speed"),
                                             selectInput("Scatter_Y_Axis", "Variable for Y-Axis", choices = numcols, selected = "Sum_claim_amount"),
                                             selectInput("Scatter_Color", "Variable for color", choices = factorcols, selected = "NULL"),
                                             selectInput("Scatter_Shape", "Variable for shape", choices = factorcols, selected = "NULL"),
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
               
               
               # Third Main-Tab Policy Protfolio Management-------------
               tabPanel("Policy Portfolio Management",
                        tabsetPanel(
                          
                          # First Sub-Tab, Densities---------
                          tabPanel("Densities",
                                   br(),
                                   sidebarLayout(
                                     sidebarPanel(
                                       p(strong("Choose the Variable you want to compare")),
                                       br(),
                                       selectInput("CompDensity_Var", "Variable to compare", choices = numcols, selected = "vh_speed"),
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
                                     selectInput("CompMass_Var", "Variable to compare", choices = factorcol, selected = "pol_coverage"),
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
                                     fluidRow(
                                       column(6,
                                              plotOutput("GenProfitPlot")),
                                       column(6,
                                              plotOutput("FilProfitPlot"))
                                     ),
                                     fluidRow(
                                       column(2, offset=2,
                                              downloadButton("downloadGenProfitPlot", "Download General Profit Plot")),
                                       column(2, offset=4,
                                              downloadButton("downloadFilProfitPlot", "Download filtered Profit Plot"),
                                              verbatimTextOutput("asdfasdf"))
                                     
                                   
                                   
                                   )
                          ))
                        
               )),
               
               # Fourth Main-Tab Premium Calculator------------
               tabPanel("Premium Calculator"),
               
               
               # Fifth Main-Tab Additional Explanations----------
               tabPanel("Additional Explanations")
               )
   
))
