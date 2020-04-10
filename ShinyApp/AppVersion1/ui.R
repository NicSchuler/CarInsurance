library(shiny)
library(tidyverse)

traincols = colnames(traindata)

numcols = c("pol_bonus", "pol_duration", "drv_age1", "drv_age2", "drv_age_lic1", "drv_age_lic2", "vh_age", "vh_cyl", "vh_speed", "vh_value", "vh_weight", "Sum_claim_amount")

factorcols = c("NULL", "pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2", "drv_sex1", "drv_sex2", "vh_fuel", "vh_make", "vh_type", "CountDistinct_id_claim")

# Define UI for application-----------
shinyUI(fluidPage(
    navbarPage("BSG Insurance",
               # First Main-Tab------------
               tabPanel("Explore Training Data",
                        tabsetPanel(
                            # First Sub-Tab
                            tabPanel("Filters",
                                     br(),
                                     p(strong("These filters are applied to all Plots and the data.table of the training data")),
                                     submitButton("Apply Filters"),
                                     br(),
                                     p(strong("Filter about the policies")),
                                     flowLayout(
                                       sliderInput("Filter_Bonus", "Filter pol_bonus", min=0.5, max=3.5, value=c(0.5, 3.5)),
                                       checkboxGroupInput("Filter_coverage", "Filter pol_coverage", c("Mini", "Median1", "Median2", "Maxi"), selected=c("Mini", "Median1", "Median2", "Maxi"), inline=TRUE),
                                       checkboxGroupInput("Filter_usage", "Filter pol_usage", c("Retired", "WorkPrivate", "Professional", "AllTrips"), selected=c("Retired", "WorkPrivate", "Professional", "AllTrips"), inline=TRUE)
                                     ),
                                     br(),
                                     p(strong("Filter about the drivers / policyholders")),
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
                            
                            # Second Sub-Tab
                            tabPanel("Scatterplot",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             p(strong("Variables for the Plot")),
                                             selectInput("Scatter_X_Axis", "Variable for X-Axis", choices = numcols, selected = "vh_speed"),
                                             selectInput("Scatter_Y_Axis", "Variable for Y-Axis", choices = numcols, selected = "Sum_claim_amount"),
                                             selectInput("Scatter_Color", "Variable for color", choices = factorcols, selected = "NULL"),
                                             selectInput("Scatter_Shape", "Variable for shape", choices = factorcols, selected = "NULL"),
                                             radioButtons("Scatter_Smooth", "Add Regression (only possible if shape = NULL)", choices = c("Yes", "No"), selected = "No", inline = TRUE),
                                             submitButton("Apply")
                                             ),
                                         mainPanel(
                                           p(strong("Scatterplot filtered according to Filter-Page")),
                                           br(),
                                           plotOutput("Scatterplot")
                                         )
                                         )
                                     ),
                            
                            # Third Sub-Tab
                            tabPanel("Data Table",
                                     br(),
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                         ),
                                         mainPanel(
                                             
                                         )
                                     )
                                     )
                        
                        )),
               
               
               # Second Main-Tab-------------
               tabPanel("Policy Portfolio Management"),
               
               
               # Third Main-Tab------------
               tabPanel("Premium Calculator"),
               
               
               # Fourth Main-Tab----------
               tabPanel("Additional Explanations")
               )
   
))
