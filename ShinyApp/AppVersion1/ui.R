library(shiny)
library(tidyverse)

traincols = colnames(traindata)

numcols = c("pol_bonus", "pol_duration", "drv_age1", "drv_age2", "drv_age_lic1", "drv_age_lic2", "vh_age", "vh_cyl", "vh_speed", "vh_value", "vh_weight", "Sum_claim_amount")

factorcols = c("pol_coverage", "pol_pay_freq", "pol_payd", "pol_usage", "drv_drv2", "drv_sex1", "drv_sex2", "vh_fuel", "vh_make", "vh_type", "CountDistinct_id_claim")

# Define UI for application-----------
shinyUI(fluidPage(
    navbarPage("BSG Insurance",
               # First Main-Tab------------
               tabPanel("Explore Training Data",
                        tabsetPanel(
                            # First Sub-Tab
                            tabPanel("Filters",
                                     br(),
                                     flowLayout(
                                             
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
                                             selectInput("Scatter_Color", "Variable for color", choices = factorcols, selected = "vh_value"),
                                             selectInput("Scatter_Size", "Variable for size", choices = numcols, selected = "CountDistinct_id_claim"),
                                             radioButtons("Scatter_Smooth", "Add Regression", choices = c("Yes", "No"), selected = "No")
                                             ),
                                         mainPanel(
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
