library(shiny)
library(DT)
library(data.table)
library(ggplot2)

# Load the data and change the claims counter (Number not amount) to a factor 
traindata = fread("../../Data/pg17traindata.csv", stringsAsFactors = TRUE)
traindata$CountDistinct_id_claim = as.factor(traindata$CountDistinct_id_claim)
traindata$drv_drv2 = factor(traindata$drv_drv2, labels=c("1 Driver", "2 Drivers"))

# Build the server
shinyServer(function(input, output) {
    
    # Filter the data according to "Filter"-Panel
    filtered = eventReactive(input$Filter_Go, {
        traindata[pol_bonus >= input$Filter_pol_bonus[1] & pol_bonus <= input$Filter_pol_bonus[2] &
                  pol_coverage %in% input$Filter_pol_coverage &
                  pol_usage %in% input$Filter_pol_usage &
                  drv_drv2 %in% input$Filter_drv_drv2 &
                  drv_age1 >= input$Filter_drv_age1[1] & drv_age1 <= input$Filter_drv_age1[2] &
                  (drv_age2 >= input$Filter_drv_age2[1] & drv_age2 <= input$Filter_drv_age2[2] | drv_age2 == 0) &
                  drv_sex1 %in% input$Filter_drv_sex1 &
                  drv_sex2 %in% c(input$Filter_drv_sex2, "") &
                  drv_age_lic1 >= input$Filter_drv_age_lic1[1] & drv_age_lic1 <= input$Filter_drv_age_lic1[2] &
                  drv_age_lic2 >= input$Filter_drv_age_lic2[1] & drv_age_lic2 <= input$Filter_drv_age_lic2[2] &
                  vh_age >= input$Filter_vh_age[1] & vh_age <= input$Filter_vh_age[2] &
                  vh_cyl >= input$Filter_vh_cyl[1] & vh_cyl <= input$Filter_vh_cyl[2] &
                  vh_din >= input$Filter_vh_din[1] & vh_din <= input$Filter_vh_din[2] &
                  vh_speed >= input$Filter_vh_speed[1] & vh_speed <= input$Filter_vh_speed[2] &
                  vh_type %in% input$Filter_vh_type &
                  vh_weight >= input$Filter_vh_weight[1] & vh_weight <= input$Filter_vh_weight[2] &
                  vh_value >= input$Filter_vh_value[1] & vh_value <= input$Filter_vh_value[2] &
                  CountDistinct_id_claim %in% input$Filter_CountDistinct_id_claim &
                  Sum_claim_amount >= input$Filter_Sum_claim_amount[1] & Sum_claim_amount <= input$Filter_Sum_claim_amount[2]]
                  
    })
    
    # Create the Scatterplot
    output$Scatterplot = renderPlot({
        Scatter_X_Axis = eventReactive(input$Scatter_Go, {
            input$Scatter_X_Axis
        })
        
        Scatter_Y_Axis = eventReactive(input$Scatter_Go, {
            input$Scatter_Y_Axis
        })
        
        Scatter_Color = eventReactive(input$Scatter_Go, {
            input$Scatter_Color
        })
        
        Scatter_Shape = eventReactive(input$Scatter_Go, {
            input$Scatter_Shape
        })
        
        PLOT = ggplot(filtered(), aes_string(x=Scatter_X_Axis(), y=Scatter_Y_Axis(), color=Scatter_Color(), shape=Scatter_Shape())) +
            geom_point()
        
        ifelse(input$Scatter_Smooth=="Yes",
               ifelse(input$Scatter_Method %in% c("gam", "auto"),
                      return(PLOT +
                                 geom_smooth(formula = y~s(x, bs="cs"), na.rm=TRUE, method=input$Scatter_Method) +
                                 theme_classic()),
                      return(PLOT +
                                 geom_smooth(formula = y~x, method=input$Scatter_Method) +
                                 theme_classic())),
               return(PLOT + theme_classic())) 
    })
    
    # Create the data table for the training data
    output$traindatatable = renderDataTable({
        filtered()
    }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")


})
