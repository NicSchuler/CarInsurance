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
    filtered = reactive({
        filtered = traindata
        filtered
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
