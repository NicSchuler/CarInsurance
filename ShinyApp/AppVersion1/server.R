library(shiny)
library(DT)
library(data.table)
library(ggplot2)

# Load the data and change the claims counter (Number not amount) to a factor 
traindata = fread("../../Data/pg17traindata.csv", stringsAsFactors = TRUE)
traindata$CountDistinct_id_claim = as.factor(traindata$CountDistinct_id_claim)

# Build the server
shinyServer(function(input, output) {
    
    # Filter the data according to "Filter"-Panel
    filtered = reactive({
        filtered = traindata
        filtered
    })
    
    output$Scatterplot = renderPlot({
        Scatter = ggplot(filtered(), aes_string(x=input$Scatter_X_Axis, y=input$Scatter_Y_Axis, color=input$Scatter_Color, size=input$Scatter_Size)) +
            geom_point()
        ScatterSmooth = Scatter +
            geom_smooth(aes_string(x=input$Scatter_X_Axis, y=input$Scatter_Y_Axis), formula=y~x,na.rm=TRUE) +
            theme_classic()
        if(input$Scatter_Smooth == "Yes"){
            return(ScatterSmooth)
        }
        else{
            return(Scatter + theme_classic())
        }
    })


})
