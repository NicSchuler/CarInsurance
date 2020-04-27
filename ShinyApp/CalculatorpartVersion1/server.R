library(shiny)
library(plotly)
library(shinyjs)



shinyServer(function(input, output, session) {
    tables <- reactiveValues(inputpersonaldata = data.frame(Information_pol_coverage = character(), Information_pol_usage = character(), Information_drv_age1 = numeric(), Information_drv_sex1 = character(), Information_drv_age_lic1 = numeric(),Information_vh_speed = numeric(), Information_vh_value = numeric(), Information_vh_weight = numeric()))
    observeEvent(input$safe, {
        oldVals <- tables$inputpersonaldata
        tables$inputpersonaldata <- rbind(oldVals, data.frame(Information_pol_coverage = input$Information_pol_coverage,
                                                              Information_pol_usage = input$Information_pol_usage,
                                                              Information_drv_age1 = input$Information_drv_age1,
                                                              Information_drv_sex1 = input$Information_drv_sex1,
                                                              Information_drv_age_lic1 = input$Information_drv_age_lic1,
                                                              Information_vh_speed = input$Information_vh_speed,
                                                              Information_vh_value = input$Information_vh_value,
                                                              Information_vh_weight = input$Information_vh_weight))
    })
    output$displayDf <- renderDataTable(tables)
})
