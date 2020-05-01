# Packages------------
library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(tidyverse)
library(stringr)
library(rgdal)      # Reading and projecting shapefiles
library(plotrix)    # Creating color scales
library(classInt)   # Assigning colors to data

# Transformations---------
# Load the data and change the claims counter (Number not amount) to a factor 
MatchedData = fread("../../Data/pg17traindata2.csv", stringsAsFactors = TRUE)
traindata = fread("../../Data/Train_pg17.csv", stringsAsFactors = TRUE)
MatchedData$CountDistinct_id_claim = as.factor(MatchedData$CountDistinct_id_claim)
MatchedData$drv_drv2 = factor(MatchedData$drv_drv2, labels=c("1 Driver", "2 Drivers"))
MatchedData$claim = as.factor(ifelse(MatchedData$Sum_claim_amount > 0, 1,0))
MatchedData$pol_insee_code = as.character(MatchedData$pol_insee_code)
MatchedData$Departement = as.factor(str_sub(MatchedData$pol_insee_code, end=-4))

load("../../Data/pg17testyear4.Rdata")
pg17testyear4 = setDT(pg17testyear4)
pg17testyear4$pol_insee_code = as.character(pg17testyear4$pol_insee_code)
pg17testyear4$Departement = as.factor(str_sub(pg17testyear4$pol_insee_code, end=-4))

# Create a subsample for the portfolio management tab
set.seed(1)
pg17testyear4corr = sample_n(pg17testyear4, 20000, replace=FALSE)
pg17testyear4corr = setDT(pg17testyear4corr)


# Load the data für the map
departements <- readOGR(dsn="../Geofla/Useful/DEPARTEMENT.shp")
departements@data$id = rownames(departements@data)
departements.points = fortify(departements)
departements.df = merge(departements.points, departements@data, by="id")

departements.dt = setDT(departements.df)

# Load the models
load("../../Data/prototypmodelle (rf und lm)/lm_tuned2.RData")
load("../../Data/prototypmodelle (rf und lm)/rf_tuned2.RData")

pg17testyear4corr$pred = predict(rf_tuned, pg17testyear4corr)
pg17testyear4corr$Premium = predict(lm_tuned3, pg17testyear4corr)

pg17testyear4corr$drv_drv2 = factor(pg17testyear4corr$drv_drv2, labels=c("1 Driver", "2 Drivers"))


# Actual Server-------------

# Build the server
shinyServer(function(input, output) {
    
    # Data Filters------------
    # Filter the traindata according to "Filter"-Panel
    filtered = eventReactive(input$Filter_Go, {
        MatchedData[pol_bonus >= input$Filter_pol_bonus[1] & pol_bonus <= input$Filter_pol_bonus[2] &
                  pol_coverage %in% input$Filter_pol_coverage &
                  pol_usage %in% input$Filter_pol_usage &
                  drv_drv2 %in% input$Filter_drv_drv2 &
                  drv_age1 >= input$Filter_drv_age1[1] & drv_age1 <= input$Filter_drv_age1[2] &
                  (drv_age2 >= input$Filter_drv_age2[1] & drv_age2 <= input$Filter_drv_age2[2] | drv_age2 == 0) &
                  drv_sex1 %in% input$Filter_drv_sex1 &
                  drv_sex2 %in% c(input$Filter_drv_sex2, "Niet") &
                  drv_age_lic1 >= input$Filter_drv_age_lic1[1] & drv_age_lic1 <= input$Filter_drv_age_lic1[2] &
                  (drv_age_lic2 >= input$Filter_drv_age_lic2[1] & drv_age_lic2 <= input$Filter_drv_age_lic2[2] | drv_age_lic2 == 0) &
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
    
    # Filter the testyear1 data according to "Filter"-Panel
    filteredtest4 = eventReactive(input$Filter_Go, {
        pg17testyear4corr[pol_bonus >= input$Filter_pol_bonus[1] & pol_bonus <= input$Filter_pol_bonus[2] &
                        pol_coverage %in% input$Filter_pol_coverage &
                        pol_usage %in% input$Filter_pol_usage &
                        drv_drv2 %in% input$Filter_drv_drv2 &
                        drv_age1 >= input$Filter_drv_age1[1] & drv_age1 <= input$Filter_drv_age1[2] &
                        (drv_age2 >= input$Filter_drv_age2[1] & drv_age2 <= input$Filter_drv_age2[2] | drv_age2 == 0) &
                        drv_sex1 %in% input$Filter_drv_sex1 &
                        drv_sex2 %in% c(input$Filter_drv_sex2, "") &
                        drv_age_lic1 >= input$Filter_drv_age_lic1[1] & drv_age_lic1 <= input$Filter_drv_age_lic1[2] &
                        (drv_age_lic2 >= input$Filter_drv_age_lic2[1] & drv_age_lic2 <= input$Filter_drv_age_lic2[2] | drv_age_lic2 == 0) &
                        vh_age >= input$Filter_vh_age[1] & vh_age <= input$Filter_vh_age[2] &
                        vh_cyl >= input$Filter_vh_cyl[1] & vh_cyl <= input$Filter_vh_cyl[2] &
                        vh_din >= input$Filter_vh_din[1] & vh_din <= input$Filter_vh_din[2] &
                        vh_speed >= input$Filter_vh_speed[1] & vh_speed <= input$Filter_vh_speed[2] &
                        vh_type %in% input$Filter_vh_type &
                        vh_weight >= input$Filter_vh_weight[1] & vh_weight <= input$Filter_vh_weight[2] &
                        vh_value >= input$Filter_vh_value[1] & vh_value <= input$Filter_vh_value[2]]
        
    })
    
    # Scatterplot - Output-----------
    # Create the Scatterplot
     Scatterplot = reactive({
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
        
        ScatterPLOT = reactive({ggplot(filtered(), aes_string(x=Scatter_X_Axis(), y=Scatter_Y_Axis(), color=Scatter_Color(), shape=Scatter_Shape())) +
            geom_point()})
        
        ifelse(input$Scatter_Smooth=="Yes",
               ifelse(input$Scatter_Method %in% c("gam", "auto"),
                      return(ScatterPLOT() +
                                 geom_smooth(formula = y~s(x, bs="cs"), na.rm=TRUE, method=input$Scatter_Method) +
                                 theme_classic()),
                      return(ScatterPLOT() +
                                 geom_smooth(formula = y~x, method=input$Scatter_Method) +
                                 theme_classic())),
               return(ScatterPLOT() + theme_classic())) 
    })
    
    output$Scatterplot = renderPlot({Scatterplot()})
    
    # Download Button for Scatterplot
    output$downloadScatter <- downloadHandler(
        filename = function() { paste('CleanDataScatterplot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,Scatterplot())
        }
    )
    
    
    # Data Table output---------
    # Create the data table for the training data
    output$traindatatable = renderDataTable({
        filtered()
    }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")
    
    # MapPlot - Output------------
    # Adapt the data for the map
    mapData = reactive({
        filtered()[,.(insured_Cars=.N,
                      Avg_pol_bonus=mean(pol_bonus),
                      Avg_claim_count=mean(as.integer(CountDistinct_id_claim)-1),
                      Avg_claim_amount=mean(Sum_claim_amount)
        ), by=Departement]
    })
    
    mapDataMatched = reactive({
        merge.data.table(departements.dt, mapData(), by.x = "CODE_DEPT", by.y = "Departement", all.x = TRUE)
    })
    
    # Create the map-plot
    MapPlot = eventReactive(input$Map_Go, {
        ggplot(mapDataMatched(), aes_string("long", "lat", group="group", fill=input$Map_Variable)) + 
            geom_polygon() +
            geom_path(color="black") +
            coord_equal() +
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="right",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())
    })
    
    # Render the Plot
    output$MapPlot = renderPlot({MapPlot()})
    
    # Download Button for Map Plot
    output$downloadMap <- downloadHandler(
        filename = function() { paste('CleanDataMap ', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,MapPlot())
        }
    )
    
    # Compare Densities Plot - Output--------
    DensTrain = eventReactive(input$ComDensity_Go, {
        a = as.data.frame(filtered())
        aa = density(a[,(input$CompDensity_Var)])
        DensTrain = aa$y})
    DensTest = eventReactive(input$ComDensity_Go, {
        b = as.data.frame(filteredtest4())
        bb = density(b[,(input$CompDensity_Var)])
        DensTest = bb$y})
    DensTestX = eventReactive(input$ComDensity_Go, {
        c = as.data.frame(filteredtest4())
        cc = density(c[,(input$CompDensity_Var)])
        DensTestX = cc$x})
    
    DensDf = reactive({
        DensDf = data.frame(
            x=DensTestX(),
            y=(DensTest()-DensTrain())*nrow(filteredtest4())
        )
    })
    
    DensityPlot = reactive({ggplot(DensDf(), aes(x=x, y=y, color = y)) +
        geom_line(size=1.5) +
        scale_color_gradient2(midpoint=0, low="red", mid="darkgrey", high="green4", space ="Lab") +
        labs(x=paste(input$CompDensity_Var), y="Deviance from Trainig-Density", title = paste("Compare the frequencies (density) of ", input$CompDensity_Var,"'s values", sep="")) +
        theme_classic()})
    
    output$ComDensPlot = renderPlot({DensityPlot()})
    
    # Download Button for Density Plot
    output$downloadDensity <- downloadHandler(
        filename = function() { paste('CompareDensityPlot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,DensityPlot())
        }
    )
    
    # Compare Mass Plot - Output---------
    MassTrain = eventReactive(input$ComMass_Go, {
        d = as.data.frame(filtered())
        MassTrain = table(d[,(input$CompMass_Var)])/nrow(filtered())})
    MassTest = eventReactive(input$ComMass_Go, {
        e = as.data.frame(filteredtest4())
        MassTest = table(e[,(input$CompMass_Var)])/nrow(filteredtest4())})
    CompMasses = reactive({as.data.frame((MassTest() - MassTrain())*nrow(filteredtest4()))})
    
    MassPlot = reactive({ggplot(CompMasses(), aes(x=Var1, fill = Freq)) +
        geom_bar(aes(weight=Freq)) +
        scale_fill_gradient2(midpoint=0, low="red", mid="darkgrey", high="green4", space ="Lab") +
        labs(x=paste(input$CompMass_Var), y="Deviance from Trainig-Mass", title = paste("Compare the frequencies of ", input$CompMass_Var,"'s categories", sep="")) +
        theme_classic()})
    
    output$ComMassPlot = renderPlot({MassPlot()})
    
    # Download Button for Mass Plot
    output$downloadMass <- downloadHandler(
        filename = function() { paste('CompareMassPlot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,MassPlot())
        }
    )
    
    
    # General Profit Plot--------------
    GeneralRevenue = sum(pg17testyear4corr$Premium)
    GeneralClaimsExp = sum(pg17testyear4corr$Premium[pg17testyear4corr$pred==1])
    GeneralProfit = GeneralRevenue - GeneralClaimsExp
    
    KPI = c("Revenue","Expenditures","Profit")
    Value = c(GeneralRevenue, GeneralClaimsExp, GeneralProfit)
    GeneralFin = data.frame(KPI, Value)
    GeneralFin$Value = round(as.integer(paste(GeneralFin$Value))/1000,0)
    
    options(scipen=999)
    
    GeneralFinPlot = ggplot(GeneralFin, aes(x=KPI, y=Value)) +
        geom_col(aes(fill=KPI), show.legend = FALSE) +
        geom_text(aes(label=Value), vjust=1.6, color="white", size=5.5)+
        scale_x_discrete(limits=c("Revenue", "Expenditures", "Profit")) +
        scale_fill_manual(values = c("Revenue"="black", "Expenditures"= "red", "Profit"=ifelse(GeneralProfit>=0,"green4","red"))) +
        labs(x=element_blank(), y="in kEuros", title = "General Revenue, Expenditures and Profit") +
        theme_classic()
    
    output$GenProfitPlot = renderPlot({GeneralFinPlot})
    
    # Download Button for Density Plot
    output$downloadGenProfitPlot <- downloadHandler(
        filename = function() { paste('GeneralProfitPlot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,GeneralFinPlot)
        }
    )
    
    
    # Filtered Profit Plot--------------
    FilteredRevenue = reactive({sum(filteredtest4()$Premium)})
    FilteredClaimsExp = reactive({sum(filteredtest4()$Premium[filteredtest4()$pred==1])})
    FilteredProfit = reactive({FilteredRevenue() - FilteredClaimsExp()})
    
    FilKPI = c("Revenue","Expenditures","Profit")
    FilValue = reactive({c(FilteredRevenue(), FilteredClaimsExp(), FilteredProfit())})
    FilteredFin = reactive({data.frame(FilKPI, FilValue=round(as.integer(paste(FilValue()))/1000,0))})
    
    options(scipen=999)
    
    FilteredFinPlot = reactive({ggplot(FilteredFin(), aes(x=FilKPI, y=FilValue)) +
        geom_col(aes(fill=FilKPI), show.legend = FALSE) +
        geom_text(aes(label=FilValue), vjust=1.6, color="white", size=5.5)+
        scale_x_discrete(limits=c("Revenue", "Expenditures", "Profit")) +
        scale_fill_manual(values = c("Revenue"="black", "Expenditures"= "red", "Profit"="green4")) +
        labs(x=element_blank(), y="in kEuros", title = "Filtered Revenue, Expenditures and Profit") +
        theme_classic()})
    
    output$FilProfitPlot = renderPlot({FilteredFinPlot()})
    
    # Download Button for Density Plot
    output$downloadFilProfitPlot <- downloadHandler(
        filename = function() { paste('GeneralProfitPlot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,FilteredFinPlot)
        }
    )
    
    output$asdfasdf = renderPrint({nrow(filteredtest4())})
    
    
})
