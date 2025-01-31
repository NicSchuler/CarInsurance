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

# load("../../Data/pg17testyear4.Rdata")
# pg17testyear4 = setDT(pg17testyear4)
# pg17testyear4$pol_insee_code = as.character(pg17testyear4$pol_insee_code)
# pg17testyear4$Departement = as.factor(str_sub(pg17testyear4$pol_insee_code, end=-4))
# 
# # Create a subsample for the portfolio management tab
# set.seed(1)
# Zwischtest = sample_n(Contracts, 200, replace=FALSE)
# Zwischtest = setDT(Zwischtest)


# Load the data für the map
departements <- readOGR(dsn="../Geofla/Useful/DEPARTEMENT.shp")
departements@data$id = rownames(departements@data)
departements.points = fortify(departements)
departements.df = merge(departements.points, departements@data, by="id")

departements.dt = setDT(departements.df)

# Load the models
# load("../../Data/prototypmodelle (rf und lm)/lm_tuned4.RData")
# load("../../Data/prototypmodelle (rf und lm)/rf_tuned2.RData")
# 
# Verwaltungsaufwand = 200
# Margin=0.25
# 
# pg17testyear4corr$pred = predict(rf_tuned, pg17testyear4corr)
# pg17testyear4corr$fairPremium = exp(predict(lm_tuned4, pg17testyear4corr))
# pg17testyear4corr$Premium = (pg17testyear4corr$fairPremium+Verwaltungsaufwand)*(1+Margin)
# 
# pg17testyear4corr$drv_drv2 = factor(pg17testyear4corr$drv_drv2, labels=c("1 Driver", "2 Drivers"))
# 
# Contracts = pg17testyear4corr %>%
#     mutate(Name=id_policy) %>%
#     select(c("Name", "vh_make", "pol_coverage", "pol_usage", "drv_drv2", "drv_sex1", "drv_age_lic1", "vh_din", "vh_speed", "vh_value", "vh_weight", "pred", "fairPremium", "Premium"))

# save(Contracts, file="../../Contracts.RData")
# 
# load("../../Data/Contracts.RData")
# Contracts = setDT(Contracts)
# 
# Zwischentabelle = Contracts[1]

mround <- function(x,base){
    base*round(x/base)
}

Zwischentabelle = readRDS("../../Data/zwischTEST.rds")


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
        unfilteredtest4()[pol_coverage %in% input$Filter_pol_coverage &
                  pol_usage %in% input$Filter_pol_usage &
                  drv_drv2 %in% input$Filter_drv_drv2 &
                  drv_sex1 %in% input$Filter_drv_sex1 &
                  drv_age_lic1 >= input$Filter_drv_age_lic1[1] & drv_age_lic1 <= input$Filter_drv_age_lic1[2] &
                  vh_din >= input$Filter_vh_din[1] & vh_din <= input$Filter_vh_din[2] &
                  vh_speed >= input$Filter_vh_speed[1] & vh_speed <= input$Filter_vh_speed[2] &
                  vh_weight >= input$Filter_vh_weight[1] & vh_weight <= input$Filter_vh_weight[2] &
                  vh_value >= input$Filter_vh_value[1] & vh_value <= input$Filter_vh_value[2]]
        
    })
    
    unfilteredtest4 = eventReactive(input$Filter_Go, {
        unfilteredtest4 = readRDS("../../Data/Contracts.rds")
        unfilteredtest4 = setDT(unfilteredtest4)
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
    GeneralRevenue = reactive({sum(unfilteredtest4()$Premium)})
    GeneralExpenditures = reactive({sum(unfilteredtest4()$fairPremium[unfilteredtest4()$pred==1])+nrow(unfilteredtest4())*200})
    GeneralProfit = reactive({GeneralRevenue() - GeneralExpenditures()})
    
    GeneralKPI = c("Revenue","Expenditures","Profit")
    GeneralValue = reactive({c(GeneralRevenue(), GeneralExpenditures(), GeneralProfit())})
    GeneralFin = reactive({data.frame(GeneralKPI, GeneralValue=round(as.integer(paste(GeneralValue()))/1000,0))})
    
    options(scipen=999)
    
    GeneralFinPlot = reactive({ggplot(GeneralFin(), aes(x=GeneralKPI, y=GeneralValue)) +
            geom_col(aes(fill=GeneralKPI), show.legend = FALSE) +
            geom_text(aes(label=GeneralValue), vjust=1.6, color="white", size=5.5)+
            scale_x_discrete(limits=c("Revenue", "Expenditures", "Profit")) +
            scale_fill_manual(values = c("Revenue"="black", "Expenditures"= "red", "Profit"="green4")) +
            labs(x=element_blank(), y="in kEuros", title = "Filtered Revenue, Expenditures and Profit") +
            theme_classic()})
    
    output$GenProfitPlot = renderPlot({GeneralFinPlot()})
    
    # Download Button for Density Plot
    output$downloadGenProfitPlot <- downloadHandler(
        filename = function() { paste('GeneralProfitPlot_', Sys.Date(), '.png', sep='') },
        content = function(file) {
            ggsave(file,GeneralFinPlot)
        }
    )
    
    
    # Filtered Profit Plot--------------
    FilteredRevenue = reactive({sum(filteredtest4()$Premium)})
    FilteredExpenditures = reactive({sum(filteredtest4()$fairPremium[filteredtest4()$pred==1])+nrow(filteredtest4())*200})
    FilteredProfit = reactive({FilteredRevenue() - FilteredExpenditures()})
    
    FilKPI = c("Revenue","Expenditures","Profit")
    FilValue = reactive({c(FilteredRevenue(), FilteredExpenditures(), FilteredProfit())})
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
    
    # KPI - Text Outputs--------------
    output$NmbContrTot = renderText({nrow(unfilteredtest4())})
    output$NmbContrFil = renderText({nrow(filteredtest4())})
    output$ROSTot = renderText({paste(round((GeneralProfit()/GeneralRevenue())*100,2),"%")})
    output$ROSFil = renderText({paste(round((FilteredProfit()/FilteredRevenue())*100,2),"%")})
    output$AvgProfTot = renderText({paste(round((GeneralProfit()/nrow(unfilteredtest4())),2)," Euros")})
    output$AvgProfFil = renderText({paste(round((FilteredProfit()/nrow(filteredtest4())),2)," Euros")})
    
    # Check Contracts - Text Outputs----------
    output$CheckNumber = renderText({nrow(Zwischentabelle())})
    output$CheckName = renderText({as.character(Zwischentabelle()$Name[1])})
    output$Checkvh_make = renderText({as.character(Zwischentabelle()$vh_make[1])})
    output$Checkpol_coverage = renderText({as.character(Zwischentabelle()$pol_coverage[1])})
    output$Checkpol_usage = renderText({as.character(Zwischentabelle()$pol_usage[1])})
    output$Checkdrv_drv2 = renderText({as.character(Zwischentabelle()$drv_drv2[1])})
    output$Checkdrv_sex1 = renderText({as.character(Zwischentabelle()$drv_sex1[1])})
    output$Checkdrv_age_lic1 = renderText({Zwischentabelle()$drv_age_lic1[1]})
    output$Checkvh_din = renderText({Zwischentabelle()$vh_din[1]})
    output$Checkvh_speed = renderText({Zwischentabelle()$vh_speed[1]})
    output$Checkvh_value = renderText({Zwischentabelle()$vh_value[1]})
    output$Checkvh_weight = renderText({Zwischentabelle()$vh_weight[1]})
    output$Checkpred = renderText({as.character(Zwischentabelle()$pred[1])})
    output$CheckfairPremium = renderText({mround(Zwischentabelle()$fairPremium[1],5)})
    output$CheckPremium = renderText({Zwischentabelle()$Premium[1]})
    
    # Accept Contract Button----------
    observeEvent(input$AcceptContract_Go, {
            # Load the old lists
            OldContracts = readRDS("../../Data/Contracts.rds")
            OldContracts = setDT(OldContracts)
            OldZwischentabelle = readRDS("../../Data/zwischTEST.rds")
            OldZwischentabelle = setDT(OldZwischentabelle)
            
            # Update the Contracts
            NewContract = OldZwischentabelle[1]
            UpdContracts = rbind(OldContracts, NewContract)
            saveRDS(UpdContracts, file="../../Data/Contracts.rds")
            
            # Update Requests
            UpdZwischentabelle = OldZwischentabelle[2:nrow(OldZwischentabelle)]
            saveRDS(UpdZwischentabelle, file="../../Data/zwischTEST.rds")
    })
    
    # Next Contract Button--------
    Zwischentabelle = eventReactive(input$CheckNextContract, {
        Zwischentabelle = readRDS("../../Data/zwischTEST.rds")
    })
    
    # Decline Contract Button--------
    observeEvent(input$DeclineContract_Go, {
        # Load old Requests
        OldZwischentabelle2 = readRDS("../../Data/zwischTEST.rds")
        OldZwischentabelle2 = setDT(OldZwischentabelle2)
        
        # Update Requests
        UpdZwischentabelle2 = OldZwischentabelle2[2:nrow(OldZwischentabelle2)]
        saveRDS(UpdZwischentabelle2, file="../../Data/zwischTEST.rds")
    })
    
    
    
    # Data Table (Contracts) output---------
    # Create the data table for the training data
    output$contractsdatatable = renderDataTable({
        unfilteredtest4()
    }, options=list(searching=FALSE, paging=TRUE), rownames=FALSE, filter="top")

    
    
})
