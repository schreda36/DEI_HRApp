suppressPackageStartupMessages(c(library(tm),library(shiny),library(dplyr),library(plyr),library(scales),library(stringr),library(ggplot2)))

#source("global.R")
setwd("C:/Users/schre/DataScience/wd/EEO/app")

ds <- read.csv("./data/SampleData.csv")
unique.country <- sort(unique(trimws(ds$Country)))

shinyServer(function(input, output, session) {
      
      updateSelectizeInput(session, 'Filter', choices = as.character(unique.country), server = TRUE)
      
      #Data Type
      dT <- reactive ({
            if(input$DataType=="Salary")
                  "CurrentSalary"
            else if(input$DataType=="TotalCompensation")
                  "TotalCompensation"
      })
      
      #Data Determining Factor
      dFact <- reactive ({
            dFact <- NULL
            if(input$FactorType=="Gender"){
                  dFact <- c("Gender") }
            if(input$FactorType=="Ethnicity") {
                  dFact <- c("Ethnicity") }
            dFact
      })
      
      #Data GroupBy
      dC <- reactive ({
            dC <- NULL
            if(input$GroupBy=="JobTitle"){
                  dC <- c("JobTitle") }
            if(input$GroupBy=="YrsOfExperience") {
                  dC <- append(dC, "YrsOfExperience") }
            dC
      })
      
      #Data Filter
      dFilt <- reactive ({
            dFilt <- NULL
            if(input$FilterType=="Country") {
                  if(input$Filter=="USA"){
                        dFilt <- c("USA") }
                  else if(input$Filter=="ESP") {
                        dFilt <- c("ESP") }
            }
            dFilt
      })
      
      #Table Header
      tH <- reactive ({
            tH <- NULL
            
            if(input$GroupBy=="JobTitle"){
                  tH <- c("Job Title") }
            if(input$GroupBy=="YrsOfExperience") {
                  tH <- append(tH, "Yrs of Experience") }
            
            if(input$DataType=="Salary") {
                  tH <- append(tH, "Avg Salary (male)")
                  tH <- append(tH, "Avg Salary (female)") }
            else if(input$DataType=="TotalCompensation") {
                  tH <- append(tH, "Avg TComp (male)")
                  tH <- append(tH, "Avg TComp (female)")
            }
            
            tH <- append(tH, "% Difference")
            tH
      })
      
      #grab data and select data type, factor and GroupBy columns
      dataset <- reactive({
            ds <- read.csv("./data/SampleData.csv")
            ds <- ds[ds[input$FilterType]==eval(dFilt()),] #Filter
            dCols <- c(dT(), dFact(), dC()) #data type, factor and group by columns
            dataSubset <- subset(ds, select=dCols) #keep only columns of interest
            #dataSubset <- subset(ds, select=c("CurrentSalary","Gender","JobTitle","YrsOfExperience"))
            dataSubset
      })

      my.fun <- { function(df, GroupBy, meanX) { 
             ddply(df, eval(GroupBy), function(d) c(SalaryMean=mean(d[[meanX]])))
             }
      }
       
      processData <- reactive({
            ds <- data.frame(dataset()) #dataSubset
            
            #higher allowed variance should produce fewer results
            allowedVariance <- as.numeric(input$variance)/100
            
            crit <- c(dFact(), dC()) #add factor + GroupBy
            
            #get mean/average by by factor + GroupBy
            ds.factor.avg <- my.fun(ds, crit, dT())
            
            ds.factor.M <- filter(ds.factor.avg,Gender=='M')
            colnames(ds.factor.M) <- c("Gender","JobTitle","meanM")
            #colnames(ds.factor.M) <- c("Gender","JobTitle","YrsOfExperience","meanM")
            ds.factor.F <- filter(ds.factor.avg,Gender=='F')
            colnames(ds.factor.F) <- c("Gender","JobTitle","meanF")
            #colnames(ds.factor.F) <- c("Gender","JobTitle","YrsOfExperience","meanF")
            
            #merge and get subset of data filter by allowed variance
            ds.factor.M<-ds.factor.M[,2:3] #remove Gender
            ds.factor.F<-ds.factor.F[,2:3]
            #ds.factor <- merge(ds.factor.F, ds.factor.M, by=c("JobTitle","YrsOfExperience"))
            ds.factor <- merge(ds.factor.M, ds.factor.F, by=c("JobTitle"))
            diffPct <- (ds.factor$meanM-ds.factor$meanF)/ds.factor$meanM
            ds.factor$difference <- diffPct
            ds.factor.var <- filter(ds.factor, abs(diffPct)>allowedVariance)
            
      })
      
      output$table <- renderDataTable({
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))
            validate(need(input$Filter != "", "\n\nPlease select a value for 'Country'"))
            
            if (input$goButton > 0) {
                  
                  ds.factor.var <- processData()
                  ds.factor.var$difference <- ds.factor.var$difference*100
                  
                  colnames(ds.factor.var) <- tH()
                  
                  ds.factor.var
            }
            
      })
      
      plot.range <- reactiveValues(x = NULL, y = NULL)
      
      output$plot <- renderPlot({
            
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))
            
            ds.factor.var <- processData() 
            ds.factor.var$difference <- ds.factor.var$difference*100
            
            p <- ggplot(ds.factor.var, aes(JobTitle, difference,color=difference))
            p <- p + geom_point()
            p <- p + scale_color_gradientn(colours = c("red","yellow","#11BB11","yellow","red"))
            p <- p + guides(color=guide_legend(title="% Difference"))
            p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
            p <- p + labs(title =" ", x = "", y = "% Difference") 
            
            p
      }, height=400)
      
      output$click_info <- renderPrint({
            
            ds.factor.var <- data.frame(processData())
            ds.factor.var$difference <- round(ds.factor.var$difference*100,2)

            #for ggplot where x is a factor xmin/xmax act like row numbers
            #on the otherhand ymin/ymax will equal the y (% Difference) values
            validate(need(input$plot_brush!="", "Click and drag mouse over data-points for more information."))
            brush <- input$plot_brush
            plot.range$x <- c(brush$xmin:brush$xmax)
            ymin <- brush$ymin
            ymax <- brush$ymax
            
            cat("Click and drag mouse over data-points for more information.\n")
            ds.factor.var <- ds.factor.var[round(plot.range$x),]
            ds.factor.var <- subset(ds.factor.var, round(ds.factor.var$difference,2)>as.numeric(ymin) & round(ds.factor.var$difference,2)<as.numeric(ymax))
            
            if (nrow(ds.factor.var) == 0)
                  return() #return NULL
            else {
                  colnames(ds.factor.var) <- tH()
                  ds.factor.var
            }

      })
})