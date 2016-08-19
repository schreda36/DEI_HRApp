suppressPackageStartupMessages(c(library(tm),library(shiny),library(dplyr),library(plyr),library(scales),library(stringr),library(ggplot2)))

#source("global.R")
setwd("C:/Users/schre/DataScience/wd/EEO/app")

shinyServer(function(input, output) {
      
      dT <- reactive ({
            if(input$DataType=="Salary")
                  "CurrentSalary"
            else if(input$DataType=="TotalCompensation")
                  "TotalCompensation"
      })
      
      dataset <- reactive({
            ds <- read.csv("./data/SampleData.csv")
            #if(input$DataType=="Salary")
            #     dataSubset <- subset(ds, select=c(Gender, CurrentSalary, JobTitle))
            #else if(input$DataType=="Total Compensation")
            #     dataSubset <- subset(ds, select=c(Gender, TotalCompensation, JobTitle))
            dCols <- c("Gender", dT(), "JobTitle")
            dataSubset <- subset(ds, select=dCols)
            dataSubset
      })

      processData <- reactive({
            ds <- dataset() #dataSubset
            
            #higher allowed variance should produce fewer results
            AllowedVariance <- as.numeric(input$variance)
            
            #get averages by Criteria for each Factor
            if(input$DataType=="Salary") {
                  ds.factor.avg <- aggregate(CurrentSalary ~ Gender + JobTitle, data=ds, mean)
                  ds.factor.avg <- data.frame(ds.factor.avg)
                  
                  ds.factor.M <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanM = mean(CurrentSalary[Gender == "M"]))
                  
                  ds.factor.F <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanF = mean(CurrentSalary[Gender == "F"]))
                  
            }
            else if(input$DataType=="TotalCompensation") {
                  ds.factor.avg <- aggregate(TotalCompensation ~ Gender + JobTitle, data=ds, mean)
                  ds.factor.avg <- data.frame(ds.factor.avg)
                  
                  ds.factor.M <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanM = mean(TotalCompensation[Gender == "M"]))
                  
                  ds.factor.F <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanF = mean(TotalCompensation[Gender == "F"]))
                  
            }
            
            #merge and get subset of data filter by allowed variance
            ds.factor <- merge(ds.factor.F, ds.factor.M)
            diffPct <- (ds.factor$meanM-ds.factor$meanF)/ds.factor$meanM
            ds.factor$difference <- diffPct
            ds.factor.var <- filter(ds.factor, abs(diffPct)>AllowedVariance)
            
      })
      
      output$table <- renderDataTable({
            if (input$goButton > 0) {
                  
                  ds.factor.var <- processData()
                  ds.factor.var$difference <- percent(ds.factor.var$difference)
                  
                  if(input$DataType=="Salary") {
                        colnames(ds.factor.var) <- c("Job Title","Avg Salary (male)","Avg Salary (female)","% Difference")
                  }
                  else if(input$DataType=="TotalCompensation") {
                        colnames(ds.factor.var) <- c("Job Title","Avg TComp (male)","Avg TComp (female)","% Difference")
                  }
                  ds.factor.var
            }
            
      })
      
      output$plot <- renderPlot({
            ds.factor.var <- processData() 
            #ds.factor.var <- subset(ds.factor.var)
            #ds.factor.var$difference <- percent(ds.factor.var$difference)
            
            # p <- ggplot(ds.factor.var, aes(JobTitle, difference))
            # p <- p + geom_point()
            # p <- p + labs(title =" New title", x = "Job Titles", y = "% Difference") 
            
            #plot_click appears to work better with basic plot, not ggplot2
            p <- plot(x=ds.factor.var[,1], y=ds.factor.var$difference, xlab="Job Title", ylab="% Difference")
            p
      })
      
      output$click_info <- renderPrint({
            
            ds.factor.var <- data.frame(processData())
            #ds.factor.var$difference <- percent(ds.factor.var$difference)
            
            # if(input$DataType=="Salary") {
            #       colnames(ds.factor.var) <- c("Job Title","Avg Salary (male)","Avg Salary (female)","% Difference")
            # }
            # else if(input$DataType=="TotalCompensation") {
            #       colnames(ds.factor.var) <- c("Job Title","Avg TComp (male)","Avg TComp (female)","% Difference")
            # }
            
            cat("Click and drag mouse over data-points for more information.\n")
            # brush <- input$plot_brush
            # plot.range <- reactiveValues(x = NULL, y = NULL)
            # plot.range$x <- c(brush$xmin, brush$xmax)
            
            #ds.factor.var <- brushedPoints(ds.factor.var,input$plot_brush, xvar ="Job Title",yvar ="% Difference")
            #ds.factor.var <- subset(ds.factor.var,select=c("Job Title","% Difference"))
            
            dpoint <- brushedPoints(ds.factor.var, input$plot_brush, xvar ="JobTitle",yvar ="difference")
            #dpoint <- nearPoints(ds.factor.var, input$plot_click, xvar="Job Title",yvar="% Difference")
            

            
            if (nrow(dpoint) == 0)
                  return()
            # else {
                   # keeprows <- round(plot.range$x) == ds.factor.var["Job Title"]
                   # head(ds.factor.var[keeprows, ], 10)
            #       #as.numeric(ds.factor.var["Job Title"])
            #       #plot.range$x
            # }
            dpoint
            #ds.factor.var

      })
})