
suppressPackageStartupMessages(c(library(tm),library(shiny),library(dplyr),library(plyr),library(stringr),library(ggplot2)))

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
      
      
      # dss.factor.avg <- reactive({
      #       ds <- dataset()
      #       dss.factor.avg <- aggregate(CurrentSalary ~ Gender + JobTitle, data=ds, mean)
      # })
      
      # basic gender, race averages by title
      output$plotX <- renderPlot({
            if (input$goButton > 0) {
                  inFactor <- input$FactorType
                  ds <- dataset()
                 
                  #mean by factor and critiera
                  ds.factor.avg <- aggregate(CurrentSalary ~ Gender + JobTitle, data=dataset(), mean) 

                  plot(pm2.5.data.balt$year, pm2.5.data.balt$Emissions, type="b", pch=23, bg="blue", xlab="Year", 
                       ylab="Total Emissions (thousands of tons)", main="Baltimore PM2.5 Emissions")
                  
                  # p <- ggplot(dataSubset, aes(dataSubset$JobTitle, dataSubset$CurrentSalaryFTE, color=dataSubset$Gender)) 
                  # p <- p + geom_point() 
                  # p <- p + geom_point(data = dataSubset, aes(x=Gender,y=as.double(predSal())), color='red', size=4)
                  # p <- p + labs(x="Job Title",y="Annual Salary (USD$)") + scale_y_continuous(labels=scales::dollar)
                  # p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
                  # p <- p + guides(color=guide_legend(title="Perf Rating"))
                  # p
            }
      }, height=300)
      
      output$table <- renderDataTable({
            if (input$goButton > 0) {
                  dCols <- c("Gender", dT(), "JobTitle")
                  ds <- dataset() #dataSubset
                  #higher allowed variance should produce fewer results
                  AllowedVariance <- as.numeric(input$variance)
      
                  #get means by Criteria for each Factor
                  if(input$DataType=="Salary") {
                        ds.factor.avg <- aggregate(CurrentSalary ~ Gender + JobTitle, data=ds, mean)
                        ds.factor.avg <- data.frame(ds.factor.avg)
                        
                        ds.factor.F <- ddply(ds.factor.avg,.(JobTitle),
                                             summarise,meanF = mean(CurrentSalary[Gender == "F"]))
                        
                        ds.factor.M <- ddply(ds.factor.avg,.(JobTitle),
                                             summarise,meanM = mean(CurrentSalary[Gender == "M"]))
                  }
                  else if(input$DataType=="TotalCompensation") {
                        ds.factor.avg <- aggregate(TotalCompensation ~ Gender + JobTitle, data=ds, mean)
                        ds.factor.avg <- data.frame(ds.factor.avg)
                        
                        ds.factor.F <- ddply(ds.factor.avg,.(JobTitle),
                                             summarise,meanF = mean(TotalCompensation[Gender == "F"]))
                        
                        ds.factor.M <- ddply(ds.factor.avg,.(JobTitle),
                                             summarise,meanM = mean(TotalCompensation[Gender == "M"]))
                  }
                        
                  
            
                  ds.factor <- merge(ds.factor.F, ds.factor.M)
      
                  #get subset of data filter by allowed variance
                  difference <- (ds.factor$meanM-ds.factor$meanF)/ds.factor$meanM
                  ds.factor.var <- filter(ds.factor, difference>AllowedVariance)
                  ds.factor.var
            }
            
      })
      
})