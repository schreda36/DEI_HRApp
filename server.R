suppressPackageStartupMessages(c(library(tm),library(shiny),library(dplyr),library(plyr),library(scales),library(stringr),library(ggplot2)))

#source("global.R")
setwd("C:/Users/schre/DataScience/wd/EEO/app")

shinyServer(function(input, output) {
      
      #Data Type
      dT <- reactive ({
            if(input$DataType=="Salary")
                  "CurrentSalary"
            else if(input$DataType=="TotalCompensation")
                  "TotalCompensation"
      })
      
      #Data Criteria
      dC <- reactive ({
            dC <- NULL
            if(input$criteria=="JobTitle"){
                  dC <- c("JobTitle") }
            if(input$criteria=="YrsOfExperience") {
                  dC <- c("JobTitle", "YrsOfExperience") }
            dC
      })
      
      dataset <- reactive({
            ds <- read.csv("./data/SampleData.csv")
            dCols <- c(dT(), "Gender", dC())
            dataSubset <- subset(ds, select=dCols)
            dataSubset
      })

      # dataCriteria <- reactive({
      #       #if(input$criteria=="JobTitle")
      #             "JobTitle"
      # })
      
      dat <- data.frame(dataSubset)
      
      my.fun <- { function(df, crit1, meanX) { 
             ddply(df, eval(crit1), function(d) c(SalaryMean=mean(d[[meanX]])))
             }
      }
       
      processData <- reactive({
            ds <- data.frame(dataset()) #dataSubset
            
            #higher allowed variance should produce fewer results
            AllowedVariance <- as.numeric(input$variance)
            
            #crit<-c('Gender','JobTitle','YrsOfExperience')
            crit<-c('Gender','JobTitle')

            #get averages by Criteria for each Factor
            if(input$DataType=="Salary") {
                  #works, but hard-coded: ds.factor.avg <- aggregate(CurrentSalary ~ Gender + JobTitle, data=ds, mean)
                  ds.factor.avg <- my.fun(ds, crit, 'CurrentSalary')
  
                  ds.factor.M <- filter(ds.factor.avg,Gender=='M')
                  colnames(ds.factor.M) <- c("Gender","JobTitle","meanM")
                  ds.factor.F <- filter(ds.factor.avg,Gender=='F')
                  colnames(ds.factor.F) <- c("Gender","JobTitle","meanF")
                  
            }
            else if(input$DataType=="TotalCompensation") {
                  ds.factor.avg <- aggregate(TotalCompensation ~ Gender + JobTitle, data=ds, mean)
                  #ds.factor.avg <- aggregate(TotalCompensation ~ ., data=ds, mean)
                  ds.factor.avg <- data.frame(ds.factor.avg)
                  
                  ds.factor.M <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanM = mean(TotalCompensation[Gender == "M"]))
                  
                  ds.factor.F <- ddply(ds.factor.avg,.(JobTitle),
                                       summarise,meanF = mean(TotalCompensation[Gender == "F"]))
                  
            }
            
            #merge and get subset of data filter by allowed variance
            ds.factor.M<-ds.factor.M[,2:3] #remove Gender
            ds.factor.F<-ds.factor.F[,2:3]
            ds.factor <- merge(ds.factor.F, ds.factor.M, by=c("JobTitle"))
            diffPct <- (ds.factor$meanM-ds.factor$meanF)/ds.factor$meanM
            ds.factor$difference <- diffPct
            ds.factor.var <- filter(ds.factor, abs(diffPct)>AllowedVariance)
            
      })
      
            output$table <- renderDataTable({
            if (input$goButton > 0) {
                  
                  ds.factor.var <- processData()
                  ds.factor.var$difference <- ds.factor.var$difference*100
                  
                  if(input$DataType=="Salary") {
                        colnames(ds.factor.var) <- c("Job Title","Avg Salary (male)","Avg Salary (female)","% Difference")
                  }
                  else if(input$DataType=="TotalCompensation") {
                        colnames(ds.factor.var) <- c("Job Title","Avg TComp (male)","Avg TComp (female)","% Difference")
                  }

                  if(input$criteria=="YrsOfExperience") {
                        colnames(ds.factor.var) <- c("Job Title","YrsOfExperience","Avg Salary (male)","Avg Salary (female)","% Difference")
                  }
                  
                  ds.factor.var
            }
            
      })
      
      plot.range <- reactiveValues(x = NULL, y = NULL)
      
      output$plot <- renderPlot({
            ds.factor.var <- processData() 
            ds.factor.var$difference <- ds.factor.var$difference*100
            
            p <- ggplot(ds.factor.var, aes(JobTitle, difference,color=difference))
            p <- p + geom_point()
            p <- p + scale_color_gradientn(colours = c("red","yellow","#11BB11","yellow","red"))
            p <- p + guides(color=guide_legend(title="% Difference"))
            p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
            #p <- p + coord_cartesian(xlim = plot.range$x, ylim = plot.range$y)
            p <- p + labs(title =" ", x = "", y = "% Difference") 
            
            p
      }, height=400)
      
      output$click_info <- renderPrint({
            
            ds.factor.var <- data.frame(processData())
            ds.factor.var$difference <- round(ds.factor.var$difference*100,2)
            
            brush <- input$plot_brush
            #for ggplot where x is a factor xmin/xmax act like row numbers
            #on the otherhand ymin/ymax will equal the y (% Difference) values
            plot.range$x <- c(brush$xmin:brush$xmax) 
            ymin <- brush$ymin
            ymax <- brush$ymax
            
            cat("Click and drag mouse over data-points for more information.\n")
            #dpoint <- brushedPoints(ds.factor.var, input$plot_brush, xvar ="Job Title",yvar ="% Difference")
            #dpoint <- nearPoints(ds.factor.var, input$plot_click, threshold=100, xvar="Job Title",yvar="% Difference")
            ds.factor.var <- ds.factor.var[round(plot.range$x),]
            ds.factor.var <- subset(ds.factor.var, round(ds.factor.var$difference,2)>as.numeric(ymin) & round(ds.factor.var$difference,2)<as.numeric(ymax))
            
            if (nrow(ds.factor.var) == 0)
                  return()
            else {
                  if(input$DataType=="Salary") {
                         colnames(ds.factor.var) <- c("Job Title","Avg Salary (M)","Avg Salary (F)","% Difference")
                  }
                  else if(input$DataType=="TotalCompensation") {
                         colnames(ds.factor.var) <- c("Job Title","Avg TComp (M)","Avg TComp (F)","% Difference")
                  }
                  ds.factor.var
            }

      })
})