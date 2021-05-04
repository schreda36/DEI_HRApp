suppressPackageStartupMessages(c(library(shiny),library(RMySQL),library(tm),library(dplyr),library(plyr),library(scales),library(stringr),library(ggplot2)))


#source("global.R")


shinyServer(function(input, output, session) {
      
      #---------------- UI Functions -------------------
      
      filterChoice <- reactive ({
             ds <- read.csv("./data/SampleData.csv")
             unique.filter <- sort(unique(trimws(ds[[input$FilterType]])))
      })
      
      observe({
            updateSelectizeInput(session, 'Filter', choices = as.character(filterChoice()), server = TRUE)
      })
      
      
      #Data Type
      dType <- reactive ({
            if(input$DataType=="Salary") {
                  dType <- "CurrentSalary"
            }
            else if(input$DataType=="TotalCompensation") {
                  dType <- "TotalCompensation"
            }
            else  {
                  dType <- input$DataType
            }
            dType
      })
      
      #Data Determining Factor
      dFact <- reactive ({
            # Factor columns must be identical to column names in datasource
            dFact <- input$FactorType      
            dFact
      })
      
      #Data Criteria 'Group By'
      dGroupBy <- reactive ({
            # GroupBy columns must be identical to column names in datasource
            dC <- input$GroupBy
            dC
      })
      
      #Data Filter
      dFilt <- reactive ({
            # Filter columns must be identical to column names in datasource
            dFilt <- input$Filter
            dFilt
      })
      
      #---------------- Process Data -------------------

      #grab data and select data type, factor and GroupBy columns
      dataset <- reactive({
            ds <- read.csv("./data/SampleData.csv")
            ds <- ds[ds[input$FilterType]==eval(dFilt()),] #Filter
            dCols <- c(dType(), dFact(), dGroupBy()) #data type, factor and group by columns
            dataSubset <- subset(ds, select=dCols) #keep only columns of interest
            dataSubset
      })

      #comp.mean will take mean of meanX field using GroupBy columns
      comp.mean <- { function(df, GroupBy, meanX) { 
                  ddply(df, eval(GroupBy), function(d) c(DataTypeMean=mean(d[[meanX]])))
             }
      }

      processData <- reactive({
            ds <- data.frame(dataset()) #dataSubset
            
            allowedDifference <- as.numeric(input$Difference)/100

            #get mean/average by GroupBy (Job, YrsOfExp,,,) criteria only
            criteria <- c(dGroupBy())
            ds.factor.GrpAvg <- comp.mean(ds, criteria, dType())
            
            #get mean/average by Factor (ie. Gender) + GroupBy (ie Job) criteria
            criteria <- c(dFact(), dGroupBy()) #add Factor + GroupBy
            ds.factor.FactorAvg <- comp.mean(ds, criteria, dType())
            
            ds.factor.var <- merge(ds.factor.FactorAvg, ds.factor.GrpAvg, by=dGroupBy())
            #calc difference
            diffPct <- (ds.factor.var$DataTypeMean.x-ds.factor.var$DataTypeMean.y)/ds.factor.var$DataTypeMean.y
            ds.factor.var$Difference <- diffPct
            #filter out allowable Differences
            ds.factor.var <- filter(ds.factor.var, abs(diffPct)>allowedDifference)
      })
      
    
      detailDataset <- reactive({
            ds <- read.csv("./data/SampleData.csv")
            ds <- ds[ds[input$FilterType]==eval(dFilt()),] #Filter
            dCols <- c("EmployeeID", "ManagerID", dType(), dFact(), dGroupBy()) #data type, factor and group by columns
            dataSubset <- subset(ds, select=dCols) #keep only columns of interest
            dataSubset
      })
      
      #-------------------- Render Table ------------------------
      
      #Table Header
      tH <- function(ds) {

            tH <- NULL
            
            tH <- paste("Group = ", input$GroupBy)
            tH <- append(tH, input$FactorType)
            tH <- append(tH, paste(input$FactorType, " Avg ", input$DataType))
            tH <- append(tH, paste("Group Avg ", input$DataType))
            tH <- append(tH, paste("% Difference"))
            
            #tH <- colnames(ds) #use default column names
            tH
      }
      
      output$table <- renderDataTable({
            #alightRight is 0-based css class
            expr=dataOutputTable()
            }
            ,options = list(aoColumnDefs = list(list(sClass="alignRight",aTargets=c(list(-1),list(-2),list(-3)))) 
                            # ,initComplete = JS(
                            #       'function(table) {
                            #       table.on("click.dt", "tr", function() {
                            #       Shiny.onInputChange("rows", table.row( this ).index());
                            #       tabs = $(".tabbable .nav.nav-tabs li a");
                            #       $(tabs[1]).click();
                            #       });
                            #   }')
                            )
            ,escape = FALSE
            # Shiny.onInputChange('rows', table.row(this).index()); #will pass index to what is shown in Table
            ,callback = "function(table) {
                  table.on('click.dt', 'tr', function() {
                   Shiny.onInputChange('rows', table.row(this).data() ); 
                  tabs = $('.tabbable .nav.nav-tabs li a');
                  $(tabs[2]).click();
                  });}"
      )

      
      dataOutputTable <- reactive({      
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))
            validate(need(input$Filter != "", "\n\nPlease select a value for 'Filter by'"))
            
            if (input$goButton > 0) {
                  
                  ds.factor.var <- processData()
                  ds.factor.var[,1] <- paste("", ds.factor.var[,1], "")
                  ds.factor.var$Difference <- percent(ds.factor.var$Difference)
                  ds.factor.var$DataTypeMean.x <-dollar_format()(ds.factor.var$DataTypeMean.x)
                  ds.factor.var$DataTypeMean.y <-dollar_format()(ds.factor.var$DataTypeMean.y)
                        
                  # set friendlier table headers
                  colnames(ds.factor.var) <- tH(ds.factor.var)
                  #colnames(ds.factor.var) <- tH() #don't need to pass ds if we transform colnames
                  
                  #example convert URL mydata$url <- paste0("<a href='",mydata$url,"'>",mydata$url,"</a>")
                  ds.factor.var
            }
            
      })
      
      
      #---------------------- Render Plots -------------------------
      
      plot.range <- reactiveValues(x = NULL, y = NULL)
      
      output$plot <- renderPlot({
            
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))
            
            ds.factor.var <- processData()
            
            validate(need(nrow(ds.factor.var)>0, "\n\nNo data returned. Please make the appropriate adjustments."))
            
            ds.factor.var$Difference <- ds.factor.var$Difference*100
            maxY <- max(abs(ds.factor.var[,"Difference"]))+5 #to center around 0

            p <- ggplot(ds.factor.var, aes(Job, Difference,color=Difference))
            p <- p + geom_point(size=3)
            p <- p + scale_y_continuous(expand = c(0, 0), limits = c(-maxY,maxY))
            p <- p + scale_color_gradientn(colours = c("red","#DDAA11","#11BB11","#DDAA11","red"))
            p <- p + guides(color=guide_legend(title="% Difference"))
            p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
            p <- p + labs(title =" ", x = "", y = "% Difference") 
            p
      }, height=400)
      
      #table created by user dragging mouse over datapoints in plot
      output$click_info <- renderDataTable({
            
            ds.factor.var <- data.frame(processData())
            ds.factor.var$Difference <- round(ds.factor.var$Difference*100,2)
            
            #for ggplot xmin/xmax will be the x-axis grid line #
            #ymin/ymax will equal the y (% Difference) values
            validate(need(input$plot_brush!="", ">>>FOR MORE INFO CLICK AND DRAG MOUSE OVER DATA-POINTS IN GRAPH<<<"))
            brush <- input$plot_brush
            plot.range$x <- c(ceiling(brush$xmin):floor(brush$xmax))
            ymin <- brush$ymin
            ymax <- brush$ymax
            
            cat("Click and drag mouse over data-points for more information.\n")
            #ds.factor.var <- ds.factor.var[plot.range$x,] #useful only if unique Job per row
            #input$plot_brush #can be used for troubleshooting
            xAxisVal <- unique(ds.factor.var[,1]) #Job x-axis value
            xAxisVal <- xAxisVal[plot.range$x]
            ds.factor.var <- ds.factor.var[ds.factor.var$Job %in% xAxisVal,] 
            ds.factor.var <- subset(ds.factor.var, round(ds.factor.var$Difference,4)>as.numeric(ymin) & round(ds.factor.var$Difference,4)<as.numeric(ymax))
            
            if (nrow(ds.factor.var) == 0)
                  return() #return NULL
            else {
                  colnames(ds.factor.var) <- tH(ds.factor.var)
                  ds.factor.var
            }
         
      }, options = list(searching = FALSE,paging = FALSE))
      
      
      #------------------ Render Detail Table/Plot ---------------------
      
      output$detail2 <- renderPrint({"Click Output to Table & Detail Tabs button at bottom of control panel."})
      output$detail <- renderPrint({
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))

            detailColumn1 <- trimws(input$rows[1])
            
            #filter ds by Group and Split variables, don't use ds.factor.var as we need individual data, not grouped
            if(is.null(input$rows[1])){
                 c("Click on a row within the Table tab to show Detail records here.")
            }else{
                  ds <- detailDataset() #get basic dataset
                  ds <- ds[ds$Job==detailColumn1,]
                  ds <- ds[order(ds$EmployeeID),]
                  #validate(need(nrow(ds)>0, "\n\nNo data returned. Please make the appropriate adjustments."))
                  ds
            }
      })

      output$detailHist <- renderPlot({
            validate(need(input$GroupBy != "", "\n\nPlease select a value for 'Group people by'"))
            
            detailColumn1 <- trimws(input$rows[1])
            
            #filter ds by Group and Split variables, don't use ds.factor.var as we need individual data, not grouped
            if(is.null(input$rows[1])){
                  c("Click on a row within the Table tab to show Detail records here.")
            }else{
                  ds <- detailDataset() #get basic dataset
                  ds <- ds[ds$Job==detailColumn1,]
                  ds <- ds[order(ds$EmployeeID),]
                  #validate(need(nrow(ds)>0, "\n\nNo data returned. Please make the appropriate adjustments."))
                  col <- input$DataType
                  #histogram has a numeric error hist(ds$col)
            }
      })
      
      
      

      #---------------- Download Table and Plots -------------------
      
      # downloadHandler() takes two arguments both functions.The content function
      # is passed a filename and it should write out data to that filename.
      output$downloadData <- downloadHandler(
            
            # return a string which will be the filename
            filename = function() {
                  paste(input$DataType, input$downloadType, sep = ".") },

            # This function writes data to the file 
            content = function(file) {
                  
                  if(input$downloadType == "pdf") {
                        # Copy to a temporary directory in case we don't have write permissions
                        tempReport <- file.path(tempdir(), "report.Rmd")
                        file.copy("report.Rmd", tempReport, overwrite = TRUE)
                        
                        # Knit document, passing in `params` and eval it in a child 
                        # of the global environment to isolate document from app
                        params <- list(dataIn=processData())
                        
                        rmarkdown::render(tempReport, output_file = file,
                                          params = params,
                                          envir = new.env(parent = globalenv())
                        )
                  }
                  else {
                        sep <- switch(input$downloadType, "csv" = ",", "text" = "\t")
                        write.table(dataOutputTable(), file, sep = sep, row.names = FALSE) 
                  }            
                  
            }
      )
})
