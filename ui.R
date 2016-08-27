#library(markdown)

shinyUI(
      navbarPage("=Opportunity!",
                 
                   tabPanel("Compare Comp",
                            tags$head(
                                  tags$style(HTML("
                                          
                                          h1 {  font-family: 'Calibri', cursive;
                                                font-weight: 500;
                                                line-height: 1.1;
                                                color: #48ca3b; }
                                                  
                                          body { background-color: #fff; }

                                          .selectize-control { margin-top: 1px }
                                          .selectize-input { font-size: 13px; }
                                          .selectize-control.label { font-size: 20px; }
                                          .selectize-dropdown-content { font-size: 13px; }
                                          #sidebar {background-color: #E1ECF7; align: center;}
                                    "))
                            ),
                            sidebarLayout(
                                  sidebarPanel(id="sidebar",
                                        selectizeInput("DataType", "Data to analyze:",
                                                     c("Total Compensation"="TotalCompensation",
                                                       "Salary"="Salary", 
                                                       "Bonus"="Bonus",
                                                       "LTI"="LTI",
                                                       "Data X"="DataX"
                                                       ),selected="Salary"
                                        ),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        selectizeInput("FactorType", "Split data by:",
                                                    c("Age"="Age Group",
                                                      "Disability"="Disability", 
                                                      "Ethnicity"="Ethnicity",
                                                      "Gender"="Gender", 
                                                      "Religion"="Religion",
                                                      "Factor X"="FactorX"
                                                      ),selected="Gender"
                                        ),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        checkboxGroupInput("GroupBy", "Group people by:",
                                                           c("Job" = "Job",
                                                             "Years of Experience" = "YrsOfExperience",
                                                             "Years of School" = "YrsOfSchool",
                                                             "Prior Performance" = "PriorPerformance",
                                                             "Group X" = "Group X"
                                                             ),selected="Job"),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        selectizeInput("FilterType", "Filter by:",
                                                    c("Country" = "Country",
                                                      "Location" = "Location",
                                                      "Organization" = "Organization",
                                                      "Manager" = "Manager"),selected="Country"
                                        ),
                                        selectizeInput("Filter", NULL, choices = "All"),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        sliderInput("variance", "Display differences > than +/-:",
                                                    min=2.5,max=20.0,round=TRUE,
                                                    step = .5, post="%",
                                                    value=10, width=200
                                        ),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        actionButton("goButton", "Go!", style="align: center"),
                                        tags$hr(style="height: 1px; margin: 5px; border-color: #bbbbbb;"),
                                        radioButtons("downloadType", "Download file type:",
                                                    choices = c("csv", "pdf", "tsv")),
                                        downloadButton('downloadData', 'Download')
                                  ,width=3),
                                  mainPanel(
                                        tabsetPanel(type = "tabs", 
                                                    tabPanel("Table", dataTableOutput("table")),
                                                    #tabPanel("Table", textOutput("table")),
                                                    tabPanel("Summary", verbatimTextOutput("summary")), 
                                                    tabPanel("Plot", 
                                                             fluidRow(column(width = 12,
                                                                             plotOutput("plot", click = "plot_click", brush=brushOpts(id="plot_brush",resetOnNew=F))
                                                             )),
                                                             fluidRow(column(width = 12,
                                                                             verbatimTextOutput("click_info")
                                                             ))
                                                    )
                                                    
                                        )
                                  ,width=9)
                            )
                   )
                   # ,tabPanel("Summary",
                   #         verbatimTextOutput("summary")
                   # )
                   # ,navbarMenu("More",
                   #            tabPanel("Table",
                   #                     verbatimTextOutput("summary")
                   #            ),
                   #            tabPanel("About",
                   #                     fluidRow(
                   #                           column(6,
                   #                                  verbatimTextOutput("summary")
                   #                                  #includeMarkdown("about.md")
                   #                           )
                   #                           ,
                   #                           column(3,
                   #                                  img(class="img-polaroid",
                   #                                      src=paste0("http://www.vayam-llc.com/wp-content/themes/Vayam%20LLC/images/vayam-logo-small.png")),
                   #                                  tags$small(
                   #                                        "Copyright 2016 ",
                   #                                        "Vayam LLC ",
                   #                                        a(href="http://www.vayam-llc.com")
                   #                                  )
                   #                           )
                   #                     )
                   #            )
                   # )
                 
))