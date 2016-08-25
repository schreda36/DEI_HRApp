#library(markdown)

shinyUI(
      navbarPage("Navbar!",
                 
                   tabPanel("Factor",
                            
                            tags$head(
                                  tags$style(HTML("
                                          
                                          h1 {  font-family: 'Calibri', cursive;
                                                font-weight: 500;
                                                line-height: 1.1;
                                                color: #48ca3b; }
                                                  
                                          body { background-color: #fff; }

                                          .selectize-control { margin-top: 1px }
                                          .selectize-input { font-size: 14px; }
                                          .selectize-control.label { font-size: 22px; }
                                          #.selectize-dropdown-content {max-height: 400px; }
                                          #.selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}
                                                  
                                    "))
                            ),
                            sidebarLayout(
                                  sidebarPanel(
                                        selectizeInput("DataType", "Data to analyze:",
                                                     c("Salary"="Salary", "Total Compensation"="TotalCompensation")
                                        ),
                                        selectizeInput("FactorType", "Determining factor:",
                                                    c("Gender"="Gender", "Ethnicity"="Ethnicity",
                                                      "Religion"="Religion","Age"="Age","Factor X"="FactorX")
                                        ),
                                        checkboxGroupInput("GroupBy", "Group people by:",
                                                           c("Job Title" = "JobTitle",
                                                             "Years of Experience" = "YrsOfExperience",
                                                             "Years of School" = "YrsSchool",
                                                             "Prior Performance" = "Performance"
                                                           ),selected="JobTitle"),
                                        selectizeInput("FilterType", "Filter by:",
                                                    c("Country" = "Country",
                                                      "Location" = "Location",
                                                      "Organization" = "Organization",
                                                      "Manager" = "Manager")
                                        ),
                                        selectizeInput("Filter", "", choices = "All"),
                                        sliderInput("variance", "Display differences > than +/-:",
                                                    min=2.5,max=20.0,round=TRUE,
                                                    step = .5, post="%",
                                                    value=10, width=200
                                        ),
                                        actionButton("goButton", "Go!"),
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
                   #          verbatimTextOutput("summary")
                   # ),
                   # navbarMenu("More",
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