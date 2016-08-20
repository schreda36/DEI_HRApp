library(markdown)

shinyUI(navbarPage("Navbar!",
                   tabPanel("Factor",
                            sidebarLayout(
                                  sidebarPanel(
                                        radioButtons("DataType", "Data to Analyze:",
                                                     c("Salary"="Salary", "Total Compensation"="TotalCompensation")
                                        ),
                                        checkboxGroupInput("criteria", "Data Criteria:",
                                                           c("Job Title" = "JobTitle",
                                                             "Years of Experience" = "YrsofExperience",
                                                             "Years of School" = "YrsSchool",
                                                             "Prior Performance" = "Performance",
                                                             "Country" = "Country",
                                                             "Location" = "Location",
                                                             "Organization" = "Organization",
                                                             "Manager" = "Manager"
                                                           ),selected="JobTitle"),
                                        radioButtons("FactorType", "Factor:",
                                                     c("Gender"="Gender", "Ethnicity"="e")
                                        ),
                                        selectInput("variance", "Display Difference > than:",
                                                    c("+/-10%" = ".1",
                                                      "+/-5%" = ".05",
                                                      "+/-2.5%" = ".025"
                                                    )),
                                        actionButton("goButton", "Go!")
                                  ),
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
                                  )
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