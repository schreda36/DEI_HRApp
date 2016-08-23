#library(markdown)

shinyUI(navbarPage("Navbar!",
                   tabPanel("Factor",
                            sidebarLayout(
                                  sidebarPanel(
                                        selectInput("DataType", "Data to analyze:",
                                                     c("Salary"="Salary", "Total Compensation"="TotalCompensation")
                                        ),
                                        selectInput("FactorType", "Determining factor:",
                                                    c("Gender"="Gender", "Ethnicity"="Ethnicity",
                                                      "Religion"="Religion","Age"="Age","Factor X"="FactorX")
                                        ),
                                        checkboxGroupInput("GroupBy", "Group people by:",
                                                           c("Job Title" = "JobTitle",
                                                             "Years of Experience" = "YrsOfExperience",
                                                             "Years of School" = "YrsSchool",
                                                             "Prior Performance" = "Performance"
                                                           ),selected="JobTitle"),
                                        # sliderInput("variance", "Display differences > than:",
                                        #             min=-15.0,max=15.0,round=TRUE,
                                        #             step = .1, post="%",
                                        #             value=c(-10,10),dragRange=TRUE
                                        #             ),
                                        selectInput("FilterType", "Filter by:",
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
                                        actionButton("goButton", "Go!")
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