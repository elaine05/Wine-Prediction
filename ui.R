library(shiny)
library(shinydashboard)
library(rmarkdown) 


ui <- dashboardPage(
  dashboardHeader(title = 'Wine-Quality Analysis and Prediction'),
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(id='sidebarmenu',
                menuItem("Dashboard", tabName="dashboard", icon = icon("database")),
                menuItem("Analysis", tabName="analysis", icon = icon("calendar"),
                         menuSubItem("Exploratory Data", tabName = "exdata"),
                         menuSubItem("Prediction with GLM and NB",tabName = "glm")
                ),
                menuItem("Raw Data", tabName="rawdata")
                
    ),
    conditionalPanel("input.sidebarmenu == 'exdata'",
                     fluidRow(
                       sliderInput("qualitySlider", label = h5("Range of Wine's Quality"), min = 3, 
                                   max = 9, value = c(4,9), width = '100%'),
                       checkboxGroupInput("variable", "Variable",c("Fixed Acid" = "fa",
                                                                   "Volatile Acid" = "va",
                                                                   "Citric Acid " = "ca",
                                                                   "Residual Sugar"="rs",
                                                                   "Chlorides"= "cl",
                                                                   "Free Sulfur Dioxide" = "fsd",
                                                                   "Total Sulfur Dioxide"="tsd",
                                                                   "Density"= "ds",
                                                                   "pH" = "ph", "Sulphates" = "sh",
                                                                   "Alcohol"="al", "Quality"="ql"
                                                                   ), 
                                          selected = c("Alcohol" = "al", "Density" = "ds")) 
                       
                       
                       
                     )
    ),
    conditionalPanel("input.sidebarmenu == 'glm'",
                     fluidRow(checkboxGroupInput("glmvar", "Variable (Note: the selected variable will not be used during training)",
                       c("Fixed Acid" = "fa",
                                                "Volatile Acid" = "va",
                                                "Citric Acid " = "ca",
                                                "Residual Sugar"="rs",
                                                "Chlorides"= "cl",
                                                "Free Sulfur Dioxide" = "fsd",
                                                "Total Sulfur Dioxide"="tsd",
                                                "Density"= "ds",
                                                "pH" = "ph", "Sulphates" = "sh",
                                                "Alcohol"="al", "Quality"="ql"
                       ), selected = c("Alcohol" = "al", "Density" = "ds")
      
    
                     ))
    )#end cond panel glm
                     
      
  ),
  
  # body
  dashboardBody(
    
    uiOutput("title"),
    tabItems(
      tabItem(tabName = "exdata",
              uiOutput("qdb"),
              #quality histo
              plotOutput("qh"),
              
              #var distribution
              uiOutput("vardb"),
              plotOutput("varBP"),
              uiOutput("st1"),
              #title correlation quality and others
              uiOutput("corq"),
              
              conditionalPanel("input.sidebarmenu == 'exdata'",
                               fluidRow(
                                 radioButtons("gr", "Group Correlation with Quality:",
                                              c("Acidity" = "ac",
                                                "Density" = "ds",
                                                "pH" = "ph",
                                                "Dioxide-Sulphates-Chlorides" = "dsc"), 
                                              width = "100%",inline=TRUE)
                               )
                               
              ),
              #plot correlation
              plotOutput("crp"),
              uiOutput("st2"),
              plotOutput("alDs")
              
              ),
      
    
      tabItem(tabName = "glm",
              
              uiOutput("glm1"),
              uiOutput("glm2"),
              conditionalPanel("input.sidebarmenu == 'glm'",
                                fluidRow(
                                  column(6,
                                  numericInput("fa", "Fixed Acid", 0, min=0),
                                  numericInput("va", "Volatile Acid", 0, min=0),
                                  numericInput("ca", "Citric Acid", 0, min=0),
                                  numericInput("rs", "Residual Sugar", 0, min=0),
                                  numericInput("cl", "Chlorides", 0, min=0),
                                  numericInput("fsd", "Free Sulfur Dioxide", 0, min=0),
                                  numericInput("tsd", "Total Sulfur Dioxide", 0, min=0),
                                  numericInput("ds", "Density", 0, min=0),
                                  numericInput("ph", "pH", 0, min=0),
                                  numericInput("sh", "Sulphates", 0, min=0),
                                  numericInput("al", "Alcohol", 0, min=0)
                                                                                 
                                ),
                                
                                column(6,  dataTableOutput("showinput"))
                                
                                ),
                               fluidRow(actionButton('predict', 'Predict'), radioButtons("mod", "Predict Using:",
                                                                                         c("GLM" = "dm",
                                                                                           "Naive Bayes" = "nb"))
                              ) , uiOutput("pred")             
                               
             
                               
              ) 
              
    
      )
    ),# end tab glm
    
    tabItem(tabName = "rawdata",
            
            conditionalPanel("input.sidebarmenu == 'rawdata'",
                             
                             fluidRow(column(12,radioButtons("whichdf", "Raw data used for:",
                                                             c("GLM" = "glmmod",
                                                               "NB" = "nbmod"),inline=TRUE)
                             )),   
                             fluidRow(column(12, dataTableOutput('rawdat'))) 
                             )     
              
      ) 
    )

)