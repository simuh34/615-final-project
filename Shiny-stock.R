library(shiny)
library(DT)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(bizdays)
library(magrittr)
library(tidyquant)



index_dow <- tq_index("DOWGLOBAL")
select_name <- c("DAI-DE" , "MC-FR" , "6752-JP" , "PFE" , "ABI-BE")
select_stock <- index_dow %>% filter(symbol %in% select_name)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

df_6752 <- index_dow %>% filter(symbol %in% "6752-JP")

# Downloading Panasonic corp price using quantmod
a <- getSymbols("PCRFF", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)  

chartSeries(to.daily(PCRFF), theme=chartTheme('white'))



df_DAI <- index_dow %>% filter(symbol %in% "DAI-DE")

a <- getSymbols("DDAIF", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)  
chartSeries(to.daily(DDAIF), theme=chartTheme('white'))



df_MC <- index_dow %>% filter(symbol %in% "MC-FR")

# Downloading LVMH Moet Hennessy Louis Vuitton SE price using quantmod
a <- getSymbols("LVMHF", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)

chartSeries(to.daily(LVMHF), theme=chartTheme('white'))




df_ABI <- index_dow %>% filter(symbol %in% "ABI-BE")

a <- getSymbols("BUDFF", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)

chartSeries(to.daily(BUDFF), theme=chartTheme('white'))



df_PFE <- index_dow %>% filter(symbol %in% "PFE")

# Downloading Pfizer Inc. price using quantmod
a <- getSymbols("PFE", from = '2020-07-01',
                to = "2020-12-01",warnings = FALSE,
                auto.assign = TRUE)  
chartSeries(to.daily(PFE), theme=chartTheme('white'))




dt_6752 <-  getSymbols("PCRFF", from = '2020-07-01',
                           to = "2020-12-01",warnings = FALSE,
                           auto.assign = FALSE) 

dt_DAI <-  getSymbols("DDAIF", from = '2020-07-01',
                          to = "2020-12-01",warnings = FALSE,
                          auto.assign = FALSE)  

dt_MC <- getSymbols("LVMHF", from = '2020-07-01',
                        to = "2020-12-01",warnings = FALSE,
                        auto.assign = FALSE) 

dt_ABI <- getSymbols("BUDFF", from = '2020-07-01',
                      to = "2020-12-01",warnings = FALSE,
                      auto.assign = FALSE)

dt_PFE <-  getSymbols("PFE", from = '2020-07-01',
                          to = "2020-12-01",warnings = FALSE,
                          auto.assign = FALSE)




##############################

header <- dashboardHeader(title="Investment")
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "sidebarmenu",
    menuItem("DOWGLOBAL", tabName="DB"),
    menuItem("Selected stocks", tabName = "select"),
    menuItem("Detailed Information", tabName = "detail"),
    menuItem("Earnings", tabName = "search1")
  )
)

body <- dashboardBody(
  
  tabItems(
    tabItem("DB",NULL,
            fluidRow(
              DT::dataTableOutput("table1")
            )),
    
    
    tabItem("select",NULL,
            fluidRow(
              DT::dataTableOutput("table2"),
              textOutput("text0"),
              tags$head(tags$style("#text0{color: steelblue;
                                 font-size: 20px;
                                 font-style: italic;
                                 
                                 }"
              )
              )
            )),
    
    tabItem("detail",NULL,
            fluidRow(
              selectInput("selected",label ="Choose the symbol of the stock:",
                          choices = sort(c(unique(as.character(select_stock$symbol))))), 
              
              box(textOutput("text1"),
                  tags$head(tags$style("#text1{color: steelblue;
                                 font-size: 20px;
                            
                                 
                                 }"
                  )
                  ),
                  DT::dataTableOutput("table4"), width = 15 ),
              
              box(DT::dataTableOutput("table3"), width = 15)
              
              
              
            )),
    
    tabItem("search1",NULL,
            fluidRow(
              selectInput("selected_e",
                          label ="Choose the symbol of the stock:",
                          
                          choices = sort(c(unique(as.character(select_stock$symbol))))),
              
              
              box(textOutput("text2"),
                  tags$head(tags$style("#text2{color: blue;
                                 font-size: 20px;
                            
                                 
                                 }"
                  )),
                  plotOutput("distPlot2"), width = 15),
              
             
            ))
  )
)



ui=dashboardPage(header, sidebar, body)	


server <- function(input, output){
  
  
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    index_dow
  }))
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    select_stock
  }))
  
  
  
  
  
  
  output$distPlot3 <- renderPlot({
    plotinput()
  })
  output$text3 <- renderText ({"This is the financial time series chart of the stock we choose."})
  
  
  dataset1<- reactive({
    if (input$selected== "ABI-BE"){
      df_ABI
    }else if(input$selected== "DAI-DE"){
      df_DAI
    }else if(input$selected== "MC-FR"){
      df_MC
    }else if(input$selected=="6752-JP"){
      df_6752
    }else if(input$selected== "PFE"){
      df_PFE
    }
  })
  
  output$table3 <- renderDataTable({
    datatable(dataset1())
  })
  
  
  dataset4<- reactive({
    if (input$selected== "ABI-BE"){
      dt_ABI
    }else if(input$selected== "DAI-DE"){
      dt_DAI
    }else if(input$selected== "MC-FR"){
      dt_MC
    }else if(input$selected=="6752-JP"){
      dt_6752
    }else if(input$selected== "PFE"){
      dt_PFE
    }
  })
  
  output$table4 <- renderDataTable({
    datatable(dataset4())
  })
  
  
  plotinput2 <- reactive({
    switch(input$selected_e,
           
           "ABI-BE" =  chartSeries(to.daily(BUDFF), theme=chartTheme('white')),
           
           "DAI-DE" = chartSeries(to.daily(DDAIF), theme=chartTheme('white')),
           
           "MC-FR" = chartSeries(to.daily(LVMHF), theme=chartTheme('white')),
           
           "6752-JP" = chartSeries(to.daily(PCRFF), theme=chartTheme('white')),
           
           "PFE" = chartSeries(to.daily(PFE), theme=chartTheme('white')),
           
    ) })
  
  output$distPlot2 <- renderPlot({
    plotinput2()
  })
  
  
  
  output$text2 <- renderText ({" Here's how the stock traded in the second half of the year after we bought it. "})
  
  output$text0 <- renderText ({" Based on the online investment advices, we picked some stocks in airlines, automobiles, manufacturing, and retail."})
  
 
  
}
shinyApp(ui = ui, server = server)
