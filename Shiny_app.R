library(shiny)
library(DT)
library(forecast)
library(ggplot2)

ui<- shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      numericInput("C",strong("Column to Analysis"),value = 2),
      (strong("Time Series Setting")),
      numericInput("Start",strong("Start Period"),value = 1897),
      
      numericInput("freq",strong("Frequency"),value=1),
      (strong("Holdout Sample Setting")),
      numericInput("Starth",strong("Start Period"),value = 2015),
      numericInput("Endh",strong("End Period"),value = 2016)
    ),
    mainPanel(navbarPage(title = "Forecast Methods",
                         tabsetPanel(tabPanel("Summary",DT::dataTableOutput('contents')),
                                     tabPanel(("Naive"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plotnaive"),
                                                         tableOutput("accuracy_table"))
                                              )),
                                     tabPanel(("Simple Exponential Smoothing"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plotses"),
                                                         tableOutput("accuracy_tableofses")))),
                                     tabPanel(("Seasonal Naive"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plotsnaive"),
                                                         tableOutput("accuracy_tableofsnaive")))),
                                     tabPanel(("Holt"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plotholt"),
                                                         tableOutput("accuracy_tableofholt")))),
                                     tabPanel(("Arima"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plotarima"),
                                                         tableOutput("accuracy_tableofarima")))),
                                     tabPanel(("Holt-Winters"),
                                              tabsetPanel(
                                                tabPanel("Plot",plotOutput("plothw"),
                                                         tableOutput("accuracy_tableofhw"))))
                         )
                         
    )
    )
  )
))

server <- function(input,output,session){
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  })
  C<-reactive({input$C})
  Start<-reactive({input$Start})
  End<-reactive({input$End})
  Fre<-reactive({input$freq})
  Hstar<-reactive({input$Starth})
  Hend<-reactive({input$Endh})
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData())
  })
  output$plotnaive<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast0<-naive(InsampleTs,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast0)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast0$fit,colour = TRUE)
    
    
  })
  output$accuracy_table <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast0<-naive(InsampleTs,h=length(OutsampleTs))
    accuracy(forecast0,OutsampleTs)
  })
  output$plotholt<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-holt(InsampleTs,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast1)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast1$fit,colour = TRUE)
  })
  output$accuracy_tableofholt <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-holt(InsampleTs,h=length(OutsampleTs))
    accuracy(forecast1,OutsampleTs)
  })
  output$plotses<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-ses(InsampleTs,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast1)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast1$fit,colour = TRUE)
    
  })
  output$accuracy_tableofses <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-ses(InsampleTs,h=length(OutsampleTs))
    accuracy(forecast1,OutsampleTs)
  })
  output$plothw<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-hw(InsampleTs,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast1)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast1$fit,colour = TRUE)
    
  })
  output$accuracy_tableofhw <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-hw(InsampleTs,h=length(OutsampleTs))
    accuracy(forecast1,OutsampleTs)
  })
  output$plotsnaive<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-snaive(InsampleTs,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast1)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast1$fit,colour = TRUE)
    
  })
  output$accuracy_tableofsnaive <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast1<-snaive(InsampleTs,h=length(OutsampleTs))
    accuracy(forecast1,OutsampleTs)
  })
  output$plotarima<- renderPlot({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    fit<-auto.arima(InsampleTs, max.p = 5, max.q = 5, max.P = 5, max.Q = 5,
                    max.d = 5, max.D = 5, seasonal=F)
    forecast1 = forecast(fit,h=length(OutsampleTs))
    autoplot(TotalTS)+autolayer(forecast1)+autolayer(OutsampleTs,colour = TRUE)+autolayer(forecast1$fit,colour = TRUE)
    
  })
  output$accuracy_tableofarima <- renderTable({
    TotalTS<-ts(myData()[,C()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    fit<-auto.arima(InsampleTs, max.p = 5, max.q = 5, max.P = 5, max.Q = 5,
                    max.d = 5, max.D = 5, seasonal=F)
    forecast1<-forecast(fit,h=length(OutsampleTs))
    accuracy(forecast1,OutsampleTs)
  })
  
}
shinyApp(ui,server)
