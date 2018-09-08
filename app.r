library(shinydashboard)
library(quantmod)
library(xts)

linearyahoo<-function(){
  yahoo <- read.csv("./yahoo.csv",header = TRUE)
  x<-yahoo$Close
  y<-yahoo$Open
  res<-lm(x~y)
  a <- yahoo$Open[1]
  b <- yahoo$Open[2]
  e <- yahoo$Open[3]
  d <- data.frame(y=c(a,b,e)) 
  prediction <- predict(res,d)
  avgobj <- prediction-d
  avgobj1<-as.numeric(unlist(avgobj))
  finalavg<-mean(avgobj1)
  return(finalavg)
}

linearamazon <-function(){
  amazon <- read.csv("./amazon.csv",header = TRUE)
  x<-amazon$Close
  y<-amazon$Open
  res<-lm(x~y)

  a <- amazon$Open[1]
  b<-amazon$Open[2]
  e <- amazon$Open[3]
  d<-data.frame(y=c(a,b,e)) 
  prediction<-predict(res,d)
  avgobj<-prediction-d
  
  avgobj1<-as.numeric(unlist(avgobj))
  finalavg<-mean(avgobj1)
  return(finalavg)
}

linearapple <-function(){
  appl <- read.csv("./appl.csv",header = TRUE)
  x<-appl$Close
  y<-appl$Open
  res<-lm(x~y)
#   a<-as.integer(readline("appl:enter the first opening price"))
#   b<-as.integer(readline("enter the second opening price"))
#   e<-as.integer(readline(prompt = "enter the three opening price"))
  a <- appl$Open[1]
  b<- appl$Open[2]
  e <- appl$Open[3]
  d<-data.frame(y=c(a,b,e)) 
  prediction<-predict(res,d)
  avgobj<-prediction-d
  
  avgobj1<-as.numeric(unlist(avgobj))
  finalavg<-mean(avgobj1)
  return(finalavg)
}

lineargoogle <-function(){
  google <- read.csv("./google.csv",header = TRUE)
  x<-google$Close
  y<-google$Open
  res<-lm(x~y)
  a <- google$Open[1]
  b<-  google$Open[2]
  e <- google$Open[3]
  d<-data.frame(y=c(a,b,e)) 
  prediction<-predict(res,d)
  avgobj<-prediction-d
  
  avgobj1<-as.numeric(unlist(avgobj))
  finalavg<-mean(avgobj1)
  return(finalavg)
}

ui<-dashboardPage(
  dashboardHeader(title = "STOCKS RECOMMENDATION"),
 ###################################################################### 
  dashboardSidebar(
  ),
###########################################################################

  dashboardBody(
    tabsetPanel(
    tabPanel(title  = "charts",
            
      box(plotOutput("plot1")),
      box(plotOutput("plot2")),
      box(plotOutput("plot3")),
      box(plotOutput("plot4"))
      
),
tabPanel(title = "regression output",
         
         fluidRow(
           
           infoBoxOutput("linearregressionyahoo"),
           infoBoxOutput("linearregressiongoogle"),
           infoBoxOutput("linearregressionapple"),
           infoBoxOutput("linearregressionamazon")
         )
),
tabPanel(title="recommendations",
         fluidRow(
           
           infoBoxOutput("yahoosale"),
           infoBoxOutput("yahoopurchase"),
           infoBoxOutput("googlesale"),
           infoBoxOutput("googlepurchase"),
           infoBoxOutput("applesale"),
           infoBoxOutput("applepurchase"),
           infoBoxOutput("amazonsale"),
           infoBoxOutput("amazonpurchase")
           
         )
         
         )
         
)
))

server <- function(input,output){
  output$plot1<-renderPlot({
  sym<-("AAPL")
  getSymbols(sym)
  
   chart_Series(AAPL)
  })
  
  output$plot2<-renderPlot({
    sym<-("AMZN")
    getSymbols(sym)
    
    chart_Series(AMZN)
  })
  
  output$plot3<-renderPlot({
    sym<-("YHOO")
    getSymbols(sym)
    
    chart_Series(YHOO)
  })
  
  output$plot4<-renderPlot({
    sym<-("GOOG")
    getSymbols(sym)
    
    chart_Series(GOOG)
  })

  output$linearregressionyahoo <- renderInfoBox({
         infoBox(
           "Yahoo", linearyahoo(), icon = icon("list"),
           color = "purple"
         )
       })
  linearyahooobj  <- linearyahoo()
  if(linearyahooobj<0)
  {
    output$yahoosale <- renderInfoBox({
      infoBox(
        "sale yahoo stock",linearyahooobj*100,icon=icon("list"),
        color="red"
        
      )
      
    })
    
  }
  else{
    output$yahoopurchase <- renderInfoBox({
      infoBox(
        "purchase yahoo stock",linearyahooobj*100,
        color="green"
        
      )
      
    })
  }
    
  output$linearregressiongoogle <- renderInfoBox({
    infoBox(
      "Google", lineargoogle(), icon = icon("list"),
      color = "purple"
    )
  })
  lineargoogleobj <- lineargoogle()
  if(lineargoogleobj<0)
  {
    output$googlesale <- renderInfoBox({
      infoBox(
        "sale google stock",lineargoogleobj*100,icon=icon("list"),
        color="red"
        
      )
      
    })
    
  }
  else{
    output$googlepurchase <- renderInfoBox({
      infoBox(
        "purchase google stock",lineargoogleobj*100,
        color="green"
        
      )
      
    })
  }
  
  
  
  output$linearregressionapple <- renderInfoBox({
    infoBox(
      "Apple", linearapple(), icon = icon("list"),
      color = "purple"
    )
  })
  linearappleobj <- linearapple()
  if(linearappleobj<0)
  {
    output$applesale <- renderInfoBox({
      infoBox(
        "sale apple stock",linearappleobj*100,icon=icon("list"),
        color="red"
        
      )
      
    })
    
  }
  else{
    output$applepurchase <- renderInfoBox({
      infoBox(
        "purchase apple stock",linearappleobj*100,
        color="green"
        
      )
      
    })
  }
  
  output$linearregressionamazon <- renderInfoBox({
    infoBox(
      "Amazon", linearamazon(), icon = icon("list"),
      color = "purple"
    )
  })
  
  linearamazonobj <- linearamazon()
  if(linearamazonobj<0)
  {
    output$amazonsale <- renderInfoBox({
      infoBox(
        "sale amazon stock",linearamazonobj*100,icon=icon("list"),
        color="red"
        
      )
      
    })
    
  }
  else{
    output$amazonpurchase <- renderInfoBox({
      infoBox(
        "purchase amazon stock",linearamazonobj*100,
        color="green"
        
      )
      
    })
  }
}

shinyApp(ui,server)