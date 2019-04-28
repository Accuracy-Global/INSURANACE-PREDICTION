
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(shinydashboard)

Previous_Button=tags$div(actionButton("Prev_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-left fa-2x"></i></div>
                                                                  ')))
Next_Button=div(actionButton("Next_Tab",HTML('<div class="col-sm-4"><i class="fa fa-angle-double-right fa-2x"></i></div>')))


shinyServer(function(input, output,session) {
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
     div(column(1,offset=1,Previous_Button))
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
    
  })
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               }
  )
  
  
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               }
  )
  
  
  output$rajesh <-renderText({input$gender})
  output$time <- renderText({input$nationality})
  
  output$prediction <- renderPlot({ 
    
    # Filter data based on user input
    library(tidyverse) # metapackage with lots of helpful functions
    ## Importing packages
    library(data.table) # Import big files (CSV or Text) 
    library(readxl) # Import Excel files
    # user can specify specific sheet name to get data from that sheet.
    df <- readxl::read_excel('D:/Srinu_TuneProtect/experiment.xlsx')
    
    #including MASS library
    require(MASS)
    #dispalying First five records of the data set, DF
    head(df,5)
    #know the data type of Psg_Gender
    class(df$Psg_Gender)
    #know the data type of IC_Passport
    class(df$IC_Passport)
    #Eliminating all the characters in the IC_Passport
    for(i in 1: length(rownames(df)))
    {
      df$IC_Passport[i]<-gsub("[a-zA-Z]","",df$IC_Passport[i])
    }
    #Recheck the data type of IC_Passport
    class(df$IC_Passport)
    #converting char to numeric terms
    df$IC_Passport <- as.numeric(as.character(df$IC_Passport))
    class(df$IC_Passport)
    print(df$Nationality)
    #converting numerica terms
    df$TripDurationDays <- as.numeric(as.character(df$TripDurationDays))
    #output variable assinged to factor 
    df$Insured=factor(df$Insured)
    ####Taking the user input and stored into variable such as age1,trip1,connecting1.....gender1
    age1 <- as.numeric(input$age)
    trip1 <- as.numeric(input$tripdays)
    connecting1 <- input$connectingtype
    channel1 <- input$channel
    nationality1 <- input$nationality
    gender1 <- input$gender
    source1 <- input$source
    dest1 <- input$destination
    #pp= factor(df$IC_Passport)
    #df$IC_Passport = as.numeric(as.character(pp))
    
    #polr function is used to train the model nd apply the formula here
    m=polr(Insured ~  Source + Destination + Psg_Age + TripDurationDays + ConnectingFlightType + Channel + Psg_Gender + Nationality, data = df, Hess = TRUE)
    #Here mapping the user input with trained model
    n1 = data.frame(Source = source1, Destination = dest1, Psg_Age = age1, TripDurationDays = trip1, ConnectingFlightType = connecting1, Channel = channel1,Psg_Gender = gender1, Nationality = nationality1 )
    #finding the probabilites for user input and stored in x
    x = predict(m, n1, type="probs")
    #x[1] represents high probability and stored in a
    a <- x[1]
    #x[2] represents low probability and stored in b
    b <- x[2]
    #x[3] represents medium probility and stored in c
    c <- x[3]
    #print(result)
    #result = result * 100
    #result
    a <- a * 100
    b <- b * 100
    c <- c * 100
    # Rounding the values 
    a <- round(a)
    b <- round(b)
    c <- round(c)
    # Converting numeric to sting value 
    a <- toString(a)
    b <- toString(b)
    c <- toString(c)
    "
    #c <- round(c, digits = 1)
    if (a > b && a > c){
    result <- a
    }else if (b > c) {
    result <- b
    }else {
    result <- c
    }"
    
    #yy <- c(10,20,30,40,50,60,70,80,90,100)
    
    #Holding the final result into result variable 
    result <- c(a,b,c)
    result
    # Storing colors into cols varaible and used for future purposes
    cols = c("green", "red","yellow")
    #paste0("High: ",result[1],"% |"," Medium: ",result[3],"% |"," Low: ",result[2],"%")
    result <- as.numeric(as.character(result))
    
    # Plotting Bar Plot 
    xx <- barplot(result, xab="Percentages in %", ylab="Insured Probabilites", horiz=TRUE, xlim=c(0,100),col=cols,names.arg=c("High", "Low", "Medium"))
    
    #Labling the plot for user perspective
    text(y = xx, x = result, label = result, pos = 4, cex = 1, col = "black")
    
  }) #ends here 
  
  
  
})