options(warn=0)
ui <- shinyUI(fluidPage(
  # Application title
  h1("Tune Project", align = "center"),
  fluidRow(
 
    column(width=3,
           fluidRow( style = "height:445px; width: 350px;border-style: groove; background-color: #85C1E9; padding-left: 10px; padding-top: 25px; margin-left: 20px;", actionButton("goButton", "STEP1",color = "primary"),
                    
                    selectInput("age","Enter Your Age:",c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),""), #taking input as User age
                    selectInput("gender","Gender:",list("M","F"),""), # Input as M or F
                    selectInput("tripdays","Duration in Hours:",c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),""),
                    selectInput("source","Source:",c("India","Kenya","Canada","England","Srilanka","Dubai","Omen"),""),
                    dateInput("date1", "Arrival Date:", value = "2019-01-01"))),
    #column(width = 1),
    column(width = 3,
           fluidRow(style = "height:445px; width: 350px;padding-left: 10px; padding-top: 25px; margin-left: 45px; border-style: groove;background-color: #85C1E9;",actionButton("goButton", "STEP2"),
                    selectInput("channel","Channel",c("WEB","APIMOBILEAPP"),""), # Type of booking
                    selectInput("nationality","Nationality",c("Australia","Indonesian","German","Russian","Iran","Indian","Italian","Turkish","American","Japan","Greek","Kuwait","New Zealand","Pakistan","Bangladesh","Canada","Italy","Nepal"),""),
                    selectInput("connectingtype","Connecting Type",c("Direct Flight","Connecting Flight","Normal"),""),
                    selectInput("destination","Destination:",c("Kenya","Sudan","US","ENG","AUS"),""),
                    dateInput("date1", "Departure Date:", value = "2019-01-01")), # Booking flight type
                 
           fluidRow(style = "height:300px; padding-left: 20px; ",submitButton("submit"))),
    column(width = 1),
    column(width = 4,
           fluidRow( style = "height: 445px; background-color: white; margin-left: 0px; border-style: groove;",h4("Insured Probabilities:", align = "center"),
                    p(plotOutput("prediction"),style = "color:blue"), # Displaying Output as Bar Chart (High, Low, Medium)
                    h4(textOutput(("number")),align = "center"))))
))

############### Server Logic Starts Here##############################
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Falowing code for displaying Output as Barchart
   
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
    #destination
    
    

    #pp= factor(df$IC_Passport)
    #df$IC_Passport = as.numeric(as.character(pp))
    
    #polr function is used to train the model nd apply the formula here
    m=polr(Insured ~  Source+ Destination+ Psg_Age + TripDurationDays + ConnectingFlightType + Channel + Psg_Gender + Nationality, data = df, Hess = TRUE)
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
  
  
    # Used to find the proababilies and stored in variable RESULT
    output$number <- renderText({
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
      m=polr(Insured ~ Source + Destination + Psg_Age + TripDurationDays + ConnectingFlightType + Channel + Psg_Gender + Nationality, data = df, Hess = TRUE)
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

      #Holding a,b and c results into another varaiable result
      result <- c(a,b,c)
      result
      # Paste the value on User Interface 
      paste0("High: ",result[1],"% |"," Low: ",result[2],"% |"," Medium: ",result[3],"%")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

