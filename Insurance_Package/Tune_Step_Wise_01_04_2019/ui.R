# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)




dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(box(width=12,
                    tabBox(width=12,id="tabBox_next_previous",
                           tabPanel("Step1:",selectInput("gender","Gender:",list("M","F"),),selectInput("age","Enter Your Age:",c(NaN,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100),""),selectInput("gender","Gender:",list("M","F"),"")),
                           tabPanel("Step2:",selectInput("channel","Channel",c("WEB","APIMOBILEAPP"),""),selectInput("source","Source:",c("India","Kenya","Canada","England","Srilanka","Dubai","Omen"),""), selectInput("destination","Destination",c(c("Kenya","Sudan","US","ENG","AUS")),"")),
                           tabPanel("Step3:",dateInput("date1", "Arrival Date:", value = "2019-01-01"),dateInput("date1", "Departure Date:", value = "2019-01-01")),
                           tabPanel("Step4:",selectInput("nationality","Nationality",c("Australia","Indonesian","German","Russian","Iran","Indian","Italian","Turkish","American","Japan","Greek","Kuwait","New Zealand","Pakistan","Bangladesh","Canada","Italy","Nepal"),""),selectInput("connectingtype","Connecting Type",c("Direct Flight","Connecting Flight","Normal"),""),selectInput("tripdays","Duration in Hours:",c(NaN,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),"")),
                           tags$script("
                                       $('body').mouseover(function() {
                                       list_tabs=[];
                                       $('#tabBox_next_previous li a').each(function(){
                                       list_tabs.push($(this).html())
                                       });
                                       Shiny.onInputChange('List_of_tab', list_tabs);})
                                       "
                           )
                    ),
                    uiOutput("Next_Previous"),
        
                    plotOutput("prediction")
                    ))
)