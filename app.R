#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(LiblineaR)
library(readr)
library(tidyverse)
library(ggplot2)
library(dcurves)
library(gtsummary); library(dplyr); library(tidyr)
library(shinyjs)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel(tags$a(href='https://www.northerncarealliance.nhs.uk/', 
                      tags$img(src="NCA.png",width = "25%", height = "25%"))),
    titlePanel(title = "CHORUS Score Calculator"), h6(div(HTML("<em>\nSelect the model parameters below to calculate the CHORUS score:\n\n</em>"))),

    useShinyjs(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width =4,
          div(id = "form",
          helpText("Chronic subdural HematOma Referral oUtcome prediction using Statistics (CHORUS) score"), 
          helpText("This score predicts the probability of acceptance of a chronic subdural hematoma referral. The CHORUS score was developed on 1000 patients with an excellent accuracy of 93.88%, sensitivity of 95.98% and an AUROC of 0.9771."),
          selectInput("age", label = "Age", choices = list("< 85" = 0, ">= 85" = -1), selected = "< 85"),
          selectInput("headache", label = "Headache", choices = list("No" = 0, "Yes" = 1), selected = "No"),
          selectInput("dementia", label = "Dementia", choices = list("No" = 0, "Yes" = -1), selected = "No"),
          selectInput("motor_weakness", label = "Motor Weakness", choices = list("No" = 0, "Yes" = 1), selected = "No"),
          selectInput("midline_shift", label = "Midline Shift", choices = list("No" = 0, "Yes" = 1), selected = "No"),
          selectInput("CSDHsize", label = "Size of CSDH", choices = list("Small" = -3, "Medium" = 0, "Large" = 3), selected = "Small"),
          selectInput("QoL", label = "Pre-morbid QoL", choices = list("Reasonable" = 0, "Poor" = -4), selected = "Reasonable")
          )),

        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("score"),
          htmlOutput("probability"),
          htmlOutput("prediction")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    calculateChorus <- function (age, headache, dementia, motor_weakness, midline_shift, CSDHsize, QoL) {
      u <- age + headache + dementia + motor_weakness + midline_shift + CSDHsize + QoL
    }
    
    calculateProb <- function (age, headache, dementia, motor_weakness, midline_shift, CSDHsize, QoL){
      v <- age + headache + dementia + motor_weakness + midline_shift + CSDHsize + QoL
      prob <- 1 / (1 + exp(1.468 - 1.028*v))
    }
    
    calculatePred <- function (probability){
      if (probability > 0.5){
        print("Accept")
      }else{print("Reject")}
    }
    
    output$score <- renderText({
      
      score <- calculateChorus(
        as.numeric(input$age),
        as.numeric(input$headache),
        as.numeric(input$dementia),
        as.numeric(input$motor_weakness),
        as.numeric(input$midline_shift),
        as.numeric(input$CSDHsize),
        as.numeric(input$QoL)
      )
      
      score <- round(score,digits = 2)
      
      paste("<div class='alert alert-info'>
            <h4>Total CHORUS score:", score, "</h4>
            </div>")
      
    }) 
    
    output$probability <- renderText({
      probability <- calculateProb(
        as.numeric(input$age),
        as.numeric(input$headache),
        as.numeric(input$dementia),
        as.numeric(input$motor_weakness),
        as.numeric(input$midline_shift),
        as.numeric(input$CSDHsize),
        as.numeric(input$QoL)
      )
      
      prob <- round(probability,digits = 4)
      
      paste("<div class='alert alert-info'>
            <h4>P (Acceptance):", prob, "</h4>
            </div>")
      
    })
    
    output$prediction <- renderText({
      probability <- calculateProb(
        as.numeric(input$age),
        as.numeric(input$headache),
        as.numeric(input$dementia),
        as.numeric(input$motor_weakness),
        as.numeric(input$midline_shift),
        as.numeric(input$CSDHsize),
        as.numeric(input$QoL)
      )
      
      prob <- round(probability,digits = 2)
      
      pred <- calculatePred(prob)
      
      paste("<div class='alert alert-info'>
            <h4>Prediction:", pred, "</h4>
            </div>")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
