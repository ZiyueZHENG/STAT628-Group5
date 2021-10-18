#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define ui

ui <- fluidPage(
    ui <- fluidPage(
        
        titlePanel("Body Index"),
        
        sidebarPanel(
            numericInput("AGE", "AGE(Years)", value = NA),
            numericInput("ADI", "ADIPOSITY(kg/cm^2)", value = NA),
            numericInput("HEI", "HEIGHT(inches)", value = NA),
            numericInput("WEI", "WEIGHT(lbs)", value = NA),
            numericInput("ABD", "ABDOMEN Circumference(cm)", value = NA),
            numericInput("NECK", "NECK Circumference(cm)", value = NA)
        ),
        
        mainPanel(
            h1("BodyFat Calculater"),
            h2("Contact: ydu76@wisc.edu"),
            p("This is an app to calculate your body fat percentage. 
              It runs in real-time and serves several methods. 
              Type in the values of your physical index and get your body fat percentage immediatelly. 
              If you have any questions, please contanct us via email."),
            h4("For BMI approach, Age and Adiposity are required."),
            textOutput("Ans1"),
            h4("For YMCA approach, Adiposity and Weight are required."),
            textOutput("Ans2"),
            h4("For Navy approach, Height, weight and Neck Circumference are required."),
            textOutput("Ans3"),
            h4("For AWN approach, Weight, Neck Circumference and Abdomen Circumference are required."),
            textOutput("Ans4")
        )
    )
)

# Define server logic

server <- function(input, output, session) {
    
    output$Ans1 <- renderText(
        if (is.na(input$AGE) | is.na(input$ADI)) {
            "Warning: BMI Approach missing data value."
        } else if (input$AGE <= 0 | input$ADI <= 0) {
            "Warning: Body Index of BMI Approach should be positive."
        } else {
            y = 1.46619 * input$ADI + 0.12430 * input$AGE - 23.87057
            paste("BMI Approach BodyFat =", y, sep = " ")
        }
    )
    
    output$Ans2  <- renderText(
        if (is.na(input$ADI) | is.na(input$WEI)) {
            "Warning: YMCA Approach missing data value."
        } else if (input$ADI <= 0 | input$WEI <= 0) {
            "Warning: Body Index of YMCA Approach should be positive."
        } else {
            y = 2.795956 * input$ADI - 0.003195 * input$ADI/input$WEI - 37.271772
            paste("YMCA Approach BodyFat =", y, sep = " ")
        }
    )
    
    output$Ans3  <- renderText(
        if (is.na(input$WEI) | is.na(input$HEI) | is.na(input$NECK)) {
            "Warning: Navy Approach missing data value."
        } else if (input$HEI <= 0  | input$WEI <= 0 | input$NECK <= 0) {
            "Warning: Body Index of Navy Approach should be positive."
        } else {
            y = 39.752 * log(input$ABD - input$NECK) - 13.710 * log(input$HEI) - 81.223
            paste("Navy Approach BodyFat =", y, sep = " ")
        }
    )
    
    output$Ans4  <- renderText(
        if (is.na(input$WEI) | is.na(input$NECK) | is.na(input$ABD)) {
            "Warning: AWN Approach missing data value."
        } else if (input$WEI <= 0 | input$NECK <= 0 | input$ABD <= 0) {
            "Warning: Body Index of AWN Approach should be positive."
        } else {
            y = 0.91839 * input$ABD - 0.40640 * input$NECK - 0.11346 * input$WEI - 30.26411
            paste("AWN Approach BodyFat =", y, sep = " ")
        }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
