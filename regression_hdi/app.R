

library(shiny)
library(bslib)
suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(broom)
library(reactable)


setwd("G:/My Drive/03_research_and_development/01_research_topics/paper_0_CRD_main/lesson_plans/crd_paper_lessons/regression_hdi")
## Bring in data
hdi_df = read_xlsx('hdi_2015.xlsx', sheet = 'raw_data') 
cn_df = read_xlsx('hdi_2015.xlsx', sheet = 'variables')

## Change column names
colnames(hdi_df) = cn_df$abbreviations
## Clean up data
hdi_df = hdi_df %>% 
  dplyr::select(-contains('HDI'))


imp_var_df = cn_df %>% 
  filter(is.na(ignore_variables ))

## Get Variable Names
real_var_names = imp_var_df$Variable
abbr_var_names = imp_var_df$abbreviations
real_var_names_list = as.list(abbr_var_names)
names(real_var_names_list) <- real_var_names

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Human Development Index Activity"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons(inputId = 'type_analysis', 
                   label = 'Type of Analysis:',
                   choices = list('Simple Linear Regression' = 'SLR',
                                  'Multiple Linear Regression' = 'MLR'),
                   selected = 'SLR'),
      
      ## For Simple Linear Regression
      conditionalPanel(
        
        condition = 'input.type_analysis == "SLR"',
        
        selectInput(inputId = 'explanatory_variable_slr',
                    label =  'Explanatory Variable: ',
                    choices = real_var_names_list
        ),
        
        selectInput(inputId = 'response_variable_slr',
                    label =  'Response Variable: ',
                    choices = real_var_names_list),
      ),
      
      ## For Multiple Linear Regression
      conditionalPanel(
        condition = 'input.type_analysis == "MLR"',
        selectInput(inputId = 'explanatory_variable_1_mlr',
                    label =  'Explanatory Variable 1: ',
                    choices = real_var_names_list
        ),
        
        selectInput(inputId = 'explanatory_variable_2_mlr',
                    label =  'Explanatory Variable 2: ',
                    choices = real_var_names_list
        ),
        
        
        selectInput('response_variable_mlr',
                    'Response Variable: ',
                    real_var_names_list),
      ),
      
      
      actionButton("button","Calculate")
      
    ),
    
    ## Display Material
    mainPanel(
      plotOutput("distPlot"),
      tags$b("Compute parameters in R:"),
      verbatimTextOutput("summary"),
      reactableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  inform <- eventReactive(input$button, {
    
    
    ## Simple Linear Regression Inputs
    ev_slr = input$explanatory_variable_slr
    rv_slr = input$response_variable_slr
    
    ## Multiple Linear Regression Inputs
    ev1_mlr = input$explanatory_variable_1_mlr
    ev2_mlr = input$explanatory_variable_2_mlr
    rv_mlr = input$response_variable_mlr
    
    ta = input$type_analysis
    
    
    return(list(exp_var_slr = ev_slr, 
                resp_var_slr = rv_slr,
                type_analysis = ta))
  })  
  
  
  output$distPlot <- renderPlot({
    inform <- inform()
    
    
    if(inform$type_analysis == 'SLR'){
      explanatory_variable <- inform$exp_var_slr
      response_variable <- inform$resp_var_slr 
      
      ## Plot Scatter Plot of response and explanatory variable
      ggplot(hdi_df,aes_string(x = explanatory_variable, 
                               y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = explanatory_variable,
             y = response_variable,
             title = paste0("Relationship Data ",explanatory_variable," and",response_variable)) +
        theme_bw() +
        theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
    } else {
      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr 
      
    }
    
    
  })
  
  
  output$summary <- renderPrint({
    inform<-inform()
    
    explanatory_variable <- inform$exp_var_slr
    response_variable <- inform$resp_var_slr 
    
    ## Formula for regression
    f <- as.formula(
      paste(response_variable, 
            paste(explanatory_variable, collapse = " + "), 
            sep = " ~ "))
    
    
    ## Run Regression Model
    summary(lm(f,data = hdi_df))
    
  })
  
  
  
  output$table <- renderReactable({
    inform<-inform()
    
    explanatory_variable <- inform$exp_var_slr
    response_variable <- inform$resp_var_slr 
    
    ## Select Based on user input
    hdi_df %>%
      dplyr::select(Country, explanatory_variable, response_variable) %>% 
      reactable()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
