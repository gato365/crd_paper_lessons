

library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(gridExtra)
library(reactable)


# setwd("G:/My Drive/03_research_and_development/01_research_topics/paper_0_CRD_main/lesson_plans/crd_paper_lessons/regression_hdi")
## Bring in data
# hdi_df = read_xlsx('hdi_2015.xlsx', sheet = 'raw_data') 
hdi_df = read.csv('hdi_2015_raw_data.csv')
# cn_df = read_xlsx('hdi_2015.xlsx', sheet = 'variables')
cn_df = read.csv('variables_dictionary.csv')

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
        
        
        selectInput(inputId = 'response_variable_slr',
                    label =  'Response Variable: ',
                    choices = real_var_names_list,
                    selected =  "Fertility_Rate"),
        selectInput(inputId = 'explanatory_variable_slr',
                    label =  'Explanatory Variable: ',
                    choices = real_var_names_list,
                    selected =  "Young_0_14_per_100_adults_15_64")
        
        
      ),
      
      ## For Multiple Linear Regression
      conditionalPanel(
        condition = 'input.type_analysis == "MLR"',
        selectInput('response_variable_mlr',
                    'Response Variable: ',
                    real_var_names_list,
                    selected =  "Fertility_Rate"),
        selectInput(inputId = 'explanatory_variable_1_mlr',
                    label =  'Explanatory Variable 1: ',
                    choices = real_var_names_list,
                    selected =  "Young_0_14_per_100_adults_15_64"
        ),
        
        selectInput(inputId = 'explanatory_variable_2_mlr',
                    label =  'Explanatory Variable 2: ',
                    choices = real_var_names_list, 
                    selected =  "Mean_years_of_schooling"
        ),
        
        
        
      ),
      
      
      actionButton("button","Display and Calculate")
      
    ),
    
    ## Display Material
    mainPanel(
      tags$h3(tags$b('Visualization:')),
      plotOutput("distPlot"),
      br(),
      br(),
      tags$h3(tags$b("Regression Analysis:")),
      verbatimTextOutput("summary"),
      br(),
      br(),
      conditionalPanel(
        condition = "input.type_analysis == 'MLR'",
        tags$h3(tags$b("VIF:")),
        verbatimTextOutput("vif")  
      ),
      
      tags$h3(tags$b('View Table of Selected Variables:')),
      reactableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  inform <- eventReactive(input$button, {
    
    
    ## Specify the Analysis
    ta = input$type_analysis
    
    ## Simple Linear Regression Inputs
    ev_slr = input$explanatory_variable_slr
    rv_slr = input$response_variable_slr
    
    ## Multiple Linear Regression Inputs
    ev1_mlr = input$explanatory_variable_1_mlr
    ev2_mlr = input$explanatory_variable_2_mlr
    rv_mlr = input$response_variable_mlr
    
    
    
    
    return(list(type_analysis = ta,
                exp_var_slr = ev_slr, 
                resp_var_slr = rv_slr,
                exp_var1_mlr = ev1_mlr, 
                exp_var2_mlr = ev2_mlr, 
                resp_var_mlr = rv_mlr
    ))
  })  
  
  
  output$distPlot <- renderPlot({
    inform <- inform()
    
    
    if(inform$type_analysis == 'SLR'){
      explanatory_variable <- inform$exp_var_slr
      response_variable <- inform$resp_var_slr 
      
      
      ## Obtain real variables names
      tmp_names = cn_df %>% 
        filter(abbreviations %in% c(explanatory_variable,
                                    response_variable)) %>% 
        pull(Variable)
      
      exp_var_real = tmp_names[1]
      res_var_real = tmp_names[2]
      
      ## Plot Scatter Plot of response and explanatory variable
      ggplot(hdi_df,aes_string(x = explanatory_variable, 
                               y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = exp_var_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var_real," and \n",
                            res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
    } else {
      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr 
      
      
      
      ## Obtain real variables names
      tmp_names = cn_df %>% 
        filter(abbreviations %in% c(explanatory_variable_1,
                                    explanatory_variable_2,
                                    response_variable)) %>% 
        pull(Variable)
      
      exp_var1_real = tmp_names[1]
      exp_var2_real = tmp_names[2]
      res_var_real = tmp_names[3]
      
      
      ## Plot Scatter Plot of response and explanatory variable
      p1 <- ggplot(hdi_df,aes_string(x = explanatory_variable_1, 
                                     y = response_variable)) +
        geom_point(color = 'black',size = 3) +
        geom_smooth(method = "lm", se = FALSE,color = 'red')  +
        labs(x = exp_var1_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var1_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
      p2 <-  ggplot(hdi_df,aes_string(x = explanatory_variable_2, 
                                      y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = exp_var2_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var2_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 19, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
      grid.arrange(p1, p2, ncol=2)
      
    }
    
    
  })
  
  
  
  output$vif <- renderPrint({
    inform<-inform()
    
    if(inform$type_analysis == 'MLR'){
      
      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr 
      
      
      ## Formula for Multiple Linear Regression
      f <- as.formula(
        paste(response_variable, 
              paste(c(explanatory_variable_1,explanatory_variable_2), collapse = " + "), 
              sep = " ~ "))
      
      
      ## Check for multicollinearity
      car::vif(lm(f,data = hdi_df)) 
      
      
    }
    
    
  })
  
  
  output$summary <- renderPrint({
    inform<-inform()
    
    
    if(inform$type_analysis == 'SLR'){
      explanatory_variable <- inform$exp_var_slr
      response_variable <- inform$resp_var_slr 
      
      
      ## Formula for Simple Linear Regression
      f <- as.formula(
        paste(response_variable, 
              paste(explanatory_variable, collapse = " + "), 
              sep = " ~ "))
      
      
      ## Run Regression Model
      summary(lm(f,data = hdi_df))
      
      
    } else {
      
      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr 
      
      
      ## Formula for Multiple Linear Regression
      f <- as.formula(
        paste(response_variable, 
              paste(c(explanatory_variable_1,explanatory_variable_2), collapse = " + "), 
              sep = " ~ "))
      
      
      ## Run Regression Model
      summary(lm(f,data = hdi_df))  
      
      
    }
    
    
    
    
  })
  
  

  output$table <- renderReactable({
    inform<-inform()
    if(inform$type_analysis == 'SLR'){
      explanatory_variable <- inform$exp_var_slr
      response_variable <- inform$resp_var_slr

      ## Select Based on user input for Simple Linear Regression
      hdi_df %>%
        dplyr::select(Country, explanatory_variable, response_variable) %>%
        reactable()

    } else {

      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr

      ## Select Based on user input for Multiple Linear Regression
      hdi_df %>%
        dplyr::select(Country, explanatory_variable_1,explanatory_variable_2, response_variable) %>%
        reactable()



    }




  })


  
}

# Run the application 
shinyApp(ui = ui, server = server)
