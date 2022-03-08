

library(shiny)
suppressPackageStartupMessages(library(tidyverse))
library(gridExtra)
library(reactable)
library(ggfortify)


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
                    span('Response Variable: ',style="color:red"),
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
        
        ## How many explanatory Variables
        
        
        radioButtons(
          inputId = "num_var",
          label = span("How many explanatory variables?",style="color:green"), 
          choices = c("2", "3")
        ),
        
      ),
        
        
      conditionalPanel(
        condition = 'input.type_analysis == "MLR" & input.num_var == "2"',
        selectInput('response_variable_mlr_2e',
                    span('Response Variable: ',style="color:red"),
                    real_var_names_list,
                    selected =  "Fertility_Rate"),
        
        selectInput(inputId = 'explanatory_variable_1_mlr_2e',
                    label =  'Explanatory Variable 1: ',
                    choices = real_var_names_list,
                    selected =  "Young_0_14_per_100_adults_15_64" ),
        
        
        selectInput(inputId = 'explanatory_variable_2_mlr_2e',
                    label =  'Explanatory Variable 2: ',
                    choices = real_var_names_list, 
                    selected =  "Mean_years_of_schooling"),
  
      ),
      
      conditionalPanel(
        condition = 'input.type_analysis == "MLR" & input.num_var == "3"',
        selectInput('response_variable_mlr_3e',
                    span('Response Variable: ',style="color:red"), 
                    real_var_names_list,
                    selected =  "Fertility_Rate"),
        
        selectInput(inputId = 'explanatory_variable_1_mlr_3e',
                    label =  'Explanatory Variable 1: ',
                    choices = real_var_names_list,
                    selected =  "Young_0_14_per_100_adults_15_64"),
        
        
        selectInput(inputId = 'explanatory_variable_2_mlr_3e',
                    label =  'Explanatory Variable 2: ',
                    choices = real_var_names_list, 
                    selected =  "Mean_years_of_schooling"),
        
        
        selectInput(inputId = 'explanatory_variable_3_mlr_3e',
                    label =  'Explanatory Variable 3: ',
                    choices = real_var_names_list, 
                    selected =  "Mean_years_of_schooling"),
        
      ),
      
      
      
      
      
      
      
      actionButton("button","Display and Calculate")
      
    ),
    
    ## Display Material
    mainPanel(
      tags$h3(tags$b('Visualization:')),
      plotOutput("scatter"),
      br(),
      br(),
      tags$h3(tags$b("Regression Analysis:")),
      br(),
      br(),
      verbatimTextOutput("summary"),
      br(),
      br(),
      tags$h3(tags$b("Assumptions:")),
      plotOutput("assume"),
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
    
    
    ## Number of explanatory variables
    n_e_v = input$num_var
    
    ## Multiple Linear Regression Inputs 2 explanatory variables
    ev1_mlr_2e = input$explanatory_variable_1_mlr_2e
    ev2_mlr_2e = input$explanatory_variable_2_mlr_2e
    rv_mlr_2e = input$response_variable_mlr_2e
    
    ## Multiple Linear Regression Inputs 3 explanatory variables
    ev1_mlr_3e = input$explanatory_variable_1_mlr_3e
    ev2_mlr_3e = input$explanatory_variable_2_mlr_3e
    ev3_mlr_3e = input$explanatory_variable_3_mlr_3e
    rv_mlr_3e = input$response_variable_mlr_3e    
    
    
    return(list(type_analysis = ta,
                num_exp_var = n_e_v,
                exp_var_slr = ev_slr, 
                resp_var_slr = rv_slr,
                exp_var1_mlr_2e = ev1_mlr_2e, 
                exp_var2_mlr_2e = ev2_mlr_2e, 
                resp_var_mlr_2e = rv_mlr_2e,
                exp_var1_mlr_3e = ev1_mlr_3e, 
                exp_var2_mlr_3e = ev2_mlr_3e, 
                exp_var3_mlr_3e = ev3_mlr_3e,
                resp_var_mlr_3e = rv_mlr_3e,
             
    ))
  })  
  
  
  output$scatter <- renderPlot({
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
    } else if(inform$type_analysis == 'MLR' & inform$num_exp_var == '2') {
      explanatory_variable_1 <- inform$exp_var1_mlr_2e
      explanatory_variable_2 <- inform$exp_var2_mlr_2e
      response_variable <- inform$resp_var_mlr_2e 
      
      
      
      ## Obtain real variables names
      tmp_names = cn_df %>% 
        filter(abbreviations %in% c(explanatory_variable_1,
                                    explanatory_variable_2,
                                    response_variable)) %>% 
        pull(Variable)
      
      exp_var1_real = tmp_names[1]
      exp_var2_real = tmp_names[2]
      res_var_real = tmp_names[3]
      
      
      ## Plot Scatter Plot of response and explanatory variable 1
      p1 <- ggplot(hdi_df,aes_string(x = explanatory_variable_1, 
                                     y = response_variable)) +
        geom_point(color = 'black',size = 3) +
        geom_smooth(method = "lm", se = FALSE,color = 'red')  +
        labs(x = exp_var1_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var1_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
      ## Plot Scatter Plot of response and explanatory variable 2
      p2 <-  ggplot(hdi_df,aes_string(x = explanatory_variable_2, 
                                      y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = exp_var2_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var2_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 17, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 14, face = "bold"))
      grid.arrange(p1, p2, ncol=2)
      
    } else if(inform$type_analysis == 'MLR' & inform$num_exp_var == '3'){ ## 3 explanatory variables
      
      
      explanatory_variable_1 <- inform$exp_var1_mlr_3e
      explanatory_variable_2 <- inform$exp_var2_mlr_3e
      explanatory_variable_3 <- inform$exp_var3_mlr_3e
      response_variable <- inform$resp_var_mlr_3e 
      
      
      
      ## Obtain real variables names
      tmp_names = cn_df %>% 
        filter(abbreviations %in% c(explanatory_variable_1,
                                    explanatory_variable_2,
                                    explanatory_variable_3,
                                    response_variable)) %>% 
        pull(Variable)
      
      exp_var1_real = tmp_names[1]
      exp_var2_real = tmp_names[2]
      exp_var3_real = tmp_names[3]
      res_var_real = tmp_names[4]
      
      
      ## Plot Scatter Plot of response and explanatory variable 1
      p1 <- ggplot(hdi_df,aes_string(x = explanatory_variable_1, 
                                     y = response_variable)) +
        geom_point(color = 'black',size = 3) +
        geom_smooth(method = "lm", se = FALSE,color = 'red')  +
        labs(x = exp_var1_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var1_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 11, face = "bold"))
      ## Plot Scatter Plot of response and explanatory variable 2
      p2 <-  ggplot(hdi_df,aes_string(x = explanatory_variable_2, 
                                      y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = exp_var2_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var2_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 11, face = "bold"))
      
      ## Plot Scatter Plot of response and explanatory variable 3
      p3 <-  ggplot(hdi_df,aes_string(x = explanatory_variable_3, 
                                      y = response_variable)) +
        geom_point(color = 'red',size = 3) +
        geom_smooth(method = "lm", se = FALSE)  +
        labs(x = exp_var3_real,
             y = res_var_real,
             title = paste0("Relationship Data \n",exp_var3_real," and \n",res_var_real)) +
        theme_bw() +
        theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
              axis.title = element_text(size = 11, face = "bold"))
      
      
      
      grid.arrange(p1, p2, p3, ncol=3)
      
      
      
      
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
  
  
  output$assume <- renderPlot({
    inform<-inform()
   
    
    if(inform$type_analysis == 'SLR'){
      explanatory_variable <- inform$exp_var_slr
      response_variable <- inform$resp_var_slr 
      
      
      ## Formula for Simple Linear Regression
      f <- as.formula(
        paste(response_variable, 
              paste(explanatory_variable, collapse = " + "), 
              sep = " ~ "))
      
      
      ## View Regression Assumptions
      point_color = "blue" ## Get Color
      autoplot(lm(f,data = hdi_df),colour = point_color,which = 1:2)
      
      
      
    } else {
      
      explanatory_variable_1 <- inform$exp_var1_mlr
      explanatory_variable_2 <- inform$exp_var2_mlr
      response_variable <- inform$resp_var_mlr 
      
      
      ## Formula for Multiple Linear Regression
      f <- as.formula(
        paste(response_variable, 
              paste(c(explanatory_variable_1,explanatory_variable_2), collapse = " + "), 
              sep = " ~ "))
      
      
      ## View Regression Assumptions
      point_color = "red" ## Get Color
      autoplot(lm(f,data = hdi_df),colour = point_color,which = 1:2)
     
      
      
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
