

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
            selectInput('explanatory_variable',
                        'Explanatory Variable: ',
                        real_var_names_list
                        ),

            selectInput('response_variable',
                        'Response Variable: ',
                        real_var_names_list),
            
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
        
        ev = input$explanatory_variable
        rv = input$response_variable
        
        
        return(list(exp_var = ev, resp_var = rv))
    })  
    
    
    output$distPlot <- renderPlot({
        inform <- inform()
        
        explanatory_variable <- inform$exp_var
        response_variable <- inform$resp_var 
        
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
    })

    
    output$summary <- renderPrint({
      inform<-inform()
      
      explanatory_variable <- inform$exp_var
      response_variable <- inform$resp_var 
      
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
      
      explanatory_variable <- inform$exp_var
      response_variable <- inform$resp_var 
      
      ## Select Based on user input
      hdi_df %>%
        dplyr::select(Country, explanatory_variable, response_variable) %>% 
        reactable()
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
