

library(shiny)
library(bslib)
library(MASS)
suppressPackageStartupMessages(library(tidyverse))
library(ggfortify)
library(gridExtra)
library(broom)


setwd("G:/My Drive/03_research_and_development/01_research_topics/paper_0_CRD_main/lesson_plans/crd_paper_lessons/regression_hdi")
## Bring in data
hdi_df = read_xlsx('hdi_2015.xlsx', sheet = 'raw_data') 
cn_df = read_xlsx('hdi_2015.xlsx', sheet = 'variables')

## Change column names
colnames(hdi_df) = cn_df$abbreviations
## Clean up data
hdi_df = hdi_df %>% 
    select(-contains('HDI'),-Country)


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

            selectInput('explanatory_variable',
                        'Explanatory Variable: ',
                        real_var_names_list
                        ),

            selectInput('response_variable',
                        'Response Variable: ',
                        real_var_names_list),
            
            actionButton("button","Calculate")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
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

    
    
    
    output$summary <- renderPrint({
        inform<-inform()
        
        explanatory_variable <- inform$exp_var# 'Trade_Percent_GDP'
        response_variable <- inform$resp_var #'Fertility_Rate'
        
        f <- as.formula(
            paste(response_variable, 
                  paste(explanatory_variable, collapse = " + "), 
                  sep = " ~ "))
        
        
        
        tidy(lm(f,data = hdi_df))
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
