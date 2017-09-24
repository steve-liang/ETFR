library(shiny)
library(quantmod)
library(tibble)
library(highcharter)

source('R/functions.R')
source('R/reactives.R', local = TRUE)

function(input, output, session){
  
  output$dateSliderUi <- renderUI({
    sliderInput(inputId = "dateSlider", label = "Slide N days ago:", min = 1, max = 365,
                value = 1, step = 1, animate = TRUE)
  })
  
  output$return_table <- DT::renderDataTable({
    d <- dataInput()
    d <- to_tbl(ROC(d, type = 'discrete'))[nrow(d) - input$dateSlider + 1,]
    DT::datatable(d %>% tail(1), rownames = FALSE)
  })
  
  output$return_treemap <- renderHighchart({
    d <- dataInput()
    d <- to_tbl(ROC(d, type = 'discrete'))[nrow(d) - input$dateSlider + 1,] %>% select(-Date) %>% tidyr::gather()
    d <- prep_for_treemap(d) %>% arrange(desc(value))
    
    tm <- treemap(d, index = "key", vSize = "size", vColor = "color", type = "value", palette = "RdYlGn")
    hctreemap(tm) %>% 
      hc_title(text = "Daily Return")
  })
  
  output$candlestick_chart <- renderHighchart({
    d <- dataInput()
    
    highchart(type = "stock") %>%
      hc_add_series(d[,1]) %>%
      hc_add_series(d[,2]) %>%
      hc_add_series(d[,3]) %>%
      hc_add_series(d[,4]) %>%
      hc_add_series(d[,5]) %>%
      hc_add_series(d[,6]) %>%
      hc_add_series(d[,7]) %>%
      hc_add_series(d[,8])
      
      
  })
}