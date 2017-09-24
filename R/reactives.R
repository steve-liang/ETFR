

dataInput <- reactive({ 
  
  etf_list <- readr::read_csv('data/vanguard_etf_list.csv')
  validate(
    need(!is.na(etf_list), "Please select a etf list")
  )
  
  data <- purrr::map(etf_list[["SYMBOL"]], function(x) {getSymbols(x, src = "google",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                                                              from = as.Date('2016-01-01'),
                                                              to = Sys.Date(),
                                                              auto.assign = FALSE)})
  
  Closes <- purrr::map(data, function(x) Cl(x))
  Closes <- do.call(merge, Closes)
  return(Closes)
})