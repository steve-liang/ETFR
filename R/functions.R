to_tbl <- function(xts_obj){
  as_tibble(data.frame(Date = index(xts_obj), coredata(xts_obj)))
}

prep_for_treemap <- function(returns){
  returns %>% mutate(size = abs(value), color = ifelse(value > 0, 1, ifelse(value < 0, -1, 0)))
}