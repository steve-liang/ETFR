to_tbl <- function(xts_obj){
  data.frame(Date = index(xts_obj), coredata(xts_obj)) %>% tbl_df()
}

to_xts <- function(tbl_obj){
  xts(tbl_obj %>% select(-Date), order.by=tbl_obj[["Date"]])
}

prep_for_treemap <- function(returns){
  returns %>% mutate(size = abs(value), color = ifelse(value > 0, 1, ifelse(value < 0, -1, 0)))
}

