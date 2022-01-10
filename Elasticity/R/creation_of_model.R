get_bootstrap <-
function(df) {
  
  # bootstrap
  df %>%
    mutate(sales = log(sales), price = log(price)) %>%
    specify(formula = sales ~ price) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "slope")
}
get_betas <-
function(sales_tbl) {
  
  # obtain betas
  bootstrap_tbl <- sales_tbl %>%
    group_by(description) %>%
    nest() %>%
    mutate(bootstrap_slopes = purrr::map(data, get_bootstrap))
  
  saveRDS(object = bootstrap_tbl, file = "../R/bootstrap_tbl.rds")
  
  return(bootstrap_tbl)
}
get_ci_for_bootstrap <-
function(bootstrap_tbl) {
  
  ci <- bootstrap_tbl %>%
    mutate(perc_ci = purrr::map(bootstrap_slopes, get_confidence_interval, level = 0.95, type = 'percentile')) %>%
    unnest(perc_ci)
  
  saveRDS(object = ci, file = "../R/ci.rds")
  
  return(ci)
}
