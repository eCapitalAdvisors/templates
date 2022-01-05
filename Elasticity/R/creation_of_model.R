get_bootstrap <-
function(sales_tbl) {
  
  # obtain betas for bootstrap
  bootstrap_tbl <- sales_tbl %>%
    mutate(sales = log(sales), price = log(price)) %>%
    specify(formula = sales ~ price + description) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    fit()
  
  saveRDS(object = bootstrap_tbl, file = "bootstrap_tbl.rds")
}
get_ci_for_bootstrap <-
function(bootstrap_tbl) {
  
  ci <- bootstrap_tbl %>%
    group_by(term) %>%
    nest() %>%
    mutate(perc_ci = map(
      data,
      get_confidence_interval,
      level = 0.95,
      type = "percentile"
    )) %>%
    unnest(perc_ci)
  
  saveRDS(object = ci, file = "ci.rds")
}
