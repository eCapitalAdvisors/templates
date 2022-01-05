plot_fitted_vs_residual <-
function(sales_sample_tbl, model = "none", method = "ML") {
  
  if (model == "REM") {
    lmfit <- lm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
    ggplotly(p)
  }
  
  else if (model == "MEM") {
    lmfit <- lm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
    ggplotly(p)
  }
}
