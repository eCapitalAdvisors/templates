plot_boxplot_sales <-
function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}
plot_boxplot_price <-
function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}
plot_histogram_sales <-
function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for sales
  p <- ggplot(data = sales_tbl, aes(x = log(sales), fill = description)) + 
    geom_density(adjust = 5, aes(x = log(sales), fill = description), alpha = .8) +
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names") + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
  
  ggplotly(p)
}
plot_histogram_price <-
function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for price
  p <- ggplot(data = sales_tbl, aes(x = log(price))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    xlim(0, 2) + 
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names") + 
    theme(plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
  
  ggplotly(p)
}
plot_scatter <-
function(sales_sample_tbl, model = "none"){
  
  if (model == "RI") {
    mod <- glm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
  else if (model == "RIRS") {
    mod <- glm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p)
  }
  else {
    p <- ggplot(data = sales_sample_tbl, aes(x = log(price), y = log(sales))) +
      geom_point(aes(color = description), alpha = .8) +
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
}
plot_bootstrap <-
function(bootstrap_tbl) {
  p <- ggplot(bootstrap_tbl %>% filter(term == "price") %>% select(replicate, estimate), aes(estimate)) +
    geom_density() + 
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(lower_ci), linetype = "dotted", color = "red") +
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(upper_ci), linetype = "dotted", color = "red") +
    labs(title = "Bootstrap of Means", x = "Estimates of Beta", y = "Count") +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
  
  ggplotly(p)
}
