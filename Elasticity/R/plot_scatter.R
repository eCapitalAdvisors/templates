# Creation of scatter plot

plot_scatter <- function(sales_sample_tbl, model = "none"){
  
  if (model == "RI") {
    mod <- glm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.", color = "Brand Names") +
      guides(color = guide_legend(title = "Brand Names")) + 
      theme(plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
  else if (model == "RIRS") {
    mod <- glm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) + 
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.", color = "Brand Names") + 
      guides(color = guide_legend(title = "Brand Names")) + 
      theme(plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p)
  }
  else {
    p <- ggplot(data = sales_sample_tbl, aes(x = log(price), y = log(sales))) +
      geom_point(aes(color = description), alpha = .8) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.", color = "Brand Names") +
      guides(color = guide_legend(title = "Brand Names")) + 
      theme(plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5)) +
      scale_color_manual(values = c("#0071CE", "#FFC627", "#646569"))
    
    ggplotly(p) 
  }
}
