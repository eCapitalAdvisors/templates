# Creation of Boxplots

plot_boxplot_sales <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}

plot_boxplot_price <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}