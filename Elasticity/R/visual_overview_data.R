plot_violin_sales <-
function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_violin() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}
plot_violin_price <-
function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_violin() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}
plot_total_revenue_line <-
function(dataset) {
  
  ggplot(data = dataset, aes(x = start_year, y = total_revenue, color = description)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Revenue per Year", x = "Year", y = "Total Revenue") +
    guides(color = guide_legend("Brand Name")) +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
}
