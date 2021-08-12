focus_years <- c(1890, 1918, 1957, 2020)

for(country in unique(results_month$Country)) {
  
  # print(paste0("Plotting ", country))
  
  max <- results_month %>%
    filter(Country == country) %>%
    filter(!is.na(Pandemic)) %>%
    summarise(Deaths = max(Deaths),
              lower = max(lower),
              upper = max(upper)) %>%
    rowwise() %>%
    mutate(max = max(c(Deaths, lower, upper))) %>%
    ungroup()
  
  # print(paste0("Max for x is ", max$max))
  
  for(year in focus_years) {
    
    # skip Spain oldest - no data
    if(year == 1890 & country == "Spain") {
      next
    }
    
    # print(paste0("    Year ", year))
    
    plot_data <- results_month %>%
      filter(Country == country) %>%
      filter(Pandemic == year)
    
    name <- paste(country, year, sep = "_")
    
    if (year == 1890 & country != "Spain") {
      my_title = paste0(country, ": ",
                        as.character(year - 1), "-", as.character(year))
    } else if (year == 1918 & country == "Spain") {
      my_title = paste0(country, ": ",
                        as.character(year - 1), "-", as.character(year + 2))
    } else if (year == 2020) {
      my_title = paste0(as.character(year - 1), "-", as.character(year))
    } else {
      my_title = paste0(as.character(year - 1), "-", as.character(year + 2))
    }    
    
    # sync breaks across plots
    my_breaks = plot_data$Date[seq(1, nrow(plot_data), 6)]
    
    plot <- plot_data %>%
      ggplot(aes(x = Date)) +
      geom_rect(xmin = as.Date(paste0(year, "-01-01")),
                xmax = as.Date(paste0(year, "-12-31")),
                ymin = -Inf, ymax = Inf,
                fill = "grey90", inherit.aes = FALSE) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
      geom_line(aes(y = pred), colour = "grey50") +
      geom_line(aes(y = Deaths), color = "#EF8A62", size = 0.5) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
      xlab("Year") +
      ylab("Deaths") +
      ggtitle(my_title) +
      scale_y_continuous(limits = c(0, max$max),
                         labels = label_number(accuracy = 1, suffix = "K", scale = 1e-3)) +
      theme_bw() +
      theme(plot.title = element_text(size = 9),
            axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.text.x = element_text(size = 8),
            plot.margin = margin(0, 0, 0, 0))
    
    assign(name, plot)
  }
}

Sweden_1890 + Sweden_1918 + Sweden_1957 + Sweden_2020 +
  Switzerland_1890 + Switzerland_1918 + Switzerland_1957 + Switzerland_2020 +
  plot_spacer() + Spain_1918 + Spain_1957 + Spain_2020 +
  plot_layout(widths = rep(c(2, 2, 2, 1), 3)) +
  plot_layout(ncol = 4) + 
  theme(plot.margin = margin(0, 0, 0, 0))

ggsave("paper/Figure_2.png", dpi = 300, width = 297, height = 210, units = "mm")

rm(Spain_1918, Spain_1957, Spain_2020,
   Sweden_1890, Sweden_1918, Sweden_1957, Sweden_2020,
   Switzerland_1890, Switzerland_1918, Switzerland_1957, Switzerland_2020,
   focus_years, max)
