# function to plot expected mortality on year t

fn_mort_plot <- function(plot_data, plot_year, extremes_remove = FALSE) {
  
  plot_data <- plot_data %>% 
    filter(Year >= plot_year - 1 & Year <= plot_year + 1)
  
  max <- plot_data %>% 
    summarise(Deaths = max(Deaths),
              upi = max(upi),
              upi_flu_hc = max(upi_flu_hc)
    ) %>% 
    rowwise() %>% 
    mutate(max = max(c(Deaths, upi, upi_flu_hc)))
  
  if (extremes_remove == FALSE) {
    
    plot_data %>% 
      ggplot(aes(x = Date)) +
      geom_rect(aes(xmin = as.Date(paste0(plot_year, "-01-01")), 
                    xmax = as.Date(paste0(plot_year, "-12-31")), 
                    ymin = -Inf, ymax = Inf), 
                fill = "#ffffbf", alpha = 0.01, inherit.aes = FALSE) +    
      geom_ribbon(aes(ymin = lpi, ymax = upi), alpha = 0.1) + 
      geom_line(aes(y = fit), colour = "grey50") +
      geom_line(aes(y = Deaths), color = "#d7191c", size = 1) + 
      scale_y_continuous(labels = comma, limits = c(0, max$max)) +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      xlab("") + ylab("Observed & predicted deaths") +
      labs(caption = "Excludes extreme years",
           title = paste(as.character(plot_year ))) +
      theme_minimal()
    
  } else if (extremes_remove == TRUE) {
    
    plot_data %>% 
      ggplot(aes(x = Date)) +
      geom_rect(aes(xmin = as.Date(paste0(plot_year, "-01-01")), 
                    xmax = as.Date(paste0(plot_year, "-12-31")), 
                    ymin = -Inf, ymax = Inf), 
                fill = "#ffffbf", alpha = 0.01, inherit.aes = FALSE) +   
      geom_ribbon(aes(ymin = lpi_flu_hc, ymax = upi_flu_hc), alpha = 0.1) + 
      geom_line(aes(y = fit_flu_hc), colour = "grey50") +
      geom_line(aes(y = Deaths), color = "#d7191c", size = 1) + 
      scale_y_continuous(labels = comma, limits = c(0, max$max)) +
      scale_x_date(date_breaks = "4 months", date_labels = "%b") +
      xlab("") + ylab("Observed & predicted deaths") +
      labs(caption = "Excludes extreme years",
           title = paste(as.character(plot_year ))) +
      theme_minimal()
  }
}