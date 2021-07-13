function_plot_appendix_month_weeks <- function(){
  
  size_axis <-15
  size_axis_title <- 15
  lwd_size <- 1.2
  size_legend <- 15
  size_legend_title<- 15
  col_brew <-viridis(7, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  
  
  CHdata <- fn_comparison_monthly_weekly("Switzerland") %>%
    select(Perc_diff, Year) %>%
    mutate(country="Switzerland")
  
  ESdata <- fn_comparison_monthly_weekly("Spain")%>%
    select(Perc_diff, Year) %>%
    mutate(country="Spain")
  
  SWdata <- fn_comparison_monthly_weekly("Sweden")%>%
    select(Perc_diff, Year) %>%
    mutate(country="Sweden")
  
  dataCom <- rbind(CHdata,ESdata,SWdata)
  
  plot_appendix <- ggplot(data=dataCom)+
    geom_line(aes(x=Year, y=Perc_diff, col=country),lwd=lwd_size)+
    ylab("Differences in percentages of excess death between months and weeks")+
    ylim(-0.5, 0.9)+
    scale_color_manual("",
                      values = c(col_brew[1],col_brew[5],col_brew[7]),
                      breaks=c("Switzerland","Spain","Sweden"),
                      labels=c("Switzerland","Spain","Sweden"))+
    theme_bw()+
    theme(aspect.ratio=1,
          legend.text=element_text(size=size_legend),
          axis.text=element_text(size=size_axis),
          axis.title=element_text(size=size_axis_title),
          legend.position="bottom")
  
  cowplot::save_plot("paper/plot_appendix_week_month.pdf", plot_appendix ,base_height=10,base_width=10)
  
  
}