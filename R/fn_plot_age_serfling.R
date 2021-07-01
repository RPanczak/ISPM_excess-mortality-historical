# plot output from fn_age_serfling

fn_plot_age_serfling = function(expected, title="", ylim=NULL) {
  gg = ggplot(expected$pred_grouped_deaths,aes(x=Age_cat)) +
    geom_col(aes(y=Deaths), colour="firebrick", fill="white") +
    geom_pointrange(aes(y=pred,ymin=lower, ymax=upper), fill="aquamarine2", colour="black", shape = 21) +
    labs(x="Age group", y="Deaths", title=title)
  if(!is.null(ylim)) 
    gg = gg + coord_cartesian(ylim=ylim)
  return(gg)
}