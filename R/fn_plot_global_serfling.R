# plot output from fn_global_serfling

fn_plot_global_serfling = function(expected, title="", ylim=NULL) {
  gg = ggplot(expected$pred_total_deaths,aes(x=Date)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), fill="aquamarine2", alpha=.5) +
    geom_line(aes(y=pred)) +
    geom_line(aes(y=Deaths), colour="firebrick") +
    geom_point(aes(y=Deaths), colour="firebrick", fill="white", shape=21, size = 1.8) +
    labs(x="Time", y="Deaths", title=title)
  if(!is.null(ylim)) 
    gg = gg + coord_cartesian(ylim=ylim)
  return(gg)
}