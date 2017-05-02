
plot_data <- Map(
  function(d, g, nam, i){
    data.frame(Distribution = nam, x = g, f = ddist(d, g), Offset = i*100)
  },
  distributions[1:15], grids[1:15], names(distributions)[1:15], 1:15
) %>%
  bind_rows %>%
  mutate(
    Distribution = number_distributions(Distribution)
  )

axis_limtis <- plot_data %>%
  group_by(Distribution, Offset) %>%
  do(with(., data.frame(
    x = c(-4, 4),
    f = c(0, ceiling(10*1.02*max(f))/10)
  )))

fig2_distrbutions <- plot_data %>%
  ggplot(aes(x=x, y=f + Offset)) +
    geom_blank(data = axis_limtis) +
    geom_polygon(fill="grey70") + geom_line(size = .3) +
    facet_wrap(~Distribution, ncol=5, scales = "free_y") +
    scale_x_continuous(
      expand = c(0,0),
      labels = extreme_labels
    ) +
    scale_y_continuous(
      breaks = with(axis_limtis, f + Offset),
      expand = c(.01,0),
      labels = extreme_labels_with_offset
    ) +
    ylab("p(x)")
my_save("plot/fig2_univariate_densities.pdf", fig2_distrbutions, 19, 7)
