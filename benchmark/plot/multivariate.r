
ise <- multivariate %>%
  mutate(
    Method = factor(
      as.character(Method),
      c("FB", "AB", "GMM2", "GMM3", "GMM5", "GMM8"),
      c("DEBA", "ADEBA-a", "GMM2", "GMM3", "GMM5", "GMM8")
    ),
    replicate=Replicate,
    Replicate=NULL,
    Duration = ifelse(is.na(ISE), NA, Duration)
  )

mise <- ise %>%
  group_by(Distribution, Method, n) %>%
  summarise(
    MISE = mean(ISE, na.rm=TRUE),
    n_ISE = sum(!is.na(ISE)),
    SD_ISE = sd(ISE, na.rm=TRUE)
  ) %>%
  ungroup %>%
  mutate(
    Distribution = drop_levels(Distribution)
  )


axis_limits <- mise %>%
  group_by(Distribution) %>%
  do(with(., data_frame(
    n = range(n),
    MISE = c(10^floor(log10(min(MISE, na.rm=TRUE))), min(2, 10^ceiling(log10(max(MISE, na.rm=TRUE)))))
  ))) %>%
  ungroup %>%
  mutate(Method = factor(NA, levels(mise$Method)))

duration <- multivariate %>%
  dplyr::filter(!is.na(ISE)) %>%
  mutate(
    Method = factor(
      as.character(Method),
      c("AB", "GMM2", "GMM3", "GMM5", "GMM8"),
      c("ADEBA-a", "GMM2", "GMM3", "GMM5", "GMM8")
    )
  ) %>%
  group_by(Distribution, n, Method) %>%
  summarize(MeanDuration = mean(Duration, na.rm=TRUE),
            nDuration = sum(!is.na(Duration))) %>%
  dplyr::filter(!is.na(MeanDuration))


#-------------------------------------------------------------------[ Figure 6 ]

fig6 <- mise %>%
  ggplot(aes(x = n, y = MISE, colour=Method, shape=Method)) +
    geom_blank(data = axis_limits) +
    geom_line(data = complete_frame(mise)) +
    geom_line(data = incomplete_frame(mise), linetype = 'dashed', size = .2) +
    geom_point() +
    facet_wrap(~Distribution, scales = "free_y") + 
    scale_x_log10(
      breaks = 25*2^(0:6),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    scale_y_log10(
      breaks = 10^-(4:0),
      expand = c(.02, 0),
      labels = extreme_labels
    ) +
    #scale_shape_manual(NULL, values=c(1, 1, 1, 2, 2,1:6, labels=method_labels) +
    scale_color_manual(
      NULL,
      values = method_palette,
      labels = method_labels
    ) +
    guides(colour = guide_legend(nrow=2)) +
    ylab("Mean ISE\n(log scale)") + 
    xlab("Sample size") +
    theme(
      plot.margin = unit(c(0, 4, 15, 0), "mm"),
      legend.position = c(.5, 0),
      legend.justification = c(.5, 1.1),
      axis.ticks.x = element_line(colour = "grey80", size = .3)
    )

my_save("plot/fig6_multivariate_mise.pdf", fig6, width = 9, height = 10)

# Duration
figS4 <- ggplot(duration, aes(x = n, y = MeanDuration, label = nDuration, colour = Method)) +
    geom_line() + 
    geom_point(colour="white", size=2.8) + 
    geom_text(size=1.5, show.legend=FALSE) + 
    facet_wrap(~Distribution) +
    scale_x_log10(
      breaks = 25*2^(0:6),
      expand = c(.08, 0),
      labels = extreme_labels
    ) +
    scale_y_log10(
      breaks = c(1, 60, 60*60),
      labels = c('1 s', '1 min', '1 h'),
      limits = c(1, 60*60),
      expand = c(0, 0)
    ) +
    scale_color_manual(
      NULL,
      values = method_palette,
      labels = method_labels
    ) +
    xlab("Sample size") +
    ylab("Average computation time (log scale)") +
    guides(colour = guide_legend(nrow=2)) +
    theme(
      plot.margin = unit(c(0, 4, 15, 0), "mm"),
      legend.position = c(.5, 0),
      legend.justification = c(.5, 1.1),
      axis.ticks.x = element_line(colour = "grey80", size = .3),
      axis.title.y = element_text(angle = 90)
    )
my_save("plot/figS4_multivariate_computation_time.pdf", figS4, width = 9, height = 10)


#--------------------------------------------------------[ Export tabular data ]

dir.create("tables", showWarnings = FALSE)

ise %>%
  mutate(distribution_id = as.integer(Distribution) + 15) %>%
  select(distribution_id, n, replicate, method=Method, ise=ISE, computation_time=Duration) %>%
  arrange(distribution_id, n, replicate, method, ise) %>%
  write_csv(path="tables/multivariate_ise.csv")

mise %>%
  mutate(distribution_id = as.integer(Distribution) + 15) %>%
  select(distribution_id, n, method=Method, mean_ise=MISE, sd_ise=SD_ISE) %>%
  arrange(distribution_id, n, method, mean_ise, sd_ise) %>%
  write_csv(path="tables/multivariate_mean_ise.csv")

ise %>%
  select(-Duration) %>%
  make_table(caption="Data underlying Figure~7. The best performance of each row is written in bold font.") %>%
  cat(file="tables/multivariate_mise.tex")


