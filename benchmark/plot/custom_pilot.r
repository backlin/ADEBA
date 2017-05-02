
#----------------------------------------------------------------[ Smooth comb ]

d <- distributions[[14]]
g <- grids[[14]]
set.seed(123)
x_sample <- rdist(d, 200)

plot_data <- data_frame(
    x = g,
    Truth = ddist(d, g),
    SJ = NA,
    DEBA = 1/6,
    `ADEBA-a` = predict(adeba(x_sample, adaptive=FALSE), g),
    `ADEBA*` = .05*exp(.5*g)
) %>%
  gather(Method, pilot, -x, -Truth)

plot_data <- plot_data %>%
  group_by(Method) %>%
  do(with(., {
    estimate <- switch(
      Method[1],
      SJ = sj(x_sample, x),
      DEBA = predict(adeba(x_sample, adaptive=FALSE), x),
      `ADEBA-a` = predict(adeba(x_sample), x),
      `ADEBA*` = {
        fit <- adeba(x_sample, adaptive=FALSE, beta = 0:4/2,
                     pilot=approx(x, pilot, x_sample)$y)
        predict(fit, x)
      }
    )
    data_frame(x = x, Truth = Truth, pilot = pilot, estimate = estimate)
  }))

ise <- plot_data %>%
  group_by(Method) %>%
  do(with(., {
    data_frame(ISE = ise_univariate(estimate, Truth, x))
  })) %>%
  ungroup %>%
  mutate(
    Method = factor(Method, c("SJ", "DEBA", "ADEBA-a", "ADEBA*")),
    label = sprintf('ISE = %.3f', ISE),
    x = 2.4,
    y = .46
  )

annot <- data_frame(
  Method = factor("DEBA", levels(ise$Method)),
  x = c(-3.2, -2.8, -.4),
  y = c(.22, .4, .3),
  label = c("Pilot", "Estimate", "Truth")
)

fig5 <- plot_data %>%
  ungroup %>%
  mutate(Method = factor(Method, levels(annot$Method))) %>%
  ggplot(aes(x = x)) +
    facet_wrap(~Method, labeller = method_labeller) +
    geom_line(aes(y = Truth), colour = method_palette['Truth']) +
    #geom_rug(data = data_frame(x = as.vector(x_sample)), size = .1) +
    geom_line(aes(y = pilot, colour = Method), linetype = "dashed") +
    geom_line(aes(y = estimate, colour = Method)) +
    geom_text(data = annot[1:2,], aes(y = y, label = label), size = 1.7, colour = method_palette['DEBA']) +
    geom_text(data = annot[3,], aes(y = y, label = label), size = 1.7, colour = method_palette['Truth']) +
    geom_text(data = annot[3,], aes(y = y, label = label), size = 1.7, colour = method_palette['Truth']) +
    geom_text(data = ise, aes(y = y, label = label), size = 1.7, colour = method_palette['Truth']) +
    scale_x_continuous(
      limits = c(-4, 4),
      breaks = c(-4, 4),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, .51),
      breaks = c(0, .5),
      expand = c(0, 0)
    ) +
    scale_color_manual(
      values = method_palette,
      labels = method_labels
    ) +
    guides(colour = FALSE) +
    ylab(expression(p(x))) +
    theme(
      axis.ticks = element_blank()
    )
my_save("plot/fig5_custom_pilot.pdf", fig5, 9, 6)
