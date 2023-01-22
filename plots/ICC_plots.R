##### Guttman and Rasch ICC plots

### Open required packages
library(tidyverse)
library(patchwork)

ICC_plot <- ((
  data.frame(beta=0) %>%
    tidyr::expand(nesting(beta
    ),
    theta = seq(from = -6, to = 6, length.out = 200)) %>%
    mutate(p = if_else(theta >= beta, 1, 0)) %>%
    ggplot(aes(x = theta, y = p)) +
    geom_line() +
    scale_y_continuous(limits = c(0,1)) +
    labs(title = "Guttman Model",
         x = expression(theta~('ability')),
         y = expression(italic(P)(success))) +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(size=10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
) /
  (
    data.frame(beta=0) %>%
      tidyr::expand(nesting(beta
      ),
      theta = seq(from = -6, to = 6, length.out = 200)) %>%
      mutate(p = exp(theta-beta)/(1 + exp(theta-beta))) %>%
      ggplot(aes(x = theta, y = p)) +
      geom_line() +
      scale_y_continuous(limits = c(0,1)) +
      labs(title = "Rasch Model",
           x = expression(theta~('ability')),
           y = expression(italic(P)(success))) +
      theme_classic() +
      theme(legend.text = element_text(size = 8),
            legend.title = element_text(size=10),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  )) +
  plot_annotation(tag_levels = "A")

save(ICC_plot, file = "plots/ICC_plot")

ICC_plot

ggsave("plots/ICC_plot.png", width = 5, height = 5, device = "png", dpi = 300)
