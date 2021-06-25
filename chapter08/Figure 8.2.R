# Figure 8.2.R
# Source: https://solomonkurz.netlify.app/post/2020-03-09-make-model-diagrams-kruschke-style/
# John Kruschke, November 2015.
# Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
# A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

library(tidyverse)
library(patchwork)
library(ggforce)

theme_set(theme_grey() +
            theme_void() +
            theme(plot.margin = margin(0, 5.5, 0, 5.5)))

# plot of a beta density
p1 <-
  tibble(x = seq(from = .01, to = .99, by = .01),
         d = (dbeta(x, 2, 2)) / max(dbeta(x, 2, 2))) %>% 
  
  ggplot(aes(x = x, y = d)) +
  geom_line(color = "skyblue", size = 4) +
  annotate(geom = "text",
           x = .5, y = .2,
           label = "beta",
           size = 7) +
  annotate(geom = "text",
           x = .5, y = .6,
           label = "italic(A)*', '*italic(B)", 
           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

## an annotated arrow
# save our custom arrow settings
my_arrow <- arrow(angle = 20, length = unit(0.35, "cm"), type = "closed")
p2 <-
  tibble(x    = .5,
         y    = 1,
         xend = .5,
         yend = 0) %>%
  
  ggplot(aes(x = x, xend = xend,
             y = y, yend = yend)) +
  geom_segment(arrow = my_arrow) +
  annotate(geom = "text",
           x = .375, y = 1/3,
           label = "'~'",
           size = 10, family = "Times", parse = T) +
  xlim(0, 1)

# bar plot of Bernoulli data
p3 <-
  tibble(x = 0:1,
         d = (dbinom(x, size = 1, prob = .6)) / max(dbinom(x, size = 1, prob = .6))) %>% 
  
  ggplot(aes(x = x, y = d)) +
  geom_col(fill = "skyblue", width = .4) +
  annotate(geom = "text",
           x = .5, y = .2,
           label = "Bernoulli",
           size = 7) +
  annotate(geom = "text",
           x = .5, y = .94,
           label = "theta", 
           size = 7, family = "Times", parse = T) +
  xlim(-.75, 1.75) +
  theme(axis.line.x = element_line(size = 0.5))

# another annotated arrow
p4 <-
  tibble(x     = c(.375, .625),
         y     = c(1/3, 1/3),
         label = c("'~'", "italic(i)")) %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = c(10, 7), parse = T, family = "Times") +
  geom_segment(x = .5, xend = .5,
               y = 1, yend = 0,
               arrow = my_arrow) +
  xlim(0, 1)

# some text
p5 <-
  tibble(x     = 1,
         y     = .5,
         label = "italic(y[i])") %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = 7, parse = T, family = "Times") +
  xlim(0, 2)

layout <- c(
  area(t = 1, b = 2, l = 1, r = 1),
  area(t = 3, b = 3, l = 1, r = 1),
  area(t = 4, b = 5, l = 1, r = 1),
  area(t = 6, b = 6, l = 1, r = 1),
  area(t = 7, b = 7, l = 1, r = 1)
)

(p1 + p2 + p3 + p4 + p5) + 
  plot_layout(design = layout) &
  ylim(0, 1)
