# Figure 9.7.R
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

# a beta density
p1 <-
  tibble(x = seq(from = .01, to = .99, by = .01),
         d = (dbeta(x, 2, 2)) / max(dbeta(x, 2, 2))) %>% 
  ggplot(aes(x = x, y = d)) +
  geom_area(fill = "skyblue", size = 0) +
  annotate(geom = "text",
           x = .5, y = .2,
           label = "beta",
           size = 7) +
  annotate(geom = "text",
           x = .5, y = .6,
           label = "italic(A[omega])*', '*italic(B[omega])", 
           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

# a gamma density
p2 <-
  tibble(x = seq(from = 0, to = 5, by = .01),
         d = (dgamma(x, 1.75, .85) / max(dgamma(x, 1.75, .85)))) %>% 
  ggplot(aes(x = x, y = d)) +
  geom_area(fill = "skyblue", size = 0) +
  annotate(geom = "text",
           x = 2.5, y = .2,
           label = "gamma",
           size = 7) +
  annotate(geom = "text",
           x = 2.5, y = .6,
           label = "list(italic(S)[kappa], italic(R)[kappa])",
           size = 7, family = "Times", parse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

p3 <-
  tibble(x = c(.5, .475, .26, .08, .06,
               .5, .55, .85, 1.15, 1.175,
               1.5, 1.4, 1, .25, .2,
               1.5, 1.49, 1.445, 1.4, 1.39),
         y = c(1, .7, .6, .5, .2,
               1, .7, .6, .5, .2,
               1, .7, .6, .5, .2,
               1, .75, .6, .45, .2),
         line = rep(letters[2:1], each = 5) %>% rep(., times = 2),
         plot = rep(1:2, each = 10)) %>% 
  
  ggplot(aes(x = x, y = y, group = interaction(plot, line))) +
  geom_bspline(aes(color = line),
               size = 2/3, show.legend = F) + 
  annotate(geom = "text",
           x = 0, y = .1,
           label = "omega(kappa-2)+1*', '*(1-omega)(kappa-2)+1",
           size = 7, parse = T, family = "Times", hjust = 0) +
  annotate(geom = "text",
           x = c(1/3, 1.15), y = .7,
           label = "'~'",
           size = 10, parse = T, family = "Times") +
  scale_color_manual(values = c("grey75", "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  ylim(0, 1)

# another beta density
p4 <-
  tibble(x = seq(from = .01, to = .99, by = .01),
         d = (dbeta(x, 2, 2)) / max(dbeta(x, 2, 2))) %>% 
  ggplot(aes(x = x, y = d)) +
  geom_area(fill = "skyblue", size = 0) +
  annotate(geom = "text",
           x = .5, y = .2,
           label = "beta",
           size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(axis.line.x = element_line(size = 0.5))

## an annotated arrow
# save our custom arrow settings
my_arrow <- arrow(angle = 20, length = unit(0.35, "cm"), type = "closed")
p5 <-
  tibble(x     = c(.375, .625),
         y     = c(1/3, 1/3),
         label = c("'~'", "italic(s)")) %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = c(10, 7), parse = T, family = "Times") +
  geom_segment(x = 0.5, xend = 0.5,
               y = 1, yend = 0,
               arrow = my_arrow) +
  xlim(0, 1)

# bar plot of Bernoulli data
p6 <-
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
p7 <-
  tibble(x     = c(.35, .65),
         y     = c(1/3, 1/3),
         label = c("'~'", "italic(i)*'|'*italic(s)")) %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = c(10, 7), parse = T, family = "Times") +
  geom_segment(x = .5, xend = .5,
               y = 1, yend = 0,
               arrow = my_arrow) +
  xlim(0, 1)

# some text
p8 <-
  tibble(x     = .5,
         y     = .5,
         label = "italic(y[i])['|'][italic(s)]") %>% 
  
  ggplot(aes(x = x, y = y, label = label)) +
  geom_text(size = 7, parse = T, family = "Times") +
  xlim(0, 1)

layout <- c(
  area(t = 1, b = 2, l = 1, r = 1),
  area(t = 1, b = 2, l = 2, r = 2),
  area(t = 4, b = 5, l = 1, r = 1),
  area(t = 3, b = 4, l = 1, r = 2),
  area(t = 6, b = 6, l = 1, r = 1),
  area(t = 7, b = 8, l = 1, r = 1),
  area(t = 9, b = 9, l = 1, r = 1),
  area(t = 10, b = 10, l = 1, r = 1)
)

(p1 + p2 + p4 + p3 + p5 + p6 + p7 + p8) + 
  plot_layout(design = layout) &
  ylim(0, 1)
