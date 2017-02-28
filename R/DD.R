library(tidyverse)
library(magrittr)

DD1 <- function() {
# make example plots
df <- data_frame(month = rep(1:12,3), 
     dufb = c(rep("no",12), rep("counterfactual",12), rep("yes",12)))

# create data
shock <- function(x, c=2, m=0) x + c + rnorm(1, m, 1)

set.seed(1992)
y1 <- runif(12, 5, 12)
y1 <- rev(y1)
y2 <- sapply(y1, shock) # counterfactual
y3 <- c(y2[1:8], sapply(y1[9:12], shock, c = 3, m = 2))
d <- y2[8] - y1[8]
y2 <- c(rep(NA,7), y1[8:12] + d)
df %<>%
  mutate(dollars = c(y1,y2,y3))

# plot
clrs <- scales::hue_pal()(2)

ggplot(data = df, aes(x = month, y = dollars)) +
  geom_line(aes(linetype = dufb, color = dufb)) +
  scale_linetype_manual(values = c("no" = 1, "yes" = 1, "counterfactual" = 3)) +
  scale_color_manual(values = c("no" = clrs[1], "yes" = clrs[2], "counterfactual" = clrs[2])) +
  scale_x_continuous(breaks = seq(0,13, by=2)) +
  theme_classic() +
  theme(legend.position="bottom") +
  geom_vline(xintercept = 8, linetype = 2, color = 'gray') +
  geom_text(aes(8, 16, label = ' post', hjust = -.2), size = 3, color = 'gray') +
  geom_text(aes(8, 16, label = 'pre ', hjust = 1.2), size = 3, color = 'gray')

}
