library(here)
library(tidyverse)
tu<- read_table(here("exercises", "ws02-marks & channels", "uber", "nyuber.dat.csv"))
ggplot(tu, aes(x = uber, y = taxi)) +
  geom_point(aes(size = rides)) +
  geom_abline(slope = -1, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_size_area()
