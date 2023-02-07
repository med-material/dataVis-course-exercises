# 
# 

library(tidyverse)
mpg
as.data.frame(mpg)
 
glimpse(mpg)

library(skimr)
skim(mpg)

ggplot(mpg, aes(x = class, y = drv)) +
  geom_count()



mpg %>%
  count(class, drv) %>%
  ggplot(aes(x = class, y = drv)) +
  geom_tile(mapping = aes(fill = n))


ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = 1:234))


ggplot(mpg, aes(x = class, y = drv,colour = displ < 5)) +
  geom_point()

ggplot(mpg, aes(x = class, y = drv,colour = displ < 5)) +
  geom_point(stroke=4,colour="white")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), colour = "blue")


