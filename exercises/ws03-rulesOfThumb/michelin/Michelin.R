install.packages("esquisse")
library(tidyverse)
script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)

michelinStars <- read.csv("MichelinStars.csv")

esquisse::esquisser()


michelinStars <- michelinStars %>%
  mutate(MRestaurantsPerMillion = total / (population / 1000000), 
         MRestaurantsPer1ksqkm = total / (area / 1000)) %>%
  arrange(MRestaurantsPerMillion)
