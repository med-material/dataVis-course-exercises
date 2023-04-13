library(tidyverse)
load(file = "mini-project/data/aggData.rda")
dfx <- agg_dataNum

agg_dataNum %>%
  filter(
    isYearAgg == FALSE, isAngelKPI == TRUE,
    h_name == "General", nameOfAggr == "dnt_leq_60",
    is.na(subGroup), !is.na(YQ), year == "2019"
  ) %>%
  mutate(condColor = ifelse(is1stDiam > 0, "#7cd461", "#4299f5")) %>%
  ggplot(aes(x = YQ, y = Value, group = 1)) +
  geom_line(color = "#42aaf5") +
  geom_line(aes(y = C_Value), color = "grey") +
  geom_point(aes(size = data_Pts, color = condColor)) +
  scale_color_identity() +
  theme_minimal() +
  ylab("dnt less or equal to 60, in %.")


agg_dataNum %>%
  filter(
    isYearAgg == FALSE, isAngelKPI == TRUE,
    h_name == "General", nameOfAggr == "dnt_leq_60",
    subGroup == "gender", !is.na(YQ), year == "2019"
  ) %>%
  mutate(gender = factor(subGroupVal)) %>%
  ggplot(aes(x = YQ, y = Value, group = gender, colour = gender)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(aes(size = data_Pts), position = position_dodge(.2)) +
  theme_minimal() +
  ylab("dnt less or equal to 60, in %.")
