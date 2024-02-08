script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)
tu <- read.table("nyuber.dat", head = TRUE)
ggplot(tu, aes(x = uber, y = taxi)) +
  geom_point(aes(size = rides)) +
  geom_abline(slope = -1, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_size_area()

