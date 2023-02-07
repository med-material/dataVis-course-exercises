if (!require("remotes")) {
  install.packages("remotes")
}
library(remotes)
#voice input package heyshiny
install_github("jcrodriguez1989/heyshiny", dependencies = TRUE)
install_github("gexijin/RTutor")