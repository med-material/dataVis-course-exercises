library(tidyverse)

script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)

ov<-read.csv("ov.csv")
