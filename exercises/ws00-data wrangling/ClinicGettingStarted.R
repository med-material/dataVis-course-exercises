# Carry out the following tasks in this order:
# 1. update R, (three options here:
# https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol/
#, e.g. download from CRAN and select from where
# 2. install git & configure git in RStudio 
# Windows: https://git-scm.com/downloads
# Mac: https://jennybc.github.io/2014-05-12-ubc/ubc-r/session03_git.html

# from here on please execute these not commented out (lines not starting with #) code lines to setup your environment
# you can run through individual lines of code by either marking the row and when having the cursor in that line

# 3. install the tidyverse
install.packages("tidyverse")
#   to make sure that it's properly installed run and watch for errors 
library(tidyverse) 
# 4. install nycflights13 and load it for the exercises
install.packages("nycflights13")
library(nycflights)

# 5. install the Rstudio API for loading example data files more easily
install.packages("rstudioapi")
# 6. install the skimr package for some easy data summaries
install.packages("skimr") 

# 7. get a quick overview with different functions (some include visualizations)
skimr::skim(flights)
glimpse(flights)
view(flights) 
summary(flights)

# 8. create a new project (how-to) from this github repo
# https://github.com/med-material/dataVis-course-exercises
# see instructions here
# https://bit.ly/DV-CinlicSteps




