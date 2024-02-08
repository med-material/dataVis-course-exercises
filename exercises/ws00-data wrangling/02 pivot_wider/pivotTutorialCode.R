# This file includes all code from the tutorial to save you time copy/pasting. 
# From the tutorial you should learn what the commands below do and why.
# The tutorial requires the package "dcldata" that you need to install first.
# You can execute a line of code below if the cursor is in it by CTRL+Enter (Windows/Linux) or CMD+Enter (Mac)

remotes::install_github("dcl-docs/dcldata") 
library(tidyverse)
library(dcldata)

example_eagle_nests

# here you find the complete code for the pivot. Read the tutorial for the individual lines below

example_eagle_nests %>% 
  pivot_longer(
    cols = c(`2007`, `2009`),
    names_to = "year",
    values_to = "num_nests"
  )

# pivot_wider example
# create the example_eagle_nests_tidy data frame with the above pivot_longer() code
# again the lines are explained in the tutorial
example_eagle_nests_tidy <- example_eagle_nests %>% 
  pivot_longer(
    cols = c(`2007`, `2009`),
    names_to = "year",
    values_to = "num_nests"
  )

# return the data into its original (untidy) format with pivot_wider. See the tutorial for the explanations of each line.

example_eagle_nests_tidy %>% 
  pivot_wider(
    names_from = year, 
    values_from = num_nests
  )

install.packages("tidycensus")
library("tidycensus")

example_acs_1

example_acs_1 %>% 
  distinct(variable)

example_acs_1 %>% 
  pivot_wider(names_from = variable, values_from = estimate)



