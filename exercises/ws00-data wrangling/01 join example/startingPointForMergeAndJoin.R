#execute these lines of code
#Get the directory of the current script to get through the first three commands from the tutorial
script_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_path)
# the first three commands are pasted here for convenience
unzip("673598238_T_ONTIME_REPORTING.zip")
mydf <- read.csv("673598238_T_ONTIME_REPORTING.csv", 
                 sep = ",", quote="\"")
mylookup <- read.csv("L_UNIQUE_CARRIERS.csv_", 
                     quote="\"", sep = "," )

# from here onward  use the code from the example/tutorial 
# 



