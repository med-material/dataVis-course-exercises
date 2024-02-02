#execute these lines of code  
#Get the directory of the current script
script_path <- normalizePath("startingPointForMergeAndJoin.R")

# Set the working directory to the script's directory
setwd(dirname(script_path))

unzip("673598238_T_ONTIME_REPORTING.zip")
mydf <- read.csv("673598238_T_ONTIME_REPORTING.csv", 
                 sep = ",", quote="\"")
mylookup <- read.csv("L_UNIQUE_CARRIERS.csv_", 
                     quote="\"", sep = "," )

#Now use the code from the example/tutorial below

