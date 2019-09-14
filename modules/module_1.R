
# Load required packages
library(readxl)

# Import dataset
FinData <- read_excel("~/WorldQuant/MScFE 610 Econometrics/Datasets/WQU_Econometrics_Module1_Data.xlsx", 
                        +     col_types = c("date", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric", 
                                            +         "numeric", "numeric", "numeric"))
View(FinData)

mean(FinData$MSFT)