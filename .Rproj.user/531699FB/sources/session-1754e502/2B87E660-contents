

# CLEAN -------------------------------------------------------------------


rm(list = ls())  # Removes all objects from the global environment


# LIBRARIES ---------------------------------------------------------------


# Load required libraries
library(epiuf)

.nodisplay <- lapply(  c(    "dplyr"
                             , "readxl"   
                             , "tidyr"  
                             , "writexl"
                             , "writexl"
                             , "rmarkdown"
                             , "purrr"
                             , "ggplot2"
                             , "tidyr"
                             , "lubridate"
                             , "glue"
                             , "stringr"
                          
), loadLib) 


# PATH TO FILES -----------------------------------------------------------

# default to working directory (usually project directory)
setPath("ROOT" , getwd())

# Others paths derived from root 
## Set paths 
setPath("SCRIPTS", pathToFile("ROOT", "scripts"))  
setPath("SOURCES", pathToFile("ROOT", "sources"))  
setPath("FIGURES", pathToFile("ROOT", "figures"))  

## Correctly define paths
scripts_path <- pathToFile("SCRIPTS", "") 
source_path <- pathToFile("SOURCES", "") 
figures_path <- pathToFile("FIGURES", "") 

# Debugging: Print paths to check if they are correct
print(scripts_path)
print(source_path)
print(figures_path)




