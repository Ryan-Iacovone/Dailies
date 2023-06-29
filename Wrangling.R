library(tidyverse)
library(ggthemes)
library(readxl)
library(writexl)
library(janitor) #Library for crosstab (tabyl) function
library(purrr) #Library for map function in ggplot
library(cowplot) #Combining ggplot outputs into 1 succinct version


dalies_orig <- read_excel("C:/Users/Ryan/Coding Projects/Dalies/Data/Dailies.xlsx")


dalies_fact <- dalies_orig %>% select(Date, Teeth,Shower, Acne)

#Converting the variables Teeth, Shower, and Acne into factor variables and then labeling them
dalies_fact$Teeth <- factor(dalies_fact$Teeth, levels=c("X", "Y", "XY"), labels=c("Morning", "Evening", "Both"))
dalies_fact$Shower <- factor(dalies_fact$Shower, levels=c("X", "XY"), labels=c("Normal", "Acne Shampoo"))
dalies_fact$Acne <- factor(dalies_fact$Acne, levels=c("X", "Y", "XY"), labels=c("Morning", "Evening", "Both"))


#Creating a new data set with only the variables we want to change into binary
dalies_binary_orig <- dalies_orig %>% select(-Date, -Teeth,-Shower, -Acne)

# define a function that takes a variable as input and returns a binary version of the variable
to_binary <- function(x) {
  
  # create a new variable with all values equal to 0 then rewriting over it for each new variable
  binary_var <- rep(0, length(x))
  
  # set the values of the new variable to 1 where x = "X"
  binary_var[x == "X"] <- 1
  
  # return the binary variable
  return(binary_var)
}

# apply the function to a list of variables in the dalies2 dataframe
binary_variables <- sapply(dalies_binary_orig, FUN = to_binary)

# change the binary_variables list into a dataframe
dalies_bin <- as.data.frame(binary_variables)

#getting rid of excess data sets
rm(dalies_binary_orig, binary_variables)


#Taking all my factor variables and changing them into binary variables 

fact_to_binary <- function(x) {
  
  # create a new variable with all values equal to 0 then rewriting over it for each new variable
  binary_var <- rep(0, length(x))
  
  # set the values of the new variable to 1 where x = "X"
  binary_var[x != 0] <- 1
  
  # return the binary variable
  return(binary_var)
}

fct_binary_variables_array <- sapply(dalies_fact, FUN = fact_to_binary)

dalies_fct_as_bin <- as.data.frame(fct_binary_variables_array)

dalies_fct_as_bin <- dalies_fct_as_bin %>% select(-Date)


# Rename factor variables to note that they're now binary 
dalies_fct_as_bin <- dalies_fct_as_bin %>%
  rename(Teeth_bin = Teeth,
         Shower_bin = Shower,
         Acne_bin = Acne)

rm(fct_binary_variables_array, dalies_orig)


#Creating a distinct data frame that contains all of the binary variables 
dalies_bin_all <- bind_cols(dalies_fct_as_bin, dalies_bin)

#combining the factor variables and the dummy variable groups together to dailies_final data frame
dalies_final <- bind_cols(dalies_fact, dalies_bin_all)

dalies_final$Date <- as.Date(dalies_final$Date)

#Extracting the week day from the newly minted date column
dalies_final$day <- weekdays(dalies_final$Date)

#Need to change the day variable to a factor variable to specify the order so that it appears nicely on the graph 
dalies_final$day <- factor(dalies_final$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Creating a new 'hidden' binary variable called Week_Num that = 1 every time the week day is Monday
dalies_final$Week_Num <- ifelse(dalies_final$day == "Monday", 1, 0)

#Using the newly created binary variable Week_Num to accumulate every time Monday appears thus creating the final variable "week_number" that accurately counts weeks
dalies_final$week_number <- cumsum(dalies_final$Week_Num)

#class(dalies_final$week_number)

#Adding the newly created variable week_number to my dalies_fact data frame to parse analysis for bar plots at end of code 
dalies_fact <- cbind(dalies_fact, dalies_final$week_number)

#THE FOLLOWING CODE WORKS BUT IT ONLY CAN BE RUN ONCE SINCE 
dalies_fact <- dalies_fact %>% rename(week_number = "dalies_final$week_number")


#Preemptively creating an empty list to store all of the dataframes in
Week_averages <- list()

#Getting the names of the binary variables from a previous data frame to input for our average analysis
variables_bin <- names(dalies_bin_all)

#Using a for loop that creates a new data frame for all binary variables that calculates the success rate of each variable by week
#The rename() function uses the := operator to assign the new name to the column, on the left side of the operator we use !!paste0(var,"Week average") to create the new name by concatenating var with the string "Week average" , this is passed as the new name for the column "mean_by_week
for (var in variables_bin){
  plz <- dalies_final %>% group_by(week_number) %>%
    summarise(mean_week = mean(!!sym(var))) %>%
    rename(!!paste0(var,"_Avg") := mean_week)
  Week_averages[[var]] <- plz
}

rm(plz)