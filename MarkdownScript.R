---
  title: "Codebook"
author: "Bandar Alsharhan"
date: "2024-02-26"
output: pdf_document

library(tidyverse)
library(knitr)
library(tinytex)
library(rmarkdown)
library(tibble)
library(readxl)
# Step 1- Opened data set

FH=read_xlsx("All_data_FIW_2013-2023.xlsx",sheet=2)

# Step 2- Pulling row 1 of the data as the column names

colnames(FH) <- as.character(FH[1, ])
FH<- FH[-1, ]

# Step 3 Create Data Set that only uses your desired columns, which are the
CL =aggregate score for the Civil Liberties category

FHnew <- subset(FH, select = c("Country/Territory",
"Region", "Edition", "Status", "CL rating", "D1", "D2", "D3", "D4", 
"E1", "E2", "E3", "E", "G1", "G2", "G3","CL"))
# Step 4 Renaming column Variables as desired and look at questions on 
#FreedomHouse Website
FHnew <- FHnew |>
  rename(indp_media = D1,
         Religous_Freedom = D2,
         academic_freedom = D3,
         personal_views = D4,
         freedom_assembly = E1,
         freedom_nongovernmental_organizations = E2,
         freedom_tradeunions = E3,
         Freedom_movement = G1,
         Right_own_property = G2,
         Personal_social_freedoms = G3)
#Step 5 Refine Sample Size so that includes countries only in Middle East 
FHnew_middle_east <- FHnew |>
  filter(Region == "Middle East" & Edition >= 2013 & Edition <= 2023)
FHnew_grouped <- FHnew_middle_east |>
  group_by(`Country/Territory`)
#Step 6, Change status column so that it spells out Free, Partly Free, Not Free
FHnew_middle_east <- FHnew_middle_east |>
  mutate(Status = case_when(
    Status == "F" ~ "Free",
    Status == "PF" ~ "Partly Free",
    Status == "NF" ~ "Not Free",
    TRUE ~ Status))
summary()
# Step 7 summary statistics and missing values
missing_values <- is.na(FHnew_middle_east)
summary(missing_values)
summary(FHnew_middle_east)
# Create Averages for countries for years 2013 to 2023
averages <-  subset(FHnew_middle_east, select = c(1,3,4))

FHnew_middle_east <- FHnew_middle_east |>
  pivot_longer(cols = starts_with("Edition"), 
               names_to = "Year", 
               values_to = "Value")
# Define a function to create a codebook table for a categorical variable
#created tables for all variables using codes below as example

numerical_frequency_table <- function(FHnew_middle_east, CL) {
  data |>
    count({{ variable_name }}) |>
    arrange({{ variable_name }})
}
numerical_frequency_table <- function(FHnew_middle_east, indp_media) {
  data |>
    count({{ variable_name }}) |>
    arrange({{ variable_name }})
}

numerical_frequency_table <- function(FHnew_middle_east, Religous_Freedom) {
  data |>
    count({{ variable_name }}) |>
    arrange({{ variable_name }})
}



numerical_frequency_table <- function(FHnew_middle_east, personal_views) {
  data |>
    count({{ variable_name }}) |>
    arrange({{ variable_name }})
}


cbfactor = function(.data, x) {
  x = enquo(x)
  count(.data, !!x) |> 
    mutate(
      values = as.numeric(!!x), 
      labels = as_factor(!!x), 
      freq = n, 
      perc = n/ sum(n) *100, 
      .keep = 'unused'
    )  |> 
    knitr::kable(format = 'pipe', digits = 1L)
}


write.csv(FHnew_middle_east, "FHnew_middle_east.csv")



