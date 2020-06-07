# Script_DataWrangling 

# This script takes the Wellcome 2018 Global Monitor Data and formats and codes it ready for analysis 

# Load packages 
library(tidyverse)
library(here)
library(readxl)
library(readr)
library(expss)
library(hablar)



# Functions used 




# Download data in csv format from: https://wellcome.ac.uk/sites/default/files/wgm2018-dataset-crosstabs-all-countries.xlsx
# Before uploading separate the data tab from the data dictionary and crosstabs

# Data pathway and read-in data
path_to_data <- here("Data", "WGM_Data.xlsx")
WGM_raw <- read_excel(path_to_data)
WGM_raw <- as.data.frame(WGM_raw)

# Change variable types 
WGM_raw <- WGM_raw %>% 
  convert(fct(WP5, Q1:Q30, WGM_Indexr, ViewOfScience, AgeCategories:Urban_Rural, Regions_Report:EMP_2010),
          num(Age, wgt, PROJWT, WGM_Index, Household_Income, WBI),
          dte(FIELD_DATE)
  )

# Add variable labels 
WGM_raw <- apply_labels(WGM_raw, 
                        WP5 = "Country", 
                        wgt = "National weight", 
                        PROJWT = "Population weight",
                        FIELD_DATE = "Study completion date", 
                        YEAR_CALENDAR = "Year of survey", 
                        Q1 = "Knowledge of science", 
                        Q2 = "Understanding of science definition", 
                        Q3 = "Is studying disease part of science", 
                        Q4 = "Is writing poetry part of science", 
                        Q5A = "Primary science education", 
                        Q5B = "Secondary science education", 
                        Q5C = "Tertiary science education", 
                        Q6 = "Science information seeking/ 30 days", 
                        Q7 = "Medicine, disease health information seeking/ 30 days", 
                        Q8 = "Would like to know more about science", 
                        Q9 = "Would like to know more about medicine, disease, health", 
                        Q10A = "Confidence in NGOs", 
                        Q10B = "Confidence in hospitals and clinics", 
                        Q11A = "Trust in people in neighbourhood", 
                        Q11B = "Trust in national government", 
                        Q11C = "Trust in scientists", 
                        Q11D = "Trust in journalists", 
                        Q11E = "Trust in doctors and nurses", 
                        Q11F = "Trust in NGO staff", 
                        Q11G = "Trust in traditional healers", 
                        Q12 = "Trust in science", 
                        Q13 = "Trust in scientists' accuracy", 
                        Q14A = "Trust in scientists to benefit public", 
                        Q14B = "Trust in scientists to be financially open", 
                        Q15A = "Trust in companies to benefit public", 
                        Q15B = "Trust in companies to be financially open", 
                        Q16 = "Who the work of scientists benefits", 
                        Q17 = "If work of scientists benefits them", 
                        Q18 = "Benefit of science and technology for next generation", 
                        Q19 = "Impact of science and technology on jobs", 
                        Q20 = "Trust to give health advice", 
                        Q21 = "Trust in medcial and health advice from gov", 
                        Q22 = "Trust in medical and health advice from HCPs", 
                        Q23 = "Awareness of vaccines", 
                        Q24 = "Belief in importance of vaccines", 
                        Q25 = "Belief in safety of vaccines", 
                        Q26 = "Belief in effectiveness of vaccines", 
                        Q27 = "Have children", 
                        Q28 = "If any children received any vaccines", 
                        D1 = "Religion", 
                        Q29 = "Has science every disagreed with your religion", 
                        Q30 = "Prioritise science or religion", 
                        WGM_Index = "Trust in scientists index", 
                        WGM_Indexr = "Trust in scientsits index cat", 
                        ViewOfScience = "How a person views personal and social benefit of science", 
                        Age = "Age", 
                        AgeCategories = "Age cohort", 
                        Gender = "Gender", 
                        Education = "Educational background", 
                        Urban_Rural = "Area type", 
                        Household_Income = "Per capita income quintiles", 
                        Regions_Report = "World regions", 
                        WBI = "Country income level (World Bank)", 
                        Subjective_Income = "Subjective income", 
                        EMP_2010 = "Employment status"
                        )





