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

# Code values 
# Countries
WGM_raw$WP5 <- recode_factor(WGM_raw$WP5,
                             "1"="United States",
                             "2"="Egypt",
                             "3"="Morocco",
                             "4"="Lebanon",
                             "5"="Saudi Arabia",
                             "6"="Jordan",
                             "8"="Turkey",
                             "9"="Pakistan",
                             "10"="Indonesia",
                             "11"="Bangladesh",
                             "12"="United Kingdom",
                             "13"="France",
                             "14"="Germany",
                             "15"="Netherlands",
                             "16"="Belgium",
                             "17"="Spain",
                             "18"="Italy",
                             "19"="Poland",
                             "20"="Hungary",
                             "21"="Czech Republic",
                             "22"="Romania",
                             "23"="Sweden",
                             "24"="Greece",
                             "25"="Denmark",
                             "26"="Iran",
                             "28"="Singapore",
                             "29"="Japan",
                             "30"="China",
                             "31"="India",
                             "32"="Venezuela",
                             "33"="Brazil",
                             "34"="Mexico",
                             "35"="Nigeria",
                             "36"="Kenya",
                             "37"="Tanzania",
                             "38"="Israel",
                             "39"="Palestinian Territories",
                             "40"="Ghana",
                             "41"="Uganda",
                             "42"="Benin",
                             "43"="Madagascar",
                             "44"="Malawi",
                             "45"="South Africa",
                             "46"="Canada",
                             "47"="Australia",
                             "48"="Philippines",
                             "49"="Sri Lanka",
                             "50"="Vietnam",
                             "51"="Thailand",
                             "52"="Cambodia",
                             "53"="Laos",
                             "54"="Myanmar",
                             "55"="New Zealand",
                             "57"="Botswana",
                             "60"="Ethiopia",
                             "61"="Mali",
                             "62"="Mauritania",
                             "63"="Mozambique",
                             "64"="Niger",
                             "65"="Rwanda",
                             "66"="Senegal",
                             "67"="Zambia",
                             "68"="South Korea",
                             "69"="Taiwan",
                             "70"="Afghanistan",
                             "71"="Belarus",
                             "72"="Georgia",
                             "73"="Kazakhstan",
                             "74"="Kyrgyzstan",
                             "75"="Moldova",
                             "76"="Russia",
                             "77"="Ukraine",
                             "78"="Burkina Faso",
                             "79"="Cameroon",
                             "80"="Sierra Leone",
                             "81"="Zimbabwe",
                             "82"="Costa Rica",
                             "83"="Albania",
                             "84"="Algeria",
                             "87"="Argentina",
                             "88"="Armenia",
                             "89"="Austria",
                             "90"="Azerbaijan",
                             "96"="Bolivia",
                             "97"="Bosnia and Herzegovina",
                             "99"="Bulgaria",
                             "100"="Burundi",
                             "103"="Chad",
                             "104"="Chile",
                             "105"="Colombia",
                             "106"="Comoros",
                             "108"="Republic of Congo",
                             "109"="Croatia",
                             "111"="Cyprus",
                             "114"="Dominican Republic",
                             "115"="Ecuador",
                             "116"="El Salvador",
                             "119"="Estonia",
                             "121"="Finland",
                             "122"="Gabon",
                             "124"="Guatemala",
                             "125"="Guinea",
                             "128"="Haiti",
                             "129"="Honduras",
                             "130"="Iceland",
                             "131"="Iraq",
                             "132"="Ireland",
                             "134"="Ivory Coast",
                             "137"="Kuwait",
                             "138"="Latvia",
                             "140"="Liberia",
                             "141"="Libya",
                             "143"="Lithuania",
                             "144"="Luxembourg",
                             "145"="Macedonia",
                             "146"="Malaysia",
                             "148"="Malta",
                             "150"="Mauritius",
                             "153"="Mongolia",
                             "154"="Montenegro",
                             "155"="Namibia",
                             "157"="Nepal",
                             "158"="Nicaragua",
                             "160"="Norway",
                             "163"="Panama",
                             "164"="Paraguay",
                             "165"="Peru",
                             "166"="Portugal",
                             "173"="Serbia",
                             "175"="Slovakia",
                             "176"="Slovenia",
                             "183"="Eswatini",
                             "184"="Switzerland",
                             "185"="Tajikistan",
                             "186"="The Gambia",
                             "187"="Togo",
                             "190"="Tunisia",
                             "191"="Turkmenistan",
                             "193"="United Arab Emirates",
                             "194"="Uruguay",
                             "195"="Uzbekistan",
                             "197"="Yemen",
                             "198"="Kosovo",
                             "202"="Northern Cyprus"
                             )


#Q1 
WGM_raw$Q1 <- recode_factor(WGM_raw$Q1,
                            "1"="A lot",
                            "2"="Some",
                            "3"="Not much",
                            "4"="Nothing at all",
                            "98"="(DK)",
                            "99"="(Refused)"
                            )

#Q2
WGM_raw$Q2 <- recode_factor(WGM_raw$Q2,
                            "1"="All of it",
                            "2"="Some of it",
                            "3"="Not much of it",
                            "4"="Nothing at all",
                            "98"="(DK)",
                            "99"="(Refused)"
)


#Q16 
WGM_raw$Q16 <- recode_factor(WGM_raw$Q16,
                             "1"="Most",
                             "2"="Some",
                             "3"="Very few",
                             "98"="(DK)",
                             "99"="(Refused)"
)

#Q19 
WGM_raw$Q19 <- recode_factor(WGM_raw$Q19,
                             "1"="Increase",
                             "2"="Decrease",
                             "3"="Neither/Have no effect",
                             "98"="(DK)",
                             "99"="(Refused)"
)

#Q20 
WGM_raw$Q20 <- recode_factor(WGM_raw$Q20,
                             "1"="Your family and friends",
                             "2"="A religious leader",
                             "3"="A doctor or nurse",
                             "4"="A famous person",
                             "5"="Traditional healer",
                             "97"="(None of these/Someone else)",
                             "98"="(DK)",
                             "99"="(Refused)"
)

#Q27 
WGM_raw$Q27 <- recode_factor(WGM_raw$Q27,
                             "1"="Yes",
                             "2"="No",
                             "3"="(Yes, but no longer living)",
                             "4"="(DK)",
                             "5"="(Refused)"
)

#D1
WGM_raw$QD1 <- recode_factor(WGM_raw$QD1,
                             "1"="Named a specific religion",
                             "2"="Secular/Non-religious",
                             "99"="(DK)"
                             )

#Q30 
WGM_raw$Q30 <- recode_factor(WGM_raw$Q30,
                             "1"="Science",
                             "2"="The teachings of your religion",
                             "97"="(It depends)",
                             "98"="(DK)",
                             "99"="(Refused)"
)

#WGM_Indexr
WGM_raw$WGM_Indexr <- recode_factor(WGM_raw$WGM_Indexr,
                                    "1"= "Low trust",
                                    "2" = "Medium trust",
                                    "3"="High trust",
                                    "99"="No score"
)

# ViewofScience 
WGM_raw$ViewOfScience <- recode_factor(WGM_raw$ViewOfScience,
                                       "1"="Enthusiast",
                                       "2"="Included",
                                       "3"="Excluded",
                                       "4"="Sceptic",
                                       "99"="Did not answer one of two questions"
)

#AgeCategories 
WGM_raw$AgeCategories <- recode_factor(WGM_raw$AgeCategories,
                                       "1"="15 to 29",
                                       "2"="30 to 49",
                                       "3"="50+"
)

#Gender 
WGM_raw$Gender <- recode_factor(WGM_raw$Gender,
                                "1"="Male",
                                "2"="Female"
)

#Education
WGM_raw$Education <- recode_factor(WGM_raw$Education,
                                   "1"="Primary",
                                   "2"="Secondary",
                                   "3"="Tertiary"
)

#Urban_Rural 
WGM_raw$Urban_Rural <- recode_factor(WGM_raw$Urban_Rural,
                                     "1"="Lives in rural area or small town",
                                     "2"="Lives in city or suburb of city"
)

#Household Income 
WGM_raw$Household_Income <- recode_factor(WGM_raw$Household_Income,
                                          "1"="Poorest 20%",
                                          "2"="Second 20%",
                                          "3"="Middle 20%",
                                          "4"="Fourth 20%",
                                          "5"="Top 20%"
)



#Regions
WGM_raw$Regions_Report <- recode_factor(WGM_raw$Regions_Report,
                                        "0"="Not assigned",
                                        "1"="Eastern Africa",
                                        "2"="Central Africa",
                                        "3"="North Africa",
                                        "4"="Southern Africa",
                                        "5"="Western Africa",
                                        "6"="Central America and Mexico",
                                        "7"="Northern America",
                                        "8"="South America",
                                        "9"="Central Asia",
                                        "10"="East Asia",
                                        "11"="Southeast Asia",
                                        "12"="South Asia",
                                        "13"="Middle East",
                                        "14"="Eastern Europe",
                                        "15"="Northern Europe",
                                        "16"="Southern Europe",
                                        "17"="Western Europe",
                                        "18"="Aus/NZ"
                                        )

#WBI 
WGM_raw$WBI <- recode_factor(WGM_raw$WBI,
                             "1"="Low income",
                             "2"="Lower Middle income",
                             "3"="Upper Middle income",
                             "4"="High income"
)

#Subjective income 
WGM_raw$Subjective_Income <- recode_factor(WGM_raw$Subjective_Income,
                                           "1"="Living comfortably by on present income",
                                           "2"="Getting by on present income",
                                           "3"= "Finding it difficult/very difficult to get by on present income"
)

# Employment status 
WGM_raw$EMP_2010 <- recode_factor(WGM_raw$EMP_2010,
                                  "1"="Employed full time for an employer",
                                  "2"="Employed full time for self",
                                  "3"="Employed part time but do not want full time",
                                  "4"="Unemployed",
                                  "5"="Employed part time want full time",
                                  "6"="Out of workforce"
)

