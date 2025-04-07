library(dplyr)
library(tidyr)

# load in combined clean data
data <- readRDS("combined-survey-data.rds")

# keep original provider type variable
data$k2q35d_original <- data$k2q35d

# convert the provider type to numeric before reducing to 3 categories
data$k2q35d <- as.numeric(data$k2q35d)


# convert factor variables to numeric for ease of creating compound variables
# these variables create the functional impairment column
data$sc_k2q16 <- as.numeric(data$sc_k2q16)
data$sc_k2q17 <- as.numeric(data$sc_k2q17)
data$sc_k2q18 <- as.numeric(data$sc_k2q18)

# these variables create the diagnosis confounding conditions variable
data$birthwt <- as.numeric(data$birthwt)
data$k2q05 <- as.numeric(data$k2q05)
data$k2q43b <- as.numeric(data$k2q43b)
data$blindness <- as.numeric(data$blindness)
data$downsyn <- as.numeric(data$downsyn)

# these variables create the race4 category
data$sc_hispanic_r <- as.numeric(data$sc_hispanic_r)
data$sc_race_r <- as.numeric(data$sc_race_r)

# copy age of diagnosis
data$k2q35a_1_years_numeric <- data$k2q35a_1_years







# create the functional impairment compound variable
data$func <- ifelse(data$sc_k2q16 == 1 & data$sc_k2q17 == 1 & data$sc_k2q18 == 1, 1, 2)

# convert "func" to a factor with appropriate labels for 1 and 2
data$func <- factor(data$func, levels = c(1, 2), labels = c("Yes", "No"))


# create the diagnosis confounding conditions compound variable
data <- data %>%
  mutate(
    # create a helper variable to count NA values across the specified columns
    na_count = rowSums(is.na(select(., birthwt, k2q05, k2q43b, blindness, downsyn))),
    
    # adjust the if/else statement to handle NA count conditions
    diag_conf_cond = if_else(
      birthwt == 1 | k2q05 == 1 | k2q43b == 1 | blindness == 1 | downsyn == 1,
      1,  # Set 1 for Yes
      NA  # Keep NA
    )
  ) %>%
  mutate(
    # no or NA
    diag_conf_cond = if_else(
      is.na(diag_conf_cond) & na_count <= 1,  # missing two or more questions count as missing NA
      # missing one will still count as a no
      2,  # Set 2 for no
      diag_conf_cond  # K=keep the existing value (either 1 or NA)
    )
  ) %>%
  # drop the helper variable if it's no longer needed
  select(-na_count)

# convert "diag_conf_cond" column to a factor with levels
data$diag_conf_cond <- factor(data$diag_conf_cond, levels = c(1,2), labels = c("Yes", "No"))




# init the new race column with NA as the default value
data$race4 <- NA

# apply the conditions for race/ethnicity categories from CAHMI nodebooks
data$race4 <- ifelse(data$sc_hispanic_r == 2 & data$sc_race_r == 1, 2, data$race4)  # White, non-Hispanic
data$race4 <- ifelse(data$sc_hispanic_r == 2 & data$sc_race_r == 2, 3, data$race4)  # Black, non-Hispanic
data$race4 <- ifelse(data$sc_hispanic_r == 2 & data$sc_race_r == 3, 4, data$race4)  # Other/Multi-racial, non-Hispanic

data$race4 <- ifelse(data$sc_hispanic_r == 1, 1, data$race4)  # Hispanic



# make sure any missing data (NA in sc_hispanic_r or sc_race_r) stays NA in race4
data$race4 <- ifelse(is.na(data$sc_hispanic_r) | is.na(data$sc_race_r), NA, data$race4)

# make race4 a factor with the four categories for clarity
data$race4 <- factor(data$race4, levels = c(1, 2, 3, 4),
                     labels = c("Hispanic", "White, non-Hispanic", "Black, non-Hispanic", 
                                "Other/Multi-racial, non-Hispanic"))

# collapse provider type variable
data$k2q35d <- ifelse(data$k2q35d == 4, 3, data$k2q35d)
data$k2q35d <- ifelse(data$k2q35d == 6, 3, data$k2q35d)
data$k2q35d <- ifelse(data$k2q35d == 5, 2, data$k2q35d)
data$k2q35d <- ifelse(data$k2q35d == 7, 3, data$k2q35d)

table(data$k2q35d)

# set provider type variable to factor
data$k2q35d <- factor(data$k2q35d, levels = c(1, 2, 3),
                      labels = c("Primary care provider", "Medical Specialist", "Other health care provider"))


table(data$k2q35d)



data$survey_diagnosis_gap = data$sc_age_years - data$k2q35a_1_years_numeric
data$diagnosis_year = data$year - pmax(data$survey_diagnosis_gap, 0)



# adjust weights by number of years as per the census guidelines
# dividing by the number of years in the study
data$adjusted_weights <- data$fwc / 11


# make sure stratum is a factor
data$stratum <- factor(data$stratum)
# make sure year is a factor
data$year <- factor(data$year)


# create the povlev4 variable based on the ranges provided
data <- data %>%
  mutate(povlev4 = case_when(
    fpl_i1 > 0 & fpl_i1 <= 99 ~ 1,
    fpl_i1 >= 100 & fpl_i1 <= 199 ~ 2,
    fpl_i1 >= 200 & fpl_i1 <= 399 ~ 3,
    fpl_i1 >= 400 ~ 4
  ))

# apply labels
data$povlev4 <- factor(data$povlev4, levels = 1:4, 
                       labels = c("0-99% FPL", "100%-199% FPL", "200%-399% FPL", "400% FPL or greater"))

# Relevel race4 so that "White, non-Hispanic" is the reference
data$race4 <- relevel(data$race4, ref = "White, non-Hispanic")

data$providers_binary <- as.numeric(data$k2q35d == "Primary care provider")

# create a unique numeric cluster variable by combining fipsst, year, and stratum
data$clusters <- as.numeric(interaction(data$year, data$fipsst, data$stratum)) #data$year, 


# our list of vars we want to check
vars_to_check <- c("year", "state", "hhid", "stratum", "adjusted_weights", "fipsst", "clusters", "diagnosis_year", "sc_sex", "k2q35d", "providers_binary", 
                   "k2q35a_1_years_numeric", "race4", "hhlanguage", "func", 
                   "diag_conf_cond", "povlev4")

# check and print the number of NAs for each variable before final output
sapply(vars_to_check, function(var) sum(is.na(data[[var]])))


# scale diagnosis year
data$diagnosis_year_scaled <- data$diagnosis_year - 2012


# subset to the final desired variables anddrop NA
data_2013_2023_final_vars <- data %>%
  filter(as.numeric(diagnosis_year) > 2012) %>%  # make sure diagnosis_year is numeric
  select(year, hhid, stratum, adjusted_weights, state, fipsst, clusters, diagnosis_year, diagnosis_year_scaled, 
         k2q35d, k2q35d_original, providers_binary, k2q35a_1_years_numeric, race4, povlev4, sc_sex) %>% # , sc_sex, hhlanguage, func, diag_conf_cond
  na.omit()

# save output
saveRDS(data_2013_2023_final_vars, "data-2013-2023-final-vars.rds")
