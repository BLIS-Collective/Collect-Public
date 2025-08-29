################################################################################
################### Fabric of Repair: Producing Report Tables ##################
################################################################################

# Created By: Christina Pao
# Last Update: 05/02/2025

# Updated By: Justin Morgan
# Last Update: 08/28/2025
# Update Note: Converting code file for public use
# Inputs: replication_swayable_2025.csv

# File Description: 
# read in cleaned public data file
# reproduce data tables from Fabric of Repair report

# Notes : Future update is to translate this into interactive QMD file for 
# output folder, as well as (potentially) update table production code

################################################################################
# Packages
library(tidyverse)
library(magrittr)
library(arsenal)
library(ggeffects)
#library(broom)
#library(dotwhisker)
#library(stargazer)
#library(showtext)
library(kableExtra)

# Data
df <- read_csv("data/replication_swayable_2025.csv")

############################ Table 1: Title of Table ###########################

# Create new data frame with Table 1 variables
df_table1 <- df %>%
  mutate(Race = race,
         `College-Educated` = education_binary,
         Income = case_when(What.is.the.total.annual.income.of.your.household.before.taxes. == "100-150-k" ~ "$100-150K",
                            What.is.the.total.annual.income.of.your.household.before.taxes. == "under-20-k" ~ "Under $20K",
                            What.is.the.total.annual.income.of.your.household.before.taxes. == "20-40-k" | What.is.the.total.annual.income.of.your.household.before.taxes. == "40-60k" ~ "$20-60K",
                            What.is.the.total.annual.income.of.your.household.before.taxes. == "60-80-k" | What.is.the.total.annual.income.of.your.household.before.taxes. == "80-100-k" ~ "$60-100K",
                            What.is.the.total.annual.income.of.your.household.before.taxes. == "150-200-k" | What.is.the.total.annual.income.of.your.household.before.taxes. == "over-200-k" ~ "Over $150K",
                            TRUE ~ NA),
         Income = factor(Income, levels = c("Under $20K", "$20-60K", "$60-100K", "$100-150K", "Over $150K")),
         `Party Affiliation` = case_when(Party == "democrat" ~ "Democrat",
                                         Party == "republican" ~ "Republican",
                                         TRUE ~ "Independent or Something Else"),
         Residence = case_when(Area == "city" ~ "City",
                               Area == "in-a-rural-area" ~ "Rural",
                               Area == "small-town-next-to-native-community-e-g-reservation" |
                                 Area == "small-town-not-next-to-native-community-e-g-reservation" ~ "Small Town",
                               TRUE ~ "Another location"),
         Residence = factor(Residence, levels = c("City", "Small Town", "Rural", "Another Location")),
         Region = case_when(Region..Midwest == T ~ "Midwest",
                            Region..Northeast == T ~ "Northeast",
                            Region..South == T ~ "South",
                            Region..West == T ~ "West"),
         `Religious Affiliation` = case_when(
           What.is.your.present.religion..if.any. == "atheist" ~ "Atheist",
           
           What.is.your.present.religion..if.any. == "catholic" ~ "Catholic",
           What.is.your.present.religion..if.any. == "evangelical-protestant" ~ "Evangelical Protestant",
           
           What.is.your.present.religion..if.any. == "mormon" ~ "Mormon",
           What.is.your.present.religion..if.any. == "muslim" ~ "Muslim",
           
           What.is.your.present.religion..if.any. == "other-protestant" ~ "Other Protestant",
           What.is.your.present.religion..if.any. == "nothing-in-particular" ~ "Not Religious",
           TRUE ~ "Other Religion"
         ),
         Religion = factor(`Religious Affiliation`, levels = c("Not Religious", "Atheist", "Catholic", "Evangelical Protestant", "Other Protestant", "Muslim",  "Other Religion")),
         `Religious Affiliation` = ifelse(Religion == "Not Religious", "Not Religious", "Religious")) %>%
  select(Age, Gender, Race, `College-Educated`, Income, `Party Affiliation`, Residence, Region, `Religious Affiliation`)

# Select functions used for the `arsenal` table
my_controls <- tableby.control(
  test = F,
  total = T,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)"
  )
)

# Create the table
table1 <- tableby(Race ~ ., data = df_table1, control = my_controls)

# To see the table in LaTeX, run this code in a .rmd or .qmd file and print to PDF/html
summary(table1)

############################ Table 2: Title of Table ###########################


# Create new dataframe, with Table 1 variables, subsample Indigenous population
df_table2 <- df %>%
  filter(race == "Indigenous") %>%
  mutate(Enrollment = enrollment_status,
         `Hawaiian Homes Beneficiary` = case_when(Are.you.a.beneficiary.of.the.Hawaiian.Homes.Commission.Act.of.1920..as.amended. == "yes" ~ "Yes",
                                                  Are.you.a.beneficiary.of.the.Hawaiian.Homes.Commission.Act.of.1920..as.amended. == "no" ~ "No",
                                                  Are.you.a.beneficiary.of.the.Hawaiian.Homes.Commission.Act.of.1920..as.amended. == "unsure" ~ "Unsure",
                                                  TRUE ~ NA),
         Residence = case_when(Area == "alaska-native-village" ~ "Alaska Native Village",
                               Area == "city" ~ "City",
                               Area == "hawaiian-homestead-through-department-of-hawaiian-home-lands" ~ "Hawaiian Homestead",
                               Area == "in-a-rural-area" ~ "Rural",
                               Area == "otsa-oklahoma-tribal-statistical-area" ~ "Oklahoma Tribal Statistical Area",
                               Area == "pueblo" ~ "Pueblo",
                               Area == "rancheria" ~ "Rancheria",
                               Area == "small-town-next-to-native-community-e-g-reservation" ~ "Small Town By Native Community",
                               Area == "small-town-not-next-to-native-community-e-g-reservation" ~ "Small Town Not By Native Community",
                               Area == "tribal-reservation" ~ "Tribal Reservation"),
         `Provided Tribal Affiliation` = tribal.affiliation.reported,
         Identity = case_when(Ethnicity..Alaska.Native == T & Ethnicity..American.Indian.Native.American == F & Ethnicity..First.Nations..Inuit.or.Metis == F & Ethnicity..Native.Hawaiian.Kanaka.Maoli == F & Ethnicity..Another.Native..ethnic..or.racial.group.not.identified.here == F ~ "Alaska Native",
                              Ethnicity..American.Indian.Native.American == T & Ethnicity..Alaska.Native == F & Ethnicity..First.Nations..Inuit.or.Metis == F & Ethnicity..Native.Hawaiian.Kanaka.Maoli == F & Ethnicity..Another.Native..ethnic..or.racial.group.not.identified.here == F 
                              ~ "American Indian, Native American",
                              Ethnicity..First.Nations..Inuit.or.Metis == T & Ethnicity..American.Indian.Native.American == F & Ethnicity..Alaska.Native == F & Ethnicity..Native.Hawaiian.Kanaka.Maoli == F & Ethnicity..Another.Native..ethnic..or.racial.group.not.identified.here == F ~ "First Nations, Inuit, Metis",
                              Ethnicity..Native.Hawaiian.Kanaka.Maoli == T & Ethnicity..First.Nations..Inuit.or.Metis == F & Ethnicity..American.Indian.Native.American == F & Ethnicity..Alaska.Native == F & Ethnicity..Another.Native..ethnic..or.racial.group.not.identified.here == F ~ "Native Hawaiian, Kanaka Maoli",
                              TRUE ~ "Other or Multiple Indigenous Identities"))

df_indigenous_table <- df_table2 %>%
  select(Identity, Enrollment, Residence, `Provided Tribal Affiliation`, `Hawaiian Homes Beneficiary`)

# Select functions used for the `arsenal` table
my_controls2 <- tableby.control(
  test = F,
  total = F,
  numeric.test = "kwt", cat.test = "chisq",
  numeric.stats = c("meansd"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)"
  )
)

# Create the table
table2 <- tableby(Identity ~ ., data = df_indigenous_table, control = my_controls2)

# To see the table in LaTeX, run this code in a .rmd or .qmd file and print to PDF/html
summary(table2)

############################ Table 3: Title of Table ###########################

# Creating a summary dataframe of baseline Reparations support by political party
df_baseline_party_reparations <- 
  df %>% 
  group_by(race, summarized_party) %>% 
  count(rep_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = rep_baseline_categorical) %>% 
  add_column(Movement = "Reparations")

# Creating a summary dataframe of baseline Land Back support by political party
df_baseline_party_landback <- 
  df %>% 
  group_by(race, summarized_party) %>% 
  count(landback_baseline_categorical) %>% 
  mutate(Percentage = n/sum(n) * 100) %>% 
  rename(baseline_support = landback_baseline_categorical) %>% 
  add_column(Movement = "Land Back")

# Combining both Reparations and Land Back dataframes
df_baseline_party <- bind_rows(df_baseline_party_reparations, df_baseline_party_landback)

# Cleaning the combined dataframe for a table
df_baseline_party_table <-
  df_baseline_party %>%
  filter(baseline_support == "Support") %>%
  select(-baseline_support)

# Creating table
table3 <- df_baseline_party_table %>%
  # Group by movement to create separate sections
  arrange(Movement, race, desc(summarized_party)) %>%
  select(-Movement) %>%
  # Create the formatted table
  kable(format = "html",
        col.names = c("Identity Group", "Political Affiliation", "Sample Size", "Support (%)"),
        caption = "Support for Reparations and Land Back Movements by Race and Political Affiliation",
        digits = 1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  # Add styling
  row_spec(0, bold = TRUE) %>%
  # Highlight Reparations and Land Back sections
  pack_rows("Support for Land Back", 1, 6, color = "#333333") %>%
  pack_rows("Support for Reparations", 7, 12, color = "#333333") %>%
  gsub("<table", "<table style='font-family: \"Jost-Regular\", sans-serif;'", .) %>%
  gsub("<caption>", "<caption style='font-weight: bold; font-size: 18px; margin-bottom: 10px;'>", .)

table3
