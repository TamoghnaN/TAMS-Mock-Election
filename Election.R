# Do install.packages("readr") if you don't have the package installed
library(dplyr)
library(readr)
library(tidyr)

# Read the election data from the CSV file
election <- read_csv("ElectionResults.csv")

#Change rank to integer
election <- election %>%
  mutate(R = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", R))) %>%
  mutate(D = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", D))) %>%
  mutate(L = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", L))) %>%
  mutate(G = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", G))) %>%
  mutate(Adithi = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", Adithi))) %>%
  mutate(Srinjoy = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", Srinjoy))) %>%
  mutate(Tracy = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", Tracy))) %>%
  mutate(Hunter = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", Hunter))) %>%
  mutate(Archith = as.integer(gsub("([0-9]+)(st|nd|rd|th)", "\\1", Archith)))
# Define isSenior and MF columns based on wing information
election <- election %>%
  mutate(
    isSenior = case_when(
      Wing %in% c("C100", "A100", "D100A", "D100B", "D200A", "D200B", "D300A", "D300B") ~ TRUE,
      Wing %in% c("A200", "C200A", "C200B", "C300A", "C300B", "B200", "A300", "B300") ~ FALSE,
      Wing == "Other" ~ sample(c(TRUE, FALSE), size = n(), replace = TRUE)
    ),
    MF = case_when(
      Wing %in% c("A100", "C200A", "C200B", "B200", "A200", "D200A", "D200B") ~ "F",
      Wing %in% c("C100", "D100A", "D100B", "A300", "B300", "C300A", "C300B", "D300A", "D300B") ~ "M",
      Wing == "Other" ~ sample(c("M", "F"), size = n(), replace = TRUE)
    )
  )

# Summarize counts of seniors, juniors, guys, and girls
summary_counts <- election %>%
  summarise(
    num_seniors = sum(isSenior == TRUE, na.rm = TRUE),
    num_juniors = sum(isSenior == FALSE, na.rm = TRUE),
    num_guys = sum(MF == "M", na.rm = TRUE),
    num_girls = sum(MF == "F", na.rm = TRUE)
  )
num_seniors = summary_counts$num_seniors
num_juniors = summary_counts$num_juniors
num_guys = summary_counts$num_guys
num_girls = summary_counts$num_girls

issues_long <- election %>%
  separate_rows(Issues, sep = ",\\s*") %>%  # Split issues by comma and remove extra spaces
  mutate(Issues = trimws(Issues))  # Trim any leading/trailing whitespace from issue names

issue_counts_class <- issues_long %>%
  group_by(Issues) %>%
  summarise(
    Senior_Frequency = round(100 * sum(isSenior == TRUE, na.rm = TRUE) / num_seniors, 1),   # Count of seniors
    Junior_Frequency = round(100 * sum(isSenior == FALSE, na.rm = TRUE) / num_juniors, 1),  # Count of juniors
    .groups = "drop" 
  )
# Count the occurrences of each issue by gender (MF)
issue_counts_gender <- issues_long %>%
  group_by(Issues) %>%
  summarise(
    Guys_Frequency = round(100 * sum(MF == "M", na.rm = TRUE) / num_guys, 1),   # Count of males
    Girls_Frequency = round(100 * sum(MF == "F", na.rm = TRUE) / num_girls, 1), # Count of females
    .groups = "drop" 
  )


# Print the issue counts
View(issue_counts_class)
View(issue_counts_gender)

# Reshape the party ranking columns (R, D, L, G) from wide to long format
ranking_long <- election %>%
  pivot_longer(cols = c(R, D, L, G), 
               names_to = "party", 
               values_to = "rank") %>%
  filter(!is.na(rank))  # Remove rows where rank is NA

# Step 1: Analyze average rank for each party by Senior vs. Junior and Guys vs. Girls

# Senior vs. Junior average ranks
senior_junior_rankings <- ranking_long %>%
  group_by(isSenior, party) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(isSenior, avg_rank)

# Guys vs. Girls average ranks
gender_rankings <- ranking_long %>%
  group_by(MF, party) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(MF, avg_rank)

# Step 2: Analyze FPTP election choices for President and Senator

# NOTE: To replicate for the other elections, simply change the cols variable in the code below to
# include the other election positions (e.g., "President", "Senator", "Texas_House64", etc.)
# Then, in the reformat step, change the filter for the position (names_to) to the desired election position.
# Finally, rename the tables accordingly for clarity in the output.

# Senior vs. Junior FPTP choices
senior_junior_fptp <- election %>%
  pivot_longer(cols = c(President, Senator), 
               names_to = "position", 
               values_to = "choice") %>%
  group_by(position, isSenior, choice) %>%
  summarise(vote_count = n(), .groups = "drop") %>%
  arrange(position, isSenior, desc(vote_count))

# Guys vs. Girls FPTP choices
gender_fptp <- election %>%
  pivot_longer(cols = c(President, Senator), 
               names_to = "position", 
               values_to = "choice") %>%
  group_by(position, MF, choice) %>%
  summarise(vote_count = n(), .groups = "drop") %>%
  arrange(position, MF, desc(vote_count))

# Reformat into tables for display (Senior vs. Junior President election)
presidential_fptp_class <- senior_junior_fptp %>%
  filter(position == "President") %>%  # Filter for Presidential position only
  group_by(choice, isSenior) %>%        # Group by candidate and Senior status
  summarise(vote_count = sum(vote_count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = isSenior, value = vote_count, fill = 0) %>%  # Spread to wide format
  rename(
    Senior = `TRUE`,   # Rename for clarity
    Junior = `FALSE`
  )

# Reformat into tables for display (Guys vs. Girls President election)
presidential_fptp_gender <- gender_fptp %>%
  filter(position == "President") %>%  # Filter for Presidential position only
  group_by(choice, MF) %>%        # Group by candidate and Senior status
  summarise(vote_count = sum(vote_count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = MF, value = vote_count, fill = 0) %>%  # Spread to wide format
  rename(
    Guys = `M`,   # Rename for clarity
    Girls = `F`
  )

# Reformat into tables for display (Senior vs. Junior Senate election)
senate_fptp_class <- senior_junior_fptp %>%
  filter(position == "Senator") %>%  # Filter for Presidential position only
  group_by(choice, isSenior) %>%        # Group by candidate and Senior status
  summarise(vote_count = sum(vote_count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = isSenior, value = vote_count, fill = 0) %>%  # Spread to wide format
  rename(
    Senior = `TRUE`,   # Rename for clarity
    Junior = `FALSE`
  )

# Reformat into tables for display (Guys vs. Girls Senate election)
senate_fptp_gender <- gender_fptp %>%
  filter(position == "Senator") %>%  # Filter for Presidential position only
  group_by(choice, MF) %>%        # Group by candidate and Senior status
  summarise(vote_count = sum(vote_count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = MF, value = vote_count, fill = 0) %>%  # Spread to wide format
  rename(
    Guys = `M`,   # Rename for clarity
    Girls = `F`
  )

# Display results
list(
  "Senior vs Junior Average Party Rankings" = senior_junior_rankings,
  "Guys vs Girls Average Party Rankings" = gender_rankings,
  "Senior vs Junior President Choices" = presidential_fptp_class,
  "Senior vs Junior Senate Choices" = senate_fptp_class,
  "Guys vs Girls President Choices" = presidential_fptp_gender,
  "Senior vs Junior Senate Choices" = senate_fptp_gender
  
)
