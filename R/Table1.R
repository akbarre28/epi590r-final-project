
#Setting up data and libraries
library(tidyverse)
library(gtsummary)
library(broom)

working_dir <- here::here("data")

readr::write_csv(
	covid_testing,
	fs::path(working_dir, "covid_testing.csv")
)

covid_testing_new <- covid_testing %>%
	mutate(total_tat = col_rec_tat + rec_ver_tat)


# Create a {gtsummary} table of descriptive statistics about your data (1 pt).

summary_table <- tbl_summary(
	covid_testing_new,
	by = 	result,
	include = c(
		gender, age, drive_thru_ind, pan_day, total_tat
		),
	label = list(
		gender ~ "Gender",
		age ~ "Age",
		drive_thru_ind ~ "Specimen collected via drive-thru",
		pan_day ~ "Days from start of pandemic to collection",
		total_tat ~ "Total specimen processing time (hrs)",
		),
	missing_text = "Missing") |>
	add_p() |>
	add_overall(col_label = "**Overall**") |>
	bold_labels()

summary_table



# Regression - Fit a regression and present well-formatted results from the regression (1 pt). The regression doesn’t have to be of any particular scientific interest, and you don’t have to interpret it in any particular way. You may use {broom} or {gtsummary} or both.


lm_model <- lm(age ~ strength + roast_level, data = coffee_survey, na.action=na.exclude)


# Create a figure (1 pt). It doesn’t need to look pretty; feel free to adapt some of the examples from class, which were as simple as hist(data$variable) and as complicated as the forest plot in the {broom} section.



# Write and use a function that does something with the data (1 pt). It could be as simple as, for example, a new function that you write by hand to calculate the standard deviation of a variable (like we did with the mean).




_______________________________________________________________

# Create and render a quarto document that includes at least:
		# The table, regression results, and figure, with appropriate captions (1 pt).
		# Inline R code in at least 2 places, 1 pulling a statistic from a table (i.e., using gtsummary::inline_text()) and 1 printing something else (like we did with the mean age in the example) (1 pt).
		# Cross-references to a table and a figure at least once each (1 pt).

# Use the {here} package every time you refer to file paths when reading in data and saving any files (1 pt). You must use it at least twice. Feel free to save any files that you create (e.g., your figure) in your code.

# Commit and push your work to GitHub as you go (1 pt).

# In a README file, include any notes necessary for us to easily reproduce your analysis (e.g., “Run script.R and then render document.qmd”) (1 pt). We should be able to make a minor change to the underlying data, then re-run the analysis to see how the change affects the results.


