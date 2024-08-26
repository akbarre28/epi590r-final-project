#Setting up data and libraries
library(tidyverse)
library(gtsummary)
library(broom)

getwd()
working_dir <- here::here("data")

trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

readr::write_csv(
	trashwheel_collection,
	fs::path(working_dir, "trashwheel_collection.csv")
)

# ____________________________________________________________

# Create a {gtsummary} table of descriptive statistics about your data (1 pt).

summary_table <- tbl_summary(
	trashwheel,
	by = Name,
	include = c(
		PlasticBottles, Polystyrene, CigaretteButts, GlassBottles,
		PlasticBags, Wrappers, SportsBalls
	),
	label = list(
		PlasticBottles ~ "Plastic Bottles",
		Polystyrene ~ "Polystyrene Items",
		CigaretteButts ~ "Cigarette Butts",
		GlassBottles ~ "Glass Bottles",
		PlasticBags ~ "Plastic Bags",
		Wrappers ~ "Wrappers",
		SportsBalls ~ "Sports Balls"
	),
	missing_text = "Missing"
) |>
	modify_header(label ~ "**Trash Collected**") |>
	modify_caption("**Table 1. Trash Wheel Collection Data**") |>
	modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Trash Wheel Name**") |>
	add_p(pvalue_fun = label_style_pvalue(digits = 2)) |>
	add_overall(col_label = "**Overall**") |>
	bold_labels()

summary_table




	# Regression - Fit a regression and present well-formatted results from the regression (1 pt). The regression doesn’t have to be of any particular scientific interest, and you don’t have to interpret it in any particular way. You may use {broom} or {gtsummary} or both.

# regression of no. of homes pwered based on each ton of trash collected (y) a series of predictor (x) variables
tbl_uvregression(
	trashwheel,
	y = HomesPowered,
	include = c(
		PlasticBottles, Polystyrene, CigaretteButts, GlassBottles,
		PlasticBags, Wrappers, SportsBalls
	),
	method = lm
)



	# Create a figure (1 pt). It doesn’t need to look pretty; feel free to adapt some of the examples from class, which were as simple as hist(data$variable) and as complicated as the forest plot in the {broom} section.

ggplot(trashwheel, aes(x = as.factor(Year), y = HomesPowered)) +
	geom_bar(stat = "identity", fill = "skyblue") +
	labs(title = "Homes Powered Over the Years",
			 x = "Year",
			 y = "Number of Homes Powered") +
	annotate("text", x = Inf, y = -Inf, label = "Note: Each ton of trash equates to on average 500 kW of electricity. An average household will use 30 kW daily.", hjust = 1, vjust = -1, size = 3, color = "black") +
	theme_minimal() +  # Clean theme
	theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Angle x-axis text



	# Write and use a function that does something with the data (1 pt). It could be as simple as, for example, a new function that you write by hand to calculate the standard deviation of a variable (like we did with the mean).

variance <- function(x) {
	# Check if input is a numeric vector
	if (!is.numeric(x)) {
		stop("Input must be a numeric vector")
	}

	# Remove NA values from the vector
	x <- na.omit(x)

	# Calculate the mean of x
	mean_x <- mean(x)

	# Calculate the variance
	mean_val <- mean((x - mean_x)^2)

	return(mean_val)
}

variance(trashwheel$PlasticBottles)
var(trashwheel$PlasticBottles, na.rm = TRUE)



	# ____________________________________________________________

	# Create and render a quarto document that includes at least:
	# The table, regression results, and figure, with appropriate captions (1 pt).
	# Inline R code in at least 2 places, 1 pulling a statistic from a table (i.e., using gtsummary::inline_text()) and 1 printing something else (like we did with the mean age in the example) (1 pt).
	# Cross-references to a table and a figure at least once each (1 pt).

	# Use the {here} package every time you refer to file paths when reading in data and saving any files (1 pt). You must use it at least twice. Feel free to save any files that you create (e.g., your figure) in your code.

	# Commit and push your work to GitHub as you go (1 pt).

	# In a README file, include any notes necessary for us to easily reproduce your analysis (e.g., “Run script.R and then render document.qmd”) (1 pt). We should be able to make a minor change to the underlying data, then re-run the analysis to see how the change affects the results.










