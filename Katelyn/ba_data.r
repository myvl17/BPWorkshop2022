
# Load the packages 
library(dplyr)
library(readr)
library(ggplot2)

# Read the data into your workspace
ba_data <- read_csv("datasets/breath_alcohol_ames.csv")

# Quickly inspect the data
head(ba_data)

# Obtain counts for each year 
ba_year <- ba_data %>% count(year)

# These packages need to be loaded in the first `@tests` cell. 
library(testthat) 
library(IRkernel.testthat)

run_tests({
    correct_ba_data <- read_csv("datasets/breath_alcohol_ames.csv")
    correct_count_data <- ba_data %>% count(year)
    test_that("read_csv is used", {
        expect_true("tbl_df" %in% class(ba_data),
                    info = "Use read_csv (with an underscore) to read in datasets/breath_alcohol_ames.csv")
    })
    test_that("ba_data is read in correctly", {
        expect_equal(correct_ba_data, ba_data,
                     info = "ba_data should contain the data in datasets/breath_alcohol_ames.csv")
    })
    test_that("ba_year is created correctly", {
        expect_equal(correct_count_data, ba_year, 
                     info = "ba_year should be a tbl.df with 5 rows and 2 columns called year and n")
    })
})

# Count the totals for each department
pds <- ba_data %>% count(location)

run_tests({
    correct_pds_data <- ba_data %>% count(location)
    test_that("pds data is correct", {
        expect_equal(correct_pds_data, pds,
                     info = "Count by the location variable to get one column named location and one column named n. There should be 2 rows.")
    })
})

# Count by hour and arrange by descending frequency
hourly <- ba_data %>% count(hour) %>% arrange(desc(hour))

# Use a geom_ to create the appropriate bar chart
ggplot(hourly, aes(x = hour, weight = hourly)) + geom_bar()

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_hour_data <- ba_data %>% count(hour) %>% arrange(desc(n))
    test_that("hourly data is correct", {
        expect_equal(correct_hour_data, hourly,
                     info = "The object hourly should have 24 rows and 2 columns, called hour and n. The first row should contain the largest n value, and the last row should contain the smallest n value.")
    })
    test_that("plot is a bar chart",{
        p <- last_plot()
        q <- p$layers[[1]]
        expect_is(q$geom, "GeomBar", 
                  info = "You should plot a bar chart with hour on the x-axis and count ('n') on the y-axis.")
    })
})

# Count by month and arrange by descending frequency
monthly <- ba_data %>% count(month) %>% arrange(desc(month))

# Make month a factor
monthly$month <- as.factor(monthly$month)

# Use a geom_ to create the appropriate bar chart
ggplot(monthly, aes(x = month, weight = monthly)) + geom_bar()

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_month_data <- ba_data %>% count(month) %>% arrange(desc(n)) %>% ungroup() %>% mutate(month = as.factor(month))
    test_that("monthly data is correct", {
        expect_equal(correct_month_data, monthly,
                     info = "The object monthly should have 12 rows and 2 columns, called month and n. The first row should contain the largest n value, and the last row should contain the smallest n value.")
    })
    test_that("plot is a bar chart",{
        p <- last_plot()
        q <- p$layers[[1]]
        expect_is(q$geom, "GeomBar", 
                  info = "You should plot a bar chart with month on the x-axis and count ('n') on the y-axis.")
    })
})

# Count by gender 
ba_data %>% count(gender)

# Create a dataset with no NAs in gender 
clean_gender <- ba_data %>% filter(!is.na(gender))

# Create a mean test result variable and save as mean_bas
mean_bas <- clean_gender %>% mutate(meanRes = (Res1 + Res2) / 2)

# Create side-by-side boxplots to compare the mean blood alcohol levels of men and women
ggplot(mean_bas, aes(x = gender, y = meanRes)) + geom_boxplot()

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_clean_gender <- ba_data %>% filter(!is.na(gender))
    correct_meanbas <- clean_gender %>% mutate(meanRes = (Res1 + Res2)/2)
    test_that("clean_gender is correct", {
        expect_equal(clean_gender, correct_clean_gender,
                     info = "Use filter and !is.na().")
    })
    test_that("mean_bas is correct", {
        expect_equal(mean_bas, correct_meanbas,
                     info = "Try creating the mean column with mutate(meanRes = (col1 + col2)/2) in the clean_gender data.")
    }) 
    test_that("plot is a boxplot",{
        p <- last_plot()
        q <- p$layers[[1]]
        expect_is(q$geom, "GeomBoxplot", 
                  info = "You should plot a boxplot with gender on the x-axis and meanRes on the y-axis.")
    })

  # You can have more than one test
})

# Filter the data
duis <- ba_data %>% filter(Res1 > 0.08 | Res2 > 0.08)

# Proportion of tests that would have resulted in a DUI
p_dui <- nrow(duis)/nrow(ba_data)

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_duis <- ba_data %>% filter(Res1 > 0.08 | Res2 > 0.08)
    p_correct <- nrow(correct_duis)/nrow(ba_data)
    test_that("duis is correct", {
        expect_equivalent(duis, correct_duis, 
                          info = "Make sure you included Res1 and Res2 values that are greater than 0.08.")
    })
    test_that("p is correct", {
        expect_equivalent(p_dui, p_correct, 
                          info = "The proportion should be the number of rows in duis divided by the number of rows in ba_data.")
    })
})

library(lubridate) 

# Create date variable using paste() and ymd()
ba_data <- ba_data %>% mutate(date = ymd(paste(year, month, day, sep = '-')))

# Create a week variable using week()
ba_data <- ba_data %>% mutate(week = week(date))

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_ba_data <- ba_data %>% mutate(date = ymd(paste(year, month, day, sep = "-")), 
                                          week = week(date))
    test_that("ba_data is correct", {
        expect_equivalent(ba_data, correct_ba_data, 
                          info = "Did you create two new columns, date (a date variable) and week (a numeric variable)?")
    })
})

# Create the weekly data set 
weekly <- ba_data %>% count(week, year)

# Make year a factor
weekly <- weekly %>% mutate(year = as.factor(year))

# Create the time series plot with one line for each year
ggplot(weekly, aes(x = week, y = year), color = year) + 
  geom_line() + 
  geom_point(aes(color = year)) +  
  scale_x_continuous(breaks = seq(0,52,2))

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    correct_weekly <- ba_data %>% count(week, year) %>% ungroup %>% mutate(year = as.factor(year))
    test_that("weekly is correct", {
        expect_equivalent(weekly, correct_weekly,
                          info = "Did you ungroup the data for the plot?")
    })
    test_that("plot is a line plot",{
        p <- last_plot()
        q <- p$layers[[1]]
        expect_is(q$geom, "GeomLine", 
                  info = "You should plot a line plot with week on the x-axis, count ('n') on the y-axis, and year mapped to color.")
    })

# You can have more than one test
})

# Run this code to create the plot 
ggplot() + 
  geom_point(data = weekly, aes(x = week, y = n, color = year)) + 
  geom_line(data = weekly, aes(x = week, y = n, color = year)) +  # included to make the plot more readable 
  geom_segment(data = NULL, arrow = arrow(angle = 20, length = unit(0.1, "inches"),
                                          ends = "last", type = "closed"), 
               aes(x = c(20,20), xend = c(15.5,16), y = c(21, 20), yend = c(21, 12.25))) + 
  geom_text(data = NULL, aes(x = 23, y = 20.5, label = "VEISHEA Weeks"), size = 3) + 
  scale_x_continuous(breaks = seq(0,52,2)) 

# Make a decision about VEISHEA. TRUE or FALSE?  
cancelling_VEISHEA_was_right <- TRUE

# one or more tests of the students code. 
# The @solution should pass the tests.
# The purpose of the tests is to try to catch common errors and to 
# give the student a hint on how to resolve these errors.
run_tests({
    test_that("decision is made", {
        expect_true(is.logical(cancelling_VEISHEA_was_right), info = "Did you make a decision about VEISHEA? Assign TRUE or FALSE to the variable cancelling_VEISHEA_was_right.")
    })
# You can have more than one test
})
