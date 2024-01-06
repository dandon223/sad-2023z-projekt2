# Accidents by day in Poland in year 2022
accidents_by_day <- c(3248, 3083, 3252, 3203, 3617, 2727, 2192)
accidents_by_day_sum <- sum(accidents_by_day)

chisq.test(x = accidents_by_day)


# Accidents by month in Poland in year 2022
accidents_by_month <- c(1300, 1220, 1584, 1479, 2119, 2349, 2153, 2199, 1941, 2009, 1506, 1463)
accidents_by_month_sum <- sum(accidents_by_month)
number_of_days_in_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
accidents_by_month_day <- accidents_by_month/number_of_days_in_months

chisq.test(x = accidents_by_month_day)
