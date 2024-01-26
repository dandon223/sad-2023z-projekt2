# Accidents by day in Poland in year 2022
accidents_by_day <- c(3248, 3083, 3252, 3203, 3617, 2727, 2192)
number_of_days_in_year <- c(52, 52, 52, 52, 52, 53, 52)
probabilities <- number_of_days_in_year/365

chisq.test(x = accidents_by_day, p=probabilities)

# Accidents by month in Poland in year 2022
accidents_by_month <- c(1300, 1220, 1584, 1479, 2119, 2349, 2153, 2199, 1941, 2009, 1506, 1463)
number_of_days_in_months <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
probabilities <- number_of_days_in_months/sum(number_of_days_in_months)

chisq.test(x = accidents_by_month, p=probabilities)
