# Test out Copilot

library(dplyr)

mtcars |>
  group_by(cyl) |>
  summarize(
    mean = mean(mpg),
    # add the standard error
    se = sd(mpg) / sqrt(n())
  )
    
# Code 

  # Create a function that raises a number x to the power n
num_to_power <- function(x, n) {
  x^n
}

num_to_power(2, 3)

# Questions

# q: What is the definition of standard error?

# The standard error (SE) is a measure of the variability or dispersion of a sample statistic, such as the mean, from the true population parameter. It quantifies how much the sample mean is expected to vary from the true population mean. The standard error is calculated by dividing the standard deviation of the sample by the square root of the sample size (n).


# Comments

# Create a robust function to calculate the standard error of a vector
# The function name will be calc_se with an argument x for the vector, with
# an argument to remove missing values called na.rm, and a default value of TRUE
# The function should have type checking to only allow numeric vectors

calc_se <- function(x, na.rm = TRUE) {
  # Check if x is a numeric vector
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  # Remove missing values if na.rm is TRUE
  if (na.rm) {
    x <- na.omit(x)
  }
  
  # Calculate the standard error
  se <- sd(x) / sqrt(length(x))
  
  return(se)
}
 
# create a function that takes a number and returns the square of that number
square_number <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric value.")
  }
  return(x^2)
}

# Copilot inline completion

library(dplyr)

mtcars |>
  group_by(cyl) |>
  summarize(
    mean = mean(mpg),
    # add the standard error for mpg
    se = sd(mpg) / sqrt(n())
  )