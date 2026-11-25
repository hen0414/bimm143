
# Practice: Calculate GC content with Copilot 

# Create a function called gc_content that takes a DNA sequence
# as a single string and returns the percentage of bases that are G or C.
# The function should accept both uppercase and lowercase letters.
# The function should return a numeric value.

gc_content <- function(dna_sequence) {
  # Check if the input is a character string
  if (!is.character(dna_sequence)) {
    stop("Input must be a character string.")
  }
  
  # Convert the sequence to uppercase to handle both cases
  dna_sequence <- toupper(dna_sequence)
  
  # Split the DNA sequence into individual bases
  bases <- strsplit(dna_sequence, NULL)[[1]]
  
  # Count the number of G and C bases
  g_count <- sum(bases == "G")
  c_count <- sum(bases == "C")
  
  # Calculate the total number of bases
  total_bases <- nchar(dna_sequence)
  
  # Calculate GC content percentage
  gc_percentage <- ((g_count + c_count) / total_bases) * 100
  
  return(gc_percentage)
}

# Predictions for Results: 

# AAAA: 0/4 * 100 = 0%
# GCCG: 4/4 * 100 = 100%
# AtgC: 2/4 * 100 = 50%
# AATGC: 2/5 * 100 = 40%

# Test the function
gc_content("AAAA")
gc_content("GCCG")
gc_content("AtgC")
gc_content("AATGC")


# Reflection: 

#In this function, I first used the Copilot by giving it specific comments based on the directions given 
# by the professor and instructed what exactly the gc_function should be acheiving. These comments were
# VERY specific and included the name of function, and what types of inputs it can expect! 
# After being suggested a code, although it was basically pristine, I wanted to still make a few changes 
# which included trying to condense the strsplit() tat was used twice. I changed it to only be used once 
# and to split the sequence only once which made the function more efficient. 
# I then went on to test the function with some of the given sequences by first predicting them manually 
# and then comparing it to when I ran the function which ended up working out perfectly! 