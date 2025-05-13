library(conflicted)
library(tidyverse)
library(fredr)
source("get_keys.R")


# Get the API key
key_value <- get_key("fred.key")

# Private payroll (USPRIV)
# Set FRED API key
fredr_set_key(key_value)

# Download USPRIV data
uspriv_data <- fredr(
    series_id = "USPRIV",
    frequency = "m"
)

# Save to CSV
write.csv(uspriv_data, "USPRIV.csv", row.names = FALSE)
