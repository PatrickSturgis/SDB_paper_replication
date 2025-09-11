# === 01_setup_environment.R ===

# Packages needed for happiness analysis
required_packages <- c(
  "dplyr",         # Data manipulation  
  "ggplot2",       # Plotting
  "haven",         # Reading .dta files
  "forcats",       # Factor manipulation (for fct_na_value_to_level, fct_recode)
  "broom",         # Tidy model outputs (for broom::tidy)
  "readxl",        # Reading Excel files
  "writexl",        # Writing Excel files
  "survey"         # Survey design and weighted analysis 
)

# Install any missing packages
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Verify working directory
cat("Working directory set to:", getwd(), "\n")

# Set global options
options(stringsAsFactors = FALSE, scipen = 999)

# Custom helper function (used in data loading)
fct_na_value_to_level <- function(x, na_label = "Missing") {
  forcats::fct_explicit_na(x, na_label)
}

# Create output directories if they don't exist
if (!dir.exists("outputs_LPM")) {
  dir.create("outputs_LPM")
  cat("Created outputs_LPM directory\n")
}

cat("Environment loaded successfully!\n")
