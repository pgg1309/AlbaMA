library(dplyr)
library(readr)

# List of relevant industry RDS files
industry_files <- c(
    "production_jobs_Mining_logging_ama.rds",
    "production_jobs_Construction_ama.rds",
    "production_jobs_Durable_goods_ama.rds",
    "production_jobs_Nondurable_goods_ama.rds",
    "production_jobs_Wholesale_trade_ama.rds",
    "production_jobs_Retail_trade_ama.rds",
    "production_jobs_Transportation_warehousing_ama.rds",
    "production_jobs_Utilities_ama.rds",
    "production_jobs_Information_ama.rds",
    "production_jobs_Financial_activities_ama.rds",
    "production_jobs_Professional_business_services_ama.rds",
    "production_jobs_Education_health_services_ama.rds",
    "production_jobs_Leisure_hospitality_ama.rds",
    "production_jobs_Other_services_ama.rds"
)

industry_names <- gsub("production_jobs_(.*)_ama.rds", "\\1", industry_files)
data_dir <- "data/ama_jobs"

# Read and prepare each industry data frame, renaming columns to avoid duplicates
industry_dfs <- mapply(
    function(file, name) {
        obj <- readRDS(file.path(data_dir, file))
        df <- obj$data %>%
            select(DATE, MAIN, Albama_center, Albama_right) %>%
            rename_with(~ paste0(., "_", name), -DATE)
        df
    },
    industry_files,
    industry_names,
    SIMPLIFY = FALSE
)

# Merge all data frames by DATE
merged_df <- Reduce(function(x, y) full_join(x, y, by = "DATE"), industry_dfs)

# Ensure all columns to be summed are numeric
for (col in names(merged_df)[-1]) {
    merged_df[[col]] <- as.numeric(merged_df[[col]])
}

# Sum across all industries for each column type
production_bottom_up_total <- merged_df %>%
    mutate(
        MAIN = rowSums(select(., starts_with("MAIN_")), na.rm = TRUE),
        Albama_center = rowSums(
            select(., starts_with("Albama_center_")),
            na.rm = TRUE
        ),
        Albama_right = rowSums(
            select(., starts_with("Albama_right_")),
            na.rm = TRUE
        )
    ) %>%
    select(DATE, MAIN, Albama_center, Albama_right)

# Write to CSV
write_csv(
    production_bottom_up_total,
    "data/production_bottom_up_total_private.csv"
)
