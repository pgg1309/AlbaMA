# Download FRED Employment Data: Table B-1 (All Employees) and Table B-6 (Production & Nonsupervisory)
library(tidyverse)
library(fredr)
source("get_keys.R")

# Set FRED API key
fredr_set_key(get_key("fred.key"))

# --- Table B-6: Production and Nonsupervisory Employees ---
prod_dir <- "data/production_jobs"
dir.create(prod_dir, showWarnings = FALSE, recursive = TRUE)

b6_series <- list(
    CES0500000006 = "Total_private",
    CES0600000006 = "Goods_producing",
    CES1000000006 = "Mining_logging",
    CES2000000006 = "Construction",
    CES3000000006 = "Manufacturing",
    CES3100000006 = "Durable_goods",
    CES3200000006 = "Nondurable_goods",
    CES0800000006 = "Private_service_providing",
    CES4000000006 = "Trade_transportation_utilities",
    CES4142000006 = "Wholesale_trade",
    CES4200000006 = "Retail_trade",
    CES4300000006 = "Transportation_warehousing",
    CES4422000006 = "Utilities",
    CES5000000006 = "Information",
    CES5500000006 = "Financial_activities",
    CES6000000006 = "Professional_business_services",
    CES6500000006 = "Education_health_services",
    CES7000000006 = "Leisure_hospitality",
    CES8000000006 = "Other_services"
)

for (series_id in names(b6_series)) {
    data <- fredr(series_id = series_id, frequency = "m")
    write.csv(
        data,
        file.path(prod_dir, paste0(b6_series[[series_id]], ".csv")),
        row.names = FALSE
    )
}

# Create Table B-6 comparison table
b6_data_list <- lapply(
    names(b6_series),
    function(series_id) fredr(series_id = series_id, frequency = "m")
)
names(b6_data_list) <- b6_series
b6_latest_date <- min(sapply(b6_data_list, function(df) max(df$date)))
b6_table <- data.frame(
    Name = names(b6_data_list),
    Value = sapply(
        b6_data_list,
        function(df) df$value[df$date == b6_latest_date]
    ),
    Date = b6_latest_date
)
write.csv(
    b6_table,
    file = file.path(prod_dir, "B6_comparison_table.csv"),
    row.names = FALSE
)

# --- Table B-1: All Employees ---
all_jobs_dir <- "data/all_jobs"
dir.create(all_jobs_dir, showWarnings = FALSE, recursive = TRUE)

b1_series <- list(
    PAYEMS = "Total_nonfarm",
    USPRIV = "Total_private",
    USGOOD = "Goods_producing",
    USMINE = "Mining_and_logging",
    USCONS = "Construction",
    MANEMP = "Manufacturing",
    DMANEMP = "Durable_goods",
    NDMANEMP = "Nondurable_goods",
    CES0800000001 = "Private_service_providing",
    USTPU = "Trade_transportation_utilities",
    USWTRADE = "Wholesale_trade",
    USTRADE = "Retail_trade",
    CES4300000001 = "Transportation_and_warehousing",
    CES4422000001 = "Utilities",
    USINFO = "Information",
    USFIRE = "Financial_activities",
    USPBS = "Professional_and_business_services",
    USEHS = "Education_and_health_services",
    USLAH = "Leisure_and_hospitality",
    USSERV = "Other_services",
    USGOVT = "Government",
    CES9091000001 = "Federal",
    CES9092000001 = "State_government",
    CES9093000001 = "Local_government"
)

for (series_id in names(b1_series)) {
    data <- fredr(series_id = series_id, frequency = "m")
    write.csv(
        data,
        file.path(all_jobs_dir, paste0(b1_series[[series_id]], ".csv")),
        row.names = FALSE
    )
}

# Create Table B-1 comparison table
b1_data_list <- lapply(
    names(b1_series),
    function(series_id) fredr(series_id = series_id, frequency = "m")
)
names(b1_data_list) <- b1_series
b1_latest_date <- min(sapply(b1_data_list, function(df) max(df$date)))
b1_table <- data.frame(
    Name = names(b1_data_list),
    Value = sapply(
        b1_data_list,
        function(df) df$value[df$date == b1_latest_date]
    ),
    Date = b1_latest_date
)
write.csv(
    b1_table,
    file = file.path(all_jobs_dir, "B1_comparison_table.csv"),
    row.names = FALSE
)
