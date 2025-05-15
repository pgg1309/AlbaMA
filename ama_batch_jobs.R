# Batch Adaptive Moving Average for All FRED Jobs Data
library(tidyverse)
library(ranger)
library(tuneRanger)
library(mlr)
library(progress)


# --- SETTINGS ---
start_date <- as.Date("1965-01-01")
min.sample <- 10 # this allows for sample.fraction >= 0.1

# Directories
input_dirs <- c("data/all_jobs", "data/production_jobs")
output_dir <- "data/ama_jobs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Helper: get all relevant CSVs (exclude comparison tables)
get_data_files <- function(dir) {
    files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    files[!grepl("comparison_table", files)]
}

all_files <- unlist(lapply(input_dirs, get_data_files))
#all_files <- all_files[1:3] # Only process the first three files for testing

# Results storage
param_list <- list()
results_list <- list()

# Progress bar for files
file_pb <- progress_bar$new(
    format = "Processing files [:bar] :current/:total :file",
    total = length(all_files),
    clear = FALSE,
    width = 60
)

for (file_path in all_files) {
    file_pb$tick(tokens = list(file = basename(file_path)))
    # Read and prep data
    data <- read.csv(file_path)
    date_col <- grep("date", tolower(names(data)), value = TRUE)[1]
    value_col <- grep("value", tolower(names(data)), value = TRUE)[1]
    data$DATE <- as.Date(data[[date_col]])
    data$MAIN <- data[[value_col]]
    data <- data[order(data$DATE), ]
    data$MAIN <- data$MAIN - dplyr::lag(data$MAIN)
    data <- data[!is.na(data$MAIN), c("DATE", "MAIN")]
    data <- data[data$DATE >= start_date, ]
    data$Time_Trend <- 1:nrow(data)

    # --- TUNE PARAMETERS ---
    # payroll_task <- makeRegrTask(
    #     data = data[, c("MAIN", "Time_Trend")],
    #     target = "MAIN"
    # )
    # res <- tuneRanger(
    #     payroll_task,
    #     measure = list(medae),
    #     num.trees = 500,
    #     iters = 70,
    #     tune.parameters = c("min.node.size", "sample.fraction"),
    #     parameters = list(
    #         replace = FALSE,
    #         respect.unordered.factors = "order",
    #         seed = 42
    #     )
    # )
    # best_params <- res$recommended.pars
    # param_list[[basename(file_path)]] <- best_params
    N_trees <- 500
    #min_node_size <- best_params$min.node.size
    #sampsize_rate <- best_params$sample.fraction
    min_node_size <- 4
    sampsize_rate <- 0.4

    # --- ADAPTIVE MOVING AVERAGE CALCULATIONS ---
    # Two-sided adaptive moving average
    rf_model_center <- ranger(
        MAIN ~ Time_Trend,
        data = data,
        num.trees = N_trees,
        mtry = 1,
        min.node.size = min_node_size,
        sample.fraction = sampsize_rate,
        keep.inbag = TRUE,
        oob.error = TRUE,
        seed = 42
    )
    data$Albama_center <- predict(rf_model_center, data)$predictions

    # obs_imp_rf_center
    obs_imp_rf_center <- as.data.frame(matrix(
        0,
        nrow = nrow(data),
        ncol = nrow(data)
    ))
    pb_center <- progress_bar$new(
        format = paste0(
            "  Two-sided [:bar] :current/:total t for ",
            basename(file_path)
        ),
        total = nrow(data) - min.sample + 1,
        clear = FALSE,
        width = 60
    )
    for (t in min.sample:nrow(data)) {
        rf_assigned_leaf_ins <- predict(
            rf_model_center,
            data,
            type = 'terminalNodes',
            predict.all = TRUE
        )$predictions
        for (tt in 1:length(rf_model_center$inbag.counts)) {
            rf_leaf_ins_tt <- rf_assigned_leaf_ins[, tt]
            rf_leaf_oos_tt <- rf_assigned_leaf_ins[t, tt]
            obs_ins_ll <- which(
                (rf_leaf_ins_tt == rf_leaf_oos_tt) &
                    (rf_model_center$inbag.counts[[tt]]) == 1
            )
            obs_oos_ll <- t
            obs_imp_rf_center[obs_ins_ll, obs_oos_ll] <- obs_imp_rf_center[
                obs_ins_ll,
                obs_oos_ll
            ] +
                1 / length(obs_ins_ll)
        }
        pb_center$tick()
    }
    obs_imp_rf_center[, -(1:min.sample)] <- obs_imp_rf_center[,
        -(1:min.sample)
    ] /
        matrix(
            colSums(obs_imp_rf_center),
            nrow(obs_imp_rf_center),
            ncol(obs_imp_rf_center),
            byrow = TRUE
        )[, -(1:min.sample)]

    # One-sided adaptive moving average
    data$Albama_right <- rep(NA, nrow(data))
    obs_imp_rf_right <- as.data.frame(matrix(
        0,
        nrow = nrow(data),
        ncol = nrow(data)
    ))
    pb_right <- progress_bar$new(
        format = paste0(
            "  One-sided [:bar] :current/:total t for ",
            basename(file_path)
        ),
        total = nrow(data) - min.sample + 1,
        clear = FALSE,
        width = 60
    )
    for (t in min.sample:nrow(data)) {
        rf_model <- ranger(
            MAIN ~ Time_Trend,
            data = data[1:t, ],
            mtry = 1,
            min.node.size = min_node_size,
            num.trees = N_trees,
            sample.fraction = sampsize_rate,
            keep.inbag = TRUE,
            oob.error = TRUE,
            seed = 42
        )
        temp <- predict(rf_model, data[1:t, ])$predictions
        data$Albama_right[t] <- temp[t]
        rf_assigned_leaf_ins <- predict(
            rf_model,
            data[1:t, ],
            type = 'terminalNodes',
            predict.all = TRUE
        )$predictions
        for (tt in 1:length(rf_model$inbag.counts)) {
            rf_leaf_ins_tt <- rf_assigned_leaf_ins[1:t, tt]
            rf_leaf_oos_tt <- rf_assigned_leaf_ins[t, tt]
            obs_ins_ll <- which(
                (rf_leaf_ins_tt == rf_leaf_oos_tt) &
                    (rf_model$inbag.counts[[tt]]) == 1
            )
            obs_oos_ll <- t
            obs_imp_rf_right[obs_ins_ll, obs_oos_ll] <- obs_imp_rf_right[
                obs_ins_ll,
                obs_oos_ll
            ] +
                1 / length(obs_ins_ll)
        }
        pb_right$tick()
    }
    obs_imp_rf_right[, -(1:min.sample)] <- obs_imp_rf_right[, -(1:min.sample)] /
        matrix(
            colSums(obs_imp_rf_right),
            nrow(obs_imp_rf_right),
            ncol(obs_imp_rf_right),
            byrow = TRUE
        )[, -(1:min.sample)]

    # Save results
    parent_dir <- basename(dirname(file_path))
    unique_name <- paste0(
        parent_dir,
        "_",
        tools::file_path_sans_ext(basename(file_path))
    )
    result <- list(
        data = data,
        #best_params = best_params,
        obs_imp_rf_center = obs_imp_rf_center,
        obs_imp_rf_right = obs_imp_rf_right
    )
    results_list[[unique_name]] <- result
    saveRDS(
        result,
        file = file.path(
            output_dir,
            paste0(unique_name, "_ama.rds")
        )
    )
}

# Save all parameters as a summary
#saveRDS(param_list, file = file.path(output_dir, "ama_tuned_params.rds"))
