library(tidyverse)
library(ggplot2)

#' Plot AMA Job Results
#' @param industry_name Name of the industry (e.g., 'Total_private')
#' @param job_type Either 'all' or 'production'
#' @param start_date Date string (e.g., '2000-01-01')
plot_ama_job <- function(
    industry_name,
    job_type = c('all', 'production'),
    start_date = '2000-01-01'
) {
    job_type <- match.arg(job_type)
    prefix <- ifelse(job_type == 'all', 'all_jobs', 'production_jobs')
    rds_file <- file.path(
        'data',
        'ama_jobs',
        paste0(prefix, '_', industry_name, '_ama.rds')
    )
    if (!file.exists(rds_file)) stop('File not found: ', rds_file)
    cc <- readRDS(rds_file)
    data <- cc$data
    data <- data[data$DATE >= as.Date(start_date), ]
    # Clip y-scale to 1st and 99th percentiles
    yvals <- c(data$MAIN, data$Albama_center, data$Albama_right)
    ylims <- quantile(yvals, probs = c(0.05, 0.95), na.rm = TRUE)
    plot1 <- ggplot(data) +
        geom_line(aes(x = DATE, y = MAIN, col = 'MAIN'), linewidth = 0.3) +
        geom_line(
            aes(x = DATE, y = Albama_center, col = 'AMA_center'),
            linewidth = 0.7
        ) +
        geom_line(
            aes(x = DATE, y = Albama_right, col = 'AMA_right'),
            linewidth = 0.7
        ) +
        scale_color_manual(
            name = '',
            values = c(
                'MAIN' = 'black',
                'AMA_center' = 'navyblue',
                'AMA_right' = 'firebrick'
            ),
            labels = c(
                'MAIN' = 'MoM Series',
                'AMA_center' = 'AMA (two-sided)',
                'AMA_right' = 'AMA (one-sided)'
            ),
            na.translate = FALSE
        ) +
        scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
        xlab('') +
        ylab('') +
        ggtitle(paste0(
            industry_name,
            ' (',
            job_type,
            ' jobs), from ',
            start_date
        )) +
        theme_bw() +
        theme(legend.position = 'bottom') +
        coord_cartesian(ylim = ylims)
    print(plot1)
}

#' Plot look-back window for one-sided (right) adaptive moving average
#' @param industry_name Name of the industry (e.g., 'Total_private')
#' @param job_type Either 'all' or 'production'
#' @param start_date Date string (e.g., '2000-01-01')
plot_lookback_right <- function(
    industry_name,
    job_type = c('all', 'production'),
    start_date = '2000-01-01'
) {
    job_type <- match.arg(job_type)
    prefix <- ifelse(job_type == 'all', 'all_jobs', 'production_jobs')
    rds_file <- file.path(
        'data',
        'ama_jobs',
        paste0(prefix, '_', industry_name, '_ama.rds')
    )
    if (!file.exists(rds_file)) stop('File not found: ', rds_file)
    cc <- readRDS(rds_file)
    data <- cc$data

    obs_imp_rf <- cc$obs_imp_rf_right
    # --- get number of leads and lags to plot
    num_rows <- nrow(obs_imp_rf)
    num_cols <- ncol(obs_imp_rf)
    lags_total <- numeric(num_cols)
    leads_total <- numeric(num_cols)
    for (i in 1:num_cols) {
        if (i > 1) {
            lags_total[i] <- sum(obs_imp_rf[1:(i - 1), i] != 0)
        } else {
            lags_total[i] <- 0
        }
        if (i < num_rows) {
            leads_total[i] <- sum(obs_imp_rf[(i + 1):num_rows, i] != 0)
        } else {
            leads_total[i] <- 0
        }
    }
    plot_imp <- as.data.frame(matrix(
        0,
        nrow = ncol(obs_imp_rf),
        ncol = max(leads_total) + max(lags_total) + 1,
        dimnames = list(
            colnames(obs_imp_rf),
            c(
                paste0('lag_', c(-max(leads_total):0)),
                paste0('lag_', c(1:max(lags_total)))
            )
        )
    ))
    for (ii in 1:ncol(obs_imp_rf)) {
        imp_ii <- obs_imp_rf[, ii]
        index_ii <- which(imp_ii != 0)
        if (length(index_ii) == 0) next
        plot_imp[ii, paste0('lag_', (index_ii - ii) * -1)] <- imp_ii[index_ii]
    }
    plot_imp_agg <- plot_imp %>%
        mutate(`y(t)` = lag_0) %>%
        mutate(`y(t-1:2)` = lag_1 + lag_2) %>%
        mutate(`y(t-3:5)` = lag_3 + lag_4 + lag_5) %>%
        mutate(
            `y(t-6:end)` = rowSums(
                select(., matches("^lag_[6-9]|[1-9][0-9]+$")),
                na.rm = TRUE
            )
        ) %>%
        mutate(DATE = data$DATE)
    plot_imp_agg <- plot_imp_agg %>%
        select(DATE, `y(t)`, `y(t-1:2)`, `y(t-3:5)`, `y(t-6:end)`)
    levels_df <- c("y(t-6:end)", "y(t-3:5)", "y(t-1:2)", "y(t)")
    plot_df_imp <- plot_imp_agg %>%
        tidyr::gather(key = lags, value = value, -DATE) %>%
        dplyr::filter(DATE >= as.Date(start_date))
    plot_df_imp$lags <- factor(plot_df_imp$lags, levels = levels_df)
    colors_decomp <- c("firebrick3", "navyblue", "#F98400", "#178864")
    names(colors_decomp) <- levels_df
    my_title <- paste0(
        industry_name,
        ' (',
        job_type,
        ' jobs) Look-back Window, from ',
        start_date
    )
    gg_imp <- ggplot(data = plot_df_imp) +
        geom_col(aes(x = DATE, y = value, fill = lags), alpha = 0.75) +
        labs(x = '', y = '') +
        theme_bw() +
        scale_x_date(breaks = scales::breaks_pretty(n = 10)) +
        scale_fill_manual(
            name = '',
            values = colors_decomp,
            labels = names(colors_decomp)
        ) +
        scale_y_continuous(breaks = scales::breaks_extended(n = 10)) +
        ggtitle(my_title) +
        theme(legend.position = 'bottom')
    print(gg_imp)
}

# Example usage:
# plot_ama_job('Total_private', 'all', '2000-01-01')
