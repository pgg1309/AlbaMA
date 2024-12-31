rm(list=ls())

library(pracma)
library(ranger)


# SETTINGS
# ------------------------------------------------
# --- Data Settings
labeling <- "CPI Inflation"
start.date <- "1963-01-01"

# --- Hyperparameter Settings (selected via CV)
N_trees <- 500
min_node_size <- 6
sampsize_rate <- 0.6


# DATA PREP
# ------------------------------------------------
# --- Load data
file_path <- "~/Dropbox/Albama/Replication/CPIAUCSL.csv" 
data <- read.csv(file_path)
data$DATE <- as.Date(data$DATE)
data$MAIN <- data[,2]

# --- Transformations
data$MAIN <- (log(data$MAIN)-log(dplyr::lag(data$MAIN)))*100
data$MAIN <- 12*data$MAIN

# --- Select variable and dates
data <- data[c("DATE","MAIN")]
data <- data[which(data$DATE==start.date):nrow(data),]

# --- Add time trend
data$Time_Trend <- 1:nrow(data)


# TWO-SIDED ADAPTIVE MOVING AVERAGE
# ------------------------------------------------
# --- Fit the final Random Forest model
rf_model_center <- ranger(MAIN ~ Time_Trend, data = data, 
                          num.trees = N_trees, mtry = 1,
                          min.node.size = min_node_size, 
                          sample.fraction = sampsize_rate, 
                          keep.inbag=TRUE,
                          oob.error = TRUE, seed = 42)

data$Albama_center = predict(rf_model_center, data)$predictions

# --- Look at Observation Importance
# loop over observations
obs_imp_rf_center <- as.data.frame(matrix(0,nrow=nrow(data),ncol=nrow(data),
                                          dimnames = list(as.character(data$DATE),paste0('obs_',c(1:nrow(data))))))

pb <- txtProgressBar(min = 0, max = nrow(data), style = 3) # Progress bar
print('Start extracting RF weights for AlbaMA two-sided...')
for (t in 2:nrow(data)) {
  # --- Which observations have been assigned to which leaf in-sample?
  rf_assigned_leaf_ins <- predict(rf_model_center, data, type = 'terminalNodes', predict.all = TRUE)$predictions
  
  for (tt in 1:length(rf_model_center$inbag.counts)){ # for (tt in 1:N_trees){
    
    # --- Get the leaves for tree 'tt' (in-sample predictions):
    rf_leaf_ins_tt <- rf_assigned_leaf_ins[,tt]
    
    # --- Get the leaves for tree 'tt' (in-sample predictions):
    rf_leaf_oos_tt <- rf_assigned_leaf_ins[t,tt]
    
    # --- Get the in-sample indices that are assigned to 'leaf_ll' and that are NOT OOB:
    obs_ins_ll <- which((rf_leaf_ins_tt == rf_leaf_oos_tt) & (rf_model_center$inbag.counts[[tt]]) == 1)
    
    # --- OOS instance
    obs_oos_ll <- t
    
    # --- Update the importance of the used training observations for each OOS instance:
    obs_imp_rf_center[obs_ins_ll, obs_oos_ll] <- obs_imp_rf_center[obs_ins_ll, obs_oos_ll] + 1 / length(obs_ins_ll)
    # --- Next tree (tt)
  }
  setTxtProgressBar(pb, t)
}
obs_imp_rf_center[,-1] <- obs_imp_rf_center[,-1]/matrix(colSums(obs_imp_rf_center),nrow(obs_imp_rf_center),ncol(obs_imp_rf_center),byrow = TRUE)[,-1]


# ONE-SIDED ADAPTIVE MOVING AVERAGE
# ------------------------------------------------
# --- Fit the Random Forest model one-sided
data$Albama_right <- data$MA_sg_right <- data$MA_wh_right <- rep(NA,nrow(data))
obs_imp_rf_right <- as.data.frame(matrix(0,nrow=nrow(data),ncol=nrow(data),
                                         dimnames = list(as.character(data$DATE),paste0('obs_',c(1:nrow(data))))))

pb <- txtProgressBar(min = 0, max = nrow(data), style = 3) # Progress bar
print('Start estimating AlbaMA one-sided...')
for(t in 2:nrow(data)){
  rf_model <- ranger(MAIN ~ Time_Trend, data = data[1:t,], mtry = 1,
                     min.node.size = min_node_size, num.trees = N_trees,
                     sample.fraction = sampsize_rate, 
                     keep.inbag=TRUE,
                     oob.error = TRUE, seed = 42)
  
  # Get fitted values
  temp = predict(rf_model, data[1:t,])$predictions
  
  data$Albama_right[t]=temp[t]
  
  # Look at Observation Importance
  # --- Which observations have been assigned to which leaf in-sample?
  rf_assigned_leaf_ins <- predict(rf_model, data[1:t,], type = 'terminalNodes', predict.all = TRUE)$predictions
  
  for (tt in 1:length(rf_model$inbag.counts)){ # for (tt in 1:N_trees){
    
    # --- Get the leaves for tree 'tt' (in-sample predictions):
    rf_leaf_ins_tt <- rf_assigned_leaf_ins[1:t,tt]
    
    # --- Get the leaves for tree 'tt' (in-sample predictions):
    rf_leaf_oos_tt <- rf_assigned_leaf_ins[t,tt]
    
    # --- Get the in-sample indices that are assigned to 'leaf_ll' and that are NOT OOB:
    obs_ins_ll <- which((rf_leaf_ins_tt == rf_leaf_oos_tt) & (rf_model$inbag.counts[[tt]]) == 1)
    
    # --- OOS instance
    obs_oos_ll <- t
    
    # --- Update the importance of the used training observations for each OOS instance:
    obs_imp_rf_right[obs_ins_ll, obs_oos_ll] <- obs_imp_rf_right[obs_ins_ll, obs_oos_ll] + 1 / length(obs_ins_ll)
    # --- Next tree (tt)
  }
  setTxtProgressBar(pb, t)
}
obs_imp_rf_right[,-1] <- obs_imp_rf_right[,-1]/matrix(colSums(obs_imp_rf_right),nrow(obs_imp_rf_right),ncol(obs_imp_rf_right),byrow = TRUE)[,-1]


# PLOT RESULTS
# ------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyverse)

# --- TIME SERIES
# ----------------
plot1 <- ggplot(data = data) +
  geom_line(aes(x=DATE, y=MAIN, col="MAIN"), linewidth = 0.3) +
  geom_line(aes(x=DATE, y=Albama_center,  col="Albama_center"), linewidth = 0.7) +
  geom_line(aes(x=DATE, y=Albama_right,  col="Albama_right"), linewidth = 0.7) +
  scale_color_manual(
        name = "",
        values = c("MAIN"="black", "Albama_center"="navyblue", "Albama_right"="firebrick"),
        labels = c("MAIN"="MoM Series","Albama_center"="AlbaMA (two-sided)", "Albama_right"="AlbaMA (one-sided)"),
        na.translate = FALSE
      ) +
  xlab('') + ylab('') + ggtitle(labeling) +
  theme_bw() + theme(legend.position = "bottom")

print(plot1)


# --- LOOK-BACK WINDOW
# -------------------------
for (ma.type in c('center','right')) {
if (ma.type == "center") obs_imp_rf <- obs_imp_rf_center
if (ma.type == "right") obs_imp_rf <- obs_imp_rf_right

# --- get number of leads and lags to plot
num_rows <- nrow(obs_imp_rf)
num_cols <- ncol(obs_imp_rf)
lags_total <- numeric(num_cols)
leads_total <- numeric(num_cols)
for (i in 1:num_cols) {
  # Before diagonal (row i, column i)
  if (i > 1) {
    lags_total[i] <- sum(obs_imp_rf[1:(i-1), i] != 0)
  } else {
    lags_total[i] <- 0  # No elements before the first diagonal entry
  }
  # After diagonal (row i, column i)
  if (i < num_rows) {
    leads_total[i] <- sum(obs_imp_rf[(i+1):num_rows, i] != 0)
  } else {
    leads_total[i] <- 0  # No elements after the last diagonal entry
  }
}

# --- create dataframe to save importance of leads and lags
plot_imp <- as.data.frame(matrix(0,nrow=ncol(obs_imp_rf),ncol=max(leads_total) + max(lags_total)+1,
                                 dimnames = list(colnames(obs_imp_rf),c(paste0('lag_',c(-max(leads_total):0)), paste0('lag_',c(1:max(lags_total)))))))
# --- loop over observations
for (ii in 1:ncol(obs_imp_rf)) {
  imp_ii <- obs_imp_rf[,ii]
  index_ii <- which(imp_ii != 0)
  if (length(index_ii) == 0)  next
  
  plot_imp[ii,paste0('lag_',(index_ii-ii)*-1)] <- imp_ii[index_ii]
  
}

# --- aggregate lags
if (ma.type == "right") {
  plot_imp_agg <- plot_imp %>%
    mutate(`y(t)` = lag_0) %>%
    mutate(`y(t-1:2)` = lag_1+lag_2) %>%
    mutate(`y(t-3:5)` = lag_3+lag_4+lag_5) %>%
    mutate(`y(t-6:end)` = rowSums(select(., matches("^lag_[6-9]|[1-9][0-9]+$")), na.rm = TRUE)) %>%
    mutate(DATE = data$DATE)
  plot_imp_agg <- plot_imp_agg %>%
    select(DATE,`y(t)`,`y(t-1:2)`,`y(t-3:5)`,`y(t-6:end)`)
  levels_df <- c("y(t-6:end)","y(t-3:5)","y(t-1:2)","y(t)")
} else if (ma.type == "center") {
  plot_imp_agg <- plot_imp %>%
    mutate(`y(t)` = lag_0) %>%
    mutate(`y(t-1:2)` = lag_1+lag_2) %>%
    mutate(`y(t+1:2)` = `lag_-1`+`lag_-2`) %>%
    mutate(`y(t-3:end)` = rowSums(select(., matches("^lag_(3|[4-9]|[1-9][0-9]+)$")), na.rm = TRUE)) %>%
    mutate(`y(t+3:end)` = rowSums(select(., matches("^lag_-[3-9]|^lag_-[1-9][0-9]+$")), na.rm = TRUE)) %>%
    mutate(DATE = data$DATE)
  plot_imp_agg <- plot_imp_agg %>%
    select(DATE,`y(t)`,`y(t-1:2)`,`y(t+1:2)`,`y(t-3:end)`, `y(t+3:end)`)
  levels_df <- c("y(t-3:end)","y(t-1:2)","y(t)","y(t+1:2)","y(t+3:end)")
}


# --- Plot POST 2019
plot_df_imp <- plot_imp_agg %>%
  gather(key=lags,value=value,-DATE) %>%
  filter(DATE > "2018-12-01")
plot_df_imp$lags <- factor(plot_df_imp$lags, levels = levels_df)

# --- color settings
if (ma.type == "center") colors_decomp <- c("firebrick3","#F98400","#178864","navyblue","#001330")
if (ma.type == "right") colors_decomp <- c("firebrick3","navyblue","#F98400","#178864")
names(colors_decomp) <- c(levels(plot_df_imp$lags))
my_names <- names(colors_decomp)
if (ma.type == "right") my_title <- "AlbaMA (one-sided)"
if (ma.type == "center") my_title <- "AlbaMA (two-sided)"

# --- plot
gg_imp <- ggplot(data = plot_df_imp) +
  geom_col(aes(x=DATE,y=value,fill=lags),alpha=0.75)+
  labs(x="",y="") +
  theme_bw()+
  scale_x_date(breaks = scales::pretty_breaks(n = 6),date_labels = "%b %y", 
               expand = c(0.0,0.0))+
  scale_fill_manual(
    name = "",
    values = colors_decomp,
    labels=my_names
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), expand = c(0,0)) +
  ggtitle(my_title) +
  theme_bw() + theme(legend.position = "bottom")

print(gg_imp)

}




