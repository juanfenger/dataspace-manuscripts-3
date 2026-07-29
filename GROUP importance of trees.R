#===============================================================================
# 0. 加载包
#===============================================================================
library(tidyverse)
library(readxl)
library(ranger)
library(caret)
library(ParBayesianOptimization)
library(fastshap)
library(patchwork)
library(colorspace)

#===============================================================================
# 1. 读取数据
#===============================================================================
data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
output_dir <- "E:/第三篇论文/plot/Final_New_Style/TCC_TCCbuf_Importance/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

data_raw <- readxl::read_excel(data_path)

Cluster_name_map <- c(
  "1" = "Inner-Green High-Rise neighborhoods",
  "2" = "Suburban Mega-Sized neighborhoods",
  "3" = "Suburban Low-Rise neighborhoods",
  "4" = "Urban Core Low-Rise neighborhoods",
  "5" = "High-Rise Compact neighborhoods"
)

data_raw$Cluster <- factor(
  data_raw$Cluster,
  levels = names(Cluster_name_map),
  labels = Cluster_name_map
)

data_raw$CY <- as.factor(data_raw$CY)
data_raw$Cluster <- as.factor(data_raw$Cluster)

#===============================================================================
# 2. 颜色
#===============================================================================
cols_Cluster <- c(
  "Inner-Green High-Rise neighborhoods" = "#f8837c",
  "Suburban Mega-Sized neighborhoods" = "#a2a501",
  "Suburban Low-Rise neighborhoods" = "#01bf7d",
  "Urban Core Low-Rise neighborhoods" = "#14b5f6",
  "High-Rise Compact neighborhoods" = "#e770f3"
)

time_label_map <- c(
  lst0708_ME = "07:08",
  lst1042_ME = "10:42",
  lst1349_ME = "13:49",
  lst1540_ME = "15:40",
  lst1959_ME = "19:59",
  lst0016_ME = "00:16",
  lst0404_ME = "04:04"
)

#===============================================================================
# 3. 单个时相：训练 RF + 计算 TCC / TCC_buf 的分 Cluster SHAP 重要性
#===============================================================================
run_tcc_importance <- function(current_lst, data) {
  
  message("正在处理：", current_lst)
  
  selected_vars <- c(
    "TCC", "BCI", "WCI", "GCI", "CY",
    "BAH", "BHSD", "SVF", "Albedo",
    "POP", "FAR", "MR",
    "TCC_buf", "BCI_buf", "WCI_buf", "GCI_buf",
    "BAH_buf", "BHSD_buf","DIST",
    current_lst,
    "Cluster"
  )
  
  model_data <- data %>%
    dplyr::select(all_of(selected_vars)) %>%
    na.omit()
  
  x_data <- model_data %>%
    dplyr::select(-all_of(current_lst), -Cluster)
  
  y_data <- model_data[[current_lst]]
  Cluster_vec <- model_data$Cluster
  
  #------------------------------
  # 贝叶斯优化 RF
  #------------------------------
  rf_optim_fun <- function(mtry, num.trees, min.node.size) {
    
    mtry <- as.integer(round(mtry))
    num.trees <- as.integer(round(num.trees))
    min.node.size <- as.integer(round(min.node.size))
    
    ctrl <- trainControl(method = "cv", number = 5)
    
    rf_model <- train(
      x = x_data,
      y = y_data,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = expand.grid(
        mtry = mtry,
        splitrule = "variance",
        min.node.size = min.node.size
      ),
      metric = "RMSE",
      num.trees = num.trees
    )
    
    list(Score = -rf_model$results$RMSE)
  }
  
  search_grid <- list(
    mtry = c(1L, 15L),
    num.trees = c(100L, 2000L),
    min.node.size = c(1L, 9L)
  )
  
  seed_list <- c(
    lst0708_ME = 123,
    lst1042_ME = 234,
    lst1349_ME = 345,
    lst1540_ME = 456,
    lst1959_ME = 567,
    lst0016_ME = 678,
    lst0404_ME = 789
  )
  
  set.seed(seed_list[current_lst])
  
  opt_res <- ParBayesianOptimization::bayesOpt(
    FUN = rf_optim_fun,
    bounds = search_grid,
    initPoints = 10,
    nIter = 30,
    verbose = 0
  )
  
  best_params <- ParBayesianOptimization::getBestPars(opt_res)
  best_params <- lapply(best_params, function(x) as.integer(round(x)))
  
  #------------------------------
  # 最终 RF 模型
  #------------------------------
  rf_final <- train(
    x = x_data,
    y = y_data,
    method = "ranger",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(
      mtry = best_params$mtry,
      splitrule = "variance",
      min.node.size = best_params$min.node.size
    ),
    num.trees = best_params$num.trees,
    metric = "RMSE"
  )
  
  pred_wrapper <- function(object, newdata) {
    predict(object, newdata = newdata)
  }
  
  #------------------------------
  # SHAP
  #------------------------------
  set.seed(seed_list[current_lst])
  
  shap_values <- fastshap::explain(
    object = rf_final,
    X = x_data,
    nsim = 50,
    pred_wrapper = pred_wrapper
  )
  
  shap_df <- as.data.frame(shap_values)
  
  #------------------------------
  # 计算所有变量的 Mean |SHAP|
  # 再提取 TCC 与 TCC_buf 在所有变量中的占比
  #------------------------------
  imp_all_df <- shap_df %>%
    mutate(Cluster = Cluster_vec) %>%
    group_by(Cluster) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = ~ mean(abs(.x), na.rm = TRUE)
      ),
      .groups = "drop"
    )
  
  imp_total_df <- imp_all_df %>%
    rowwise() %>%
    mutate(
      total_importance = sum(c_across(-Cluster), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    dplyr::select(Cluster, total_importance)
  
  imp_df <- imp_all_df %>%
    dplyr::select(Cluster, TCC, TCC_buf) %>%
    pivot_longer(
      cols = c(TCC, TCC_buf),
      names_to = "variable",
      values_to = "importance"
    ) %>%
    left_join(imp_total_df, by = "Cluster") %>%
    mutate(
      importance_pct = importance / total_importance * 100,
      y_var = current_lst,
      time_label = time_label_map[current_lst],
      variable = factor(variable, levels = c("TCC", "TCC_buf"))
    )
  
  return(imp_df)
}

#===============================================================================
# 4. 批量运行 7 个时相
#===============================================================================
y_vars <- c(
  "lst0708_ME",
  "lst1042_ME",
  "lst1349_ME",
  "lst1540_ME",
  "lst1959_ME",
  "lst0016_ME",
  "lst0404_ME"
)

all_imp_df <- purrr::map_dfr(
  y_vars,
  ~ run_tcc_importance(
    current_lst = .x,
    data = data_raw
  )
)

write.csv(
  all_imp_df,
  file.path(output_dir, "TCC_TCCbuf_SHAP_importance_by_Cluster.csv"),
  row.names = FALSE
)

#===============================================================================
# 5. 绘图函数：七个时间段合并展示 TCC 与 TCC_buf 重要性
#===============================================================================
plot_tcc_importance_all <- function(df, title_text) {
  
  ggplot(
    df,
    aes(
      x = time_label,
      y = importance,
      fill = Cluster
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.85),
      width = 0.75,
      alpha = 0.85
    ) +
    geom_text(
      aes(
        label = sprintf(
          "%.2f\n(%.0f%%)",
          importance,
          importance_pct
        )
      ),
      position = position_dodge(width = 0.85),
      vjust = -0.35,
      size = 3.3,
      fontface = "bold",
      lineheight = 0.85
    ) +
    facet_wrap(
      ~ variable,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_fill_manual(values = cols_Cluster) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(
      x = NULL,
      y = "Mean |SHAP|",
      fill = "Neighborhood Cluster",
      title = title_text
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 1),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(5, "pt"),
      axis.text.x = element_text(size = 13, face = "bold"),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 15, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 12)
    )
}

#===============================================================================
# 6. 七个时间段合并绘图
#===============================================================================
all_time_vars <- c(
  "lst0708_ME",
  "lst1042_ME",
  "lst1349_ME",
  "lst1540_ME",
  "lst1959_ME",
  "lst0016_ME",
  "lst0404_ME"
)

all_time_imp_df <- all_imp_df %>%
  filter(y_var %in% all_time_vars) %>%
  mutate(
    time_label = factor(
      time_label,
      levels = time_label_map[all_time_vars]
    ),
    variable = factor(
      variable,
      levels = c("TCC", "TCC_buf")
    )
  )

p_all <- plot_tcc_importance_all(
  all_time_imp_df,
  "Importance of TCC and TCC_buf across Seven Time Periods"
)

#===============================================================================
# 7. 保存结果
#===============================================================================
ggsave(
  file.path(output_dir, "All_Time_TCC_TCCbuf_Importance_by_Cluster.png"),
  p_all,
  width = 15,
  height = 10,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_dir, "All_Time_TCC_TCCbuf_Importance_by_Cluster.pdf"),
  p_all,
  width = 15,
  height = 10,
  bg = "white"
)

message("全部完成！七个时间段合并图已保存至：", output_dir)