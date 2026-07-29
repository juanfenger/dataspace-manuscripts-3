#===============================================================================
# 0. 库加载
#===============================================================================

library(tidyverse)
library(readxl)
library(ranger)
library(caret)
library(ParBayesianOptimization)
library(fastshap)
library(shapviz)
library(MASS)
library(patchwork)
library(tidytext)
library(scales)
library(purrr)
library(future)
library(furrr)
library(colorspace)

#===============================================================================
# 1. 数据读取与基础处理
#===============================================================================

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data_raw <- readxl::read_excel(data_path)

cluster_name_map <- c(
  "1" = "Inner-Green High-Rise neighborhoods",
  "2" = "Suburban Mega-Sized neighborhoods",
  "3" = "Suburban Low-Rise neighborhoods",
  "4" = "Urban Core Low-Rise neighborhoods",
  "5" = "High-Rise Compact neighborhoods"
)

data_raw$Cluster <- factor(
  data_raw$Cluster,
  levels = names(cluster_name_map),
  labels = cluster_name_map
)

data_raw$CY <- as.factor(data_raw$CY)
data_raw$Cluster <- as.factor(data_raw$Cluster)

#===============================================================================
# 2. 变量名称映射
#===============================================================================

var_name_map <- c(
  TCC = "TCC",
  BCI = "BCI",
  WCI = "WCI",
  GCI = "GCI",
  CY = "CY",
  BAH = "BAH",
  BHSD = "BHSD",
  SVF = "SVF",
  Albedo = "Albedo",
  POP = "POP",
  FAR = "FAR",
  RD = "RD",
  TCC_buf = "TCC_buf",
  BCI_buf = "BCI_buf",
  WCI_buf = "WCI_buf",
  GCI_buf = "GCI_buf",
  BAH_buf = "BAH_buf",
  BHSD_buf = "BHSD_buf",
  DIST = "DIST"
)

rename_with_map <- function(df, var_name_map) {
  old_names <- colnames(df)
  new_names <- ifelse(
    old_names %in% names(var_name_map),
    var_name_map[old_names],
    old_names
  )
  colnames(df) <- new_names
  df
}

#===============================================================================
# 3. 颜色设置
#===============================================================================

cols_point <- c(
  "Inner-Green High-Rise neighborhoods" = "#f8837c",
  "Suburban Mega-Sized neighborhoods" = "#a2a501",
  "Suburban Low-Rise neighborhoods" = "#01bf7d",
  "Urban Core Low-Rise neighborhoods" = "#14b5f6",
  "High-Rise Compact neighborhoods" = "#e770f3"
)

cols_line <- c(
  "Inner-Green High-Rise neighborhoods" = darken("#f8837c", 0.3),
  "Suburban Mega-Sized neighborhoods" = darken("#a2a501", 0.3),
  "Suburban Low-Rise neighborhoods" = darken("#01bf7d", 0.1),
  "Urban Core Low-Rise neighborhoods" = darken("#14b5f6", 0.3),
  "High-Rise Compact neighborhoods" = darken("#e770f3", 0.3)
)

bar_color_map <- c(
  lst0708_ME = "#FDAE61",
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"
)

#===============================================================================
# 4. 全集 SHAP Bar 图函数
#===============================================================================

create_overall_shap_bar <- function(shap_df_all, current_lst, bar_color_map) {
  
  current_bar_color <- bar_color_map[current_lst]
  
  imp_overall_df <- shap_df_all %>%
    summarise(across(everything(), ~ mean(abs(.), na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "importance"
    )
  
  var_order <- imp_overall_df %>%
    arrange(desc(importance)) %>%
    pull(variable)
  
  imp_overall_df$variable <- factor(
    imp_overall_df$variable,
    levels = rev(var_order)
  )
  
  ggplot(imp_overall_df, aes(x = importance, y = variable)) +
    geom_col(fill = current_bar_color, alpha = 0.5, color = NA) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Mean |SHAP| (Overall)",
      y = NULL,
      title = paste(sub("_ME", "", current_lst), "Overall Importance")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      axis.text.y = element_text(size = 13, face = "bold"),
      axis.text.x = element_text(size = 13),
      axis.title.x = element_text(size = 13, margin = margin(t = 5)),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
    )
}

#===============================================================================
# 5. 分 Cluster 依赖图函数：5行 × 2列
#===============================================================================

create_dep_plot_list <- function(
    vv,
    x_df_all,
    shap_df_all,
    cluster_vec,
    cols_point,
    cols_line
) {
  
  find_loess_roots <- function(df, span = 0.75, n_grid = 500) {
    
    df <- df %>%
      filter(is.finite(x), is.finite(shap)) %>%
      arrange(x)
    
    if (nrow(df) < 10 || length(unique(df$x)) < 5) {
      return(numeric(0))
    }
    
    fit <- tryCatch(
      loess(shap ~ x, data = df, span = span, degree = 2),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(numeric(0))
    
    x_grid <- seq(
      min(df$x, na.rm = TRUE),
      max(df$x, na.rm = TRUE),
      length.out = n_grid
    )
    
    y_pred <- tryCatch(
      predict(fit, newdata = data.frame(x = x_grid)),
      error = function(e) rep(NA_real_, length(x_grid))
    )
    
    valid <- is.finite(y_pred)
    x_grid <- x_grid[valid]
    y_pred <- y_pred[valid]
    
    if (length(x_grid) < 2) return(numeric(0))
    
    roots <- c()
    
    for (i in seq_len(length(x_grid) - 1)) {
      
      y1 <- y_pred[i]
      y2 <- y_pred[i + 1]
      x1 <- x_grid[i]
      x2 <- x_grid[i + 1]
      
      if (is.na(y1) || is.na(y2)) next
      
      if (y1 == 0) {
        roots <- c(roots, x1)
      }
      
      if (y1 * y2 < 0) {
        root_x <- x1 - y1 * (x2 - x1) / (y2 - y1)
        roots <- c(roots, root_x)
      }
    }
    
    unique(round(roots, 4))
  }
  
  plot_data_all <- tibble(
    x = x_df_all[[vv]],
    shap = shap_df_all[[vv]],
    Cluster = cluster_vec
  ) %>%
    drop_na()
  
  x_lim <- range(plot_data_all$x, na.rm = TRUE)
  y_lim <- range(plot_data_all$shap, na.rm = TRUE)
  
  dep_list <- lapply(levels(cluster_vec), function(cl) {
    
    plot_data <- plot_data_all %>%
      filter(Cluster == cl)
    
    roots <- find_loess_roots(plot_data)
    
    p <- ggplot(plot_data, aes(x = x, y = shap)) +
      geom_point(
        color = cols_point[cl],
        alpha = 0.35,
        size = 1.1,
        stroke = 0
      ) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        color = cols_line[cl],
        linewidth = 1.1
      ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "gray40",
        linewidth = 0.6
      ) +
      coord_cartesian(xlim = x_lim, ylim = y_lim) +
      labs(
        title = NULL,
        x = vv,
        y = paste0("SHAP value of ", vv)
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.9),
        axis.ticks = element_line(color = "black", linewidth = 0.8),
        axis.ticks.length = unit(5, "pt"),
        axis.text = element_text(size = 10.5, color = "black"),
        axis.title = element_text(size = 11.5, color = "black")
      )
    
    if (length(roots) > 0) {
      p <- p +
        geom_vline(
          xintercept = roots,
          linetype = "dotted",
          color = "black",
          linewidth = 0.7
        ) +
        annotate(
          "text",
          x = roots,
          y = y_lim[2],
          label = paste0("x=", round(roots, 2)),
          angle = 90,
          vjust = -0.25,
          hjust = 1,
          size = 3,
          fontface = "bold"
        )
    }
    
    p
  })
  
  names(dep_list) <- levels(cluster_vec)
  dep_list
}


create_cluster_pair_dep_grid <- function(
    x_df_all,
    shap_df_all,
    cluster_vec,
    current_lst,
    cols_point,
    cols_line
) {
  
  dep_tcc_list <- create_dep_plot_list(
    vv = "TCC",
    x_df_all = x_df_all,
    shap_df_all = shap_df_all,
    cluster_vec = cluster_vec,
    cols_point = cols_point,
    cols_line = cols_line
  )
  
  dep_buf_list <- create_dep_plot_list(
    vv = "TCC_buf",
    x_df_all = x_df_all,
    shap_df_all = shap_df_all,
    cluster_vec = cluster_vec,
    cols_point = cols_point,
    cols_line = cols_line
  )
  
  plot_list <- list()
  
  for (cl in levels(cluster_vec)) {
    
    p_tcc <- dep_tcc_list[[cl]] +
      labs(title = paste0(cl, " - TCC")) +
      theme(
        plot.title = element_text(size = 12.5, face = "bold", hjust = 0.5)
      )
    
    p_buf <- dep_buf_list[[cl]] +
      labs(title = paste0(cl, " - TCC_buf")) +
      theme(
        plot.title = element_text(size = 12.5, face = "bold", hjust = 0.5)
      )
    
    plot_list <- c(plot_list, list(p_tcc, p_buf))
  }
  
  wrap_plots(plot_list, ncol = 2) +
    plot_annotation(
      title = paste0(sub("_ME", "", current_lst), " Cluster-specific SHAP Dependence"),
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    )
}

#===============================================================================
# 6. 核心函数：RF + Bayesian Optimization + SHAP
#===============================================================================

run_shap_analysis <- function(
    current_lst,
    data,
    var_name_map,
    cluster_name_map,
    cols_point,
    cols_line,
    bar_color_map
) {
  
  message(paste0("--- 开始处理因变量: ", current_lst, " ---"))
  
  selected_vars <- c(
    "TCC", "BCI", "WCI", "GCI", "CY",
    "BAH", "BHSD", "SVF", "Albedo",
    "POP", "FAR", "MR",
    "TCC_buf", "BCI_buf", "WCI_buf", "GCI_buf",
    "BAH_buf", "BHSD_buf", "DIST",
    current_lst,
    "Cluster"
  )
  
  missing_vars <- setdiff(selected_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop(paste0(
      "以下变量在数据中不存在: ",
      paste(missing_vars, collapse = ", ")
    ))
  }
  
  model_data <- data[, selected_vars] %>%
    na.omit()
  
  x_data <- model_data[, !(colnames(model_data) %in% c(current_lst, "Cluster"))]
  y_data <- model_data[[current_lst]]
  cluster_vec <- model_data$Cluster
  
  #---------------------------------------------------------------------------
  # 6.1 Bayesian Optimization
  #---------------------------------------------------------------------------
  
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
    "lst0708_ME" = 123,
    "lst1042_ME" = 234,
    "lst1349_ME" = 345,
    "lst1540_ME" = 456,
    "lst1959_ME" = 567,
    "lst0016_ME" = 678,
    "lst0404_ME" = 789
  )
  
  set.seed(as.integer(seed_list[current_lst]))
  
  opt_res <- ParBayesianOptimization::bayesOpt(
    FUN = rf_optim_fun,
    bounds = search_grid,
    initPoints = 10,
    nIter = 30,
    verbose = 0
  )
  
  best_params <- ParBayesianOptimization::getBestPars(opt_res)
  best_params <- lapply(best_params, function(x) as.integer(round(x)))
  
  #---------------------------------------------------------------------------
  # 6.2 最终 RF 模型
  #---------------------------------------------------------------------------
  
  rf_cv_final <- train(
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
  
  #---------------------------------------------------------------------------
  # 6.3 SHAP 计算
  #---------------------------------------------------------------------------
  
  pred_wrapper <- function(object, newdata) {
    predict(object, newdata = newdata)
  }
  
  set.seed(as.integer(seed_list[current_lst]))
  
  shap_values <- fastshap::explain(
    object = rf_cv_final,
    X = x_data,
    nsim = 50,
    pred_wrapper = pred_wrapper
  )
  
  shap_df_all <- as.data.frame(shap_values)
  x_df_all <- as.data.frame(x_data)
  
  shap_df_all <- rename_with_map(shap_df_all, var_name_map)
  x_df_all <- rename_with_map(x_df_all, var_name_map)
  
  #---------------------------------------------------------------------------
  # 6.4 全集 SHAP Bar 图
  #---------------------------------------------------------------------------
  
  p_overall_bar <- create_overall_shap_bar(
    shap_df_all = shap_df_all,
    current_lst = current_lst,
    bar_color_map = bar_color_map
  )
  
  #---------------------------------------------------------------------------
  # 6.5 分 Cluster SHAP Bar 图
  #---------------------------------------------------------------------------
  
  shap_list_raw <- split(shap_df_all, cluster_vec)
  
  imp_df <- map_dfr(names(shap_list_raw), function(cl_name) {
    
    df <- shap_list_raw[[cl_name]]
    imp <- colMeans(abs(df), na.rm = TRUE)
    
    tibble(
      variable = names(imp),
      importance = imp,
      Cluster = cl_name
    )
  })
  
  imp_df$Cluster <- factor(imp_df$Cluster, levels = levels(cluster_vec))
  
  var_order <- imp_df %>%
    group_by(variable) %>%
    summarise(mean_imp = mean(importance), .groups = "drop") %>%
    arrange(desc(mean_imp)) %>%
    pull(variable)
  
  imp_df$variable <- factor(imp_df$variable, levels = var_order)
  
  p_bar_clustered <- ggplot(
    imp_df,
    aes(x = variable, y = importance, fill = Cluster)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = cols_point) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = NULL,
      y = "Mean |SHAP|",
      title = paste("Feature Importance:", sub("_ME", "", current_lst))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.line = element_line(color = "black", linewidth = 1),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 11,
        face = "bold"
      ),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  #---------------------------------------------------------------------------
  # 6.6 分 Cluster Beeswarm 图
  #---------------------------------------------------------------------------
  
  sv_list_beeswarm <- lapply(levels(cluster_vec), function(cl) {
    
    idx <- which(cluster_vec == cl)
    
    shapviz(
      as.matrix(shap_df_all[idx, ]),
      X = x_df_all[idx, ]
    )
  })
  
  names(sv_list_beeswarm) <- levels(cluster_vec)
  
  plots_beeswarm <- lapply(names(sv_list_beeswarm), function(cl_name) {
    
    sv_importance(
      sv_list_beeswarm[[cl_name]],
      kind = "beeswarm",
      max_display = 10,
      show_numbers = FALSE
    ) +
      ggtitle(cl_name) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.ticks.length = unit(6, "pt"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        panel.border = element_blank()
      )
  })
  
  p_beeswarm_row <- wrap_plots(plots_beeswarm, nrow = 1)
  
  p_clustered_combo <- p_bar_clustered / p_beeswarm_row +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(title = "Clustered Importance")
  
  p_final_importance <- wrap_plots(
    p_overall_bar + theme(plot.margin = margin(l = 5, r = 5, t = 10)),
    p_clustered_combo + theme(plot.margin = margin(l = 5, r = 5, t = 10)),
    ncol = 2,
    widths = c(2, 5)
  ) +
    plot_annotation(
      tag_levels = "a",
      title = sub("_ME", "", current_lst)
    )
  
  #---------------------------------------------------------------------------
  # 6.7 分 Cluster 依赖图：5行 × 2列
  #---------------------------------------------------------------------------
  
  p_dep_cluster_pair_grid <- create_cluster_pair_dep_grid(
    x_df_all = x_df_all,
    shap_df_all = shap_df_all,
    cluster_vec = cluster_vec,
    current_lst = current_lst,
    cols_point = cols_point,
    cols_line = cols_line
  )
  
  message(paste0("--- ", current_lst, " 处理完成 ---"))
  
  return(list(
    y_var = current_lst,
    plot_importance_combo = p_final_importance,
    plot_dep_cluster_pair = p_dep_cluster_pair_grid
  ))
}

#===============================================================================
# 7. 并行执行
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

n_cores <- max(1, future::availableCores() - 1)
plan(multisession, workers = n_cores)

message("开始并行处理...")

all_results <- furrr::future_map(
  y_vars,
  ~ run_shap_analysis(
    current_lst = .,
    data = data_raw,
    var_name_map = var_name_map,
    cluster_name_map = cluster_name_map,
    cols_point = cols_point,
    cols_line = cols_line,
    bar_color_map = bar_color_map
  ),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
)

future::plan(sequential)

#===============================================================================
# 8. 输出路径
#===============================================================================

output_dir <- "E:/第三篇论文/plot/Final_New_Style/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#===============================================================================
# 9. 保存每个时相的重要性组合图
#===============================================================================

purrr::walk(all_results, function(res) {
  
  fname <- paste0(
    "Final_Importance_Combo_",
    sub("_ME", "", res$y_var),
    ".png"
  )
  
  ggsave(
    filename = file.path(output_dir, fname),
    plot = res$plot_importance_combo,
    width = 26,
    height = 12,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
})

message("所有重要性组合图已保存。")

#===============================================================================
# 10. 保存所有时相的 5行 × 2列分 Cluster 依赖图
#===============================================================================

target_dep_yvars <- c(
  "lst0708_ME",
  "lst1042_ME",
  "lst1349_ME",
  "lst1540_ME",
  "lst1959_ME",
  "lst0016_ME",
  "lst0404_ME"
)

dep_results <- all_results %>%
  purrr::keep(~ .$y_var %in% target_dep_yvars)

for (res in dep_results) {
  
  time_label <- case_when(
    res$y_var == "lst0708_ME" ~ "07:08",
    res$y_var == "lst1042_ME" ~ "10:42",
    res$y_var == "lst1349_ME" ~ "13:49",
    res$y_var == "lst1540_ME" ~ "15:40",
    res$y_var == "lst1959_ME" ~ "19:59",
    res$y_var == "lst0016_ME" ~ "00:16",
    res$y_var == "lst0404_ME" ~ "04:04",
    TRUE ~ sub("_ME", "", res$y_var)
  )
  
  p_final_dep <- res$plot_dep_cluster_pair +
    plot_annotation(
      title = paste0(time_label, " Cluster-specific SHAP Dependence"),
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
    )
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "Cluster_Pair_Dep_TCC_TCCbuf_",
        sub("_ME", "", res$y_var),
        ".png"
      )
    ),
    plot = p_final_dep,
    width = 10,
    height = 20,
    dpi = 300,
    scale = 0.7,
    bg = "white",
    limitsize = FALSE
  )
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "Cluster_Pair_Dep_TCC_TCCbuf_",
        sub("_ME", "", res$y_var),
        ".pdf"
      )
    ),
    plot = p_final_dep,
    width = 10,
    height = 20,
    scale = 0.7,
    bg = "white",
    limitsize = FALSE
  )
}

message("所有时相的 TCC / TCC_buf 分 Cluster 依赖图已保存。")

#===============================================================================
# 11. 拼接 13:49 与 00:16
#===============================================================================

res_1349 <- all_results %>%
  purrr::keep(~ .$y_var == "lst1349_ME") %>%
  purrr::pluck(1)

res_0016 <- all_results %>%
  purrr::keep(~ .$y_var == "lst0016_ME") %>%
  purrr::pluck(1)

p_1349 <- res_1349$plot_dep_cluster_pair +
  plot_annotation(
    title = "13:49",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      )
    )
  )

p_0016 <- res_0016$plot_dep_cluster_pair +
  plot_annotation(
    title = "00:16",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5
      )
    )
  )

#------------------------------------------------------------------------------
# 横向拼接
#------------------------------------------------------------------------------

p_compare_1349_0016 <- p_1349 | p_0016

#------------------------------------------------------------------------------
# PNG
#------------------------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "Cluster_Pair_Dep_1349_vs_0016.png"
  ),
  plot = p_compare_1349_0016,
  width = 20,
  scale = 0.7,
  height = 20,
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)

#------------------------------------------------------------------------------
# PDF
#------------------------------------------------------------------------------

ggsave(
  filename = file.path(
    output_dir,
    "Cluster_Pair_Dep_1349_vs_0016.pdf"
  ),
  plot = p_compare_1349_0016,
  width = 20,
  height = 20,
  scale = 0.7,
  bg = "white",
  limitsize = FALSE
)

message("13:49 与 00:16 对比图已保存。")
