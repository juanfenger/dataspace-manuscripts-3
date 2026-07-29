# 1. 设置国内CRAN镜像（加快安装速度，可选）
# 清华镜像
options(repos = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
# 也可以选择阿里云镜像
# options(repos = c(CRAN="https://mirrors.aliyun.com/CRAN/"))

# 2. 定义需要安装的包列表
required_packages <- c(
  "tidyverse",    # 包含ggplot2、dplyr等核心包
  "ranger",       # 随机森林包
  "caret",        # 机器学习建模包
  "ParBayesianOptimization",  # 贝叶斯优化包
  "fastshap",     # SHAP值计算包
  "shapviz",      # SHAP值可视化包
  "MASS",         # 经典统计包（基础包但需确认安装）
  "patchwork",    # 拼图包
  "tidytext",     # 文本挖掘包
  "scales",       # 尺度调整包
  "purrr",        # 函数式编程包（tidyverse已包含，但单独列出确保安装）
  "future",       # 并行计算包
  "furrr",        # future版purrr
  "colorspace"    # 颜色空间处理包
)

# 3. 定义安装函数（带检查和错误处理）
install_if_missing <- function(pkg) {
  # 检查包是否已安装
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("正在安装包：", pkg))
    # 安装包，同时安装依赖
    install.packages(pkg, dependencies = c("Depends", "Imports", "Suggests"))
    
    # 验证安装是否成功
    if (requireNamespace(pkg, quietly = TRUE)) {
      message(paste("包", pkg, "安装成功！"))
    } else {
      warning(paste("包", pkg, "安装失败，请手动检查！"))
    }
  } else {
    message(paste("包", pkg, "已安装，跳过！"))
  }
}

# 4. 批量安装所有包
invisible(lapply(required_packages, install_if_missing))

# 5. 验证并加载所有包（可选）
message("\n开始加载所有包...")
load_packages <- function(pkg) {
  library(pkg, character.only = TRUE)
  message(paste("已加载包：", pkg))
}

# 尝试加载所有包，失败时给出提示
sapply(required_packages, function(p) {
  tryCatch(load_packages(p), 
           error = function(e) warning(paste("加载", p, "失败：", e$message)))
})

message("\n所有包安装/加载流程完成！")





#===============================================================================
# 步骤 0: 库加载与并行化环境设置
#===============================================================================
library(tidyverse)
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

# --------------------------------------------------
# 🔑 原始数据读取与基础处理
# --------------------------------------------------
data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data_raw <- readxl::read_excel(data_path) 

cluster_name_map <- c(
  "1" = "Low-rise open neighborhoods",
  "2" = "Greening buffer neighborhoods",
  "3" = "Dense population neighborhoods",
  "4" = "Green mid-rise neighborhoods",
  "5" = "High-rise compact neighborhoods"
)
# 注意：为了图表标签简洁，我稍微简化了 Cluster 名称，您可以根据需要改回全称

data_raw$cluster <- factor(
  data_raw$Cluster,
  levels = names(cluster_name_map),
  labels = cluster_name_map
)

data_raw$CY <- as.factor(data_raw$CY)
data_raw$cluster <- as.factor(data_raw$cluster)

# --------------------------------------------------
# 🔑 变量名称映射
# --------------------------------------------------
var_name_map <- c(
  TCC="TCC", BCI="BCI", WCI="WCI", GCI="GCI", CY="CY",
  BAH="BAH", BHSD="BHSD", SVF="SVF", Albedo="Albedo",
  POP="POP", FAR="FAR", MR="MR",DIST="DIST",
  TCC_buf="TCC_buf", BCI_buf="BCI_buf", WCI_buf ="WCI_buf", # 稍微缩短以便图表显示
  GCI_buf="GCI_buf",
  BAH_buf="BAH_buf", BHSD_buf="BHSD_buf"
)

# --------------------------------------------------
# 🎨 颜色美学定义 (关键修改)
# --------------------------------------------------
# 1. 散点颜色 (用户指定)
cols_point <- c(
  "Low-rise open neighborhoods" = "#f8837c",
  "Greening buffer neighborhoods" = "#a2a501",
  "Dense population neighborhoods" = "#01bf7d",
  "Green mid-rise neighborhoods" = "#14b5f6",
  "High-rise compact neighborhoods" = "#e770f3"
)

# 2. 拟合线颜色 (基于散点颜色加深，保证辨识度)
# 使用 colorspace::darken 加深 20%-30%
cols_line <- c(
  "Low-rise open neighborhoods" = darken("#f8837c", 0.3),
  "Greening buffer neighborhoods" = darken("#a2a501", 0.3),
  "Dense population neighborhoods" = darken("#01bf7d", 0.1),
  "Green mid-rise neighborhoods" = darken("#14b5f6", 0.3),
  "High-rise compact neighborhoods" = darken("#e770f3", 0.3)
)
# 3. ⚠️ 新增：全集 SHAP Bar 图的颜色映射
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
# 新函数：创建全集 SHAP Bar 图
#===============================================================================

#' @title 创建全集 Mean |SHAP| 柱状图
create_overall_shap_bar <- function(shap_df_all, current_lst, bar_color_map) {
  
  current_bar_color <- bar_color_map[current_lst]
  
  # 1. 计算全集 Mean |SHAP|
  imp_overall_df <- shap_df_all %>%
    summarise_all(~mean(abs(.), na.rm = TRUE)) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "importance")
  
  # 2. 排序
  var_order <- imp_overall_df %>%
    arrange(desc(importance)) %>%
    pull(variable)
  
  imp_overall_df$variable <- factor(imp_overall_df$variable, levels = rev(var_order)) # 倒序，让最重要的在顶部
  
  # 3. 绘图
  p_overall_bar <- ggplot(imp_overall_df, aes(x = importance, y = variable)) +
    geom_col(fill = current_bar_color, alpha = 0.5, color = NA) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Mean |SHAP| (Overall)",
      y = NULL,
      title = paste(sub("_ME", "", current_lst), "Overall Importance")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(), # 移除横向网格线
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      axis.text.y = element_text(size = 13, face = "bold"), # Y轴变量名加粗
      axis.text.x = element_text(size = 13),
      axis.title.x = element_text(size = 13, margin = margin(t = 5)),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
    )
  
  return(p_overall_bar)
}


#===============================================================================
# 核心函数封装：run_shap_analysis
#===============================================================================
#' @title 运行随机森林模型、SHAP分析并生成所有图表
run_shap_analysis <- function(current_lst, data, var_name_map, cluster_name_map, cols_point, cols_line, bar_color_map) {
  message(paste0("--- 开始处理因变量: ", current_lst, " ---"))
  # 1. 数据准备
  selected_vars <- c(
    "DIST","TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf",current_lst,
    "cluster"
  )
  
  model_data <- na.omit(data[, selected_vars])
  x_data <- model_data[, !(colnames(model_data) %in% c(current_lst, "cluster"))]
  y_data <- model_data[[current_lst]]
  cluster_vec <- model_data$cluster

  # 2. 随机森林贝叶斯优化与训练 (此处使用 caret::train)
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
  
  seed_list <- c("lst0708_ME"=123, "lst1042_ME"=234, "lst1349_ME"=345, "lst1540_ME"=456, 
                 "lst1959_ME"=567, "lst0016_ME"=678, "lst0404_ME"=789)
  set.seed(seed_list[current_lst]) 
  
  # 贝叶斯优化
  opt_res <- ParBayesianOptimization::bayesOpt(
    FUN = rf_optim_fun,
    bounds = search_grid,
    initPoints = 10,
    nIter = 30,
    verbose = 0 
  )
  
  best_params <- ParBayesianOptimization::getBestPars(opt_res)
  best_params <- lapply(best_params, function(x) as.integer(round(x)))
  
  # 训练最终模型 (caret::train 对象，用于 SHAP 计算)
  rf_cv_final <- train(
    x = x_data,
    y = y_data,
    method = "ranger",
    # 此处使用 CV 5折来评估最终性能，并作为 SHAP 的 object
    trControl = trainControl(method = "cv", number = 5), 
    tuneGrid = expand.grid(
      mtry = best_params$mtry,
      splitrule = "variance",
      min.node.size = best_params$min.node.size
    ),
    num.trees = best_params$num.trees,
    metric = "RMSE"
  )
  
  # 3. SHAP 值计算
  # ----------------------------------------------------------------------------
  # ⚠️ 关键修正：针对 caret::train 对象的 pred_wrapper 
  pred_wrapper <- function(object, newdata) {
    # caret::train 对象的 predict 默认返回数值向量，无需 $ 操作符
    predict(object, newdata = newdata)
  }
  # ----------------------------------------------------------------------------
  
  set.seed(seed_list[current_lst]) # 再次设置种子以确保 SHAP 采样可复现
  shap_values <- fastshap::explain(
    object = rf_cv_final,
    X = x_data,
    nsim = 50, # 增加 nsim 确保结果更稳定
    pred_wrapper = pred_wrapper
  )
  
  # ----------------------------------------------------------------------------
  # 4. 图表生成 (重大调整)
  # ----------------------------------------------------------------------------
  
  # --- 准备工作：统一重命名 ---
  shap_df_all <- as.data.frame(shap_values)
  x_df_all <- x_data
  colnames(shap_df_all) <- var_name_map[colnames(shap_df_all)]
  colnames(x_df_all) <- var_name_map[colnames(x_df_all)]
  
  # 创建整体 shapviz 对象
  sv_all <- shapviz(as.matrix(shap_df_all), X = x_df_all)
  
  # --- 4.0 新增：全集 SHAP 柱状图 ---
  p_overall_bar <- create_overall_shap_bar(shap_df_all, current_lst, bar_color_map)
  
  # --- 4.0.1 新增：全集 SHAP 蜂窝图 ---
  p_overall_beeswarm <- sv_importance(
    sv_all,
    kind = "beeswarm",
    max_display = 20,
    show_numbers = FALSE
  ) +
    labs(
      title = paste("SHAP Beeswarm:", sub("_ME", "", current_lst))
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
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
  
  # 准备分组数据
  shap_list_raw <- split(shap_df_all, cluster_vec)
  
  # ==========================================================
  # 4.1. 簇状柱状图 (Clustered Bar Plot) - 展示所有因子
  # ==========================================================
  imp_df <- map_dfr(names(shap_list_raw), function(cl_name) {
    df <- shap_list_raw[[cl_name]]
    imp <- colMeans(abs(df), na.rm = TRUE)
    tibble(
      variable = names(imp),
      importance = imp,
      cluster = cl_name
    )
  })
  
  # 确保 Cluster 顺序
  imp_df$cluster <- factor(imp_df$cluster, levels = levels(cluster_vec))
  
  # 对变量进行排序（按所有 Cluster 的总重要性排序，或者按 Cluster 1 排序）
  # 这里选择按“总平均重要性”排序变量，使图表有序
  var_order <- imp_df %>%
    group_by(variable) %>%
    summarise(mean_imp = mean(importance)) %>%
    arrange(desc(mean_imp)) %>%
    pull(variable)
  
  imp_df$variable <- factor(imp_df$variable, levels = var_order)
  
  p_bar_clustered <- ggplot(imp_df, aes(x = variable, y = importance, fill = cluster)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = cols_point) + # 使用统一配色
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = "Mean |SHAP|", title = paste("Feature Importance:", sub("_ME", "", current_lst))) +
    theme_minimal(base_size = 13) +
    theme(
      axis.line = element_line(color = "black", linewidth = 1), 
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11, face = "bold"),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      legend.position = "top", # 仅保留一次定义
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA)
    )

  # ==========================================================
  # 4.2. 蜂窝图 (Beeswarm) - 分组拼接
  # ==========================================================
  # 构建每个 Cluster 的 sv 对象
  sv_list_beeswarm <- lapply(levels(cluster_vec), function(cl) {
    idx <- which(cluster_vec == cl)
    shapviz(as.matrix(shap_df_all[idx,]), X = x_df_all[idx,])
  })
  names(sv_list_beeswarm) <- levels(cluster_vec)
  
  # 生成 5 个蜂窝图
  plots_beeswarm <- lapply(names(sv_list_beeswarm), function(cl_name) {
    sv_importance(sv_list_beeswarm[[cl_name]], kind = "beeswarm", max_display = 10, show_numbers = FALSE) +
      ggtitle(cl_name) +
      theme_bw(base_size = 11) +
      theme(
        # ⬅️ 关键修正：添加此行以移除 Feature Value 的垂直图例
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.ticks.length = unit(6, "pt"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13),
        panel.border = element_blank(),
        strip.text = element_text(size = 13, face = "bold") # 调整为 13
      )
  })
  
  # 拼接蜂窝图 (1行5列)
  p_beeswarm_row <- wrap_plots(plots_beeswarm, nrow = 1)
  
  # ==========================================================
  # 4.3. 组合图 2: 分簇的重要性组合图 (新的 p_clustered_combo)
  # ==========================================================
  # 上面是 Cluster Bar，下面是 Beeswarm Row
  # 为了对齐，让 Cluster Bar 的高度与 Beeswarm 的高度接近
  p_clustered_combo <- p_bar_clustered / p_beeswarm_row +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Clustered Importance")
  
  # ==========================================================
  # 4.4. 最终拼接：全集Bar | 分簇Combo (p_final_importance)
  # ⚠️ 修正：变量名统一为 p_final_importance
  # ==========================================================
  p_final_importance <- wrap_plots(
    p_overall_bar + theme(plot.margin = margin(l=5, r=5, t=10)), 
    p_clustered_combo + theme(plot.margin = margin(l=5, r=5, t=10)),
    ncol = 2,
    widths = c(2, 5)
  ) + plot_annotation(tag_levels = 'a', title = sub("_ME", "", current_lst))
  
  # ==========================================================
  # 4.4. 依赖图 (Dependence) - 单轴多组整合
  # ==========================================================
  target_vars <- c("TCC", "TCC_buf") # 对应的变量名 (var_name_map 后的)
  
  # 我们需要手动绘图以精确控制颜色和图层
  dep_plots <- lapply(target_vars, function(vv) {
    
    # 提取绘图数据
    plot_data <- tibble(
      x = x_df_all[[vv]],
      shap = shap_df_all[[vv]],
      cluster = cluster_vec
    )
    
    # 绘图
    ggplot(plot_data, aes(x = x, y = shap, color = cluster, group = cluster)) +
      # 1. 散点 (使用指定的 point 颜色)
      geom_point(alpha = 0.3, size = 1.2, stroke = 0) +
      scale_color_manual(values = cols_point) +
      
      # 2. 拟合线 (使用加深的 line 颜色)
      # 这里的技巧是：添加一个新的图层，利用 ggnewscale 或者直接由 fill 控制?
      # 最简单的方法：手动添加 geom_smooth，指定 color 映射
      # 为了使用不同的颜色集，我们利用 ggnewscale 或者简单的图层覆盖技巧并不容易。
      # 最佳方案：散点用 alpha 区分，线用实色。或者这里我们使用 scale_color_manual 定义好的映射。
      # 为了实现“线是深色，点是浅色”，我们其实可以将 group 映射到 color，但使用 alpha 控制点。
      # 为了严格实现您的颜色要求（点一个色，线一个深色），我们需要稍微 hack 一下：
      # 方法：画线时，使用 overwrite 的 color aesthetic。
      
      new_scale_color() + # 需要 library(ggnewscale)，如果没有，可以用深色统一画线，或者只用一种颜色方案
      
      geom_smooth(aes(color = cluster), method = "loess", se = FALSE, linewidth = 1.2) +
      scale_color_manual(values = cols_line) + # 拟合线使用深色系
      
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      labs(
        title = paste(vv, "vs SHAP"),
        y = paste("SHAP value (", sub("_ME", "", current_lst), ")"),
        x = vv
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none", # 最终拼接时可以统一加图例，这里先去掉
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # ⬅️ 关键修正：添加这一行以显示坐标轴线
        axis.line = element_line(color = "black", linewidth = 1),
        axis.text.x  = element_text(size = 13), axis.text.y  = element_text(size = 13),
        axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13),
        axis.ticks = element_line(color = "black", linewidth = 1),
        axis.ticks.length = unit(6, "pt")
      )
  })
  
  names(dep_plots) <- target_vars
  
  # 将 TCC 和 TCC_buffer 拼在一起 (左右排列，或上下排列，您提到最后要拼成 4列2行)
  # 这里我们返回这两个独立的图对象，方便后面提取
  
  message(paste0("--- ", current_lst, " 处理完成 ---"))
  
  return(list(
    y_var = current_lst,
    
    # 仅全集重要性 bar 图
    plot_overall_bar = p_overall_bar,
    plot_overall_beeswarm = p_overall_beeswarm,
    # 重要性 bar 图 + 对应蜂窝图并列
    plot_bar_beeswarm_pair = wrap_plots(
      p_overall_bar,
      p_beeswarm_row,
      ncol = 2,
      widths = c(1.2, 3.8)
    ) +
      plot_annotation(title = sub("_ME", "", current_lst)),
    
    # 原依赖图保留
    plot_dep_tcc = dep_plots[["TCC"]],
    plot_dep_buf = dep_plots[["TCC_buf"]]
  ))
}

#===============================================================================
# 步骤 1 & 2: 执行并行计算
#===============================================================================
# 必须加载 ggnewscale 用于双重颜色映射
if(!require(ggnewscale)) install.packages("ggnewscale"); library(ggnewscale)

y_vars <- c("lst0708_ME", "lst1042_ME", "lst1349_ME", "lst1540_ME", "lst1959_ME", "lst0016_ME", "lst0404_ME")
# 仅做测试用，正式跑请用全量
#y_vars <- c("lst1540_ME") 

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

# ------------------------------------------------------------------------------
# 🚀 重新保存图表 (使用修正后的 all_results_fixed)
# ------------------------------------------------------------------------------
# 此时再运行你之前的“步骤 7”或“步骤 3”的保存代码即可
# 例如：
# purrr::walk(all_results_fixed, function(res) { ... ggsave(...) ... })
#===============================================================================
# 步骤 3: 结果保存与终极拼图 (2列7行布局)
#===============================================================================
output_dir <- "E:/第三篇论文/plot/Final_New_Style/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3.05 保存新的 "最终重要性组合图" (全集Bar | 分簇Combo)
# ------------------------------------------------------------------------------
purrr::walk(all_results, function(res) {
  fname <- paste0("Final_Importance_Combo_", sub("_ME", "", res$y_var), ".png")
  ggsave(file.path(output_dir, fname), res$plot_importance_combo,
         width = 30, height = 12, dpi = 300, bg = "white") # 增加宽度以容纳三张图
})

message("新的最终重要性组合图已保存。")

# ------------------------------------------------------------------------------
# 3.1 保存每个时间段的 "重要性组合图" (柱状 + 蜂窝) - 这一步保持不变
# ------------------------------------------------------------------------------
# 既然跑了一次，建议还是把这些中间结果存一下
purrr::walk(all_results, function(res) {
  fname <- paste0("Imp_Combo_", sub("_ME", "", res$y_var), ".png")
  ggsave(file.path(output_dir, fname), res$plot_importance_combo, 
         width = 18, height = 12, dpi = 300, bg = "white")
})


#===============================================================================
# 步骤 3: 结果保存与终极拼图 (4x4 布局，含空白补位)
#===============================================================================
output_dir <- "E:/第三篇论文/plot/Final_New_Style/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3.1 保存每个时间段的 "重要性组合图" (柱状 + 蜂窝) - 保持不变
# ------------------------------------------------------------------------------
purrr::walk(all_results, function(res) {
  fname <- paste0("Imp_Combo_", sub("_ME", "", res$y_var), ".png")
  ggsave(file.path(output_dir, fname), res$plot_importance_combo, 
         width = 26, height = 12, dpi = 300, bg = "white")
})


# ------------------------------------------------------------------------------
# 3.2 终极拼接：依赖图 (4列 x 4行)
# ------------------------------------------------------------------------------
# 逻辑：
# 1. TCC 组：7张图 + 1张空白 -> 占 2行 (每行4张)
# 2. Buffer 组：7张图 + 1张空白 -> 占 2行 (每行4张)
# 3. 上下拼接

# --- A. 准备 TCC 列表 (凑够8个) ---
list_tcc <- purrr::map(all_results, "plot_dep_tcc")
# 加入空白图补位
list_tcc_padded <- c(list_tcc, list(patchwork::plot_spacer()))

# 生成 TCC 部分的网格 (4列)
p_grid_tcc <- wrap_plots(list_tcc_padded, ncol = 4) + 
  plot_annotation(title = "Dependence: TCC (Top 2 Rows)",
                  theme = theme(plot.title = element_text(size = 14, face = "bold")))

# --- B. 准备 Buffer 列表 (凑够8个) ---
list_buf <- purrr::map(all_results, "plot_dep_buf")
# 加入空白图补位
list_buf_padded <- c(list_buf, list(patchwork::plot_spacer()))

# 生成 Buffer 部分的网格 (4列)
p_grid_buf <- wrap_plots(list_buf_padded, ncol = 4) + 
  plot_annotation(title = "Dependence: TCC_buffer (Bottom 2 Rows)",
                  theme = theme(plot.title = element_text(size = 14, face = "bold")))

# --- C. 准备图例 (独立制作) ---
# 定义颜色 (确保与之前一致)
cols_point <- c(
  "Low-rise open neighborhoods" = "#f8837c",
  "Greening buffer neighborhoods" = "#a2a501",
  "Dense population neighborhoods" = "#01bf7d",
  "Green mid-rise neighborhoods" = "#14b5f6",
  "High-rise compact neighborhoods" = "#e770f3"
)

# 制作虚拟图例
dummy_legend_plot <- ggplot(
  data.frame(cluster = factor(names(cols_point), levels = names(cols_point)), x=1, y=1), 
  aes(x, y, color = cluster)
) + 
  geom_point(size = 5, alpha = 0.8) + 
  # 加一条粗线模拟拟合线，表明图中包含线
  geom_segment(aes(x=1, xend=1.5, y=1, yend=1), linewidth=1) + 
  scale_color_manual(values = cols_point, name = "Neighborhood Cluster") +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = "bold"),
        legend.key.width = unit(1.5, "cm"))

# 提取图例对象
legend_only <- cowplot::get_legend(dummy_legend_plot)

# --- D. 最终组装 ---
# 结构：TCC网格 / Buffer网格 / 图例
# 高度比：TCC占10, Buffer占10, 图例占1
p_final_4x4 <- (p_grid_tcc / p_grid_buf / legend_only) + 
  plot_layout(heights = c(10, 10, 1))

# --- E. 保存 ---
message("正在保存 4x4 布局的大图...")

# 这里的尺寸设置为近似正方形或稍高，因为是 4列 x 4行
ggsave(file.path(output_dir, "Final_Dependence_4x4_Padded.png"), 
       p_final_4x4, 
       width = 20,   # 宽度 20 英寸
       height = 22,  # 高度 22 英寸 (留点空间给标题和图例)
       dpi = 300, 
       bg = "white",
       limitsize = FALSE)

message(paste0("处理完成！结果已保存至: ", file.path(output_dir, "Final_Dependence_4x4_Padded.png")))


  #===============================================================================
  # 步骤 7: 新增最终拼图输出
  #===============================================================================
  
  output_dir <- "E:/第三篇论文/plot/Final_New_Style/"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ------------------------------------------------------------
  # 7.0 工具函数：按指定 y_var 顺序提取图
  # ------------------------------------------------------------
  get_plot_by_order <- function(results, y_order, plot_name) {
    purrr::map(y_order, function(vv) {
      res <- purrr::keep(results, ~ .x$y_var == vv)[[1]]
      res[[plot_name]]
    })
  }
  
  # ------------------------------------------------------------
  # 7.1 第一个大图：仅排列不同时间段的重要性 bar 图
  # 新排列：
  # 第一行：07:08 + 10:42 + 13:49 + 15:40
  # 第二行：19:59 + 00:16 + 04:04
  # ------------------------------------------------------------
  
  p_bar_0708 <- get_plot_by_order(all_results, "lst0708_ME", "plot_overall_bar")[[1]]
  p_bar_1042 <- get_plot_by_order(all_results, "lst1042_ME", "plot_overall_bar")[[1]]
  p_bar_1349 <- get_plot_by_order(all_results, "lst1349_ME", "plot_overall_bar")[[1]]
  p_bar_1540 <- get_plot_by_order(all_results, "lst1540_ME", "plot_overall_bar")[[1]]
  
  p_bar_1959 <- get_plot_by_order(all_results, "lst1959_ME", "plot_overall_bar")[[1]]
  p_bar_0016 <- get_plot_by_order(all_results, "lst0016_ME", "plot_overall_bar")[[1]]
  p_bar_0404 <- get_plot_by_order(all_results, "lst0404_ME", "plot_overall_bar")[[1]]
  
  p_importance_bar_all <- 
    (p_bar_0708 | p_bar_1042 | p_bar_1349 | p_bar_1540) /
    (p_bar_1959 | p_bar_0016 | p_bar_0404 | patchwork::plot_spacer()) +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(tag_levels = "A")&
    theme(
      axis.text = element_text(
        size = 14,
        face = "bold"
      ),
      axis.title = element_text(
        size = 14,
        face = "bold"
      )
    )
  
  ggsave(
    file.path(output_dir, "dt.png"),
    p_importance_bar_all,
    width = 26,
    height = 16,
    dpi = 300,
    bg = "white",
    limitsize = FALSE,
    scale = 0.7
  )
  
  ggsave(
    file.path(output_dir, "Final_Overall_SHAP_Bar_Daytime_Nighttime_2rows.pdf"),
    p_importance_bar_all,
    width = 26,
    height = 16,
    bg = "white",
    limitsize = FALSE,
    scale = 0.88
  )
  
  # ------------------------------------------------------------
  # 7.2 新增大图：全集 SHAP 蜂窝图
  # 第一行：07:08 + 10:42 + 13:49 + 15:40
  # 第二行：19:59 + 00:16 + 04:04
  # ------------------------------------------------------------
  
  p_bee_0708 <- get_plot_by_order(all_results, "lst0708_ME", "plot_overall_beeswarm")[[1]]
  p_bee_1042 <- get_plot_by_order(all_results, "lst1042_ME", "plot_overall_beeswarm")[[1]]
  p_bee_1349 <- get_plot_by_order(all_results, "lst1349_ME", "plot_overall_beeswarm")[[1]]
  p_bee_1540 <- get_plot_by_order(all_results, "lst1540_ME", "plot_overall_beeswarm")[[1]]
  
  p_bee_1959 <- get_plot_by_order(all_results, "lst1959_ME", "plot_overall_beeswarm")[[1]]
  p_bee_0016 <- get_plot_by_order(all_results, "lst0016_ME", "plot_overall_beeswarm")[[1]]
  p_bee_0404 <- get_plot_by_order(all_results, "lst0404_ME", "plot_overall_beeswarm")[[1]]
  
  p_beeswarm_all <- 
    (
      (p_bee_0708 | p_bee_1042 | p_bee_1349 | p_bee_1540) /
        (p_bee_1959 | p_bee_0016 | p_bee_0404 | patchwork::plot_spacer())
    ) +
    plot_layout(heights = c(1,1)) +
    plot_annotation(tag_levels = "A") &
    theme(
      axis.text = element_text(
        size = 14,
        face = "bold"
      ),
      axis.title = element_text(
        size = 14,
        face = "bold"
      )
    )
  
  ggsave(
    file.path(output_dir, "Final_Overall_SHAP_Beeswarm_Daytime_Nighttime_2rows.png"),
    p_beeswarm_all,
    width = 36,
    height = 16,
    dpi = 300,
    bg = "white",
    limitsize = FALSE,
    scale = 0.7
  )
  
  ggsave(
    file.path(output_dir, "Final_Overall_SHAP_Beeswarm_Daytime_Nighttime_2rows.pdf"),
    p_beeswarm_all,
    width = 36,
    height = 16,
    bg = "white",
    limitsize = FALSE,
    scale = 0.7
  )
  
  # ------------------------------------------------------------
  # 7.3 第三类大图：重要性图 + 对应蜂窝图并列
  # 第二张：
  # 19:59 bar + beeswarm
  # 00:16 bar + beeswarm
  # 04:04 bar + beeswarm
  # ------------------------------------------------------------
  
  night_order <- c(
    "lst1959_ME",
    "lst0016_ME",
    "lst0404_ME"
  )
  
  plots_night_bar_beeswarm <- get_plot_by_order(
    all_results,
    night_order,
    "plot_bar_beeswarm_pair"
  )
  
  p_night_bar_beeswarm <- wrap_plots(
    plots_night_bar_beeswarm,
    ncol = 1
  ) +
    plot_annotation(tag_levels = "A")
  
  ggsave(
    file.path(output_dir, "Final_Nighttime_SHAP_Bar_Beeswarm_3x1.png"),
    p_night_bar_beeswarm,
    width = 30,
    height = 16,
    dpi = 300,
    bg = "white",
    limitsize = FALSE,
    scale = 0.7
  )
  
  ggsave(
    file.path(output_dir, "Final_Nighttime_SHAP_Bar_Beeswarm_3x1.pdf"),
    p_night_bar_beeswarm,
    width = 30,
    height = 16,
    bg = "white",
    limitsize = FALSE,
    scale = 0.7
  )
  
  message("新的4个大图已全部输出完成。")







