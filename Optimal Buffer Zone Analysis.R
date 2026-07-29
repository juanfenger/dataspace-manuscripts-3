 # ========================
 # 增强版批量分析脚本：输出 δLST + R²/RMSE 性能表，并绘制 Stacked Bar 图
 # ========================
 
 library(ranger)
 library(dplyr)
 library(tidyr)
 library(ggplot2)
 library(readxl)
 library(purrr)
 library(forcats)
 library(patchwork)
 
 # --- 参数设置 ---
 data_path <- "E:/第三篇论文/data/xqsx4.xls"
# data_path <- "E:/第三篇论文/脚本/Uploaded scripts and data/raw data before clustering.xls"
 
 target_vars <- c("lst0016_ME", "lst0404_ME", "lst0708_ME", 
                  "lst1042_ME", "lst1349_ME", "lst1540_ME", "lst1959_ME")
 
 base_vars <- c("TREE", "BUILDING", "WATER", "GRASS", "CY", 
                "BAH0M", "BHSD0M", "SVF", "Albedo", "POP", "FAR", "MR","DIST")
 
 buffer_distances <- c(70, 140, 210, 280, 350, 420, 490)
 
 get_buffer_vars <- function(dist) {
   paste0(c("b", "b", "b", "b", "BAH", "BHSD"), dist, c("t", "b", "w", "i", "M", "M"))
 }
 
 # 存储结果
 all_summary_results <- list()
 performance_results <- list()  # 存储每个模型的 R² 和 RMSE
 
 dir.create("figures", showWarnings = FALSE)
 dir.create("output", showWarnings = FALSE)
 
 # ========================
 # 主函数：单个时间点分析
 # ========================
 run_single_analysis <- function(tgt) {
   cat("🔍 开始分析:", tgt, "\n")
   
   data <- read_excel(data_path)
   if (!(tgt %in% colnames(data))) {
     warning("⚠️ 跳过 ", tgt, "：未找到该列")
     return(NULL)
   }
   
   all_buffer_vars <- unlist(lapply(buffer_distances, get_buffer_vars))
   all_predictors <- unique(c(base_vars, all_buffer_vars))
   all_vars_used <- c(tgt, all_predictors)
   
   selected_data <- data[, all_vars_used, drop = FALSE]
   selected_data_complete <- na.omit(selected_data)
   
   if (nrow(selected_data_complete) < 10) {
     warning("⚠️ 数据不足，跳过: ", tgt)
     return(NULL)
   }
   
   set.seed(123)
   n_train <- floor(0.8 * nrow(selected_data_complete))
   idx <- sample(nrow(selected_data_complete), n_train)
   
   train_data <- selected_data_complete[idx, ]
   test_data  <- selected_data_complete[-idx, ]
   test_ids <- rownames(test_data)
   
   # -------------------------------
   # 1. 基线模型（0m）
   # -------------------------------
   baseline_model <- ranger(
     formula = as.formula(paste(tgt, "~ .")),
     data = train_data[c(tgt, base_vars)],
     num.trees = 500,
     mtry = 5,
     min.node.size = 5,
     verbose = FALSE,
     seed = 123
   )
   
   pred_baseline <- predict(baseline_model, test_data[base_vars])$predictions
   actual <- test_data[[tgt]]
   
   # 评估函数
   evaluate <- function(pred, actual) {
     r2 <- cor(pred, actual)^2
     rmse <- sqrt(mean((pred - actual)^2))
     data.frame(R2 = r2, RMSE = rmse)
   }
   
   perf_baseline <- evaluate(pred_baseline, actual)
   perf_baseline$Buffer <- 0
   perf_baseline$TimePoint <- tgt
   
   predictions <- data.frame(ID = test_ids, pred_baseline = pred_baseline)
   
   # -------------------------------
   # 2. 多缓冲区模型 + 性能评估
   # -------------------------------
   buffer_performance <- list()
   
   for (buf in buffer_distances) {
     buf_vars <- get_buffer_vars(buf)
     current_predictors <- union(base_vars, buf_vars)
     current_predictors <- intersect(current_predictors, names(train_data))
     
     model <- ranger(
       formula = as.formula(paste(tgt, "~ .")),
       data = train_data[c(tgt, current_predictors)],
       num.trees = 500,
       mtry = 5,
       min.node.size = 5,
       verbose = FALSE,
       seed = 123
     )
     
     pred_buf <- predict(model, test_data[current_predictors])$predictions
     predictions[[paste0("pred_", buf)]] <- pred_buf
     
     # 评估性能
     eval_metrics <- evaluate(pred_buf, actual)
     eval_metrics$Buffer <- buf
     eval_metrics$TimePoint <- tgt
     
     buffer_performance[[as.character(buf)]] <- eval_metrics
   }
   
   # 合并所有缓冲区性能（含基线）
   all_perf <- bind_rows(perf_baseline, bind_rows(buffer_performance))
   all_perf$TargetVar <- tgt
   
   # -------------------------------
   # 3. 计算 δLST
   # -------------------------------
   delta_long <- predictions %>%
     pivot_longer(starts_with("pred_"), names_to = "Model", values_to = "Pred_LST") %>%
     filter(Model != "baseline") %>%
     mutate(
       Buffer = as.numeric(gsub("pred_", "", Model)),
       Pred_baseline = rep(pred_baseline, length(unique(Buffer))),
       dLST = Pred_LST - Pred_baseline
     )
   
   summary_dlst <- delta_long %>%
     group_by(Buffer) %>%
     summarise(
       TimePoint = unique(substring(tgt, 4, 7)),
       TargetVar = tgt,
       mean_dLST = mean(dLST),
       sd_dLST = sd(dLST),
       se_dLST = sd(dLST) / sqrt(n()),
       .groups = "drop"
     ) %>%
     arrange(Buffer)
   
   # -------------------------------
   # 4. 绘图 (δLST 单图)
   # -------------------------------
   p <- ggplot(summary_dlst, aes(x = Buffer, y = mean_dLST)) +
     geom_point(size = 3, color = "#D65D0E") +
     geom_line(color = "#D65D0E", linewidth = 1) +
     geom_errorbar(aes(ymin = mean_dLST - se_dLST, ymax = mean_dLST + se_dLST),
                   width = 3, alpha = 0.7, color = "#D65D0E") +
     geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
     scale_x_continuous(breaks = buffer_distances, labels = paste0(buffer_distances, "m")) +
     labs(
       title = paste("δLST vs Buffer Size:", gsub("lst(\\d{4})_ME", "\\1:00", tgt)),
       x = "Buffer Radius (m)",
       y = expression(delta*LST~(degree*C))
     ) +
     theme_minimal() +
     theme(plot.title = element_text(size = 12, face = "bold"))
   
   ggsave(paste0("figures/delta_lst_", gsub("lst(\\d{4})_ME", "\\1", tgt), ".png"),
          plot = p, width = 8, height = 5, dpi = 300, type = "cairo")
   
   # 返回结果
   list(summary = summary_dlst, plot = p, performance = all_perf)
 }
 
 # ========================
 # 批量运行所有时间点
 # ========================
 results_list <- lapply(target_vars, run_single_analysis)
 
 # --- 1. 合并 δLST 结果 ---
 final_delta_table <- bind_rows(
   lapply(results_list, function(x) if (!is.null(x)) x$summary)
 )
 write.csv(final_delta_table, "output/delta_lst_all_timepoints.csv", row.names = FALSE)
 
 # --- 2. 合并 performance 结果 ---
 performance_long <- bind_rows(
   lapply(results_list, function(x) {
     if (is.null(x)) return(NULL)
     x$performance
   })
 )
 
 # 添加原始时间标签列
 time_labels <- gsub("lst(\\d{4})_ME", "\\1:00", performance_long$TimePoint)
 performance_long <- performance_long %>%
   mutate(TimeLabel = factor(time_labels, levels = unique(time_labels)))
 
 
 # ==============================================================================
 #                               绘图部分
 # ==============================================================================
 
 # 确保 performance_long 存在
 if (!exists("performance_long")) {
   stop("请先运行主分析生成 performance_long")
 }
 
 # --------------------------
 # 1. 配置颜色映射和堆叠顺序
 # --------------------------
 
 # 定义自定义颜色映射 (注意：Key 需要匹配转换后的 "HH:MM" 格式)
 bar_color_map <- c(
   "07:08" = "#FDAE61",  # 清晨 (浅橙)
   "10:42" = "#F46D43",  # 上午 (橙红)
   "13:49" = "#D73027",  # 中午 (红)
   "15:40" = "#A50026",  # 午后 (深红) - 最热
   "19:59" = "#74ADD1",  # 傍晚 (浅蓝)
   "00:16" = "#4575B4",  # 深夜 (中蓝)
   "04:04" = "#313695"   # 凌晨 (深蓝) - 最冷
 )
 
 # 定义堆叠顺序 (Levels 越靠前，在堆积图中越靠下)
 # 需求：1959, 0016, 0404 在下面；0708, 1042, 1349, 1540 在上面
 custom_stack_order <- c(
   # --- 顶部组 (Top Group: Red/Orange tones) ---
   "07:08", 
   "10:42", 
   "13:49", 
   "15:40",
   # --- 底部组 (Bottom Group: Blue tones) ---
   "19:59", 
   "00:16", 
   "04:04"
 )
 
 # --------------------------
 # 2. 数据预处理
 # --------------------------
 plot_data <- performance_long %>%
   mutate(
     # 统一TimeLabel格式（转换 lstXXXX_ME 为 HH:MM）
     TimeLabel = gsub(":00$", "", TimeLabel) %>% 
       gsub("^0016", "00:16", .) %>%
       gsub("^0404", "04:04", .) %>%
       gsub("^0708", "07:08", .) %>%
       gsub("^1042", "10:42", .) %>%
       gsub("^1349", "13:49", .) %>%
       gsub("^1540", "15:40", .) %>%
       gsub("^1959", "19:59", .),
     
     # 【关键修改】应用自定义堆叠顺序
     TimeLabel = factor(TimeLabel, levels = custom_stack_order),
     
     # 包含Buffer=0的因子水平
     Buffer_f = factor(Buffer, levels = c(0, 70, 140, 210, 280, 350, 420, 490))
   )
 
 # 计算每个Buffer的R²和RMSE总和（用于堆叠高度）+ 均值
 summary_data <- plot_data %>%
   group_by(Buffer_f) %>%
   summarise(
     R2_sum = sum(R2),          
     R2_avg = mean(R2),         
     RMSE_sum = sum(RMSE),      
     RMSE_avg = mean(RMSE),     
     .groups = "drop"
   )
 
 # --------------------------
 # 3. 通用绘图主题
 # --------------------------
 common_theme_sci <- theme_minimal(base_size = 10) +
   theme(
     axis.text.x = element_text(size = 11),
     axis.text.y = element_text(size = 11),
     axis.title = element_text(size = 11, face = "plain"),
     legend.title = element_text(size = 11, face = "plain"),
     legend.text = element_text(size = 11),
     legend.position = "bottom",       
     legend.box = "horizontal",        
     panel.grid.minor = element_blank(), 
     panel.grid.major.x = element_blank(), 
     panel.grid.major.y = element_line(
       linetype = "solid",    
       color = "gray80",      
       linewidth = 0.3        
     )
   )
 
 # --------------------------
 # 4. 绘图构建
 # --------------------------
 
 # === 图1：R²堆叠图 ===
 p_r2 <- ggplot(plot_data, aes(x = Buffer_f, y = R2, fill = TimeLabel)) +
   geom_bar(stat = "identity", position = "stack", width = 0.7, alpha = 0.5, color = "white", linewidth = 0.1) +
   # 块内数值
   geom_text(
     aes(label = sprintf("%.3f", R2)), 
     position = position_stack(vjust = 0.5),
     size = 3.5, color = "black"
   ) +
   # 顶部均值标注
   geom_text(
     data = summary_data,
     aes(x = Buffer_f, y = R2_sum, label = sprintf("Avg: %.3f", R2_avg), fill = NULL),
     vjust = -0.5, 
     size = 2.8, color = "black", fontface = "bold"
   ) +
   # 【修改】使用自定义颜色
   scale_fill_manual(values = bar_color_map, name = "Observation Time") +
   labs(x = "Buffer Radius (m)", y = expression(R^2)) +
   common_theme_sci +
   theme(legend.position = "none") + # 隐藏R2图例，最后统一收集
   expand_limits(y = max(summary_data$R2_sum) * 1.15)
 
 # === 图2：RMSE堆叠图 ===
 p_rmse <- ggplot(plot_data, aes(x = Buffer_f, y = RMSE, fill = TimeLabel)) +
   geom_bar(stat = "identity", position = "stack", width = 0.7, alpha = 0.5, color = "white", linewidth = 0.1) +
   # 块内数值
   geom_text(
     aes(label = sprintf("%.3f", RMSE)), 
     position = position_stack(vjust = 0.5),
     size = 3.5, color = "black"
   ) +
   # 顶部均值标注
   geom_text(
     data = summary_data,
     aes(x = Buffer_f, y = RMSE_sum, label = sprintf("Avg: %.3f", RMSE_avg), fill = NULL),
     vjust = -0.5,
     size = 2.8, color = "black", fontface = "bold"
   ) +
   # 【修改】使用自定义颜色
   scale_fill_manual(values = bar_color_map, name = "Observation Time") +
   labs(x = "Buffer Radius (m)", y = "RMSE (°C)") +
   common_theme_sci +
   theme(legend.position = "none") +
   expand_limits(y = max(summary_data$RMSE_sum) * 1.15)
 
 # --------------------------
 # 5. 拼接与保存
 # --------------------------
 final_plot <- (p_r2 | p_rmse) +
   plot_annotation(
     theme = theme(
       plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
     )
   ) &
   theme(legend.position = "bottom") +
   plot_layout(guides = "collect", widths = c(1, 1))
 
 print(final_plot)
 
 # 保存图形
 ggsave("E:/第三篇论文/plot/BUFFER_Custom_Color.png", final_plot, 
        width = 14, height = 10, dpi = 300, bg = "white")

