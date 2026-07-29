# ==============================================================================
# 1. 环境准备
# ==============================================================================
library(tidyverse)
library(broom)
library(readxl)
library(patchwork)
library(openxlsx)
library(cowplot)
library(emmeans)

data_path  <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
output_dir <- "E:/第三篇论文/plot/Forest_Plots/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cluster_name_map <- c(
  "1" = "Inner-Green High-Rise neighborhoods",
  "2" = "Suburban Mega-Sized neighborhoods",
  "3" = "Suburban low-rise neighborhoods",
  "4" = "Urban core low-rise neighborhoods",
  "5" = "High-rise compact neighborhoods"
)

cols_cluster <- c(
  "Inner-Green High-Rise neighborhoods" = "#f8837c",
  "Suburban Mega-Sized neighborhoods"   = "#a2a501",
  "Suburban low-rise neighborhoods"     = "#01bf7d",
  "Urban core low-rise neighborhoods"   = "#14b5f6",
  "High-rise compact neighborhoods"     = "#e770f3"
)

bar_color_map <- c(
  lst0708_ME = "#FDAE61", lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027", lst1540_ME = "#A50026",
  lst1959_ME = "#74ADD1", lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"
)

# ==============================================================================
# 2. 数据处理与【严谨版 + 整体效应】建模逻辑
# ==============================================================================
data_raw <- readxl::read_excel(data_path) %>%
  mutate(cluster = factor(cluster,
                          levels = names(cluster_name_map),
                          labels = cluster_name_map))

target_features <- c("TCC", "TCC_buf", "Albedo", "SVF")
lst_vars        <- names(bar_color_map)

# --- 核心函数1：计算亚组边际效应（严谨版，保持不变）---
get_forest_data_rigorous <- function(lst_name, iv_list, data) {
  all_results <- list()
  
  for (iv in iv_list) {
    formula_str <- as.formula(paste(lst_name, "~ cluster *", iv))
    model       <- lm(formula_str, data = data)
    emm_slopes <- emtrends(model, specs = ~ cluster, var = iv, infer = c(TRUE, TRUE))
    
    res <- as.data.frame(emm_slopes) %>%
      rename(
        estimate = paste0(iv, ".trend"),
        std.error = SE,
        conf.low = lower.CL,
        conf.high = upper.CL
      ) %>%
      mutate(
        variable = iv,
        time = lst_name,
        p.value.adj = p.adjust(p.value, method = "fdr")
      ) %>%
      dplyr::select(cluster, variable, time, estimate, std.error, conf.low, conf.high, p.value, p.value.adj)
    
    all_results[[paste(lst_name, iv)]] <- res
  }
  
  return(bind_rows(all_results))
}

# --- 核心函数2：计算【整体主效应】（新增）---
get_overall_effect <- function(lst_name, iv_list, data) {
  overall_results <- list()
  
  for (iv in iv_list) {
    # 拟合不含交互项的模型，提取 iv 的主效应
    formula_str <- as.formula(paste(lst_name, "~", iv))
    model       <- lm(formula_str, data = data)
    res         <- tidy(model, conf.int = TRUE) %>%
      filter(term == iv) %>%
      mutate(
        variable = iv,
        time = lst_name,
        type = "Overall"
      ) %>%
      dplyr::select(variable, time, type, estimate, std.error, conf.low, conf.high, p.value)
    
    overall_results[[paste(lst_name, iv)]] <- res
  }
  
  return(bind_rows(overall_results))
}

# --- 生成数据 ---
# 1. 亚组数据
forest_master_df <- map_dfr(lst_vars, ~get_forest_data_rigorous(.x, target_features, data_raw))
forest_master_df$time_label <- sub("_ME", "", forest_master_df$time)

# 2. 整体效应数据（新增）
overall_effect_df <- map_dfr(lst_vars, ~get_overall_effect(.x, target_features, data_raw))
overall_effect_df$time_label <- sub("_ME", "", overall_effect_df$time)

# ==============================================================================
# 3. 批量执行并绘图（已加入整体效应灰色实线）
# ==============================================================================
draw_forest_plot <- function(current_time, df, colors, overall_df) {
  plot_df <- df %>% filter(time == current_time)
  # 提取当前时间点的整体效应量
  overall_lines <- overall_df %>% filter(time == current_time)
  
  ggplot(plot_df, aes(x = estimate, y = cluster, color = cluster)) +
    # 🔴 新增1：整体效应灰色实线（放在最底层）
    geom_vline(data = overall_lines, aes(xintercept = estimate), 
               color = "gray30", linewidth = 1.2, linetype = "solid") +
    # 原有的0值虚线
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8) +
    geom_point(size = 3) +
    facet_wrap(~variable, scales = "free_x", nrow = 1) +
    scale_color_manual(values = colors) +
    labs(title = paste("Effect Size on LST at", sub("_ME", "", current_time)),
         x = "Estimate (95% CI)", y = NULL,
         caption = "Note: Gray solid line indicates overall effect size.") + # 加个注释说明
    theme_bw() +
    theme(
      legend.position  = "none",
      strip.background = element_rect(fill = "gray95"),
      strip.text       = element_text(face = "bold"),
      axis.text.y      = element_text(size = 9),
      plot.caption     = element_text(hjust = 0, size = 8, color = "gray40") # 注释样式
    )
}

for (t in lst_vars) {
  p <- draw_forest_plot(t, forest_master_df, cols_cluster, overall_effect_df)
  ggsave(file.path(output_dir, paste0("Forest_", t, ".png")), p, width = 15, height = 5, dpi = 300)
}
message("✅ 带整体效应线的森林图生成完毕！")

# ==============================================================================
# 4. 合并为 4列 x 7行 森林矩阵（已加入整体效应线）
# ==============================================================================
all_variable_forest_rows <- list()

for (feat in target_features) {
  feat_time_plots <- lapply(lst_vars, function(t_name) {
    sub_df       <- forest_master_df %>% filter(variable == feat, time == t_name)
    # 🔴 新增2：提取当前变量+时间的整体效应
    overall_val  <- overall_effect_df %>% filter(variable == feat, time == t_name)
    
    is_first_col <- (t_name == lst_vars[1])
    is_last_row  <- (t_name == lst_vars[length(lst_vars)])
    
    ggplot(sub_df, aes(x = estimate, y = cluster, color = cluster)) +
      # 🔴 新增3：整体效应灰色实线
      geom_vline(data = overall_val, aes(xintercept = estimate), 
                 color = "gray60", linewidth = 0.8, linetype = "solid") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), width = 0.3, linewidth = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = cols_cluster, name = "Neighborhood Types") +
      labs(
        title = ifelse(feat == target_features[1], sub("_ME", "", t_name), ""),
        x     = if (is_last_row) "Estimate (95% CI)" else NULL,
        y     = if (is_first_col) feat else NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.text.y     = element_blank(),
        axis.ticks.y    = element_blank(),
        
        # Y 轴标题优化
        axis.title.y = if (is_first_col) 
          element_text(
            size = 14, 
            face = "bold", 
            hjust = 0.5, 
            vjust = 3,
            margin = margin(r = 12)
          ) else element_blank(),
        
        # X 轴标题优化
        axis.title.x = if (is_last_row)
          element_text(
            size = 12, 
            face = "bold", 
            hjust = 0.5,
            margin = margin(t = 8)
          ) else element_blank(),
        
        panel.grid.minor = element_blank(),
        plot.title       = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x      = element_text(size = 9, color = "black")
      )
  })
  
  all_variable_forest_rows[[feat]] <- patchwork::wrap_plots(feat_time_plots, ncol = 1)
}

final_forest_matrix <- patchwork::wrap_plots(all_variable_forest_rows, nrow = 1) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(final_forest_matrix)

ggsave(
  filename = file.path(output_dir, "Final_Forest_Matrix_Clean_Legend.pdf"),
  plot     = final_forest_matrix,
  width    = 20, height = 30, bg = "white", scale=0.5
)
message("✅ 带整体效应线的4x7矩阵图已保存！")

# ==============================================================================
# 5. 导出【严谨版亚组表】+【新增整体效应显著性表】
# ==============================================================================
# --- 5.1 亚组系数表（保持不变）---
result_table <- forest_master_df %>%
  mutate(
    Estimate = round(estimate, 4),
    Std.Error = round(std.error, 4),
    CI_95_Low = round(conf.low, 4),
    CI_95_High = round(conf.high, 4),
    P_Value_Unadj = round(p.value, 4),
    P_Value_FDR = round(p.value.adj, 4),
    Significance = case_when(
      P_Value_FDR < 0.001 ~ "***",
      P_Value_FDR < 0.01  ~ "**",
      P_Value_FDR < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    Estimate_CI = paste0(round(estimate, 3), " (", round(conf.low, 3), ", ", round(conf.high, 3), ")")
  ) %>%
  select(
    时间段 = time_label,
    社区类型 = cluster,
    变量名称 = variable,
    系数 = Estimate,
    标准误 = Std.Error,
    下限95CI = CI_95_Low,
    上限95CI = CI_95_High,
    原始P值 = P_Value_Unadj,
    校正后P值_FDR = P_Value_FDR,
    显著性 = Significance,
    论文格式列 = Estimate_CI
  ) %>%
  arrange(时间段, 社区类型, 变量名称)

openxlsx::write.xlsx(
  result_table,
  file = file.path(output_dir, "Forest_Plot_Coefficient_Table_Rigorous.xlsx"),
  rowNames = FALSE
)

# --- 🔴 5.2 新增：整体效应显著性表 ---
overall_result_table <- overall_effect_df %>%
  mutate(
    Estimate = round(estimate, 4),
    Std.Error = round(std.error, 4),
    CI_95_Low = round(conf.low, 4),
    CI_95_High = round(conf.high, 4),
    P_Value = round(p.value, 4),
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01  ~ "**",
      P_Value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    Estimate_CI = paste0(round(estimate, 3), " (", round(conf.low, 3), ", ", round(conf.high, 3), ")")
  ) %>%
  select(
    时间段 = time_label,
    变量名称 = variable,
    整体效应系数 = Estimate,
    标准误 = Std.Error,
    下限95CI = CI_95_Low,
    上限95CI = CI_95_High,
    P值 = P_Value,
    显著性 = Significance,
    论文格式列 = Estimate_CI
  ) %>%
  arrange(时间段, 变量名称)

openxlsx::write.xlsx(
  overall_result_table,
  file = file.path(output_dir, "Overall_Effect_Significance_Table.xlsx"),
  rowNames = FALSE
)

message("✅ 表格导出完毕：")
message("1. 亚组系数表：Forest_Plot_Coefficient_Table_Rigorous.xlsx")
message("2. 整体效应表：Overall_Effect_Significance_Table.xlsx") # ⭐ 新表

# ==============================================================================
# 6. 交互项显著性表（保留用于判断亚组异质性）
# ==============================================================================
extract_interaction_p <- function(data, lst_vars, iv_vars) {
  interaction_results <- list()
  
  for (lst in lst_vars) {
    for (iv in iv_vars) {
      form      <- as.formula(paste(lst, "~ cluster *", iv))
      model     <- lm(form, data = data)
      inter_terms <- tidy(model) %>%
        filter(str_detect(term, ":")) %>%
        mutate(Dependent_Var   = lst,
               Independent_Var = iv,
               Test_Type       = "Slope Difference (Interaction)") %>%
        dplyr::select(Dependent_Var, Independent_Var, term, estimate, std.error, p.value)
      
      interaction_results[[paste(lst, iv)]] <- inter_terms
    }
  }
  return(bind_rows(interaction_results))
}

interaction_table <- extract_interaction_p(data_raw, names(bar_color_map), target_features) %>%
  mutate(Significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ "ns"
  ))

write.xlsx(interaction_table, file.path(output_dir, "Interaction_Significance_Tests.xlsx"))
message("✅ 交互项显著性表已导出。")


