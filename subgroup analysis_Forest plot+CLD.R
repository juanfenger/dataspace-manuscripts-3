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
library(multcomp)       # ← 新增：生成 CLD 字母
library(multcompView)   # ← 新增：cld() 依赖

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
# 2. 数据处理
# ==============================================================================
data_raw <- readxl::read_excel(data_path) %>%
  mutate(Cluster = factor(Cluster,
                          levels = names(cluster_name_map),
                          labels = cluster_name_map))

target_features <- c("TCC", "TCC_buf", "Albedo", "SVF")
lst_vars        <- names(bar_color_map)

# ==============================================================================
# 3. 核心函数：计算亚组边际效应 + CLD 字母（关键改动）
# ==============================================================================

# --- 函数1：亚组斜率 + pairwise 多重比较 + CLD ---
get_forest_data_with_cld <- function(lst_name, iv_list, data) {
  all_results <- list()
  
  for (iv in iv_list) {
    formula_str <- as.formula(paste(lst_name, "~ Cluster *", iv))
    model       <- lm(formula_str, data = data)
    
    # 各亚组边际斜率
    emm_slopes <- emtrends(model,
                           specs = ~ Cluster,
                           var   = iv,
                           infer = c(TRUE, TRUE))
    
    # Pairwise 多重比较（sidak 法校正）
    pairs_res <- pairs(emm_slopes, adjust = "sidak")
    
    # 生成 CLD 字母（基于 pairwise p 值）
    # cld() 来自 multcomp，直接接受 emmGrid 对象
    cld_res <- cld(emm_slopes,
                   adjust  = "sidak",   # 与 pairs 保持一致
                   Letters = letters,   # 使用小写字母
                   alpha   = 0.05)
    
    # 整理 CLD 结果（去掉字母前后空格）
    cld_df <- as.data.frame(cld_res) %>%
      mutate(.group = str_trim(.group)) %>%         # 去除 multcomp 加的空格
      dplyr::select(Cluster, cld_letter = .group)
    
    # 整理斜率主表
    res <- as.data.frame(emm_slopes) %>%
      rename(
        estimate  = paste0(iv, ".trend"),
        std.error = SE,
        conf.low  = lower.CL,
        conf.high = upper.CL
      ) %>%
      mutate(
        variable    = iv,
        time        = lst_name,
        p.value.adj = p.adjust(p.value, method = "fdr")
      ) %>%
      left_join(cld_df, by = "Cluster") %>%           # ← 合并 CLD 字母
      dplyr::select(Cluster, variable, time,
                    estimate, std.error, conf.low, conf.high,
                    p.value, p.value.adj, cld_letter)
    
    all_results[[paste(lst_name, iv)]] <- res
    
    # 同时保存 pairwise 比较细节（用于导出表格）
    pair_df <- as.data.frame(pairs_res) %>%
      mutate(variable = iv, time = lst_name)
    attr(res, "pairs") <- pair_df   # 附加到结果（后面导出用）
  }
  
  return(bind_rows(all_results))
}

# --- 函数2：整体主效应（无变化）---
get_overall_effect <- function(lst_name, iv_list, data) {
  overall_results <- list()
  for (iv in iv_list) {
    formula_str <- as.formula(paste(lst_name, "~", iv))
    model       <- lm(formula_str, data = data)
    res         <- tidy(model, conf.int = TRUE) %>%
      filter(term == iv) %>%
      mutate(variable = iv, time = lst_name, type = "Overall") %>%
      dplyr::select(variable, time, type, estimate, std.error, conf.low, conf.high, p.value)
    overall_results[[paste(lst_name, iv)]] <- res
  }
  return(bind_rows(overall_results))
}

# --- 函数3：提取 pairwise 比较表（用于单独导出）---
get_pairwise_table <- function(lst_name, iv_list, data) {
  all_pairs <- list()
  for (iv in iv_list) {
    formula_str <- as.formula(paste(lst_name, "~ Cluster *", iv))
    model       <- lm(formula_str, data = data)
    emm_slopes  <- emtrends(model, specs = ~ Cluster, var = iv)
    pairs_res   <- pairs(emm_slopes, adjust = "sidak")
    df          <- as.data.frame(pairs_res) %>%
      mutate(variable = iv, time = lst_name)
    all_pairs[[paste(lst_name, iv)]] <- df
  }
  return(bind_rows(all_pairs))
}

# ==============================================================================
# 4. 生成数据
# ==============================================================================
message("⏳ 正在计算亚组斜率与 CLD 字母，请稍候...")
forest_master_df <- map_dfr(lst_vars,
                            ~get_forest_data_with_cld(.x, target_features, data_raw))
forest_master_df$time_label <- sub("_ME", "", forest_master_df$time)

message("⏳ 正在计算整体效应...")
overall_effect_df <- map_dfr(lst_vars,
                             ~get_overall_effect(.x, target_features, data_raw))
overall_effect_df$time_label <- sub("_ME", "", overall_effect_df$time)

message("⏳ 正在提取 pairwise 比较表...")
pairwise_detail_df <- map_dfr(lst_vars,
                              ~get_pairwise_table(.x, target_features, data_raw))
pairwise_detail_df$time_label <- sub("_ME", "", pairwise_detail_df$time)

# ==============================================================================
# 5. 单时间点森林图（含整体效应线 + CLD 字母）
# ==============================================================================
draw_forest_plot <- function(current_time, df, colors, overall_df) {
  plot_df      <- df %>% filter(time == current_time)
  overall_lines <- overall_df %>% filter(time == current_time)
  
  # CLD 字母位置：放在置信区间右侧外一点
  label_df <- plot_df %>%
    mutate(label_x = conf.high)
  
  ggplot(plot_df, aes(x = estimate, y = Cluster, color = Cluster)) +
    geom_vline(data = overall_lines, aes(xintercept = estimate),
               color = "gray30", linewidth = 1.2, linetype = "solid") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3, linewidth = 0.8) +
    geom_point(size = 3) +
    # ← 新增：CLD 字母标注
    geom_text(data = label_df,
              aes(x = label_x, label = cld_letter),
              hjust = -0.3, size = 3.5, fontface = "bold", color = "black") +
    facet_wrap(~variable, scales = "free_x", nrow = 1) +
    scale_color_manual(values = colors) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # 右侧留空给字母
    labs(
      title   = paste("Effect Size on LST at", sub("_ME", "", current_time)),
      x       = "Estimate (95% CI)",
      y       = NULL,
      caption = "Note: Gray solid line = overall effect. Letters indicate sidak HSD grouping (p < 0.05)."
    ) +
    theme_bw() +
    theme(
      legend.position  = "none",
      strip.background = element_rect(fill = "gray95"),
      strip.text       = element_text(face = "bold"),
      axis.text.y      = element_text(size = 9),
      plot.caption     = element_text(hjust = 0, size = 8, color = "gray40")
    )
}

message("🖼️ 正在生成单时间点森林图...")
for (t in lst_vars) {
  p <- draw_forest_plot(t, forest_master_df, cols_cluster, overall_effect_df)
  ggsave(file.path(output_dir, paste0("Forest_", t, ".png")),
         p, width = 18, height = 5, dpi = 300)
}
message("✅ 单时间点森林图已保存！")

# ==============================================================================
# 6. 4列 × 7行 大矩阵图（含整体效应线 + CLD 字母）【核心改动】
# ==============================================================================
all_variable_forest_rows <- list()

for (feat in target_features) {
  feat_time_plots <- lapply(lst_vars, function(t_name) {
    
    sub_df      <- forest_master_df %>% filter(variable == feat, time == t_name)
    overall_val <- overall_effect_df %>% filter(variable == feat, time == t_name)
    
    is_first_col <- (t_name == lst_vars[1])
    is_last_row  <- (t_name == lst_vars[length(lst_vars)])
    
    # CLD 字母位置：conf.high 右侧
    label_sub <- sub_df %>% mutate(label_x = conf.high)
    
    ggplot(sub_df, aes(x = estimate, y = Cluster, color = Cluster)) +
      
      # 整体效应竖线
      geom_vline(data = overall_val, aes(xintercept = estimate),
                 color = "gray60", linewidth = 0.8, linetype = "solid") +
      
      # 零值虚线
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
      
      # 误差棒 + 点
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                     width = 0.3, linewidth = 1) +
      geom_point(size = 3) +
      
      # ← 关键新增：CLD 字母（跟随各组颜色，便于辨识）
      geom_text(data = label_sub,
                aes(x = label_x, label = cld_letter, color = Cluster),
                hjust    = -0.5,
                size     = 5,
                fontface = "bold",
                show.legend = FALSE) +
      
      scale_color_manual(values = cols_cluster, name = "Neighborhood Types") +
      
      # 右侧留出字母空间（乘数可按需调整）
      scale_x_continuous(expand = expansion(mult = c(0.05, 0.20))) +
      
      labs(
        title = ifelse(feat == target_features[1], sub("_ME", "", t_name), ""),
        x     = NULL,
        y     = NULL
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        
        # Y 轴：隐藏刻度（亚组名太长，由图例替代）
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        
        axis.title.y = if (is_first_col)
          element_text(size = 14, face = "bold", hjust = 0.5,
                       vjust = 3, margin = margin(r = 12))
        else element_blank(),
        
        axis.title.x = if (is_last_row)
          element_text(size = 12, face = "bold", hjust = 0.5,
                       margin = margin(t = 8))
        else element_blank(),
        
        panel.grid.minor = element_blank(),
        plot.title       = element_text(hjust = 0.5, face = "bold", size = 12),
        axis.text.x      = element_text(size = 9, color = "black")
      )
  })
  
  all_variable_forest_rows[[feat]] <- patchwork::wrap_plots(feat_time_plots, ncol = 1)
}

# 添加整体注释说明 CLD 含义
final_forest_matrix <- patchwork::wrap_plots(all_variable_forest_rows, nrow = 1) +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# 用 patchwork 的 plot_annotation 加全局标注
final_forest_matrix <- final_forest_matrix +
  patchwork::plot_annotation(
    caption = paste(
NULL,
      sep = "\n"
    ),
    theme = theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "gray30",
                                  margin = margin(t = 10))
    )
  )

print(final_forest_matrix)

ggsave(
  filename = file.path(output_dir, "Final_Forest_Matrix_CLD.pdf"),
  plot     = final_forest_matrix,
  width    = 20, height = 30, bg = "white", scale = 0.5
)
ggsave(
  filename = file.path(output_dir, "Final_Forest_Matrix_CLD.png"),
  plot     = final_forest_matrix,
  width    = 20, height = 30, dpi = 200, bg = "white", scale = 0.5
)
message("✅ 带 CLD 字母的 4×7 矩阵图已保存！")

