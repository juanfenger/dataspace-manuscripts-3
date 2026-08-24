#===============================================================================
# 0. 加载包
#===============================================================================
library(MuMIn)
library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)
library(performance)
library(openxlsx)
library(nlme)

#===============================================================================
# 1. 读取长数据
#===============================================================================


data_path <- 
  "E:/第三篇论文/table/xqsx4_Clustering_Long.xlsx"


long_data <- read_excel(
  data_path
)



# 查看数据结构

str(long_data)

head(long_data)



#===============================================================================
# 2. 数据预处理
#===============================================================================


long_data <- long_data %>%
  
  mutate(
    
    
    #---------------------------------------------------
    # 将遥感过境时间转换为连续小时
    #---------------------------------------------------
    
    Hour = case_when(
      
      Time == "lst0708_ME" ~ 7 + 8/60,
      
      Time == "lst1042_ME" ~ 10 + 42/60,
      
      Time == "lst1349_ME" ~ 13 + 49/60,
      
      Time == "lst1540_ME" ~ 15 + 40/60,
      
      Time == "lst1959_ME" ~ 19 + 59/60,
      
      Time == "lst0016_ME" ~ 0 + 16/60,
      
      Time == "lst0404_ME" ~ 4 + 4/60
      
    ),
    
    
    
    #---------------------------------------------------
    # 日夜分类
    #---------------------------------------------------
    
    Period = case_when(
      
      Hour >= 6 & Hour <= 18 ~ "Day",
      
      TRUE ~ "Night"
      
    ),
    
    
    Period = factor(
      Period,
      levels=c(
        "Day",
        "Night"
      )
    ),
    
    
    #---------------------------------------------------
    # 分类变量
    #---------------------------------------------------
    
    Cluster = factor(Cluster),
    
    
    
    FID = factor(FID),
    
    Hour = factor(Hour),
    
    Date = factor(Date)
    
  )



# 检查

table(long_data$Period)

table(long_data$Cluster)


library(dplyr)

long_data <- long_data %>%
  mutate(
    TCC      = TCC - mean(TCC, na.rm = TRUE),
    TCC_b490m = TCC_b490m - mean(TCC_b490m, na.rm = TRUE)
  )

mean(long_data$TCC, na.rm = TRUE)
mean(long_data$TCC_b490m, na.rm = TRUE)

summary(long_data$TCC)

library(dplyr)

control_vars <- c(
  "BCI",
  "WCI",
  "GCI",
  "ISF",
  "CY",
  "BAH",
  "BHSD",
  "SVF",
  "Albedo",
  "POP",
  "Area",
  "FAR",
  "MR",
  "BCI_b490m",
  "WCI_b490m",
  "GCI_b490m",
  "BAH_b490m",
  "BHSD_b490m",
  "ISF_b490m",
  "NEAR_DIST"
)

# 建议先备份原始数据
long_data_original <- long_data

# Z-score标准化并覆盖原变量
long_data <- long_data %>%
  mutate(
    across(
      all_of(control_vars),
      ~ as.numeric(scale(.x))
    )
  )

standardization_check <- long_data %>%
  summarise(
    across(
      all_of(control_vars),
      list(
        Mean = ~ mean(.x, na.rm = TRUE),
        SD = ~ sd(.x, na.rm = TRUE),
        Min = ~ min(.x, na.rm = TRUE),
        Max = ~ max(.x, na.rm = TRUE)
      )
    )
  )

print(standardization_check)
#===============================================================================
# 4. Model 2
# check_model()
# 随机截距模型
#
# LST ~ environmental variables + weather + Cluster
#       + (1|FID)
#
#===============================================================================



model_1 <- lme(
  LST ~  Hour,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_1 )
icc(model_1)

library(nlme)

model_2 <- lme(
  LST ~ TCC + TCC_b490m + Cluster + Hour,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_2 )
icc(model_2)

model_3 <- lme(
  LST ~ TCC + TCC_b490m + Cluster + Hour +  BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
  FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_3 )
icc(model_3)


model_4_1 <- lme(
  LST ~  TCC +  TCC_b490m*Hour + Cluster + BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_4_1)
icc(model_4_1)

model_4_2 <- lme(
  LST ~  TCC*Hour + TCC_b490m  + Cluster +  BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_4_2)
icc(model_4_2)

model_5_1 <- lme(
  LST ~  TCC +  TCC_b490m*Cluster*Hour + BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_5_1)
icc(model_5_1)

model_5_2 <- lme(
  LST ~  TCC*Cluster*Hour +  TCC_b490m + BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  random = ~1|FID,
  weights = varIdent(form=~1|Hour),
  data=long_data,
  method="ML"
)
summary(model_5_2)
icc(model_5_2)


model_6 <- nlme::lme(
  
  LST ~
    
    TCC * Cluster * Hour +
    TCC_b490m * Cluster * Hour +
    BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  
  random = ~ 1 | FID,
  
  weights = nlme::varIdent(
    form = ~ 1 | Hour
  ),
  
  data = long_data,
  
  method = "ML"
  
)
summary(model_6)
icc(model_6)


anova(model_5_1, model_6)
anova(model_5_2, model_6)


#===============================================================================
# 诊断模型
#===============================================================================

diag_data <- data.frame(
  
  fitted = fitted(model_5),
  
  residual = residuals(
    model_5,
    type = "normalized"
  ),
  
  Hour = long_data$Hour,
  
  FID = long_data$FID
  
)


head(diag_data)

#================================

p1<-ggplot(
  diag_data,
  aes(
    x = fitted,
    y = residual
  )
)+
  
  geom_point(
    alpha = 0.25
  )+
  
  geom_smooth(
    method="loess",
    se=TRUE
  )+
  
  geom_hline(
    yintercept=0,
    linetype="dashed"
  )+
  
  theme_bw()+
  
  labs(
    x="Fitted values",
    y="Normalized residuals"
  )


ggsave("E:/第三篇论文/plot/p1.png", p1, 
       width = 15, height = 15, dpi = 300, bg = "white")
#=================================================================================





library(car)

vif_model <- lm(
  LST ~ TCC * Cluster * Hour +
    TCC_b490m * Cluster * Hour +
    GCI + WCI +
    GCI_b490m + WCI_b490m,
  data = long_data
)

vif_result <- car::vif(vif_model)
vif_result





vif_model <- lm(
  LST ~ TCC + TCC_b490m + Cluster + Hour +  BCI + WCI + GCI + ISF + CY + BAH + BHSD + SVF + Albedo + POP + Area +
    FAR + MR + BCI_b490m + WCI_b490m + GCI_b490m + BAH_b490m + BHSD_b490m + ISF_b490m + NEAR_DIST,
  data = long_data
)

vif_result <- car::vif(vif_model)
vif_result














#===============================================================================
# Marginal effects from two separate three-way-interaction models
#
# model_5_2: TCC * Cluster * Hour       -> internal tree canopy
# model_5_1: TCC_b490m * Cluster * Hour -> surrounding tree canopy (TCC_buf)
#
# The two canopy variables are estimated from their respective models.
# Cluster comparisons are conducted within each model and each Hour.
#===============================================================================


#===============================================================================
# 0. Packages
#===============================================================================

library(emmeans)
library(dplyr)
library(openxlsx)
library(multcomp)
library(multcompView)
library(ggplot2)
library(patchwork)


#===============================================================================
# 1. Output settings
#===============================================================================

table_output_dir <-
  "E:/第三篇论文/table/model5_marginal_effects"

plot_output_dir <-
  "E:/第三篇论文/plot/model5_marginal_effects"

dir.create(
  table_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  plot_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

adjust_method <- "tukey"


#===============================================================================
# 2. Check and assign the two source models
#===============================================================================

required_models <- c("model_5_1", "model_5_2")

missing_models <- required_models[
  !vapply(required_models, exists, logical(1), inherits = TRUE)
]

if (length(missing_models) > 0) {
  stop(
    paste0(
      "The following model objects are missing: ",
      paste(missing_models, collapse = ", "),
      ". Fit these models before running this script."
    )
  )
}

# Internal TCC has the three-way interaction in model_5_2.
model_internal <- get("model_5_2", inherits = TRUE)

# Surrounding TCC has the three-way interaction in model_5_1.
model_surrounding <- get("model_5_1", inherits = TRUE)


#-------------------------------------------------------------------------------
# Verify that each model contains the intended three-way interaction.
# This check is insensitive to the order of variables in a term label.
#-------------------------------------------------------------------------------

has_three_way_term <- function(model, focal_variable) {
  term_labels <- attr(
    terms(formula(model)),
    "term.labels"
  )
  
  any(
    vapply(
      strsplit(term_labels, ":", fixed = TRUE),
      function(term_parts) {
        length(term_parts) == 3L &&
          setequal(
            term_parts,
            c(focal_variable, "Cluster", "Hour")
          )
      },
      logical(1)
    )
  )
}

if (!has_three_way_term(model_internal, "TCC")) {
  stop(
    "model_5_2 does not contain the required TCC:Cluster:Hour interaction."
  )
}

if (!has_three_way_term(model_surrounding, "TCC_b490m")) {
  stop(
    paste0(
      "model_5_1 does not contain the required ",
      "TCC_b490m:Cluster:Hour interaction."
    )
  )
}


#===============================================================================
# 3. Labels and helper functions
#===============================================================================

cluster_name_map <- c(
  "1" = "Low-rise hard-scape",
  "2" = "Green-buffered open",
  "3" = "Large-scale",
  "4" = "Green interior",
  "5" = "High-density compact"
)

hour_order <- c(
  "00:16",
  "04:04",
  "07:08",
  "10:42",
  "13:49",
  "15:40",
  "19:59"
)

convert_hour_label <- function(x) {
  hour_character <- as.character(x)
  hour_numeric <- suppressWarnings(as.numeric(hour_character))
  
  output <- dplyr::case_when(
    abs(hour_numeric - (0 + 16 / 60)) < 0.001 ~ "00:16",
    abs(hour_numeric - (4 + 4 / 60)) < 0.001 ~ "04:04",
    abs(hour_numeric - (7 + 8 / 60)) < 0.001 ~ "07:08",
    abs(hour_numeric - (10 + 42 / 60)) < 0.001 ~ "10:42",
    abs(hour_numeric - (13 + 49 / 60)) < 0.001 ~ "13:49",
    abs(hour_numeric - (15 + 40 / 60)) < 0.001 ~ "15:40",
    abs(hour_numeric - (19 + 59 / 60)) < 0.001 ~ "19:59",
    TRUE ~ hour_character
  )
  
  factor(output, levels = hour_order)
}

significance_symbol <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
}


#===============================================================================
# 4. Function for marginal slopes and Cluster comparisons
#===============================================================================

analyse_canopy_model <- function(
    model,
    focal_variable,
    canopy_label,
    source_model
) {
  
  # Conditional marginal slopes for every Cluster-Hour combination.
  trends <- emmeans::emtrends(
    model,
    specs = ~ Cluster | Hour,
    var = focal_variable
  )
  
  slope_df <- as.data.frame(
    summary(
      trends,
      infer = c(TRUE, TRUE),
      adjust = "none"
    )
  )
  
  trend_column <- paste0(focal_variable, ".trend")
  
  if (!trend_column %in% names(slope_df)) {
    stop(
      paste0(
        "The expected trend column '",
        trend_column,
        "' was not returned by emtrends()."
      )
    )
  }
  
  names(slope_df)[names(slope_df) == trend_column] <-
    "marginal_effect"
  
  slope_df <- slope_df %>%
    mutate(
      Source_model = source_model,
      Canopy = canopy_label,
      Focal_variable = focal_variable,
      Cluster_code = as.character(Cluster),
      Cluster_label = unname(cluster_name_map[Cluster_code]),
      Hour_label = convert_hour_label(Hour),
      significance = significance_symbol(p.value),
      
      # TCC and TCC_b490m are proportions. Multiplying by 0.10 gives
      # the LST change associated with a 10-percentage-point increase.
      effect_per_10pct = marginal_effect * 0.10,
      lower_per_10pct = lower.CL * 0.10,
      upper_per_10pct = upper.CL * 0.10,
      
      effect_result = case_when(
        upper.CL < 0 & p.value < 0.05 ~ "Significant cooling",
        lower.CL > 0 & p.value < 0.05 ~ "Significant warming",
        TRUE ~ "Not significant"
      )
    ) %>%
    arrange(Hour_label, Cluster_code)
  
  # Pairwise differences between Cluster-specific slopes within each Hour.
  pairwise_grid <- contrast(
    trends,
    method = "pairwise",
    by = "Hour",
    adjust = adjust_method
  )
  
  pairwise_df <- as.data.frame(
    summary(
      pairwise_grid,
      infer = c(TRUE, TRUE),
      adjust = adjust_method
    )
  ) %>%
    mutate(
      Source_model = source_model,
      Canopy = canopy_label,
      Focal_variable = focal_variable,
      Hour_label = convert_hour_label(Hour),
      significance = significance_symbol(p.value),
      difference_per_10pct = estimate * 0.10,
      lower_per_10pct = lower.CL * 0.10,
      upper_per_10pct = upper.CL * 0.10,
      comparison_result = case_when(
        p.value < 0.05 & estimate < 0 ~
          "First Cluster has a more negative slope",
        p.value < 0.05 & estimate > 0 ~
          "Second Cluster has a more negative slope",
        TRUE ~ "No significant difference"
      )
    ) %>%
    arrange(Hour_label, contrast)
  
  pairwise_significant <- pairwise_df %>%
    filter(p.value < 0.05)
  
  # Joint test of whether Cluster-specific slopes differ within each Hour.
  cluster_effect_contrasts <- contrast(
    trends,
    method = "eff",
    by = "Hour"
  )
  
  omnibus_df <- as.data.frame(
    test(
      cluster_effect_contrasts,
      joint = TRUE
    )
  ) %>%
    mutate(
      Source_model = source_model,
      Canopy = canopy_label,
      Focal_variable = focal_variable,
      Hour_label = convert_hour_label(Hour),
      significance = significance_symbol(p.value)
    ) %>%
    arrange(Hour_label)
  
  # Compact-letter display based on Tukey-adjusted pairwise comparisons.
  cld_df <- as.data.frame(
    multcomp::cld(
      trends,
      by = "Hour",
      adjust = adjust_method,
      Letters = letters,
      alpha = 0.05
    )
  )
  
  if (!trend_column %in% names(cld_df)) {
    stop(
      paste0(
        "The expected trend column '",
        trend_column,
        "' was not returned by cld()."
      )
    )
  }
  
  names(cld_df)[names(cld_df) == trend_column] <-
    "marginal_effect"
  
  cld_df <- cld_df %>%
    mutate(
      Source_model = source_model,
      Canopy = canopy_label,
      Focal_variable = focal_variable,
      Cluster_code = as.character(Cluster),
      Cluster_label = unname(cluster_name_map[Cluster_code]),
      Hour_label = convert_hour_label(Hour),
      CLD = trimws(.group)
    ) %>%
    arrange(Hour_label, Cluster_code)
  
  list(
    trends = trends,
    simple_slopes = slope_df,
    pairwise_all = pairwise_df,
    pairwise_significant = pairwise_significant,
    omnibus = omnibus_df,
    cld = cld_df
  )
}


#===============================================================================
# 5. Run the two models separately
#===============================================================================

# Internal canopy: model_5_2
internal_results <- analyse_canopy_model(
  model = model_internal,
  focal_variable = "TCC",
  canopy_label = "Internal tree canopy",
  source_model = "model_5_2"
)

# Surrounding canopy: model_5_1
surrounding_results <- analyse_canopy_model(
  model = model_surrounding,
  focal_variable = "TCC_b490m",
  canopy_label = "Surrounding tree canopy",
  source_model = "model_5_1"
)


#===============================================================================
# 6. Combine results for convenient export
#===============================================================================

all_simple_slopes <- bind_rows(
  internal_results$simple_slopes,
  surrounding_results$simple_slopes
)

all_pairwise_comparisons <- bind_rows(
  internal_results$pairwise_all,
  surrounding_results$pairwise_all
)

all_significant_comparisons <- bind_rows(
  internal_results$pairwise_significant,
  surrounding_results$pairwise_significant
)

all_omnibus_tests <- bind_rows(
  internal_results$omnibus,
  surrounding_results$omnibus
)

all_cld_results <- bind_rows(
  internal_results$cld,
  surrounding_results$cld
)


#===============================================================================
# 7. Print key results
#===============================================================================

cat(
  "\n============================================================\n",
  "Internal canopy: significant Cluster comparisons (model_5_2)\n",
  "============================================================\n"
)
print(internal_results$pairwise_significant)

cat(
  "\n============================================================\n",
  "Surrounding canopy: significant Cluster comparisons (model_5_1)\n",
  "============================================================\n"
)
print(surrounding_results$pairwise_significant)

cat(
  "\n============================================================\n",
  "Omnibus tests by Hour\n",
  "============================================================\n"
)
print(all_omnibus_tests)


#===============================================================================
# 8. Export marginal effects and comparisons to Excel
#===============================================================================

output_file <- file.path(
  table_output_dir,
  "Model5_1_Model5_2_canopy_marginal_effect_comparisons.xlsx"
)

openxlsx::write.xlsx(
  list(
    Internal_simple_slopes = internal_results$simple_slopes,
    Internal_pairwise_all = internal_results$pairwise_all,
    Internal_pairwise_sig = internal_results$pairwise_significant,
    Internal_omnibus = internal_results$omnibus,
    Internal_CLD = internal_results$cld,
    
    Surrounding_simple_slopes = surrounding_results$simple_slopes,
    Surrounding_pairwise_all = surrounding_results$pairwise_all,
    Surrounding_pairwise_sig = surrounding_results$pairwise_significant,
    Surrounding_omnibus = surrounding_results$omnibus,
    Surrounding_CLD = surrounding_results$cld,
    
    All_simple_slopes = all_simple_slopes,
    All_pairwise = all_pairwise_comparisons,
    All_significant = all_significant_comparisons,
    All_omnibus = all_omnibus_tests,
    All_CLD = all_cld_results
  ),
  file = output_file,
  overwrite = TRUE
)

message("Marginal-effect results saved to: ", output_file)


#===============================================================================
# 9. Forest-plot settings
#===============================================================================

# "raw": slope for a one-unit increase in canopy proportion.
# "per10pct": LST change for a 10-percentage-point increase in canopy.
effect_mode <- "raw"

# "free_x": each Hour panel has its own x-axis range.
# "fixed": all Hour panels use the same x-axis range.
facet_x_scales <- "free_x"

cld_offset_ratio <- 0.045

if (!effect_mode %in% c("raw", "per10pct")) {
  stop("effect_mode must be either 'raw' or 'per10pct'.")
}

if (!facet_x_scales %in% c("free_x", "fixed")) {
  stop("facet_x_scales must be either 'free_x' or 'fixed'.")
}

cluster_y_order <- c(
  "Low-rise hard-scape",
  "Green-buffered open",
  "Large-scale",
  "Green interior",
  "High-density compact"
)

cluster_colors <- c(
  "1" = "#f8837c",
  "2" = "#a2a501",
  "3" = "#01bf7d",
  "4" = "#14b5f6",
  "5" = "#e770f3"
)


#===============================================================================
# 10. Prepare plotting data
#===============================================================================

prepare_effect_data <- function(
    effect_data,
    cld_data,
    effect_mode = "raw",
    cld_offset_ratio = 0.045
) {
  cld_clean <- cld_data %>%
    transmute(
      Cluster_code = as.character(Cluster_code),
      Hour_label = as.character(Hour_label),
      CLD = trimws(as.character(CLD))
    ) %>%
    distinct(Cluster_code, Hour_label, .keep_all = TRUE)
  
  data <- effect_data %>%
    mutate(
      Cluster_code = as.character(Cluster_code),
      Cluster_label = as.character(Cluster_label),
      Hour_label = as.character(Hour_label)
    ) %>%
    left_join(
      cld_clean,
      by = c("Cluster_code", "Hour_label")
    )
  
  if (any(is.na(data$CLD) | data$CLD == "")) {
    warning(
      paste0(
        sum(is.na(data$CLD) | data$CLD == ""),
        " observations did not match a CLD letter."
      )
    )
  }
  
  data <- data %>%
    mutate(
      Cluster_code = factor(
        Cluster_code,
        levels = c("1", "2", "3", "4", "5")
      ),
      Cluster_label = factor(
        Cluster_label,
        levels = cluster_y_order
      ),
      Hour_label = factor(
        Hour_label,
        levels = hour_order
      )
    )
  
  if (effect_mode == "per10pct") {
    data <- data %>%
      mutate(
        estimate_plot = effect_per_10pct,
        lower_plot = lower_per_10pct,
        upper_plot = upper_per_10pct
      )
  } else {
    data <- data %>%
      mutate(
        estimate_plot = marginal_effect,
        lower_plot = lower.CL,
        upper_plot = upper.CL
      )
  }
  
  data %>%
    group_by(Hour_label) %>%
    mutate(
      panel_min = min(c(lower_plot, 0), na.rm = TRUE),
      panel_max = max(c(upper_plot, 0), na.rm = TRUE),
      panel_span = panel_max - panel_min,
      panel_span = ifelse(panel_span <= 0, 1, panel_span),
      cld_x = upper_plot + panel_span * cld_offset_ratio,
      plot_significance = case_when(
        upper_plot < 0 & p.value < 0.05 ~ "Significant cooling",
        lower_plot > 0 & p.value < 0.05 ~
          "Significant positive association",
        TRUE ~ "Not significant"
      )
    ) %>%
    ungroup() %>%
    arrange(Hour_label, Cluster_label)
}

internal_plot_df <- prepare_effect_data(
  effect_data = internal_results$simple_slopes,
  cld_data = internal_results$cld,
  effect_mode = effect_mode,
  cld_offset_ratio = cld_offset_ratio
)

surrounding_plot_df <- prepare_effect_data(
  effect_data = surrounding_results$simple_slopes,
  cld_data = surrounding_results$cld,
  effect_mode = effect_mode,
  cld_offset_ratio = cld_offset_ratio
)


#===============================================================================
# 11. Plot theme and function
#===============================================================================

effect_theme <-
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major.x = element_line(
      color = "#E5E5E5",
      linewidth = 0.35
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "#666666",
      fill = NA,
      linewidth = 0.55
    ),
    strip.background = element_rect(
      fill = "#E8E6EA",
      color = "#666666",
      linewidth = 0.55
    ),
    strip.text = element_text(
      size = 10,
      face = "bold",
      color = "black",
      margin = margin(t = 3, r = 3, b = 3, l = 3)
    ),
    axis.text.x = element_text(size = 8.5, color = "black"),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.title.x = element_text(
      size = 11,
      color = "black",
      margin = margin(t = 7)
    ),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(
      size = 12,
      face = "plain",
      color = "black",
      hjust = 0,
      margin = margin(b = 5)
    ),
    panel.spacing = grid::unit(0.12, "cm"),
    legend.position = "none",
    plot.margin = margin(t = 5.5, r = 15, b = 5.5, l = 5.5)
  )

make_effect_forest_plot <- function(data, plot_title, x_title) {
  ggplot(
    data,
    aes(
      x = estimate_plot,
      y = Cluster_label,
      color = Cluster_code
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.55,
      color = "#808080"
    ) +
    geom_errorbar(
      aes(xmin = lower_plot, xmax = upper_plot),
      orientation = "y",
      width = 0.18,
      linewidth = 0.75
    ) +
    geom_point(size = 2.8) +
    geom_text(
      aes(x = cld_x, label = CLD),
      hjust = 0,
      vjust = 0.5,
      size = 3.3,
      fontface = "bold",
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    facet_wrap(
      vars(Hour_label),
      ncol = 4,
      scales = facet_x_scales,
      drop = FALSE
    ) +
    scale_color_manual(values = cluster_colors, drop = FALSE) +
    scale_y_discrete(limits = rev,drop = FALSE) +
    scale_x_continuous(
      expand = expansion(mult = c(0.06, 0.20))
    ) +
    coord_cartesian(clip = "off") +
    labs(title = plot_title, x = x_title, y = NULL) +
    effect_theme
}

if (effect_mode == "per10pct") {
  internal_x_label <-
    "LST change per 10-percentage-point increase in internal TCC (°C)"
  surrounding_x_label <-
    "LST change per 10-percentage-point increase in surrounding TCC (°C)"
} else {
  internal_x_label <- "Marginal effect of TCC on LST"
  surrounding_x_label <- "Marginal effect of TCC_buf on LST"
}

p_internal <- make_effect_forest_plot(
  data = internal_plot_df,
  plot_title = "Internal tree canopy effect (model_5_2)",
  x_title = internal_x_label
)

p_surrounding <- make_effect_forest_plot(
  data = surrounding_plot_df,
  plot_title = "Surrounding tree canopy effect (model_5_1)",
  x_title = surrounding_x_label
)

final_effect_plot <-
  (p_internal / p_surrounding) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    caption = paste0(
      "CLD letters compare neighborhood-type marginal slopes within each ",
      "observation time and within the corresponding source model. Types ",
      "sharing at least one letter do not differ significantly; types with ",
      "no shared letters differ based on Tukey-adjusted comparisons ",
      "(alpha = 0.05)."
    ),
    theme = theme(
      plot.caption = element_text(
        size = 9,
        color = "#333333",
        hjust = 0,
        margin = margin(t = 8)
      )
    )
  )

print(final_effect_plot)


#===============================================================================
# 12. Save plots
#===============================================================================

combined_png <- file.path(
  plot_output_dir,
  "Model5_1_Model5_2_marginal_effect_forest_plot_with_CLD.png"
)

combined_pdf <- file.path(
  plot_output_dir,
  "Model5_1_Model5_2_marginal_effect_forest_plot_with_CLD.pdf"
)

ggsave(
  filename = combined_png,
  plot = final_effect_plot,
  width = 18,
  height = 18,
  units = "in",
  dpi = 300,
  scale = 0.5,
  bg = "white"
)

ggsave(
  filename = combined_pdf,
  plot = final_effect_plot,
  width = 18,
  height = 18,
  units = "in",
  scale = 0.5,
  bg = "white"
)

ggsave(
  filename = file.path(
    plot_output_dir,
    "Model5_2_internal_canopy_forest_plot_with_CLD.png"
  ),
  plot = p_internal,
  width = 18,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    plot_output_dir,
    "Model5_1_surrounding_canopy_forest_plot_with_CLD.png"
  ),
  plot = p_surrounding,
  width = 18,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Marginal-effect analysis and forest plots completed.")
message("Excel output: ", output_file)
message("Combined PNG: ", combined_png)
message("Combined PDF: ", combined_pdf)





