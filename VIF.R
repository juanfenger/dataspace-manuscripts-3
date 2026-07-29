# 1. 加载必要的库
library(readxl)
library(car)
library(dplyr)
library(tidyr)

# 2. 路径与变量定义
data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"

# 因变量列表
dv_list <- c("lst0016_ME", "lst0404_ME", "lst0708_ME", "lst1042_ME", 
             "lst1349_ME", "lst1540_ME", "lst1959_ME")

# 自变量映射（Key为数据框中的原始列名，Value为展示名）
var_name_map <- c(
  TCC="TCC", BCI="BCI", WCI="WCI", GCI="GCI", CY="CY",
  BAH="BAH", BHSD="BHSD", SVF="SVF", Albedo="Albedo",
  POP="POP", FAR="FAR", RD="RD",
  TCC_buf="TCC_buf", BCI_buf="BCI_buf", WCI_buf="WCI_buf", GCI_buf="GCI_buf",
  BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST"
)

# 3. 读取数据
df <- read_excel(data_path)

# 4. 循环计算 VIF
# 创建一个空的列表存储结果
vif_results <- list()

for (dv in dv_list) {
  cat("\n--- 正在处理因变量:", dv, "---\n")
  
  # 构建公式：dv ~ iv1 + iv2 + ...
  iv_names <- names(var_name_map)
  formula_str <- paste(dv, "~", paste(iv_names, collapse = " + "))
  
  # 拟合线性模型
  fit <- lm(as.formula(formula_str), data = df)
  
  # 计算 VIF
  # 注意：如果存在完全共线性，vif() 会报错，这里加个 tryCatch
  vif_values <- tryCatch({
    vif(fit)
  }, error = function(e) {
    message("计算错误（可能存在完全共线性变量）: ", dv)
    return(NULL)
  })
  
  if (!is.null(vif_values)) {
    # 转换为易读的数据框，并映射变量名
    vif_df <- data.frame(
      Original_Name = names(vif_values),
      Mapped_Name = var_name_map[names(vif_values)],
      VIF = as.numeric(vif_values)
    )
    vif_results[[dv]] <- vif_df
    print(vif_df)
  }
}

# 5. 可选：合并所有结果为一个大表方便导出
all_vif_combined <- bind_rows(vif_results, .id = "Dependent_Variable")

# 导出结果到 CSV (如果需要)
write.csv(all_vif_combined, "E:/第三篇论文/table/VIF_Results_Summary.csv", row.names = FALSE)


































# 1. 加载必要的库
library(readxl)
library(car)
library(dplyr)
library(tidyr)

# 2. 路径与变量定义（统一路径，避免重复覆盖）
data_path <- "E:/第三篇论文/data/xq2725.xls"  # 确认后缀：xlsx 或 xls

# 因变量列表
dv_list <- c("lst0016_ME", "lst0404_ME", "lst0708_ME", "lst1042_ME", 
             "lst1349_ME", "lst1540_ME", "lst1959_ME")

# 自变量列表（直接使用，无需names()）
iv_list <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", 
  "POP", "FAR", "RD","TCC_buf","BCI_buf", "WCI_buf", "GCI_buf", 
  "BAH_buf","BHSD_buf","DIST"
)

# 3. 读取数据
df <- read_excel(data_path)

# 4. 循环计算 VIF
vif_results <- list()

for (dv in dv_list) {
  cat("\n--- 正在处理因变量:", dv, "---\n")
  
  # 构建公式（核心修复：直接用自变量列表）
  formula_str <- paste(dv, "~", paste(iv_list, collapse = " + "))
  
  # 拟合线性模型
  fit <- lm(as.formula(formula_str), data = df)
  
  # 计算 VIF
  vif_values <- tryCatch({
    vif(fit)
  }, error = function(e) {
    message("计算错误（可能存在完全共线性变量）: ", dv)
    return(NULL)
  })
  
  if (!is.null(vif_values)) {
    vif_df <- data.frame(
      Variable = names(vif_values),
      VIF = as.numeric(vif_values)
    )
    vif_results[[dv]] <- vif_df
    print(vif_df)
  }
}
