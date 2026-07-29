# 安装并加载包
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(readxl)
library(dplyr)
library(tidyr)

# ====================== 你的文件路径 ======================
file_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"

# 读取数据
data <- read_excel(file_path)

# ====================== 需要统计的变量 ======================
# LST 变量
lst_vars <- c("lst0016_ME", "lst0404_ME", "lst0708_ME", "lst1042_ME", 
              "lst1349_ME", "lst1540_ME", "lst1959_ME")

# 建成环境变量
env_vars <- c("TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", 
              "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf", 
              "WCI_buf", "GCI_buf", "BAH_buf","BHSD_buf","Area","DIST")

all_vars <- c(lst_vars, env_vars)
stat_data <- data %>% select(all_of(all_vars))

# ====================== 核心：修正统计函数 ======================
basic_stats <- function(x) {
  x <- na.omit(x)
  data.frame(
    样本量 = length(x),
    # 输出格式：最小值 - 最大值（文本拼接）
    `最小值-最大值` = paste0(round(min(x), 3), " - ", round(max(x), 3)),
    平均值 = round(mean(x), 3),
    中位数 = round(median(x), 3),
    标准差 = round(sd(x), 3)
  )
}

# 批量计算
results <- stat_data %>% 
  summarise_all(basic_stats) %>% 
  pivot_longer(cols = everything(), names_to = "变量名", values_to = "统计值") %>%
  unnest(统计值)

# ====================== 输出结果 ======================
cat("========== 变量基础统计结果 ==========\n")
print(results, n = Inf)

# 导出Excel（方便论文使用）
if (!require("writexl")) install.packages("writexl")
library(writexl)
write_xlsx(results, "E:/第三篇论文/table/new/变量基础统计结果.xlsx")