# 1. 加载必要的包
library(readxl)
library(corrplot)
library(reshape2)
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggtext)
library(psych) # 用于快速计算 p 值

# ---------------------- 处理数据：重命名并保留特定变量 ----------------------

library(dplyr)
library(readxl)

# 1. 读取原始数据
data_path <- "E:/第三篇论文/data/xqsx4.xls"
df <- read_xls(data_path)

# 2. 执行重命名并筛选，命名为 data_hot
data_hot <- df %>%
  dplyr::rename(
    TCC      = TREE,
    BCI      = BUILDING,
    WCI      = WATER,
    GCI      = GRASS,
    BAH      = BAH0M,
    BHSD     = BHSD0M,
    TCC_buf  = b420t,
    BCI_buf  = b420b,
    WCI_buf  = b420w,
    GCI_buf  = b420g,
    BAH_buf  = BAH420M,
    BHSD_buf = BHSD420M,
    Area     = XQ_AREA
  ) %>%
  dplyr::select(
    TCC, BCI, WCI, GCI, CY, BAH, BHSD,
    SVF, Albedo, POP, FAR, MR,
    TCC_buf, BCI_buf, WCI_buf, GCI_buf,
    BAH_buf, BHSD_buf, Area, DIST
  )

# 3. 检查结果
# 查看前几行数据，确认变量名和数量是否正确
head(data_hot)
dim(data_hot) # 应该是 N 行 19 列

# 2. 数据预处理
# 确保使用你之前读取的 data_hot，并剔除含有 NA 的行
data_clean <- na.omit(data_hot)

# 3. 计算相关系数和 P 值
# 使用 spearman 或 pearson，根据你的研究需求选择
res <- corr.test(data_clean, method = "spearman", adjust = "none")
cor_matrix <- res$r
p_matrix <- res$p

# 4. 构建绘图数据框
df <- melt(cor_matrix) %>%
  mutate(pvalue = melt(p_matrix)[[3]],
         p_signif = symnum(pvalue, corr = FALSE, na = FALSE,
                           cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                           symbols = c("***", "**", "*", "")))

colnames(df) <- c("env", "genus", "r", "p", "p_signif")

# 5. 定义颜色（保持你喜欢的红灰蓝配色）
col <- colorRampPalette(c("#4472C4", "white", "#ED7D31"))(100)

# 6. 绘图：关键在于将 color 映射改为 fill 映射
p <- ggplot(df, aes(genus, env)) +
  # 使用 geom_tile 进行满格填充
  # fill = r 控制格子内部颜色，color = "white" 控制格子之间的线条颜色
  geom_tile(aes(fill = r), color = "grey90", size = 0.3) + 
  
  # 加入显著性标签
  geom_text(aes(label = p_signif), size = 5, color = "black",
            hjust = 0.5, vjust = 0.7) +
  
  labs(x = NULL, y = NULL) +
  
  # 注意：这里需要从 scale_color_gradientn 改为 scale_fill_gradientn
  scale_fill_gradientn(colours = col, limits = c(-1, 1)) +
  
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), position = 'right') +
  
  # 删除了 scale_size，因为满格填充不再需要大小映射
  
  # 修改图例属性：从 guide_colorbar(color=...) 改为 guide_colorbar(fill=...)
  guides(fill = guide_colorbar(title = "Spearman's r", 
                               barheight = 10, 
                               ticks.colour = "black")) +
  theme_test() +
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1, vjust = 1, color = "black"),
        axis.text.y = element_text(size=12, color = "black"),
        axis.ticks = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
        panel.background = element_blank())

print(p)

ggsave("E:/第三篇论文/plot/xin/相关性热图.png",
       p, width = 17.5, height = 15 ,scale=0.7, dpi = 300)







