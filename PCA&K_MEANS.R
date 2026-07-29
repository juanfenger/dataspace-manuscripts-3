# ---------------------- 0. 你之前的包加载 ----------------------

library(writexl)
library(readxl)
library(factoextra)
library(dplyr)
library(ggplot2)
library(cluster)

# ---------------------- 1. 读取数据 + 重命名变量（你给的命名） ----------------------
data_path <- "E:/第三篇论文/data/xqsx4.xls"
#data_path <- "E:/第三篇论文/data/raw data before clustering.xls"
df <- read_xls(data_path)

# 【关键】变量重命名（完全按你要求）
df <- df %>%
  rename(
    TCC    = TREE,
    BCI    = BUILDING,
    WCI    = WATER,
    GCI    = GRASS,
    BAH    = BAH0M,
    BHSD   = BHSD0M,
    TCC_buf = b420t,
    BCI_buf = b420b,
    WCI_buf = b420w,
    GCI_buf = b420g,
    BAH_buf = BAH420M,
    BHSD_buf= BHSD420M,
    Area   = XQ_AREA,
    DIST = DIST
  )

# 所有用于PCA+聚类的变量（19个）
all_vars <- c("TCC","BCI","WCI","GCI","CY","BAH","BHSD",
              "SVF","Albedo","POP","FAR","MR",
              "TCC_buf","BCI_buf","WCI_buf","GCI_buf",
              "BAH_buf","BHSD_buf","Area","DIST")

# 提取数据 + 去缺失值
df_pca <- df %>% select(all_of(all_vars)) %>% na.omit()

# ---------------------- 2. 数据标准化（PCA必须标准化） ----------------------
df_scaled <- scale(df_pca)

# ---------------------- 3. 执行PCA（核心步骤） ----------------------
pca_model <- prcomp(df_scaled, center = FALSE, scale. = FALSE)

# 查看PCA方差解释率（看前几个主成分能代表数据）
summary(pca_model)

# 提取 PCA 载荷矩阵（每个变量在各主成分上的系数）
loadings <- as.data.frame(pca_model$rotation)

# 只看前 11 个主成分（你已经确定用 PC1~PC11）
loadings[, 1:6]

# 画碎石图（选主成分用）
fviz_screeplot(pca_model, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title="PCA 碎石图",subtitle="柱子高度=该主成分解释力度")
#------------------------3.5使用ggplot绘图------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# ===================== 1. 准备学术绘图数据 =====================
# 1. 计算特征值和比例
eig_values <- pca_model$sdev^2
print(eig_values)
# 计算方差解释比例 (Proportion, 0-1)
variance_prop <- eig_values / sum(eig_values)
cumulative_prop <- cumsum(variance_prop)

# 2. 构建数据框
# 我们展示前 10 个主成分，这在学术上通常足够了
n_comp_to_show <- 10 
scree_data <- data.frame(
  PC = 1:n_comp_to_show,
  # 柱状图：单个解释比例
  Individual_Prop = variance_prop[1:n_comp_to_show],
  # 累计解释比例 (左轴实线)
  Cumulative_Prop = cumulative_prop[1:n_comp_to_show],
  # 特征值 (右轴虚线)
  Eigenvalue = eig_values[1:n_comp_to_show]
)

# PC 设为 Factor 方便绘图
scree_data$PC <- factor(scree_data$PC)

# ===================== 2. 设定关键的缩放因子 =====================
# 核心：左轴(Proportion)最大值是 1。
# 我们需要找到右轴(Eigenvalue)在绘图空间内的最大值。
# 为了美观，我们取特征值最大值并向上取整。
# 比如最大特征值是 3.8，我们就取 4。
# (注意：你的 18 个变量，特征值之和是 18，前几个通常在 1-4 之间)
max_right_axis <- ceiling(max(scree_data$Eigenvalue))

# 缩放因子 = 右轴数值 / 左轴数值 = N / 1 = N
# 这样，在 geom_hline 或其他需要对齐右轴的地方，y = 数值 * (1 / N)
sc_factor <- 1 / max_right_axis

# ===================== 3. 仿照 Fig. 7 绘图 =====================
pca_fig <- ggplot(scree_data, aes(x = PC)) +
  
  # 1. 绘制方差比例参考水平线 (dotted line at 0.00, 0.25, 0.50, 0.75)
  # 仿照参考图中的淡灰色网格背景效果
  geom_hline(yintercept = c(0.25, 0.50, 0.75), linetype = "dotted", color = "grey80") +
  
  # 2. 绘制特征值参考线 Eigenvalues = 1 (Kaiser Criterion)
  # 这是最重要的参考线，它对应右轴的 1。
  
  geom_hline(yintercept = 1 * sc_factor, linetype = "dashed", color = "grey30", size = 0.5) +
  
  # 我们需要用左轴的量纲来画：y = 1 * sc_factor
  
  # 3. 绘制柱状图 (Proportion of Variance)
  # 仿照其深灰色，柱底部紧贴 X 轴
  geom_bar(aes(y = Individual_Prop), stat = "identity", fill = "grey50", width = 0.7) +
  
  # 4. 绘制累计方差比例曲线 (Cumulative Prop, 左轴实线)
  geom_line(aes(y = Cumulative_Prop, group = 1), color = "black", size = 0.8) +
  geom_point(aes(y = Cumulative_Prop), color = "black", size = 2, shape = 16) +
  
  # 5. 绘制特征值曲线 (Eigenvalues, 右轴虚线)
  # 核心对齐：需要用左轴的量纲来画：y = Eigenvalue * sc_factor
  geom_line(aes(y = Eigenvalue * sc_factor, group = 1), color = "black", linetype = "dashed", size = 0.8) +
  geom_point(aes(y = Eigenvalue * sc_factor), color = "black", size = 2, shape = 17) + # 使用不同形状(三角形)
  
  # 6. 设置坐标轴和双 Y 轴
  scale_y_continuous(
    # 左 Y 轴标题和格式
    name = "Proportion of Variance",
    expand = expansion(mult = c(0, 0.1)), # 底部贴紧，顶部留出小空白
    limits = c(0, 1.05), # 确保顶部曲线不被切掉
    breaks = seq(0, 1, by = 0.25), # 仿照参考图刻度 (0, 0.25, 0.50, 0.75, 1.00)
    labels = scales::label_number(accuracy = 0.01), # 强制保留两位小数
    
    # 右 Y 轴标题和格式 (由左轴数值通过 sc_factor 转换得到)
    sec.axis = sec_axis(
      ~ . / sc_factor, 
      name = "Eigenvalues",
      breaks = seq(0, max_right_axis, by = 1) # 特征值通常是整数刻度
    )
  ) +
  
  # 7. 设置标题和 X 轴标签
  labs(title = NULL, x = "Principal Components") +
  
  # 8. 设置学术主题风格 (High-resolution paper style)
  theme_classic() + # 使用 classic 确保坐标轴线清晰
  theme(
    # 字体大小和粗细调整
    text = element_text(family = "", size = 12), # 使用 Serif 字体，更有学术感
    axis.title = element_text(face = "plain", size = 14), # 轴标题设为平体
    axis.text = element_text(size = 12, color = "black"),
    # 调整标题对齐和大小
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    # 去掉所有图例，参考图没有图例
    legend.position = "none",
    # 确保坐标轴线是黑色的，刻度线向外
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(6, "pt")
  )

# ===================== 4. 预览并导出 PDF =====================
# 预览
print(pca_fig)

# 导出为 8x6 英寸的 PDF
ggsave(
  "E:/第三篇论文/plot/Fig7_PCA_Analysis_Aligned.pdf", 
  plot = pca_fig, 
  device = "pdf", 
  width = 10, 
  height = 8, 
  units = "in"
)


# ---------------------- 4. Varimax 旋转 ----------------------

# 设定你想旋转的主成分个数（根据碎石图建议选 6 个，或者按你之前的思路选更多）
n_factors <- 6 

# 提取原始载荷
raw_loadings <- pca_model$rotation[, 1:n_factors]

# 执行 Varimax 旋转
rotated_result <- varimax(raw_loadings)

# 提取旋转后的载荷矩阵
rotated_loadings <- as.data.frame(rotated_result$loadings[1:20, 1:n_factors])
colnames(rotated_loadings) <- paste0("RC", 1:n_factors) # RC 代表 Rotated Component

# 查看旋转后的载荷（你会发现系数变得更非 0 即 1，更清晰了）
print(rotated_loadings)

# ---------------------- 4. 提取前 N 个主成分（一般取累计解释 ≥70%~80%） ----------------------
# 通常前 5~7 个就能解释你这类城市数据 80% 以上
pca_data <- as.data.frame(pca_model$x[, 1:6])  # 提取前6个主成分
# 计算旋转后的得分 (Rotated Scores)
# 使用标准化后的原始数据 乘以 旋转后的载荷矩阵
pca_rotated_scores <- as.matrix(df_scaled) %*% rotated_result$loadings
pca_data <- as.data.frame(pca_rotated_scores)
colnames(pca_data) <- paste0("RC", 1:n_factors)


# ===================== 【终极方案】第5步：CCC + 伪F =====================
library(NbClust)
set.seed(123)

# 确保数据是纯数值矩阵
pca_matrix <- as.matrix(pca_data)

# 使用 "all" 模式运行（这可能需要 10-20 秒，因为它计算 26 个指标）
# 如果样本量极大，建议稍微等待
nb_all <- NbClust(data = pca_matrix, 
                  distance = "euclidean", 
                  min.nc = 2, 
                  max.nc = 10, 
                  method = "kmeans", 
                  index = "all")

# 1. 自动识别列名（防止大小写差异导致的下标出界）
all_colnames <- colnames(nb_all$All.index)

# 肘部法则
wcss <- sapply(2:10, function(k) kmeans(pca_data, k)$tot.withinss)
plot(2:10, wcss, type = "b", pch = 19, frame = FALSE)

# 轮廓系数
library(cluster)
sil <- sapply(2:10, function(k) mean(silhouette(kmeans(pca_data, k)$cluster, dist(pca_data))[,3]))
plot(2:10, sil, type = "b", pch = 19, frame = FALSE)

library(ggplot2)
library(cluster)


# ======================
# WCSS + PtBiserial 并排图
# ======================

colnames(nb_all$All.index)

library(ggplot2)
library(patchwork)

# 1. WCSS 数据框：直接使用你已经算好的 wcss
df_wcss <- data.frame(
  k = 2:10,
  wcss = wcss
)

# 2. PtBiserial 数据框：从 NbClust 结果中提取
df_ptb <- data.frame(
  k = 2:10,
  ptbiserial = nb_all$All.index[, "Ptbiserial"]
)

# ======================
# 3. 绘制 WCSS 肘部图
# ======================
p_elbow <- ggplot(df_wcss, aes(x = k, y = wcss)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 3, shape = 19, color = "black") +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Elbow Method",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.7)
  )

# ======================
# 4. 绘制 PtBiserial 图
# ======================
p_ptb <- ggplot(df_ptb, aes(x = k, y = ptbiserial)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 3, shape = 19, color = "black") +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Point-Biserial Index",
    x = "Number of Clusters (k)",
    y = "Point-Biserial Index"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "black"),
    axis.line = element_line(linewidth = 0.7),
    axis.ticks = element_line(linewidth = 0.7)
  )

# ======================
# 5. 并排输出
# ======================
p_pin <- p_elbow + p_ptb
p_pin

ggsave(
  "E:/第三篇论文/plot/xin/elbow_ptbiserial.pdf",
  p_pin,
  height = 12,
  width = 20,
  scale = 0.7
)
#===============================================================================
# 1. 查看个变量对前 6 个主成分的贡献 (Loadings)
loadings_matrix <- as.data.frame(pca_model$rotation[, 1:6])

# 2. 聚类并写回（以 K=5 为例）
best_k <- 5
set.seed(123)
final_clusters <- kmeans(pca_data, centers = best_k, nstart = 25)
df_pca$Cluster <- as.factor(final_clusters$cluster)

# 3. 核心步骤：计算原始变量在各簇的均值（画像描述）
# 这能让你知道 Cluster 1 是不是“高绿化、低高度、老年代”
cluster_character <- df_pca %>%
  group_by(Cluster) %>%
  summarise(across(all_of(all_vars), list(mean = mean)))

print(cluster_character)
write_xlsx(cluster_character,"E:/第三篇论文/table/meanvalue.xlsx")

# ---------------------- 5. 用【降维后数据】做 K-means = 5 ----------------------
set.seed(123)
km_pca <- kmeans(pca_data, centers = 5, nstart = 25)

# ---------------------- 6. 统计每类样本数量（你最需要的） ----------------------
cat("===== PCA降维后 K=5 各类样本数 =====\n")
table(km_pca$cluster)

# 带占比
count_result <- data.frame(table(聚类类别=km_pca$cluster)) %>%
  rename(样本数量=Freq) %>%
  mutate(占比=paste0(round(样本数量/sum(样本数量)*100,3),"%"))
print(count_result)

# ---------------------- 7. 把聚类结果放回原始数据（方便后续作图/导出） ----------------------
df_final <- df[as.numeric(rownames(pca_data)), ] # 匹配行
df_final$cluster <- km_pca$cluster


# ---------------------- 8. 聚类质量检验（轮廓系数） ----------------------
fviz_silhouette(silhouette(km_pca$cluster, dist(pca_data))) +
  labs(title="PCA降维后 K=5 轮廓系数图")

# 计算每一类在 6 个旋转主成分上的均值得分=============================================
# 确保 pca_data 的列名是正确的
colnames(pca_data)[1:6] <- paste0("RC", 1:6)

# 计算均值
pca_cluster_profile <- pca_data %>%
  mutate(Cluster = df_pca$Cluster) %>%
  group_by(Cluster) %>%
  summarise(across(starts_with("RC"), mean, .names = "{col}_mean"))

# 打印结果
print("各聚类在旋转主成分上的得分均值：")
print(pca_cluster_profile)
write_xlsx(pca_cluster_profile,"E:/第三篇论文/table/PCA_meanvalue.xlsx")
# ---------------------- 导出合并结果 ----------------------
library(writexl)
library(dplyr)

# 1. 给原始数据 df 添加一个临时的唯一 ID (行号)
# 这样即便后续有 na.omit，我们也能通过这个 ID 把结果挂回去
df_with_id <- df %>% mutate(temp_id = row_number())

# 2. 准备聚类结果数据框
# 假设你的聚类是在 df_pca（去过缺失值的数据）上跑的
# 我们把 RC 得分和 Cluster 标签整合成一个结果表
cluster_results <- data.frame(
  # 这里的行名通常保留了原始数据的索引
  temp_id = as.numeric(rownames(df_pca)), 
  Cluster = final_clusters$cluster,
  RC1 = pca_data$RC1,
  RC2 = pca_data$RC2,
  RC3 = pca_data$RC3,
  RC4 = pca_data$RC4,
  RC5 = pca_data$RC5,
  RC6 = pca_data$RC6
)

# 3. 将聚类结果连接回最原始的表 (保留 xqsx4.xls 的所有原始列)
# 使用 left_join，这样即便某些行因为缺失值没参与聚类，也会保留在原表中（结果为 NA）
final_merged_data <- df_with_id %>%
  left_join(cluster_results, by = "temp_id") %>%
  select(-temp_id) # 删掉临时 ID 列

# 4. 导出为新的 Excel 文件
# 建议加上时间或版本号，防止覆盖
output_file <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
write_xlsx(final_merged_data, output_file)

cat("导出成功！原始数据与聚类结果已合并至：", output_file)
