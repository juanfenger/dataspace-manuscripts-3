library(terra)

# === 1. 设置路径 ===
folder <- "E:/KX10_GIU_20211126_E117.35_N38.90_202400045942_L4A"
pan_file <- file.path(folder, "KX10_GIU_20211126_E117.35_N38.90_202400045942_L4A_A_LH.tif")
shp_file <- "E:/第三篇论文/data/xqbj.shp"


# --- 路径 ---
pan_file <- "E:/KX10_GIU_20211126_E117.35_N38.90_202400045942_L4A/KX10_GIU_20211126_E117.35_N38.90_202400045942_L4A_A_LH.tif"
shp_file <- "E:/第三篇论文/data/xqbj.shp"

# --- 读取并选择 HDR 波段 ---
pan <- rast(pan_file)
pan <- pan[["HDR"]]  # ✅ 关键一步：只保留 HDR 波段

# --- 读取矢量 ---
xq <- vect(shp_file)
if (!crs(pan) == crs(xq)) xq <- project(xq, crs(pan))

# --- 可视化验证 ---
plot(pan, xlim = c(430000, 460000), ylim = c(4400000, 4430000))
plot(xq, add = TRUE, border = "red", lwd = 2)

# --- 提取第一个小区测试 ---
test <- extract(pan, xq[1, ], na.rm = FALSE)

# 查看结果
cat("提取到", length(test[[2]][[1]]), "个像素\n")
if (length(test[[2]][[1]]) > 0) {
  v <- test[[2]][[1]]
  cat("值范围:", range(v, na.rm = TRUE), "\n")
  cat("平均值:", mean(v, na.rm = TRUE), "\n")
}

# 添加 ID
xq$id <- 1:nrow(xq)
name_vec <- as.character(xq$name)

# 提取所有小区
vals <- extract(pan, xq, ID = TRUE, na.rm = FALSE)  # 使用 HDR 波段

# 初始化
n <- nrow(xq)
SOL_vec <- rep(NA, n)
Mean_vec <- rep(NA, n)
cell_area <- prod(res(pan))

for (i in 1:n) {
  pixel_vals <- vals$HDR[vals$ID == i]  # 注意列名变成 $HDR
  valid_vals <- pixel_vals[!is.na(pixel_vals)]
  
  if (length(valid_vals) == 0) next
  
  SOL_vec[i] <- sum(valid_vals, na.rm = TRUE)
  Mean_vec[i] <- mean(valid_vals, na.rm = TRUE)
}

Radiance_Density_vec <- Mean_vec / cell_area

# 构建结果
results_final <- data.frame(
  name = name_vec,
  SOL = SOL_vec,
  Mean_Radiance = Mean_vec,
  Radiance_Density = Radiance_Density_vec
)

# 查看最亮的几个小区
head(results_final[order(-results_final$Mean_Radiance), ])

# 保存
write.csv(results_final, "E:/第三篇论文/data/小区灯光统计结果.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")
