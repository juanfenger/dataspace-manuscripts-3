current_lst <- "lst0708_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf", "WCI_buf", "GCI_buf", "BAH_buf","BHSD_buf","DIST", current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = as.data.frame(x_data),
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD",
  SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf",
  BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar <- p_bar & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP <- wrap_plots(p_dep_list, ncol = 2)
print(SHAP_DP)

p_dep_list0708 <- p_dep_list
#————————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————————
#——————————————————————————————————————————————————————————————————————————————————


current_lst <- "lst1042_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",
  BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",
  TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar1042 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar1042 <- p_bar1042 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar1042)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list1042 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP1042 <- wrap_plots(p_dep_list1042, ncol = 2)
print(SHAP_DP1042)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------



current_lst <- "lst1349_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar1349 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar1349 <- p_bar1349 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar1349)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list1349 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP1349 <- wrap_plots(p_dep_list1349, ncol = 2)
print(SHAP_DP1349)
p_dep_list1349 <- p_dep_list
#______________________________________________________________________________
#_______________________________________________________________________________
#_______________________________________________________________________________
current_lst <- "lst1540_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar1540 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar1540 <- p_bar1540 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar1540)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list1540 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP1540 <- wrap_plots(p_dep_list1540, ncol = 2)
print(SHAP_DP1540)

#——————————————————————————————————————————————————————————————————————————————————
#————————————————————————————————————————————————————————————————————————————————————
#——————————————————————————————————————————————————————————————————————————————————

current_lst <- "lst1959_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar1959 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar1959 <- p_bar1959 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar1959)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list1959 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP1959 <- wrap_plots(p_dep_list1959, ncol = 2)
print(SHAP_DP1959)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


current_lst <- "lst0016_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar0016 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar0016 <- p_bar0016 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar0016)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list0016 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP0016 <- wrap_plots(p_dep_list0016, ncol = 2)
print(SHAP_DP0016)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


current_lst <- "lst0404_ME"   # ←←← 每次运行只改这一行
bar_color_map <- c(
  lst0708_ME = "#FDAE61",  # 清晨
  lst1042_ME = "#F46D43",
  lst1349_ME = "#D73027",
  lst1540_ME = "#A50026",  # 午后最热
  lst1959_ME = "#74ADD1",
  lst0016_ME = "#4575B4",
  lst0404_ME = "#313695"   # 夜间
)

current_bar_color <- bar_color_map[current_lst]

#释放包

library(ranger)
library(readxl)
library(caret)
library(tidyverse)
library(ParBayesianOptimization)
library(fastshap)
library(DALEX)
library(shapviz)
library(patchwork)
library(MASS)

#数据预处理

data_path <- "E:/第三篇论文/table/xqsx4_Clustering_Results.xlsx"
data <- readxl::read_excel(data_path)
data$CY <- as.numeric(data$CY)

selected_vars <- c(
  "TCC", "BCI", "WCI", "GCI","CY","BAH", "BHSD", "SVF", "Albedo", "POP", "FAR", "MR","TCC_buf","BCI_buf","WCI_buf","GCI_buf","BAH_buf","BHSD_buf","DIST",current_lst)

model_data <- na.omit(data[, selected_vars])

x_data <- model_data[, setdiff(selected_vars, current_lst), drop = FALSE]
y_data <- model_data[[current_lst]]

#贝叶斯优化与随机森林模型

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

set.seed(123)
opt_res <- bayesOpt(
  FUN = rf_optim_fun,
  bounds = search_grid,
  initPoints = 10,
  nIter = 30,
  verbose = 1
)

best_params <- getBestPars(opt_res)
best_params <- lapply(best_params, function(x) as.integer(round(x)))

rf_cv_final <- train(
  x = x_data,
  y = y_data,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    mtry = best_params$mtry,
    splitrule = "variance",
    min.node.size = best_params$min.node.size
  ),
  num.trees = best_params$num.trees,
  metric = "RMSE"
)

cat("五折交叉验证RMSE：", round(rf_cv_final$results$RMSE, 4), "\n")
cat("五折交叉验证R²：", round(rf_cv_final$results$Rsquared, 4), "\n")


#shap计算

pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata)
}

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_cv_final,
  X = x_data,
  nsim = 50,
  pred_wrapper = pred_wrapper
)

shap_imp <- colMeans(abs(shap_values))

#变量重新命名

var_name_map <- c(
  TCC="TCC",BCI="BCI",WCI="WCI",GCI="GCI",CY="CY",BAH="BAH",BHSD="BHSD", SVF="SVF",Albedo="Albedo",POP="POP",FAR="FAR",MR="MR",TCC_buf="TCC_buf", BCI_buf="BCI_buf",WCI_buf="WCI_buf",GCI_buf="GCI_buf",BAH_buf="BAH_buf", BHSD_buf="BHSD_buf",DIST="DIST")

#SHAP BAR+ BEESWARM
top10_vars <- names(sort(shap_imp, decreasing = TRUE))[1:10]

shap_top10 <- shap_values[, top10_vars]
x_top10 <- x_data[, top10_vars]

colnames(shap_top10) <- var_name_map[colnames(shap_top10)]
colnames(x_top10) <- var_name_map[colnames(x_top10)]

shp_top10 <- shapviz(shap_top10, X = x_top10)

p_bar0404 <- (
  sv_importance(shp_top10, kind = "bar",
                fill = current_bar_color, alpha = 0.7, color = NA) +
    sv_importance(shp_top10, kind = "beeswarm", alpha = 0.4)
) &
  theme(
    # 背景
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    # 网格线（一般期刊建议关掉）
    # 主 y 轴网格线
    panel.grid.major.x = element_line(
      color = "grey85",
      linewidth = 0.5
    ),
    
    # 关闭其他网格
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # 坐标轴线 —— 核心
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    
    # 轴刻度
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(6, "pt"),
    
    #字体大小
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    
    # 防止出现完整边框
    panel.border = element_blank(),
    
    # 图例
    legend.position = "right"
  )

p_bar0404 <- p_bar0404 & scale_x_continuous(
  expand = expansion(mult = c(0, 0.1))
)

print(p_bar0404)

#SHAP_DP

top5_vars <- c("TCC","TCC_buf")

shap_top5 <- shap_values[, top5_vars]
x_top5 <- x_data[, top5_vars]

colnames(shap_top5) <- var_name_map[colnames(shap_top5)]
colnames(x_top5) <- var_name_map[colnames(x_top5)]

shp_top5 <- shapviz(shap_top5, X = x_top5)

vars_top5 <- colnames(x_top5)

p_dep_list0404 <- lapply(vars_top5, function(vv) {
  
  p0 <- sv_dependence(shp_top5, v = vv, smooth = FALSE)
  d <- ggplot_build(p0)$data[[1]]
  colnames(d)[1:2] <- c("x", "y")
  
  kd <- kde2d(d$x, d$y, n = 100)
  ix <- findInterval(d$x, kd$x)
  iy <- findInterval(d$y, kd$y)
  d$density <- kd$z[cbind(ix, iy)]
  
  ggplot(d, aes(x, y)) +
    geom_point(aes(color = density), size = 1.4, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", se = FALSE, color = "red") +
    scale_color_viridis_c(name = "Density",option = "viridis") +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(6, "pt"),
      
      axis.text = element_text(size = 11),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      
      legend.position = "right"
    )  +
    labs(x = vv, y = NULL)
})

SHAP_DP0404 <- wrap_plots(p_dep_list0404, ncol = 2)
print(SHAP_DP0404)




#______________________总拼图,bar+beeswarm-----------------------------------------
# 加载必备包

library(patchwork)
library(ggplot2)


#--------------------------------------------------
# 1. 统一 bar : beeswarm 比例（非常关键）
#--------------------------------------------------
bar_bee_ratio <- c(1, 1)

p_bar       <- p_bar       + plot_layout(widths = bar_bee_ratio)
p_bar1042   <- p_bar1042   + plot_layout(widths = bar_bee_ratio)
p_bar1349   <- p_bar1349   + plot_layout(widths = bar_bee_ratio)
p_bar1540   <- p_bar1540   + plot_layout(widths = bar_bee_ratio)
p_bar1959   <- p_bar1959   + plot_layout(widths = bar_bee_ratio)
p_bar0016   <- p_bar0016   + plot_layout(widths = bar_bee_ratio)
p_bar0404   <- p_bar0404   + plot_layout(widths = bar_bee_ratio)

#--------------------------------------------------
# 2. 强制统一 margin（解决“看起来不齐”的关键）
#--------------------------------------------------
fix_margin <- theme(
  plot.margin = margin(6, 6, 6, 6)
)

p_bar       <- p_bar       & fix_margin
p_bar1042   <- p_bar1042   & fix_margin
p_bar1349   <- p_bar1349   & fix_margin
p_bar1540   <- p_bar1540   & fix_margin
p_bar1959   <- p_bar1959   & fix_margin
p_bar0016   <- p_bar0016   & fix_margin
p_bar0404   <- p_bar0404   & fix_margin

#--------------------------------------------------
# 3. 白天 / 夜间分别拼接（收集 legend）
#--------------------------------------------------
p_day <- (
  p_bar | p_bar1042 | p_bar1349 | p_bar1540
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(legend.position = "right")

p_night <- (
  p_bar1959 | p_bar0016 | p_bar0404 | p_bar1042 
) +
  plot_layout(
    guides = "collect"
  ) &
  theme(legend.position = "right")

#--------------------------------------------------
# 4. 最终两行结构（白天 / 夜间）
#--------------------------------------------------
p_final <- p_day / p_night +
  plot_layout(
    ncol = 1,
    heights = c(1, 1)
  )

#--------------------------------------------------
# 5. 输出
#--------------------------------------------------
print(p_final)

# 可选：保存图形
ggsave("E:/第三篇论文/plot/p_final.png", p_final, 
       width = 32, height = 8, dpi = 300, bg = "white")


#-------------------------总拼图（依赖图）----------

library(patchwork)

# 每一行（5 个 dependence plot）统一宽度
dp_widths <- rep(1, 5)

SHAP_DP0016 <- SHAP_DP0016 + plot_layout(widths = dp_widths)
SHAP_DP0404 <- SHAP_DP0404 + plot_layout(widths = dp_widths)
SHAP_DP     <- SHAP_DP     + plot_layout(widths = dp_widths)
SHAP_DP1042 <- SHAP_DP1042 + plot_layout(widths = dp_widths)
SHAP_DP1349 <- SHAP_DP1349 + plot_layout(widths = dp_widths)
SHAP_DP1540 <- SHAP_DP1540 + plot_layout(widths = dp_widths)
SHAP_DP1959 <- SHAP_DP1959 + plot_layout(widths = dp_widths)

dp_margin <- theme(
  plot.margin = margin(4, 4, 4, 4)
)

SHAP_DP0016 <- SHAP_DP0016 & dp_margin
SHAP_DP0404 <- SHAP_DP0404 & dp_margin
SHAP_DP     <- SHAP_DP     & dp_margin
SHAP_DP1042 <- SHAP_DP1042 & dp_margin
SHAP_DP1349 <- SHAP_DP1349 & dp_margin
SHAP_DP1540 <- SHAP_DP1540 & dp_margin
SHAP_DP1959 <- SHAP_DP1959 & dp_margin


p_dep_all_time <- (
  
  SHAP_DP     /
    SHAP_DP1042 /
    SHAP_DP1349 /
    SHAP_DP1540 /
    SHAP_DP1959 /
    SHAP_DP0016 /
    SHAP_DP0404 
) +
  plot_layout(
    heights = rep(1, 7),   # 每一行等高
    guides = "collect"     # 合并 legend（如果有）
  ) +
  scale_color_viridis_c(
    name = "Relative point density"
  ) &
  theme(
    legend.position = "right"
  )

print(p_dep_all_time)

# 可选：保存图形
ggsave("E:/第三篇论文/plot/p_dep_all_time.png", p_dep_all_time, 
       width = 15, height = 20, dpi = 300, bg = "white")

ggsave("E:/第三篇论文/plot/p_dep_all_time.pdf", p_dep_all_time, 
       width = 15, height = 20, bg = "white")

#===================================================================================

#-------------------------总拼图（依赖图 + y=0交点）----------

library(patchwork)
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)

# 如果0708、1959、0404前面没有单独保存列表，这里补充
# 注意：最好在对应时相刚生成后保存，避免 p_dep_list 被后面覆盖
# p_dep_list0708 <- p_dep_list
# p_dep_list1959 <- p_dep_list
# p_dep_list0404 <- p_dep_list

# ============================================================
# 1. 计算拟合线与 y = 0 的交点
# ============================================================

get_zero_crossings <- function(p, time_label, var_label){
  
  gb <- ggplot_build(p)
  
  smooth_layers <- gb$data %>%
    keep(~ all(c("x", "y") %in% names(.x)) && nrow(.x) > 20)
  
  if (length(smooth_layers) == 0) {
    return(tibble(
      Time = time_label,
      Variable = var_label,
      x_intercept = NA_real_
    ))
  }
  
  sm <- smooth_layers[[length(smooth_layers)]] %>%
    arrange(x) %>%
    filter(is.finite(x), is.finite(y))
  
  idx <- which(sm$y[-1] * sm$y[-nrow(sm)] <= 0)
  
  if (length(idx) == 0) {
    return(tibble(
      Time = time_label,
      Variable = var_label,
      x_intercept = NA_real_
    ))
  }
  
  roots <- map_dbl(idx, function(i){
    x1 <- sm$x[i]
    x2 <- sm$x[i + 1]
    y1 <- sm$y[i]
    y2 <- sm$y[i + 1]
    
    if (y1 == y2) {
      return(x1)
    } else {
      return(x1 - y1 * (x2 - x1) / (y2 - y1))
    }
  })
  
  tibble(
    Time = time_label,
    Variable = var_label,
    x_intercept = roots
  )
}

calc_crossing_table <- function(plot_list, time_label){
  map2_dfr(
    plot_list,
    seq_along(plot_list),
    function(p, i){
      var_label <- p$labels$x
      if (is.null(var_label)) var_label <- paste0("Variable_", i)
      get_zero_crossings(p, time_label, var_label)
    }
  )
}

zero_crossing_table <- bind_rows(
  calc_crossing_table(p_dep_list0708, "0708"),
  calc_crossing_table(p_dep_list1042, "1042"),
  calc_crossing_table(p_dep_list1349, "1349"),
  calc_crossing_table(p_dep_list1540, "1540"),
  calc_crossing_table(p_dep_list1959, "1959"),
  calc_crossing_table(p_dep_list0016, "0016"),
  calc_crossing_table(p_dep_list0404, "0404")
)

print(zero_crossing_table)

write_csv(
  zero_crossing_table,
  "E:/第三篇论文/plot/SHAP_dependence_y0_crossings.csv"
)

# ============================================================
# 2. 统一依赖图边距
# ============================================================

dp_margin <- theme(
  plot.margin = margin(4, 4, 4, 4)
)

SHAP_DP      <- SHAP_DP      & dp_margin
SHAP_DP1042  <- SHAP_DP1042  & dp_margin
SHAP_DP1349  <- SHAP_DP1349  & dp_margin
SHAP_DP1540  <- SHAP_DP1540  & dp_margin
SHAP_DP1959  <- SHAP_DP1959  & dp_margin
SHAP_DP0016  <- SHAP_DP0016  & dp_margin
SHAP_DP0404  <- SHAP_DP0404  & dp_margin

# ============================================================
# 3. 按指定顺序拼接：
# 0708  1042
# 1349  1540
# 1959  0016
# 0404  空白
# ============================================================

blank_panel <- plot_spacer()

p_dep_all_time <- (
  (SHAP_DP     | SHAP_DP1042) /
    (SHAP_DP1349 | SHAP_DP1540) /
    (SHAP_DP1959 | SHAP_DP0016) /
    (SHAP_DP0404 | blank_panel)
) +
  plot_layout(
    widths  = c(1, 1),
    heights = c(1, 1, 1, 1),
    guides  = "collect"
  ) &
  theme(
    legend.position = "right"
  )

print(p_dep_all_time)

ggsave(
  "E:/第三篇论文/plot/p_dep_all_time_4x2.png",
  p_dep_all_time,
  width = 22,
  height = 20,
  dpi = 300,
  bg = "white",
  scale = 0.55
)

ggsave(
  "E:/第三篇论文/plot/p_dep_all_time_4x2.pdf",
  p_dep_all_time,
  width = 22,
  height = 20,
  bg = "white",
  scale = 0.55
)

