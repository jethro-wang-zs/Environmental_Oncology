# ==============================================================================
# 项目名称: Data Inspection (侦察脚本)
# 目的: 检查环境暴露数据(UHI)、Medicare的格式、分辨率和变量名，以决定清洗策略
# 日期: 2026-01-15
# ==============================================================================

library(tidyverse)
library(readxl) #以此防备数据是Excel格式
# 加载必要的非空间包
# 如果没有安装，请先运行 install.packages("haven") 和 install.packages("tidyverse")
library(haven)     # 专门用于读取 Stata/SAS/SPSS 数据
library(tidyverse) # 数据清洗全家桶 (包含 dplyr, stringr 等)
library(labelled)

# ==============================================================================
# 甲、 Medicare数据处理
# ==============================================================================

# 1. 路径设置
# ------------------------------------------------
### 任务 A. 设置工作目录 (读取数据的位置)
# 注意：请确保此路径在您的服务器上存在
setwd("/users/PAS2978/wongzs/1. Pawlik/UHI/Rawdata")

# 设置输出目录 (结果保存的位置)
Input <- "/users/PAS2978/wongzs/1. Pawlik/UHI/RInputData/"
Output <- "/users/PAS2978/wongzs/1. Pawlik/UHI/260115_UHI with Medicare/"

# 如果输出目录不存在，尝试创建它
if(!dir.exists(Input)) {
  dir.create(Input, recursive = TRUE)
}
if(!dir.exists(Output)) {
  dir.create(Output, recursive = TRUE)
}

message("正在读取数据，请稍候...")

### 任务 B. 读取数据

# 建议使用 zap_formats 移除 Stata 特有的格式属性，避免后续报错
raw_cohort <- read_dta("MedicareGIsurgerycohort.dta")

### 任务 C. [关键检查] 查看数据结构
# 这一步是为了确认您的地理标识符变量名（通常是 bene_zip 或 fips_code）
print(head(raw_cohort))

# 如果您想查看 Stata 的变量标签（即 Variable Label，解释每个变量是什么）
#以此对照 Table 1 中的变量含义
var_label(raw_cohort)

# 2. 数据预处理
# ------------------------------------------------

### 任务 A: 提取变量标签 (制作数据字典)
# Stata 的标签存储在列的属性中，我们需要把它们“抠”出来变成一张表
var_dictionary <- tibble(
  var_name = names(raw_cohort),
  var_label = map_chr(raw_cohort, ~ attr(.x, "label") %||% "No Label") # 如果没有标签则填 No Label
)

# 输出为 CSV 文件
write_csv(var_dictionary, paste0(Input, "Variable_Labels_Dictionary.csv"))
message("✅ [输出 1/3] 变量标签字典已保存为 'Variable_Labels_Dictionary.csv'")

### 任务 B: CSV 输出 
# 注意：Stata 数据里有很多分类变量是数字（1, 0），但标签是（Yes, No）
# 我们生成两个版本：
# 1. raw版本：保留数字代码（适合做数据分析和匹配）。这个版本保留了原始数值 (例如 Sex=1, Race=1)，适合用于后续的数据分析和软件导入。
# 2. decoded版本：将数字转为文字（适合肉眼查看）。这个版本将数值代码转换为可读的文字 (例如 Sex="Male")。

## 将 raw_cohort 输出为 CSV(数值型)
# 输出 Raw 版本
write_csv(raw_cohort, paste0(Input, "Medicare_Cohort_Raw.csv"))
message("✅ [输出 2/3] Raw 数据已导出为 'Medicare_Cohort_Raw.csv'")

## 导出 Decoded 版本 CSV (修复报错版)
# 修复逻辑：使用 where(is.labelled) 精确识别 Stata 标签列，避开日期列报错
cohort_decoded <- raw_cohort %>% 
  mutate(across(where(is.labelled), as_factor))

write_csv(cohort_decoded, paste0(Input, "Medicare_Cohort_Decoded.csv"))
message("✅ [输出 3/3] Decoded 数据已导出为 'Medicare_Cohort_Decoded.csv'")

# 2. 生成预览数据 (诊断关键步骤)
# ------------------------------------------------
# 为了编写下一步的 UHI 匹配代码，我们需要确认您的地理和时间变量名。
# 请运行以下函数，并将控制台 (Console) 的输出结果复制给分析师。

raw_cohort <- read_csv("/users/PAS2978/wongzs/1. Pawlik/UHI/RInputData/Medicare_Cohort_Raw.csv")

preview_data_for_diagnosis <- function(df) {
  cat("\n===============================================\n")
  cat("           数据诊断报告 (Data Diagnosis)        \n")
  cat("===============================================\n")
  
  cat("1. 数据维度 (行 x 列):\n")
  print(dim(df))
  cat("\n")
  
  cat("2. 自动搜索潜在的地理/时间关键列:\n")
  # 关键词搜索：Zip Code, County, FIPS, Date, Admission, Year, Month, ID
  keywords <- "zip|bene|county|fips|state|date|adm|surg|year|month|time"
  target_cols <- grep(keywords, names(df), ignore.case = TRUE, value = TRUE)
  
  if(length(target_cols) > 0) {
    cat(paste("   找到相关变量:", paste(target_cols, collapse = ", "), "\n\n"))
    
    cat("3. 关键列前 5 行预览 (用于确认格式):\n")
    print(head(df[, target_cols], 5))
    
    cat("\n4. 关键列数据类型结构:\n")
    str(df[, target_cols])
  } else {
    cat("⚠️ 警告: 未自动识别出地理或时间变量。\n")
    cat("   请检查数据字典，并手动告知分析师代表 '邮编' 和 '手术日期' 的变量名。\n")
    
    cat("\n   显示前 10 列作为参考:\n")
    print(head(df[, 1:min(10, ncol(df))], 5))
  }
  cat("\n===============================================\n")
}

# 执行诊断函数
preview_data_for_diagnosis(raw_cohort)


# ==============================================================================
# 乙、 UHI数据处理
# ==============================================================================

# 1. 确定文件位置
# ------------------------------------------------
# 设置您的原始数据目录
data_dir <- "/users/PAS2978/wongzs/1. Pawlik/UHI/Rawdata/drive-download-20260116T024941Z-1-001/uhi_results_2017/"
setwd(data_dir)

# 2. 第一步：看看文件夹里有什么
# ------------------------------------------------
message("正在列出目录下的所有文件...")
all_files <- list.files(data_dir)
print(all_files)

# [交互点]：请在控制台(Console)查看输出的文件列表，找到您的 UHI 数据文件名。
# 假设您的 UHI 文件名包含 "UHI" 或 "Heat"，我们可以尝试自动搜索：
uhi_candidates <- grep("UHI|Heat|Temp", all_files, ignore.case = TRUE, value = TRUE)

if(length(uhi_candidates) > 0) {
  message(paste("\n推测可能是 UHI 数据的文件:", paste(uhi_candidates, collapse = ", ")))
} else {
  message("\n⚠️ 未自动找到带 'UHI' 字样的文件，请您手动确认文件名。")
}

# 3. 第二步：读取 UHI 数据 (请根据实际文件名修改)
# ------------------------------------------------

# ⚠️⚠️⚠️ 请在此处修改为您真实的 UHI 文件名 ⚠️⚠️⚠️
# 例如: uhi_filename <- "US_UHI_2017_2021_Zipcode.csv"
uhi_filename <- "county_uhi_2017_part97.csv" 

# 如果您还不知道文件名，请先运行上面代码看列表，
# 然后取消下面对应格式的注释来读取数据：

# [情况 A: 如果是 CSV 文件]
uhi_data <- read_csv(uhi_filename)

# [情况 B: 如果是 Excel 文件]
# uhi_data <- read_excel(uhi_filename)

# [情况 C: 如果是 Stata 文件]
# uhi_data <- haven::read_dta(uhi_filename)

# [情况 D: 如果是 RDS 文件]
# uhi_data <- readRDS(uhi_filename)


# 4. 第三步：诊断 UHI 数据结构
# ------------------------------------------------

# 如果成功读取了 uhi_data，请运行以下诊断代码：
if(exists("uhi_data")) {
  
  cat("\n===============================================\n")
  cat("           UHI 数据诊断报告 (Exposure Diagnosis)        \n")
  cat("===============================================\n")
  
  cat("1. 数据维度:\n")
  print(dim(uhi_data))
  
  cat("\n2. 前 5 行预览 (查看地理键值和暴露值):\n")
  print(head(uhi_data, 5))
  
  cat("\n3. 变量类型结构:\n")
  str(uhi_data)
  
  cat("\n4. 关键问题核查:\n")
  # 检查是否有 Zip Code (通常是 5 位)
  zip_cols <- grep("zip|zcta", names(uhi_data), ignore.case = TRUE, value = TRUE)
  if(length(zip_cols) > 0) {
    cat(paste("   ✅ 发现疑似 Zip Code 列:", paste(zip_cols, collapse = ", "), "\n"))
    cat("   -> 请检查它是否为字符型(chr)且保留了前导零？\n")
  } else {
    cat("   ⚠️ 未发现带有 'zip' 字样的列，请检查是否使用 FIPS 或 County Code。\n")
  }
  
  # 检查是否有年份/时间列 (Time-varying)
  time_cols <- grep("year|date|time", names(uhi_data), ignore.case = TRUE, value = TRUE)
  if(length(time_cols) > 0) {
    cat(paste("   ✅ 发现时间维度列:", paste(time_cols, collapse = ", "), "\n"))
    cat("   -> 这意味着我们需要进行【时空匹配】(按年/月匹配)。\n")
  } else {
    cat("   ℹ️ 未发现明显的时间列 -> 这可能是【横断面数据】(Cross-sectional)，即多年平均值。\n")
  }
  
} else {
  message("\n❌尚未读取数据。请先在代码中填入正确的文件名 (uhi_filename) 并运行读取命令。")
}




