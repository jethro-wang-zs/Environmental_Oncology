# ==============================================================================
# 项目名称: UHI with Medicare GI Surgery Cohort
# 目的: 分析UHI与Medicare的直接相关性 - 数据清洗主流程
# 日期: 2026-01-15
# ==============================================================================

# 加载必要的非空间包
# 如果没有安装，请先运行 install.packages("haven") 和 install.packages("tidyverse")
library(haven)     # 专门用于读取 Stata/SAS/SPSS 数据
library(tidyverse) # 数据清洗全家桶 (包含 dplyr, stringr 等)
library(labelled)

# ==============================================================================
# 甲、 路径设置与数据读取
# ==============================================================================

# 1. 路径设置
# ------------------------------------------------
# 设置工作目录 (读取数据的位置)
# 注意：请确保此路径在您的服务器上存在
setwd("/users/PAS2978/wongzs/1. Pawlik/UHI/0. Rawdata")

# 设置输出目录 (结果保存的位置)
Input <- "/users/PAS2978/wongzs/1. Pawlik/UHI/0. RInputData/"
Output <- "/users/PAS2978/wongzs/1. Pawlik/UHI/1. 260115_UHI with Medicare/"

# 如果输出目录不存在，尝试创建它
if(!dir.exists(Input)) {
  dir.create(Input, recursive = TRUE)
}
if(!dir.exists(Output)) {
  dir.create(Output, recursive = TRUE)
}

message("正在读取数据，请稍候...")

# 2. 读取数据
# ------------------------------------------------
raw_cohort <- read_csv("/users/PAS2978/wongzs/1. Pawlik/UHI/RInputData/Medicare_Cohort_Raw.csv")


# ==============================================================================
# 乙、 数据预处理
# ==============================================================================

# 1. 任务 A: Medicare 关键变量清洗 (Adjust Medicare Rawdata)
# ------------------------------------------------
# 目的：将原始数据转换为适合与 UHI 匹配的格式
# 策略调整：根据 UHI 诊断，我们优先使用 FIPS (County) 进行匹配

message("正在执行 Task D: Medicare 关键变量清洗...")

clean_cohort <- raw_cohort %>%
  mutate(
    # --- 1. 地理编码清洗 (关键步骤) ---
    # 原始 FIPS 是数值型 (12086)，需要转为字符型并补齐 5 位 (01001) 以匹配 UHI
    fips_clean = str_pad(as.character(FIPS), width = 5, side = "left", pad = "0"),
    
    # 保留 Zip Code 备用 (如果未来需要更细粒度)
    zip5 = substr(as.character(MLOCZIP), 1, 5),
    
    # --- 2. 时间变量清洗 ---
    surgery_date = as.Date(INDEX_CLM_ADMSN_DT),
    
    # 提取年份用于匹配 UHI 的 year 列
    surg_year = as.numeric(format(surgery_date, "%Y")),
    surg_month = as.numeric(format(surgery_date, "%m")),
    
    # 定义暖季 (Warm Season: 5月-9月) - 用于后续分层分析
    is_warm_season = if_else(surg_month >= 5 & surg_month <= 9, 1, 0)
  ) %>%
  # 过滤掉 FIPS 缺失的行 (无法匹配环境数据)
  filter(!is.na(fips_clean) & fips_clean != "NA")

message("✅ Medicare 数据清洗完成！")
print(clean_cohort %>% select(FIPS, fips_clean, surg_year) %>% head())

# 2. 任务 B: [重大更新] 批量读取并清洗 UHI 数据 (Batch Process)
# ------------------------------------------------
# 目的：自动遍历 uhi_results_20xx 文件夹，读取所有 split parts 并合并

message("正在执行 Task F: 批量读取 UHI 数据文件...")

# I. 获取所有 CSV 文件列表 (递归搜索子文件夹)
# pattern = "county_uhi" 确保只读取相关文件
# recursive = TRUE 确保能钻进 uhi_results_2017 等子文件夹里找
setwd("/users/PAS2978/wongzs/1. Pawlik/UHI/Rawdata/drive-download-20260116T024941Z-1-001")
uhi_files <- list.files(path = getwd(), 
                        pattern = "county_uhi.*\\.csv$", 
                        recursive = TRUE, 
                        full.names = TRUE)

if(length(uhi_files) > 0) {
  message(paste0("✅ 找到 ", length(uhi_files), " 个 UHI 数据文件。开始合并..."))
  
  # II. 批量读取并堆叠 (purrr::map_dfr 是神器)
  # [关键修复] 添加 col_types 参数，强制 GEOID 为字符型
  # .default = "?" 表示其他列让 R 自己猜
  uhi_raw_combined <- map_dfr(uhi_files, ~ suppressMessages(
    read_csv(.x, col_types = cols(GEOID = col_character(), .default = "?"))
  ))
  
  message("数据读取完毕，正在进行清洗和去重...")
  
  # III. 清洗 UHI 数据
  uhi_clean <- uhi_raw_combined %>%
    mutate(
      # 处理地理键值: GEOID (num) -> fips_clean (char, 5位)
      fips_clean = str_pad(as.character(GEOID), width = 5, side = "left", pad = "0"),
      
      # 处理时间键值
      year = as.numeric(year)
    ) %>%
    # 只保留关键变量
    select(
      fips_clean, 
      year, 
      uhi_c,      # 核心暴露: 热岛强度
      t_rural_c,  # 农村基准温度
      t_urban_c   # 城市温度
    ) %>%
    # [关键步骤] 去重
    # GEE 导出的 split files 可能在切片边缘有重叠，或者文件夹里有重复下载的文件
    distinct(fips_clean, year, .keep_all = TRUE)
  
  message(paste0("✅ UHI 数据清洗完成！总共有 ", nrow(uhi_clean), " 条县-年记录。"))
  
  # 3. 任务 C: 执行时空合并 (Spatio-Temporal Merge)
  # ------------------------------------------------ 
  # 逻辑: Left Join (保留所有病人)，匹配键为 [FIPS] + [Year]

  final_dataset <- clean_cohort %>%
    left_join(uhi_clean, by = c("fips_clean" = "fips_clean", "surg_year" = "year"))
  
  # 4. 任务 D: 合并质量检查 (QC)
  # ------------------------------------------------
  
  # 计算匹配成功率
  n_total <- nrow(final_dataset)
  n_missing <- sum(is.na(final_dataset$uhi_c))
  match_rate <- round((1 - n_missing/n_total) * 100, 2)
  
  message("\n===============================================")
  message(paste0("合并完成！总样本量: ", n_total))
  message(paste0("UHI 数据缺失数: ", n_missing))
  message(paste0("匹配成功率: ", match_rate, "%"))
  message("===============================================\n")
  
  if(match_rate < 90) {
    message("⚠️ 警告: 匹配率低于 90%。可能原因：")
    message("1. UHI 数据年份范围 (2017-2021) 与手术年份不完全重合")
    message("2. 某些县的 FIPS 代码变更或数据缺失")
  }
  
  # 5. 任务 E: 保存最终分析数据集
  # ------------------------------------------------
  saveRDS(final_dataset, paste0(Output, "Medicare_UHI_Merged_Analysis_Set.rds"))
  write_csv(final_dataset, paste0(Output, "Medicare_UHI_Merged_Analysis_Set.csv"))
  message("✅ 最终数据集已保存！文件名为: Medicare_UHI_Merged_Analysis_Set.rds")
  
} else {
  message("❌ 错误: 未找到任何文件名包含 'county_uhi' 的 CSV 文件。")
  message("请检查 getwd() 路径下是否存在 'uhi_results_xxxx' 文件夹。")
}
  





