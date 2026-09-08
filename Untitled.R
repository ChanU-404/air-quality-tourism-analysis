# ==========================================
# 1. 필수 패키지 로드 및 경로 설정
# ==========================================
library(tidyverse)
library(lubridate)
library(plm)         # 패널 데이터 분석을 위한 계량경제 패키지
library(xgboost)     # 머신러닝
library(Metrics)
library(fastDummies) # 더미 변수(One-Hot Encoding) 생성

# 📌 본인의 작업 경로로 변경해 주세요
BASE_PATH <- "/Users/chanu/Desktop/다변량분석플젝/"

# ==========================================
# 2. 패널 데이터(Panel Data) 로드 및 병합
# ==========================================
cat("데이터를 로드하고 패널 구조로 변환합니다...\n")

df_purpose <- read_csv(paste0(BASE_PATH, "Enter_korea_by_purpose.csv"))
df_pollution <- read_csv(paste0(BASE_PATH, "south-korean-pollution-data.csv"))
df_weather <- read_csv(paste0(BASE_PATH, "south_korean_weather.csv"))

# (1) 미세먼지 월평균
df_poll_monthly <- df_pollution %>%
  mutate(date_clean = as.Date(date)) %>%
  filter(date_clean >= "2019-01-01") %>%
  mutate(year_month = format(date_clean, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarize(pm25_mean = mean(pm25, na.rm = TRUE))

# (2) 기상 월평균 (전처리 강화)
df_weather_monthly <- df_weather %>%
  mutate(
    date_clean = ymd_hms(DATE),
    year_month = format(date_clean, "%Y-%m"),
    TEMP_clean = gsub("\\+", "", TEMP),
    TEMP_clean = gsub(",", ".", TEMP_clean),
    TEMP_num = as.numeric(TEMP_clean) / 10
  ) %>%
  group_by(year_month) %>%
  summarize(temp_mean = mean(TEMP_num, na.rm = TRUE))

# 🌟 (3) 관광객 패널 데이터 전처리 (국가별 차원 유지!) 🌟
panel_ts_data <- df_purpose %>%
  mutate(
    date_clean = ymd(paste0(date, "-01")),
    year_month = format(date_clean, "%Y-%m")
  ) %>%
  # 기존처럼 퉁치지 않고, year_month와 'nation'을 기준으로 그룹화
  group_by(year_month, nation) %>%
  summarize(tourism = sum(tourism, na.rm = TRUE), .groups = "drop") %>%
  inner_join(df_poll_monthly, by = "year_month") %>%
  inner_join(df_weather_monthly, by = "year_month") %>%
  arrange(nation, year_month) %>%
  drop_na()

cat("✅ 패널 데이터 구축 완료! 총 샘플 수(N):", nrow(panel_ts_data), "개\n\n")

# ==========================================
# 3. 모델 1: 패널 고정효과 모델 (Fixed Effects Model)
# ==========================================
# SARIMAX 대신 국가별 고유 특성(Fixed Effects)을 통제하여 p-value를 구합니다.
cat("📊 [통계 분석] 패널 고정효과 모델 학습 중...\n")

# 데이터를 패널 데이터프레임 형식으로 변환 (인덱스: 국가, 시간)
pdata <- pdata.frame(panel_ts_data, index = c("nation", "year_month"))

# 고정효과(within) 회귀 모델 적합
# 국가별 베이스라인(중국은 원래 많이 오고, 노르웨이는 원래 적게 옴)을 통제한 순수 환경 효과 추출
fixed_model <- plm(tourism ~ pm25_mean + temp_mean, data = pdata, model = "within")

print(summary(fixed_model))

# ==========================================
# 4. 모델 2: 다변량 머신러닝 (XGBoost) - Train/Test 분할
# ==========================================
cat("\n🤖 [머신러닝] XGBoost 패널 예측 학습 중...\n")

# 머신러닝을 위해 국가(nation) 컬럼을 더미 변수(0과 1)로 변환
ml_data <- dummy_cols(panel_ts_data, select_columns = "nation", remove_first_dummy = FALSE, remove_selected_columns = TRUE)

# Train / Test 분할 (마지막 6개월인 2019-11 ~ 2020-04를 Test로 설정)
test_months <- c("2019-11", "2019-12", "2020-01", "2020-02", "2020-03", "2020-04")

train_df <- ml_data %>% filter(!year_month %in% test_months)
test_df  <- ml_data %>% filter(year_month %in% test_months)

# 독립변수(X)와 종속변수(Y) 분리 (year_month 제거)
X_train <- as.matrix(train_df %>% select(-year_month, -tourism))
Y_train <- train_df$tourism
X_test  <- as.matrix(test_df %>% select(-year_month, -tourism))
Y_test  <- test_df$tourism

# XGBoost 모델 훈련 (데이터가 500개 이상으로 늘어났으므로 딥러닝/ML이 힘을 냅니다)
xgb_model <- xgboost(
  data = X_train, 
  label = Y_train, 
  max_depth = 5, 
  eta = 0.1, 
  nrounds = 100, 
  objective = "reg:squarederror",
  verbose = 0
)

# 예측 및 오차(RMSE) 계산
xgb_preds <- predict(xgb_model, X_test)
xgb_rmse <- rmse(Y_test, xgb_preds)
xgb_mae <- mae(Y_test, xgb_preds)

cat("📈 XGBoost (패널 데이터) RMSE:", round(xgb_rmse, 2), "| MAE:", round(xgb_mae, 2), "\n")

# ==========================================
# 5. 파이썬(TimeXer)용 데이터 추출
# ==========================================
write_csv(panel_ts_data, paste0(BASE_PATH, "extended_panel_data.csv"))
cat("\n💾 파이썬 딥러닝용 패널 데이터가 저장되었습니다: extended_panel_data.csv\n")

