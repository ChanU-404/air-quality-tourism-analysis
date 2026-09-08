# ============================================================
#  [ARIMAX 분석] 미세먼지가 외국인 관광객 수에 미치는 영향
#  교수님 피드백 반영: 예측력 + 설명력 동시 검정
#
#  ── 이 코드가 하는 일 ──────────────────────────────────────
#  1. [설명력] SARIMAX 모델에서 미세먼지 계수의 유의성 검정
#              → t검정 / Wald 검정
#              → "미세먼지가 관광객 수에 정말 영향을 주는가?"
#
#  2. [예측력] SARIMA vs SARIMAX의 실제 예측 성능 비교
#              → RMSE, MAE, Diebold-Mariano 검정
#              → "미세먼지를 넣으면 예측이 더 정확해지는가?"
#
#  ── 모델 구조 ─────────────────────────────────────────────
#  SARIMA  : log(visitors_t) 의 과거 패턴 + COVID 더미
#  SARIMAX : log(visitors_t) 의 과거 패턴 + COVID 더미 + PM_t
#
#  ── 데이터 분할 ───────────────────────────────────────────
#  훈련(train) : 2016.01 ~ 2023.12  (96개월) → 모델 학습
#  테스트(test): 2024.01 ~ 2025.06  (18개월) → 예측 성능 평가
# ============================================================


# ── 0. 패키지 설치 및 로드 ───────────────────────────────────
pkgs <- c("readxl","dplyr","tidyr","forecast","tseries","lmtest","ggplot2")
for (p in pkgs) {
  if (!requireNamespace(p, quietly=TRUE))
    install.packages(p, repos="https://cran.rstudio.com/")
}
library(readxl); library(dplyr); library(tidyr)
library(forecast)   # auto.arima(), forecast(), dm.test()
library(tseries)    # adf.test() - 정상성 검정
library(lmtest)     # coeftest() - 계수 유의성 검정
library(ggplot2)


# ── 1. 이전 전처리 코드 (df 생성) ────────────────────────────
#  ★ 이 블록은 "미세먼지_관광객_회귀분석.R" 섹션 0~5 와 동일합니다.
#     이미 df 객체가 있다면 이 블록은 건너뛰어도 됩니다.
stat_file <- "연도별통계(1975-2025).xlsx"
pm_file   <- "미세먼지데이터2026.xlsx"

visitors_raw <- read_excel(stat_file, sheet="방한 외래관광객",
                            skip=6, col_names=FALSE)
names(visitors_raw) <- c("year","total","growth", paste0("m",1:12))
visitors_long <- visitors_raw %>%
  mutate(year=suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year>=2016, year<=2025) %>%
  select(year, m1:m12) %>%
  pivot_longer(m1:m12, names_to="mon", values_to="visitors") %>%
  mutate(month=as.integer(sub("m","",mon)), visitors=as.numeric(visitors)) %>%
  select(year, month, visitors)

revenue_raw <- read_excel(stat_file, sheet="관광수입",
                           skip=5, col_names=FALSE)
names(revenue_raw)[1:16] <- c("year","total_rev","growth","per_capita_annual",
                               paste0("m",1:12))
revenue_long <- revenue_raw %>%
  mutate(year=suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year>=2016, year<=2025) %>%
  select(year, per_capita_annual, m1:m12) %>%
  pivot_longer(m1:m12, names_to="mon", values_to="monthly_rev_1000usd") %>%
  mutate(month=as.integer(sub("m","",mon)),
         monthly_rev_1000usd=as.numeric(monthly_rev_1000usd),
         per_capita_annual=as.numeric(per_capita_annual)) %>%
  select(year, month, monthly_rev_1000usd, per_capita_annual)

pm_raw   <- read_excel(pm_file, sheet="미세먼지 통계 데이터", col_names=TRUE)
pm_total <- pm_raw %>% filter(`구분`=="총계", `항목`=="월평균") %>%
            select(-`구분`,-`항목`)
pm_df <- data.frame(col_label=names(pm_total),
                    pm=as.numeric(pm_total[1,]),
                    stringsAsFactors=FALSE) %>%
  mutate(year =as.integer(sub("^(\\d{4})\\..*","\\1",col_label)),
         month=as.integer(sub("^\\d{4}\\.(\\d{2}).*","\\1",col_label))) %>%
  filter(year>=2016, year<=2025) %>% select(year, month, pm)

df <- visitors_long %>%
  left_join(revenue_long, by=c("year","month")) %>%
  left_join(pm_df,        by=c("year","month")) %>%
  filter(!is.na(pm), !is.na(visitors), visitors>0,
         !is.na(monthly_rev_1000usd), monthly_rev_1000usd>0) %>%
  mutate(per_capita    = (monthly_rev_1000usd*1000)/visitors,
         covid         = as.integer(year %in% c(2020,2021)),
         year_trend    = year - 2016,
         log_visitors  = log(visitors),
         log_per_capita= log(per_capita))


# ── 2. 시계열(ts) 객체 생성 ──────────────────────────────────
# ts()는 "시간 순서가 있는 데이터"임을 R에게 알려주는 함수
# start=c(2016,1) → 2016년 1월부터 시작
# frequency=12    → 1년에 12번 측정(월별)
ts_y     <- ts(df$log_visitors, start=c(2016,1), frequency=12)
ts_pm    <- ts(df$pm,           start=c(2016,1), frequency=12)
ts_covid <- ts(df$covid,        start=c(2016,1), frequency=12)


# ── 3. 정상성 검정 (ADF Test) ────────────────────────────────
# ARIMA는 "정상성(stationarity)" 가정이 필요합니다.
# 정상성 = 시간이 지나도 평균·분산이 크게 변하지 않는 성질
# ADF 검정: p < 0.05 이면 "정상 시계열"로 판단
cat("══════════════════════════════════════════\n")
cat("  STEP 1: 정상성 검정 (ADF Test)\n")
cat("══════════════════════════════════════════\n")
cat("  [로그 관광객 수]\n")
print(adf.test(ts_y))
# p-값이 0.05 미만이면 정상 시계열 → ARIMA(d=0) 가능
# p-값이 0.05 이상이면 차분 필요 → auto.arima()가 자동으로 d 결정


# ── 4. 훈련 / 테스트 분할 ────────────────────────────────────
# 훈련(train): 모델이 패턴을 배우는 데이터 (2016~2023)
# 테스트(test): 모델이 얼마나 잘 예측하는지 채점하는 데이터 (2024~2025)
# → 교수님이 말씀하신 "out-of-sample 예측 성능 비교"가 바로 이것!

train_y     <- window(ts_y,     end=c(2023,12))
test_y      <- window(ts_y,     start=c(2024,1))
train_pm    <- window(ts_pm,    end=c(2023,12))
test_pm     <- window(ts_pm,    start=c(2024,1))
train_covid <- window(ts_covid, end=c(2023,12))
test_covid  <- window(ts_covid, start=c(2024,1))

# xreg: 외생변수 행렬 (모델에게 추가 정보를 줄 때 사용)
train_xreg_base <- matrix(train_covid, ncol=1, dimnames=list(NULL,"covid"))
test_xreg_base  <- matrix(test_covid,  ncol=1, dimnames=list(NULL,"covid"))

train_xreg_full <- cbind(covid=as.numeric(train_covid), pm=as.numeric(train_pm))
test_xreg_full  <- cbind(covid=as.numeric(test_covid),  pm=as.numeric(test_pm))

n_test <- length(test_y)
cat(sprintf("\n훈련 기간: 2016.01~2023.12 (%d개월)\n", length(train_y)))
cat(sprintf("테스트 기간: 2024.01~2025.06 (%d개월)\n\n", n_test))


# ── 5. 모델 적합 ─────────────────────────────────────────────
# auto.arima(): 수많은 ARIMA(p,d,q)(P,D,Q) 조합 중 가장 좋은 것을 자동 선택
# xreg      : 외생변수(exogenous variable) - 시계열 밖에서 들어오는 추가 정보
# seasonal=TRUE : 계절성 ARIMA (SARIMA) - 매년 반복되는 패턴 포착
# stepwise=FALSE, approximation=FALSE : 더 꼼꼼히 최적 모델 탐색 (시간이 조금 걸림)

cat("══════════════════════════════════════════\n")
cat("  STEP 2: 모델 적합\n")
cat("══════════════════════════════════════════\n")

cat("▶ SARIMA 모델 적합 중... (COVID만 통제, 미세먼지 없음)\n")
sarima_base <- auto.arima(train_y,
                           xreg         = train_xreg_base,
                           seasonal     = TRUE,
                           stepwise     = FALSE,
                           approximation= FALSE,
                           trace        = FALSE)

cat("▶ SARIMAX 모델 적합 중... (COVID + 미세먼지 포함)\n")
sarimax_full <- auto.arima(train_y,
                            xreg         = train_xreg_full,
                            seasonal     = TRUE,
                            stepwise     = FALSE,
                            approximation= FALSE,
                            trace        = FALSE)

cat("\n[SARIMA 모델 구조]\n"); print(sarima_base)
cat("\n[SARIMAX 모델 구조]\n"); print(sarimax_full)


# ── 6. [설명력 검정] 미세먼지 계수 유의성 ────────────────────
# 교수님 말씀: "ARIMAX에서 외생변수 회귀계수에 대해 t-test나 Wald test"
#
# 원리: 미세먼지 계수(β)가 0이면 → 미세먼지는 관광객 수에 영향 없음
#       미세먼지 계수(β)가 0이 아니면 → 미세먼지는 영향이 있음
# → t통계량 = 계수 / 표준오차  → |t| > 1.96 이면 5% 유의수준에서 유의

cat("\n══════════════════════════════════════════\n")
cat("  STEP 3: [설명력] 미세먼지 계수 유의성 검정\n")
cat("══════════════════════════════════════════\n")

# 방법 1: 직접 계산 (t통계량 / p-값)
cat("\n[방법 1] t통계량 직접 계산\n")
all_coefs <- coef(sarimax_full)          # 모델의 모든 계수 추출
all_ses   <- sqrt(diag(vcov(sarimax_full))) # 모든 계수의 표준오차 추출

pm_idx  <- which(names(all_coefs) == "pm")  # pm 계수 위치 찾기
pm_coef <- all_coefs[pm_idx]    # pm 계수값
pm_se   <- all_ses[pm_idx]      # pm 표준오차
pm_z    <- pm_coef / pm_se      # z통계량(=t통계량) = 계수/표준오차
pm_p    <- 2 * (1 - pnorm(abs(pm_z)))  # 양측 p-값

cat(sprintf("  PM 계수     : %8.5f\n",  pm_coef))
cat(sprintf("  표준오차    : %8.5f\n",  pm_se))
cat(sprintf("  z통계량     : %8.4f\n",  pm_z))
cat(sprintf("  p-값        : %8.4f\n",  pm_p))
if (pm_p < 0.05) {
  cat("  결론: 미세먼지 계수가 5% 유의수준에서 유의 ★\n")
} else if (pm_p < 0.10) {
  cat("  결론: 미세먼지 계수가 10% 유의수준에서 유의 (경계)\n")
} else {
  cat("  결론: 미세먼지 계수가 통계적으로 유의하지 않음\n")
}

# 방법 2: coeftest()를 이용한 Wald 검정 (모든 계수 한꺼번에)
cat("\n[방법 2] Wald 검정 (coeftest)\n")
cat("  → 모든 계수의 유의성을 한눈에 확인\n\n")
print(coeftest(sarimax_full))

# AIC 비교 (모델 적합도)
cat("\n[AIC 비교: 훈련 데이터 적합도]\n")
cat(sprintf("  SARIMA  AIC : %.2f\n", AIC(sarima_base)))
cat(sprintf("  SARIMAX AIC : %.2f\n", AIC(sarimax_full)))
cat(sprintf("  차이(ΔAIC)  : %.2f  ", AIC(sarima_base) - AIC(sarimax_full)))
if (AIC(sarimax_full) < AIC(sarima_base)) {
  cat("→ SARIMAX가 더 좋은 적합도 (미세먼지 추가 효과 있음)\n")
} else {
  cat("→ SARIMA가 더 좋은 적합도 (미세먼지 추가 효과 없음)\n")
}


# ── 7. [예측력 검정] Out-of-sample 예측 성능 비교 ─────────────
# 교수님 말씀: "ARIMA와 ARIMAX를 적합한 후 예측 성능을 비교"
# → 2024~2025년 실제 관광객 수와 예측값을 비교

cat("\n══════════════════════════════════════════\n")
cat("  STEP 4: [예측력] Out-of-sample 예측 성능\n")
cat("══════════════════════════════════════════\n")

# h=n_test: 테스트 기간 길이만큼 예측
fc_base <- forecast(sarima_base,  xreg=test_xreg_base, h=n_test)
fc_full <- forecast(sarimax_full, xreg=test_xreg_full, h=n_test)

actual    <- as.numeric(test_y)
pred_base <- as.numeric(fc_base$mean)
pred_full <- as.numeric(fc_full$mean)

# 성능 지표 계산 함수
rmse_fn <- function(a, p) sqrt(mean((a-p)^2))  # 평균제곱근오차 (작을수록 좋음)
mae_fn  <- function(a, p) mean(abs(a-p))        # 평균절대오차  (작을수록 좋음)
mape_fn <- function(a, p) mean(abs((a-p)/a))*100 # 평균절대백분율오차 (%)

cat(sprintf("\n%-25s %8s %8s %8s\n", "모델", "RMSE", "MAE", "MAPE(%)"))
cat(rep("-",52), "\n", sep="")
cat(sprintf("%-25s %8.4f %8.4f %8.2f\n", "SARIMA (COVID만)",
            rmse_fn(actual,pred_base), mae_fn(actual,pred_base), mape_fn(actual,pred_base)))
cat(sprintf("%-25s %8.4f %8.4f %8.2f\n", "SARIMAX (COVID+PM)",
            rmse_fn(actual,pred_full), mae_fn(actual,pred_full), mape_fn(actual,pred_full)))

rmse_base <- rmse_fn(actual,pred_base)
rmse_full <- rmse_fn(actual,pred_full)
if (rmse_full < rmse_base) {
  cat(sprintf("\n→ SARIMAX가 RMSE 기준 %.1f%% 더 정확\n",
              (rmse_base-rmse_full)/rmse_base*100))
} else {
  cat(sprintf("\n→ SARIMA가 RMSE 기준 %.1f%% 더 정확\n",
              (rmse_full-rmse_base)/rmse_full*100))
}

# Diebold-Mariano 검정: 두 모델의 예측 성능 차이가 우연인지 검정
# H0: 두 모델의 예측 정확도가 동일하다
# p < 0.05 이면 → 한 모델이 통계적으로 유의미하게 더 정확함
cat("\n[Diebold-Mariano 검정: 예측력 차이의 유의성]\n")
cat("  H0: 두 모델의 예측 정확도가 같다\n")
e_base <- actual - pred_base  # SARIMA 오차
e_full <- actual - pred_full  # SARIMAX 오차
dm_result <- dm.test(e_base, e_full, alternative="two.sided", h=1, power=2)
print(dm_result)
if (dm_result$p.value < 0.05) {
  cat("  결론: 두 모델의 예측 정확도가 통계적으로 유의미하게 다름\n")
} else {
  cat("  결론: 두 모델의 예측 정확도가 통계적으로 유의미하게 다르지 않음\n")
}


# ── 8. 잔차 진단 ─────────────────────────────────────────────
# 잔차(residual) = 실제값 - 예측값
# 좋은 모델은 잔차에 패턴이 없어야 함 (백색잡음)
cat("\n══════════════════════════════════════════\n")
cat("  STEP 5: 잔차 진단\n")
cat("══════════════════════════════════════════\n")

cat("\n[SARIMA 잔차 진단]\n")
checkresiduals(sarima_base)  # Ljung-Box 검정 포함

cat("\n[SARIMAX 잔차 진단]\n")
checkresiduals(sarimax_full)


# ── 9. 시각화 ────────────────────────────────────────────────
theme_set(theme_bw(base_size=12))

## 그래프 1: 훈련/테스트 기간 예측값 vs 실제값 비교
date_test <- seq(as.Date("2024-01-01"), by="month", length.out=n_test)

plot_df <- data.frame(
  date      = date_test,
  actual    = exp(actual),      # log 역변환 → 원래 관광객 수
  pred_base = exp(pred_base),
  pred_full = exp(pred_full),
  lower_base= exp(as.numeric(fc_base$lower[,2])),
  upper_base= exp(as.numeric(fc_base$upper[,2])),
  lower_full= exp(as.numeric(fc_full$lower[,2])),
  upper_full= exp(as.numeric(fc_full$upper[,2]))
)

p_forecast <- ggplot(plot_df, aes(x=date)) +
  # 신뢰구간 (95%)
  geom_ribbon(aes(ymin=lower_base/10000, ymax=upper_base/10000),
              fill="steelblue", alpha=0.15) +
  geom_ribbon(aes(ymin=lower_full/10000, ymax=upper_full/10000),
              fill="tomato", alpha=0.15) +
  # 예측값
  geom_line(aes(y=pred_base/10000, color="SARIMA (COVID만)"), linewidth=1, linetype="dashed") +
  geom_line(aes(y=pred_full/10000, color="SARIMAX (COVID+PM)"), linewidth=1, linetype="dotted") +
  # 실제값
  geom_line(aes(y=actual/10000, color="실제값"), linewidth=1.2) +
  geom_point(aes(y=actual/10000, color="실제값"), size=2) +
  scale_color_manual(values=c("실제값"="black",
                               "SARIMA (COVID만)"="steelblue",
                               "SARIMAX (COVID+PM)"="tomato")) +
  labs(title   ="SARIMA vs SARIMAX 예측 비교 (2024~2025)",
       subtitle="음영: 95% 신뢰구간  |  점선: 예측값  |  실선: 실제값",
       x="연월", y="외래관광객 수 (만 명)", color=NULL) +
  theme(legend.position="bottom")

## 그래프 2: 전체 시계열 + 예측 구간
all_dates <- seq(as.Date("2016-01-01"), by="month", length.out=length(ts_y))
train_dates <- all_dates[1:length(train_y)]
test_dates  <- all_dates[(length(train_y)+1):length(ts_y)]

p_full_ts <- data.frame(
  date  = all_dates,
  value = exp(as.numeric(ts_y))/10000,
  type  = c(rep("훈련 데이터", length(train_y)), rep("테스트 데이터", n_test))
) %>%
  ggplot(aes(x=date, y=value, color=type)) +
  geom_line(linewidth=1) +
  geom_vline(xintercept=as.numeric(as.Date("2024-01-01")),
             linetype="dashed", color="gray40") +
  annotate("text", x=as.Date("2024-01-01"), y=Inf,
           label=" ← 예측 시작", hjust=0, vjust=1.5, size=3.5, color="gray40") +
  scale_color_manual(values=c("훈련 데이터"="steelblue","테스트 데이터"="tomato")) +
  labs(title="외래관광객 수 시계열 전체 (2016~2025)",
       x="연월", y="외래관광객 수 (만 명)", color=NULL) +
  theme(legend.position="bottom")

## 그래프 3: PM 계수 효과 시각화 (전체 모델 계수 비교)
coef_df <- data.frame(
  variable = names(all_coefs),
  estimate = as.numeric(all_coefs),
  se       = as.numeric(all_ses)
) %>% filter(!grepl("^ar|^ma|^sar|^sma|intercept", variable, ignore.case=TRUE))

p_coef <- ggplot(coef_df, aes(x=reorder(variable, estimate), y=estimate)) +
  geom_col(aes(fill=estimate > 0), width=0.5) +
  geom_errorbar(aes(ymin=estimate-1.96*se, ymax=estimate+1.96*se), width=0.2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_manual(values=c("TRUE"="steelblue","FALSE"="tomato"),
                    labels=c("TRUE"="양(+)","FALSE"="음(-)")) +
  coord_flip() +
  labs(title="SARIMAX 외생변수 계수 (95% 신뢰구간)",
       x=NULL, y="계수값", fill="방향") +
  theme(legend.position="right")

ggsave("ARIMAX_그래프1_예측비교.png",   plot=p_forecast, width=8, height=5, dpi=150)
ggsave("ARIMAX_그래프2_전체시계열.png", plot=p_full_ts,  width=9, height=4, dpi=150)
ggsave("ARIMAX_그래프3_계수비교.png",   plot=p_coef,     width=6, height=4, dpi=150)

# 잔차 진단 플롯 저장
png("ARIMAX_잔차진단_SARIMA.png",  width=1200, height=600, res=120)
checkresiduals(sarima_base,  plot=TRUE)
dev.off()

png("ARIMAX_잔차진단_SARIMAX.png", width=1200, height=600, res=120)
checkresiduals(sarimax_full, plot=TRUE)
dev.off()

cat("\n══════════════════════════════════════════\n")
cat("  분석 완료! 저장된 파일:\n")
cat("  ARIMAX_그래프1_예측비교.png\n")
cat("  ARIMAX_그래프2_전체시계열.png\n")
cat("  ARIMAX_그래프3_계수비교.png\n")
cat("  ARIMAX_잔차진단_SARIMA.png\n")
cat("  ARIMAX_잔차진단_SARIMAX.png\n")
cat("══════════════════════════════════════════\n")
