library(readxl); library(dplyr); library(lubridate)
library(forecast); library(tseries)

setwd("C:/Users/jmg90/OneDrive/바탕 화면/26-1학기 파일/다변량분석/다변량_프로젝트")
tour_path <- "연도별통계(1975-2025).xlsx"
dust_path <- "월별_미세먼지.xls"

# ══════════════════════════════════════════════════════════════════════════════
# Part A. 데이터 로딩
# ══════════════════════════════════════════════════════════════════════════════
parse_pm10 <- function(path) {
  con <- file(path, open = "r", encoding = "CP949")
  lines <- readLines(con, warn = FALSE); close(con)
  txt <- paste(lines, collapse = "\n")
  row_blocks <- strsplit(txt, "<Row\\b", perl = TRUE)[[1]][-1]
  cell_texts <- function(b) {
    m <- gregexpr("(?s)(?<=<Data[^>]{0,200}>).*?(?=</Data>)", b, perl = TRUE)
    trimws(regmatches(b, m)[[1]])
  }
  hdr_i <- which(grepl("2016\\.01", row_blocks))[1]
  hdr  <- cell_texts(row_blocks[hdr_i])
  tot  <- cell_texts(row_blocks[hdr_i + 1])
  is_m <- grepl("^\\d{4}\\.\\d{2}", hdr)
  dates <- as.Date(paste0(sub("\\.", "-", sub("\\s.*", "", hdr[is_m])), "-01"))
  vals  <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", tot[is_m])))
  n <- min(length(dates), length(vals))
  data.frame(date = dates[seq_len(n)], pm10 = vals[seq_len(n)]) |>
    filter(!is.na(pm10)) |> arrange(date)
}

reshape_monthly <- function(path, sheet, month_cols) {
  df <- read_excel(path, sheet = sheet, col_names = FALSE)
  out <- list()
  for (i in seq_len(nrow(df))) {
    yr <- suppressWarnings(as.integer(df[[1]][i]))
    if (is.na(yr) || yr < 1975 || yr > 2025) next
    for (k in seq_along(month_cols)) {
      val <- suppressWarnings(as.numeric(df[[month_cols[k]]][i]))
      out[[length(out)+1]] <- data.frame(
        date = as.Date(sprintf("%d-%02d-01", yr, k)), value = val)
    }
  }
  bind_rows(out) |> arrange(date)
}

pm10_df     <- parse_pm10(dust_path)
arrivals_df <- reshape_monthly(tour_path, "방한 외래관광객", 4:15)
receipts_df <- reshape_monthly(tour_path, "관광수입",       5:16)

merged <- arrivals_df |> rename(arrivals = value) |>
  inner_join(receipts_df |> rename(receipts = value), by = "date") |>
  inner_join(pm10_df, by = "date") |>
  filter(!is.na(arrivals), !is.na(receipts), !is.na(pm10)) |>
  arrange(date)

cat(sprintf("분석 기간: %s ~ %s  (%d개월)\n",
            min(merged$date), max(merged$date), nrow(merged)))

# ══════════════════════════════════════════════════════════════════════════════
# [수정] Train / Test Split  (80% train, 20% test)
# ══════════════════════════════════════════════════════════════════════════════
n_total   <- nrow(merged)
n_train   <- floor(n_total * 0.8)
n_test    <- n_total - n_train

merged_train <- merged[seq_len(n_train), ]
merged_test  <- merged[seq(n_train + 1, n_total), ]

cat(sprintf("Train: %s ~ %s  (%d개월)\n",
            min(merged_train$date), max(merged_train$date), n_train))
cat(sprintf("Test : %s ~ %s  (%d개월)\n",
            min(merged_test$date),  max(merged_test$date),  n_test))

# [수정] mk_ts, ts 객체, dates_plt 모두 merged_train 기준으로 변경
start_yr <- year(min(merged_train$date)); start_mo <- month(min(merged_train$date))
mk_ts <- function(x, df = merged_train) {
  sy <- year(min(df$date)); sm <- month(min(df$date))
  ts(x, start = c(sy, sm), frequency = 12)
}

# [수정] Train 시계열 (merged -> merged_train)
y_arr_ts <- mk_ts(merged_train$arrivals)
y_rec_ts <- mk_ts(merged_train$receipts)
pm10_ts  <- mk_ts(merged_train$pm10)

# [수정] Test 벡터 추가 (예측 평가용)
y_arr_test <- merged_test$arrivals
y_rec_test <- merged_test$receipts
pm10_test  <- merged_test$pm10

# [수정] 날짜 벡터 3종 (train / test / 전체)
dates_plt       <- merged_train$date
dates_plt_test  <- merged_test$date
dates_plt_all   <- merged$date

# ══════════════════════════════════════════════════════════════════════════════
# Part A-2. 정상성 검정 — ADF Test
# ══════════════════════════════════════════════════════════════════════════════
# H0: 단위근 존재 (비정상) / H1: 정상
# p < 0.05 → H0 기각 → 정상

run_adf <- function(x, name) {
  # 1) 원시계열
  a0 <- adf.test(x, alternative = "stationary")
  # 2) 1차 차분
  a1 <- adf.test(diff(x), alternative = "stationary")
  # 3) 계절 차분 (lag 12)
  a12 <- adf.test(diff(x, lag = 12), alternative = "stationary")
  # 4) 계절 + 1차 차분
  a12_1 <- adf.test(diff(diff(x, lag = 12)), alternative = "stationary")
  
  out <- data.frame(
    시리즈 = c(name,
            paste0(name, " 1차차분"),
            paste0(name, " 계절차분(lag12)"),
            paste0(name, " 계절+1차차분")),
    ADF_통계량 = round(c(a0$statistic, a1$statistic,
                      a12$statistic, a12_1$statistic), 3),
    p값        = round(c(a0$p.value,   a1$p.value,
                        a12$p.value,  a12_1$p.value), 4),
    lag        = c(a0$parameter, a1$parameter,
                   a12$parameter, a12_1$parameter),
    정상성     = ifelse(c(a0$p.value, a1$p.value,
                       a12$p.value, a12_1$p.value) < 0.05,
                     "정상 (H0 기각)", "비정상 (H0 채택"),
    stringsAsFactors = FALSE
  )
  out
}

# [수정] 문구: "Train 기준" 추가
cat("\n========== ADF 정상성 검정 (Train 기준) ==========\n")
adf_arr <- run_adf(y_arr_ts, "arrivals")
adf_rec <- run_adf(y_rec_ts, "receipts")
adf_all <- rbind(adf_arr, adf_rec)
print(adf_all, row.names = FALSE)

write.csv(adf_all, "adf_results.csv", row.names = FALSE)
cat("adf_results.csv 저장 완료\n")

# ══════════════════════════════════════════════════════════════════════════════
# Part A-3. 시계열 분해 — decompose()
# ══════════════════════════════════════════════════════════════════════════════
for (nm in c("arrivals", "receipts")) {
  y   <- if (nm == "arrivals") y_arr_ts else y_rec_ts
  dec <- decompose(y, type = "multiplicative")
  
  png(sprintf("decompose_%s.png", nm), width = 1200, height = 900, res = 110)
  plot(dec, xlab = "")
  mtext(sprintf("%s — multiplicative 분해", nm),
        side = 3, line = -1.5, outer = TRUE, cex = 1.1, font = 2)
  dev.off()
  
  cat(sprintf("\n===== %s decompose 요약 =====\n", nm))
  cat(sprintf("  추세 범위: %.1f ~ %.1f\n",
              min(dec$trend, na.rm = TRUE), max(dec$trend, na.rm = TRUE)))
  cat("  계절 지수 (1월~12월):\n")
  seasonal_idx <- dec$figure; names(seasonal_idx) <- month.abb
  print(round(seasonal_idx, 4))
  cat(sprintf("  불규칙 성분 sd: %.4f\n", sd(dec$random, na.rm = TRUE)))
}
cat("decompose_arrivals.png, decompose_receipts.png 저장 완료\n")

# ══════════════════════════════════════════════════════════════════════════════
# Part B. EDA — ACF / PACF
# ══════════════════════════════════════════════════════════════════════════════
png("acf_pacf.png", width = 1400, height = 1400, res = 110)
par(mfrow = c(4, 2), mar = c(3, 4, 3, 1))
for (nm in c("arrivals", "receipts")) {
  y  <- if (nm == "arrivals") y_arr_ts else y_rec_ts
  dy <- diff(y)
  acf (y,  lag.max = 36, main = paste0(nm, ": 원시계열 ACF"))
  pacf(y,  lag.max = 36, main = paste0(nm, ": 원시계열 PACF"))
  acf (dy, lag.max = 36, main = paste0(nm, ": 1차차분 ACF"))
  pacf(dy, lag.max = 36, main = paste0(nm, ": 1차차분 PACF"))
}
dev.off()
cat("acf_pacf.png 저장 완료\n")

# ══════════════════════════════════════════════════════════════════════════════
# Part C. 모델 적합 — AR / MA / ARIMA / SARIMA (단변량) + SARIMAX (PM10 외생)
# ══════════════════════════════════════════════════════════════════════════════
fit_all <- function(y, xreg = NULL) {
  cat("  AR 적합 중...\n")
  m_ar <- auto.arima(y, max.p = 5, max.q = 0, max.P = 0, max.Q = 0,
                     d = 0, D = 0, seasonal = FALSE,
                     ic = "aic", stepwise = FALSE, approximation = FALSE)
  cat("  MA 적합 중...\n")
  m_ma <- auto.arima(y, max.p = 0, max.q = 5, max.P = 0, max.Q = 0,
                     d = 0, D = 0, seasonal = FALSE,
                     ic = "aic", stepwise = FALSE, approximation = FALSE)
  cat("  ARIMA 적합 중...\n")
  m_arima <- auto.arima(y, max.P = 0, max.Q = 0, D = 0, seasonal = FALSE,
                        ic = "aic", stepwise = FALSE, approximation = FALSE)
  cat("  SARIMA 적합 중...\n")
  m_sarima <- auto.arima(y, seasonal = TRUE,
                         ic = "aic", stepwise = FALSE, approximation = FALSE)
  cat("  SARIMAX 적합 중...\n")
  m_sarimax <- auto.arima(y, xreg = xreg, seasonal = TRUE,
                          ic = "aic", stepwise = FALSE, approximation = FALSE)
  list(AR = m_ar, MA = m_ma, ARIMA = m_arima,
       SARIMA = m_sarima, SARIMAX = m_sarimax)
}

# [수정] xreg: merged$pm10 -> merged_train$pm10
xreg_pm10 <- matrix(merged_train$pm10, ncol = 1); colnames(xreg_pm10) <- "pm10"
# [수정] test용 외생변수 행렬 추가
xreg_pm10_test <- matrix(pm10_test, ncol = 1); colnames(xreg_pm10_test) <- "pm10"

cat("\n[arrivals 모델 적합]\n")
models_arr <- fit_all(y_arr_ts, xreg = xreg_pm10)
cat("\n[receipts 모델 적합]\n")
models_rec <- fit_all(y_rec_ts, xreg = xreg_pm10)

# ══════════════════════════════════════════════════════════════════════════════
# [수정] summarize_models: y_test, xreg_test 인자 추가 → Test_RMSE / Test_MAE 계산
# ══════════════════════════════════════════════════════════════════════════════
summarize_models <- function(models, label, y_test, xreg_test = NULL) {
  rows <- lapply(names(models), function(k) {
    m  <- models[[k]]
    r  <- as.numeric(residuals(m))
    ord <- arimaorder(m)
    lb  <- tryCatch(Box.test(r, lag = 12, type = "Ljung-Box"),
                    error = function(e) list(p.value = NA))
    jb  <- tryCatch(jarque.bera.test(r),
                    error = function(e) list(p.value = NA))
    ord_str <- if (length(ord) >= 7)
      sprintf("(%d,%d,%d)(%d,%d,%d)[%d]",
              ord[1],ord[2],ord[3],ord[4],ord[5],ord[6],ord[7])
    else sprintf("(%d,%d,%d)", ord[1], ord[2], ord[3])
    
    # [수정] Out-of-sample 예측 및 지표 계산
    h <- length(y_test)
    fc <- tryCatch({
      if (k == "SARIMAX") forecast(m, h = h, xreg = xreg_test)$mean
      else                 forecast(m, h = h)$mean
    }, error = function(e) rep(NA, h))
    
    test_rmse <- round(sqrt(mean((y_test - as.numeric(fc))^2, na.rm = TRUE)), 2)
    test_mae  <- round(mean(abs(y_test - as.numeric(fc)), na.rm = TRUE), 2)
    
    # [수정] 컬럼명: RMSE/MSE/MAE -> Train_RMSE/Train_MAE/Test_RMSE/Test_MAE
    data.frame(
      model       = k,  order = ord_str,
      Train_RMSE  = round(sqrt(mean(r^2)), 2),
      Train_MAE   = round(mean(abs(r)), 2),
      Test_RMSE   = test_rmse,
      Test_MAE    = test_mae,
      AIC         = round(AIC(m), 2),
      BIC         = round(BIC(m), 2),
      LB12_p      = round(lb$p.value, 3),
      JB_p        = round(jb$p.value, 4),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  # [수정] 헤더 문구: "Train | Test" 추가
  cat(sprintf("\n========== %s — 모델 비교 (Train | Test) ==========\n", label))
  print(out, row.names = FALSE)
  invisible(out)
}

# [수정] y_test, xreg_test 인자 전달
cmp_arr <- summarize_models(models_arr, "arrivals (관광객수, 명)",
                            y_test = y_arr_test, xreg_test = xreg_pm10_test)
cmp_rec <- summarize_models(models_rec, "receipts (관광소비금액)",
                            y_test = y_rec_test, xreg_test = xreg_pm10_test)

# ══════════════════════════════════════════════════════════════════════════════
# Part D. 잔차 진단 플롯 (원본 스케일)
# ══════════════════════════════════════════════════════════════════════════════
plot_residuals <- function(models, y_ts, label, filename) {
  y_vec <- as.numeric(y_ts)
  png(filename, width = 1500, height = 1800, res = 110)
  par(mfrow = c(5, 3), mar = c(3, 4, 3, 1))
  for (k in names(models)) {
    m    <- models[[k]]
    r    <- as.numeric(residuals(m))
    yhat <- as.numeric(fitted(m))
    # 날짜 맞추기 (차분으로 앞 행 소실 시)
    n_r  <- length(r)
    d_r  <- tail(dates_plt, n_r)
    
    # 잔차 시계열
    plot(d_r, r, type = "l", col = "#c0392b", lwd = 1,
         main = sprintf("[%s] %s — 잔차", label, k),
         xlab = "", ylab = "잔차")
    abline(h = 0, lwd = .5)
    
    # ACF
    acf(r, lag.max = 24, main = sprintf("%s — ACF", k))
    
    # Q-Q
    qqnorm(r, main = sprintf("%s — Q-Q  (MAE=%.1f)", k, mean(abs(r))))
    qqline(r, col = 2)
  }
  dev.off()
}

plot_residuals(models_arr, y_arr_ts, "arrivals", "resid_arrivals.png")
plot_residuals(models_rec, y_rec_ts, "receipts", "resid_receipts.png")
cat("resid_arrivals.png, resid_receipts.png 저장 완료\n")

# ══════════════════════════════════════════════════════════════════════════════
# [수정] plot_fitted: y_test, dates_test, xreg_test 인자 추가
#         → train 적합 + test 예측(보라색) + 95% CI 음영 + 경계선 시각화
# ══════════════════════════════════════════════════════════════════════════════
plot_fitted <- function(models, y_ts, label, filename,
                        y_test, dates_test, xreg_test = NULL) {
  y_vec <- as.numeric(y_ts)
  png(filename, width = 1500, height = 1200, res = 110)
  par(mfrow = c(3, 2), mar = c(3, 4, 3, 1))
  h <- length(y_test)
  for (k in names(models)) {
    m    <- models[[k]]
    yhat <- as.numeric(fitted(m))
    n_f  <- length(yhat)
    d_f  <- tail(dates_plt, n_f)
    yv   <- tail(y_vec, n_f)
    
    fc <- tryCatch({
      if (k == "SARIMAX") forecast(m, h = h, xreg = xreg_test)
      else                 forecast(m, h = h)
    }, error = function(e) NULL)
    
    train_rmse <- round(sqrt(mean((yv - yhat)^2)), 2)
    test_rmse  <- if (!is.null(fc))
      round(sqrt(mean((y_test - as.numeric(fc$mean))^2, na.rm = TRUE)), 2) else NA
    
    ylim <- range(c(yv, yhat, y_test,
                    if (!is.null(fc)) as.numeric(fc$mean) else NULL), na.rm = TRUE)
    
    plot(d_f, yv, type = "l", col = "#2471a3", lwd = 1.4,
         xlim = range(c(d_f, dates_test)),
         ylim = ylim,
         main = sprintf("[%s] %s  Train RMSE=%.1f | Test RMSE=%.1f",
                        label, k, train_rmse, test_rmse),
         xlab = "", ylab = label)
    lines(d_f, yhat, col = "#e67e22", lwd = 1.4, lty = 2)
    lines(dates_test, y_test, col = "#2471a3", lwd = 1.4, lty = 3)
    if (!is.null(fc)) {
      lines(dates_test, as.numeric(fc$mean), col = "#8e44ad", lwd = 1.4, lty = 2)
      polygon(c(dates_test, rev(dates_test)),
              c(as.numeric(fc$lower[,2]), rev(as.numeric(fc$upper[,2]))),
              col = adjustcolor("#8e44ad", 0.12), border = NA)
    }
    abline(v = max(d_f), col = "gray40", lty = 3, lwd = 1.2)
    text(max(d_f), ylim[2], " train|test", cex = 0.7, col = "gray40", adj = 0)
    legend("topleft",
           c("Train 실제", "Train 적합", "Test 실제", "Test 예측"),
           col = c("#2471a3","#e67e22","#2471a3","#8e44ad"),
           lty = c(1,2,3,2), cex = 0.75, bty = "n")
  }
  dev.off()
}

# [수정] y_test, dates_test, xreg_test 인자 전달
plot_fitted(models_arr, y_arr_ts, "arrivals", "fitted_arrivals.png",
            y_test = y_arr_test, dates_test = dates_plt_test,
            xreg_test = xreg_pm10_test)
plot_fitted(models_rec, y_rec_ts, "receipts", "fitted_receipts.png",
            y_test = y_rec_test, dates_test = dates_plt_test,
            xreg_test = xreg_pm10_test)
cat("fitted_arrivals.png, fitted_receipts.png 저장 완료\n")

# ══════════════════════════════════════════════════════════════════════════════
# Part E. 최적 모델 잔차 → Step 3 군집화 입력
# ══════════════════════════════════════════════════════════════════════════════
# [수정] 최적 모델 선정 기준: RMSE -> Test_RMSE
best_arr <- cmp_arr$model[which.min(cmp_arr$Test_RMSE)]
best_rec <- cmp_rec$model[which.min(cmp_rec$Test_RMSE)]
cat(sprintf("\n최저 Test RMSE: arrivals=%s, receipts=%s\n", best_arr, best_rec))

r_arr <- as.numeric(residuals(models_arr[[best_arr]]))
r_rec <- as.numeric(residuals(models_rec[[best_rec]]))
n_out <- min(length(r_arr), length(r_rec))

resid_df <- data.frame(
  date           = tail(merged$date, n_out),
  resid_arrivals = tail(r_arr, n_out),
  resid_receipts = tail(r_rec, n_out),
  pm10           = tail(merged$pm10, n_out)
)
write.csv(resid_df,  "residuals_for_clustering.csv", row.names = FALSE)
write.csv(cmp_arr, "model_comparison_arrivals.csv", row.names = FALSE)
write.csv(cmp_rec, "model_comparison_receipts.csv", row.names = FALSE)
cat("저장 완료: residuals_for_clustering.csv /",
    "model_comparison_arrivals.csv / model_comparison_receipts.csv\n")