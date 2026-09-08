# ============================================================
#  미세먼지가 외국인 관광객 수·소비금액에 미치는 영향
#  회귀분석 (월별 패널, 2016.01 ~ 2025.06)
#
#  가설 1: 미세먼지는 외래관광객 수에 유의미한 영향을 미치며,
#          미세먼지를 포함할 때 예측력이 향상된다.
#  가설 2: 미세먼지는 외래관광객의 1인당 소비금액에
#          유의미한 영향을 미친다.
#
#  ─── 회귀식 ───────────────────────────────────────────────
#  H1: log(visitors_t) = β0 + β1·PM_t + β2·month_f
#                       + β3·trend + β4·covid + ε
#  H2: log(per_capita_t) = β0 + β1·PM_t + β2·month_f
#                         + β3·trend + β4·covid + ε
#
#  ─── 변수 정의 ────────────────────────────────────────────
#  visitors_t       : t월 방한 외래관광객 수 (명)
#  per_capita_t     : t월 1인당 관광수입 (US$) = 월간수입/방문객
#  PM_t             : t월 전국 미세먼지 월평균 농도 (μg/m³) ← 핵심
#  month_f          : 월 요인(factor) — 계절성 통제 (1~12월)
#  trend (year-2016): 선형 시간 추세 — 관광산업 성장 추세 통제
#  covid            : COVID 더미 (2020·2021=1, 나머지=0)
# ============================================================


# ── 0. 패키지 ─────────────────────────────────────────────────
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "lmtest", "car", "stargazer")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p, repos = "https://cran.rstudio.com/")
}
library(readxl); library(dplyr); library(tidyr)
library(ggplot2); library(lmtest); library(car); library(stargazer)


# ── 1. 파일 경로 ─────────────────────────────────────────────
# 이 .R 파일과 같은 폴더에 두 엑셀 파일을 위치시키세요
stat_file <- "연도별통계(1975-2025).xlsx"
pm_file   <- "미세먼지데이터2026.xlsx"


# ── 2. 방한 외래관광객 (월별) ────────────────────────────────
#  시트 구조: 행1-4 타이틀, 행5 한글헤더, 행6 영문헤더, 행7~ 데이터
visitors_raw <- read_excel(stat_file,
                            sheet     = "방한 외래관광객",
                            skip      = 6,
                            col_names = FALSE)

names(visitors_raw) <- c("year", "total", "growth", paste0("m", 1:12))

visitors_long <- visitors_raw %>%
  mutate(year = suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year >= 2016, year <= 2025) %>%
  select(year, m1:m12) %>%
  pivot_longer(m1:m12, names_to = "mon", values_to = "visitors") %>%
  mutate(month    = as.integer(sub("m", "", mon)),
         visitors = as.numeric(visitors)) %>%
  select(year, month, visitors)


# ── 3. 관광수입 (월별 revenue) ───────────────────────────────
#  시트 구조: 행1-4 타이틀/단위, 행5 헤더, 행6~ 데이터
#  열순서: year | total_rev | growth | per_capita_annual | m1…m12 | (dummy)
revenue_raw <- read_excel(stat_file,
                           sheet     = "관광수입",
                           skip      = 5,
                           col_names = FALSE)

names(revenue_raw)[1:16] <- c("year", "total_rev", "growth",
                               "per_capita_annual", paste0("m", 1:12))

revenue_long <- revenue_raw %>%
  mutate(year = suppressWarnings(as.integer(year))) %>%
  filter(!is.na(year), year >= 2016, year <= 2025) %>%
  select(year, per_capita_annual, m1:m12) %>%
  pivot_longer(m1:m12, names_to = "mon", values_to = "monthly_rev_1000usd") %>%
  mutate(month               = as.integer(sub("m", "", mon)),
         monthly_rev_1000usd = as.numeric(monthly_rev_1000usd),
         per_capita_annual   = as.numeric(per_capita_annual)) %>%
  select(year, month, monthly_rev_1000usd, per_capita_annual)


# ── 4. 미세먼지 (전국 총계 월평균 PM10) ──────────────────────
#  열이름 형식: "2016.01 월", "2016.02 월", …
pm_raw   <- read_excel(pm_file, sheet = "미세먼지 통계 데이터", col_names = TRUE)
pm_total <- pm_raw %>% filter(`구분` == "총계", `항목` == "월평균") %>%
            select(-`구분`, -`항목`)

pm_df <- data.frame(
  col_label = names(pm_total),
  pm        = as.numeric(pm_total[1, ]),
  stringsAsFactors = FALSE
) %>%
  mutate(
    year  = as.integer(sub("^(\\d{4})\\..*",        "\\1", col_label)),
    month = as.integer(sub("^\\d{4}\\.(\\d{2}).*",  "\\1", col_label))
  ) %>%
  filter(year >= 2016, year <= 2025) %>%
  select(year, month, pm)


# ── 5. 데이터 병합 ───────────────────────────────────────────
df <- visitors_long %>%
  left_join(revenue_long, by = c("year", "month")) %>%
  left_join(pm_df,        by = c("year", "month")) %>%
  filter(!is.na(pm),
         !is.na(visitors),         visitors > 0,
         !is.na(monthly_rev_1000usd), monthly_rev_1000usd > 0) %>%
  mutate(
    # 월별 1인당 소비 = (수입 US$1,000 × 1,000) ÷ 방문객 수
    per_capita     = (monthly_rev_1000usd * 1000) / visitors,

    # 통제 변수
    covid          = as.integer(year %in% c(2020, 2021)),
    year_trend     = year - 2016,
    month_f        = factor(month, levels = 1:12,
                            labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec")),

    # 로그 변환 (정규분포 근사)
    log_visitors   = log(visitors),
    log_per_capita = log(per_capita)
  )

cat("─── 병합 완료 ──────────────────────────────────────\n")
cat("관측치:", nrow(df), "| 기간:", min(df$year), "~", max(df$year), "\n")
cat("PM 범위:  ", round(min(df$pm),1), "~", round(max(df$pm),1), "μg/m³\n")
cat("방문객:   ", formatC(min(df$visitors), big.mark=","),
    "~", formatC(max(df$visitors), big.mark=","), "명\n")
cat("1인당소비:", round(min(df$per_capita),1),
    "~", round(max(df$per_capita),1), "US$\n\n")


# ── 6. 기초 통계 & 상관관계 ──────────────────────────────────
cat("=== 기초 통계량 ===\n")
print(summary(df[, c("visitors","per_capita","pm","year","month","covid")]))

cat("\n=== 주요 변수 상관관계 ===\n")
print(round(cor(df[, c("pm","visitors","per_capita","month","year_trend","covid")],
                use = "complete.obs"), 3))


# ── 7. 가설 1: log(외래관광객 수) ────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("  가설 1: 미세먼지 → 외래관광객 수\n")
cat("══════════════════════════════════════════════════\n")

h1_base <- lm(log_visitors ~ month_f + year_trend + covid, data = df)
h1_full <- lm(log_visitors ~ pm + month_f + year_trend + covid, data = df)

cat("\n[H1-Base] PM 미포함\n");  print(summary(h1_base))
cat("\n[H1-Full] PM 포함\n");    print(summary(h1_full))
cat("\n[F-검정: PM 추가 유의성]\n"); print(anova(h1_base, h1_full))
cat("AIC │ 기본:", round(AIC(h1_base),2), "│ PM포함:", round(AIC(h1_full),2), "\n")
b1 <- coef(h1_full)["pm"]
cat(sprintf("PM 1μg/m³ 증가 시 관광객 변화: %.4f → 약 %.3f%%\n\n", b1, (exp(b1)-1)*100))


# ── 8. 가설 2: log(1인당 소비금액) ───────────────────────────
cat("══════════════════════════════════════════════════\n")
cat("  가설 2: 미세먼지 → 1인당 소비금액\n")
cat("══════════════════════════════════════════════════\n")

h2_base <- lm(log_per_capita ~ month_f + year_trend + covid, data = df)
h2_full <- lm(log_per_capita ~ pm + month_f + year_trend + covid, data = df)

cat("\n[H2-Base] PM 미포함\n");  print(summary(h2_base))
cat("\n[H2-Full] PM 포함\n");    print(summary(h2_full))
cat("\n[F-검정: PM 추가 유의성]\n"); print(anova(h2_base, h2_full))
cat("AIC │ 기본:", round(AIC(h2_base),2), "│ PM포함:", round(AIC(h2_full),2), "\n")
b2 <- coef(h2_full)["pm"]
cat(sprintf("PM 1μg/m³ 증가 시 1인당소비 변화: %.4f → 약 %.3f%%\n\n", b2, (exp(b2)-1)*100))


# ── 9. 회귀 진단 ─────────────────────────────────────────────
cat("══════════════════════════════════════════════════\n")
cat("  회귀 진단\n")
cat("══════════════════════════════════════════════════\n")
for (tag in c("H1","H2")) {
  mdl <- if (tag == "H1") h1_full else h2_full
  cat(sprintf("\n[%s] Breusch-Pagan 이분산성 검정\n", tag)); print(bptest(mdl))
  cat(sprintf("[%s] Durbin-Watson 자기상관 검정\n",   tag)); print(dwtest(mdl))
  cat(sprintf("[%s] 다중공선성 VIF\n", tag)); print(vif(mdl))
}


# ── 10. 종합 결과 테이블 ─────────────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("  종합 결과 비교 테이블 (stargazer)\n")
cat("══════════════════════════════════════════════════\n")
stargazer(h1_base, h1_full, h2_base, h2_full,
          type           = "text",
          column.labels  = c("H1-기본","H1-PM포함","H2-기본","H2-PM포함"),
          dep.var.labels = c("log(외래관광객)", "log(1인당소비 US$)"),
          covariate.labels = c("미세먼지 PM (μg/m³)",
                               "시간추세 (year-2016)",
                               "COVID 더미 (2020·21)"),
          omit        = "month_f",
          omit.labels = "월 더미(계절성 통제)",
          add.lines   = list(c("월 더미 포함", "Yes","Yes","Yes","Yes")),
          no.space    = TRUE, digits = 4,
          star.cutoffs = c(0.1, 0.05, 0.01))


# ── 11. 시각화 ───────────────────────────────────────────────
theme_set(theme_bw(base_size = 12))

## 그래프 1: PM vs 외래관광객 수
p1 <- ggplot(df, aes(x = pm, y = visitors / 10000,
                     color = factor(covid), shape = factor(covid))) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = factor(covid)), method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = c("0"="#2166ac","1"="#d73027"),
                     labels = c("일반 기간(2016-19, 22-25)","COVID(2020-21)")) +
  scale_shape_manual(values = c("0"=16,"1"=17),
                     labels = c("일반 기간(2016-19, 22-25)","COVID(2020-21)")) +
  labs(title    = "미세먼지 농도와 외래관광객 수",
       subtitle = "가설 1 – 월별 데이터 (2016~2025)",
       x = "미세먼지 월평균 (μg/m³)", y = "외래관광객 수 (만 명)",
       color = NULL, shape = NULL) +
  theme(legend.position = "bottom")

## 그래프 2: PM vs 1인당 소비금액
p2 <- ggplot(df, aes(x = pm, y = per_capita,
                     color = factor(covid), shape = factor(covid))) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = factor(covid)), method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = c("0"="#2166ac","1"="#d73027"),
                     labels = c("일반 기간(2016-19, 22-25)","COVID(2020-21)")) +
  scale_shape_manual(values = c("0"=16,"1"=17),
                     labels = c("일반 기간(2016-19, 22-25)","COVID(2020-21)")) +
  labs(title    = "미세먼지 농도와 1인당 소비금액",
       subtitle = "가설 2 – 월별 데이터 (2016~2025)",
       x = "미세먼지 월평균 (μg/m³)", y = "1인당 소비금액 (US$)",
       color = NULL, shape = NULL) +
  theme(legend.position = "bottom")

## 그래프 3: 표준화 시계열 비교
p3 <- df %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = scale(pm)[,1],          color = "미세먼지"),       linewidth = 0.8) +
  geom_line(aes(y = scale(log_visitors)[,1], color = "외래관광객(log)"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = scale(log_per_capita)[,1], color = "1인당소비(log)"),linewidth = 0.8, linetype = "dotted") +
  scale_color_manual(values = c("미세먼지"="brown",
                                "외래관광객(log)"="steelblue",
                                "1인당소비(log)"="darkgreen")) +
  labs(title    = "미세먼지·외래관광객·1인당소비 표준화 추세 (2016~2025)",
       subtitle = "z-score 표준화로 동일 축 비교",
       x = "연월", y = "표준화 값 (z-score)", color = NULL) +
  theme(legend.position = "bottom")

## 진단 플롯 저장
png("진단플롯_H1.png", width = 1200, height = 900, res = 120)
par(mfrow = c(2, 2))
plot(h1_full, main = "H1 진단: log(외래관광객) ~ PM + 계절 + 추세 + COVID")
dev.off()

png("진단플롯_H2.png", width = 1200, height = 900, res = 120)
par(mfrow = c(2, 2))
plot(h2_full, main = "H2 진단: log(1인당소비) ~ PM + 계절 + 추세 + COVID")
dev.off()

ggsave("그래프1_PM_vs_관광객.png",   plot = p1, width = 7, height = 5, dpi = 150)
ggsave("그래프2_PM_vs_소비금액.png", plot = p2, width = 7, height = 5, dpi = 150)
ggsave("그래프3_시계열비교.png",     plot = p3, width = 9, height = 4, dpi = 150)

cat("\n✓ 완료! 저장된 파일:\n")
cat("  그래프1_PM_vs_관광객.png\n")
cat("  그래프2_PM_vs_소비금액.png\n")
cat("  그래프3_시계열비교.png\n")
cat("  진단플롯_H1.png / 진단플롯_H2.png\n")
