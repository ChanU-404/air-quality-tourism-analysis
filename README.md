# 대기질과 관광객 시계열 분석

대기오염과 외국인 관광객의 관계를 회귀, 시계열 및 ARIMAX 모형으로 분석하는 R/R Markdown 프로젝트입니다.

## 사용 방법

R 및 RStudio를 준비하고 필요한 패키지를 설치하세요.

```r
install.packages(c("GGally", "Metrics", "car", "cluster", "corrplot", "dplyr", "factoextra", "fastDummies", "forecast", "ggplot2", "lmtest", "lubridate", "plm", "psych", "readr", "readxl", "reshape2", "scales", "showtext", "stargazer", "tidyr", "tidyverse", "tseries", "xgboost", "zoo"))
```

각 스크립트의 입력 파일 경로와 작업 디렉터리를 로컬 데이터 위치에 맞춰 수정한 뒤 실행하세요. R Markdown 문서는 `rmarkdown` 패키지와 해당 출력 형식의 도구가 필요합니다.

## 코드 목록

- `Final_project(임시) (1).R`
- `tourist_airquality_analysis.Rmd`
- `Final 6-14_최종.Rmd`
- `Final_project_수정_상관관계포함버전.Rmd`
- `EDA_correlation_distribution.Rmd`
- `Untitled.R`
- `미세먼지_관광객_회귀분석.R`
- `미세먼지_ARIMAX_분석.R`

## 로컬 자료

수업 설문 등 원본 데이터, 보고서, 화면 캡처와 R 세션 기록은 로컬에 보관하고 코드 백업에서는 제외했습니다. 원본 데이터가 있어야 분석을 재현할 수 있습니다. 강의 코드의 기존 저작자 표시는 유지합니다. 분석 실행 검증은 수행하지 않았습니다.
