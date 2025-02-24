## 사용법

스파이크 파서를 사용하려면 다음 R 스크립트를 실행하세요:

```r
args = commandArgs(trailingOnly=TRUE)
pulsef_1 = args[1]
pulsef_2 = args[2]
result_prefix = args[3]
pulse_df_1 = parser_df(pulsef_1)
pulse_df_2 = parser_df(pulsef_2)
filter_df_1 = filter_df(pulse_df_1)
filter_df_2 = filter_df(pulse_df_2)
formatted_df_1 = format_df(filter_df_1,"pulse1")
formatted_df_2 = format_df(filter_df_2,"pulse2")
merged_df = merge_df(formatted_df_1,formatted_df_2)
write_csv(merged_df,paste0(result_prefix,".csv"))
```

### 인수

- `pulsef_1`: 첫 번째 펄스 데이터 파일의 경로.
- `pulsef_2`: 두 번째 펄스 데이터 파일의 경로.
- `result_prefix`: 결과 CSV 파일의 접두사.

### 예제

```sh
Rscript spike_parser.R pulse_data1.csv pulse_data2.csv result
```

이 명령은 병합되고 형식화된 스파이크 데이터가 포함된 `result.csv` 파일을 생성합니다.

### 윈도우 사용자를 위한 R Studio 사용법

1. R Studio를 엽니다.
2. `File` 메뉴에서 `Open File...`을 선택하고 `spike_parser.R` 파일을 엽니다.
3. 스크립트 상단에 다음과 같이 인수를 설정합니다:

    ```r
    args = c("pulse_data1.csv", "pulse_data2.csv", "result")
    ```

4. 스크립트의 나머지 부분을 실행합니다.
5. 작업 디렉토리에 `result.csv` 파일이 생성됩니다.