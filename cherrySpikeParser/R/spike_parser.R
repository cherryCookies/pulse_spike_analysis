if (!require("hash")) install.packages("hash")
library(hash)
if (!require("qmrparser")) install.packages("qmrparser")
library(qmrparser)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
#' @title Parse spike data
#' @description A function to parse spike data
#' @return A data frame
#' @export
parser_df <- function(pulse_f) {
  case_sample_hash <- hash()
  sample_second_hash <- hash()
  second_milli_hash <- hash()
  lines = readLines(pulse_f)
  df = data.frame()
  for (line in lines[2:length(lines)]){
    line_split = strsplit(line,',')
    if (length(line_split[[1]])<3){
      next
    }
    if (line_split[[1]][length(line_split[[1]])-2] == ''){
      next
    }
    TIME = line_split[[1]][length(line_split[[1]])-2]
    ELECTRODE = line_split[[1]][length(line_split[[1]])-1]
    AMPLITUDE = as.numeric(line_split[[1]][length(line_split[[1]])])
    first_charater_lte = strsplit(TIME,"")[[1]][1]
    if (!isDigit(first_charater_lte)){
      next
    }
    TIME = as.numeric(TIME)
    TIME_SEC = as.integer(TIME)
    case = strsplit(ELECTRODE,'_')[[1]][1]
    sample = as.integer(strsplit(ELECTRODE,'_')[[1]][2])
    num_row = nrow(df) + 1
    df[num_row,1] = case
    df[num_row,2] = sample
    df[num_row,3] = TIME_SEC
    df[num_row,4] = TIME
    df[num_row,5] = AMPLITUDE
  }
  colnames(df) <- c("case","electrode","seconds","millies","amplitude")
  return(df)
}
filter_df <- function(parsed_df) {
  parsed_df <- parsed_df %>% mutate(
    case = trimws(case),
    electrode = trimws(electrode),
    seconds = as.integer(seconds)
  )
  df_3plus <- parsed_df %>%
    group_by(case, electrode, seconds) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 4) %>%
    dplyr::select(case, electrode) %>%
    distinct()

  # 2) 원본 데이터에서 위에서 추출된 (case, electrode, seconds)만 제거
  df_filtered <- parsed_df %>%
    anti_join(df_3plus, by = c("case","electrode"))

  df_1only <- parsed_df %>%
    group_by(case, electrode, seconds) %>%
    summarise(n=n(), .groups="drop") %>%
    filter(n == 1) %>%
    distinct()

  df_filtered <- df_filtered %>%
    anti_join(df_1only, by=c("case","electrode","seconds"))
  return(df_filtered)
}
format_df <- function(df,prefix){
  df_wide <- df %>%
    group_by(case, electrode, seconds) %>%
    arrange(millies, .by_group = TRUE) %>%
    mutate(timepoint = paste0("자극시점", row_number())) %>%
    pivot_wider(
      names_from = timepoint,
      values_from = c(millies, amplitude),
      names_glue = paste0(prefix,"_","{.value}_{timepoint}")
    )
  new_order <- c(1, 2, 3)
  max_row_num <- as.integer((ncol(df_wide) - 3) / 2)

  for (i in seq_len(max_row_num)){
    new_order <- c(new_order, 3 + i, 3 + i + max_row_num)
  }
  #new_order <- c(1, 2, 3, 4, 8, 5, 9, 6, 10, 7, 11)
  df_wide <- df_wide[, new_order]
  return(df_wide)
}
merge_df <- function(filtered_df_pulse1, filtered_df_pulse2){

  output_df = data.frame()

  pulse1_transform <- transform(filtered_df_pulse1, caseElectrode = paste0(case,"_",electrode))
  pulse2_transform <- transform(filtered_df_pulse2, caseElectrode = paste0(case,"_",electrode))

  unique_caseElectrode <- unique(sort(c(pulse1_transform$caseElectrode, pulse2_transform$caseElectrode)))

  for (each_caseElectrode in unique_caseElectrode){
    each_pulse1 <- pulse1_transform[pulse1_transform$caseElectrode==each_caseElectrode,]
    each_pulse2 <- pulse2_transform[pulse2_transform$caseElectrode==each_caseElectrode,]

    nrow_pulse1 = nrow(each_pulse1)
    nrow_pulse2 = nrow(each_pulse2)

    if (nrow_pulse1 < nrow_pulse2) {
      NA_df <- data.frame(matrix(NA, nrow = nrow_pulse2 - nrow_pulse1, ncol = ncol(each_pulse1)))
      colnames(NA_df) <- colnames(each_pulse1)
      each_pulse1 <- rbind(each_pulse1,NA_df)
      #names(each_pulse1) <- names(pulse1_transform)
    } else if (nrow_pulse2 < nrow_pulse1) {
      NA_df <- data.frame(matrix(NA, nrow = nrow_pulse1 - nrow_pulse2, ncol = ncol(each_pulse2)))
      colnames(NA_df) <- colnames(each_pulse2)
      each_pulse2 <- rbind(each_pulse2,NA_df)
      #names(each_pulse2) <- names(pulse2_transform)
    }
    combined_df <- cbind(each_pulse1, each_pulse2)
    output_df <- rbind(output_df,combined_df)
  }
  return(output_df)
}
#args = commandArgs(trailingOnly=TRUE)
#pulsef_1 = args[1]
#pulsef_2 = args[2]
#result_prefix = args[3]
#pulse_df_1 = parser_df(pulsef_1)
#pulse_df_2 = parser_df(pulsef_2)
#filter_df_1 = filter_df(pulse_df_1)
#filter_df_2 = filter_df(pulse_df_2)
#formatted_df_1 = format_df(filter_df_1,"pulse1")
#formatted_df_2 = format_df(filter_df_2,"pulse2")
#merged_df = merge_df(formatted_df_1,formatted_df_2)
#write_csv(merged_df,paste0(result_prefix,".csv"))
