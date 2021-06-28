#' Score 8-item Purpose in Life Questionnaire
#'
#' Calculate mean scores, tScores, and percentiles from raw Purpose in Life data. Input raw data from survey as a dataframe, and recieve a scored dataframe as output
#'
#' @param input_df dataframe
#' @param idVar string
#'
#' @return
#' @export
#'
#' @examples
#' score_purpose(survey_raw_data, "ResponseId")

load("data/purpose.rda")

score_purpose <- function(input_df, idVar){
  purpose_factored <- input_df %>%
    dplyr::select(idVar, dplyr::starts_with('purpose')) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with('purpose')),
              ~factor(., levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree",
                                    "Somewhat agree", "Strongly agree")))


  purpose_num <- purpose_factored %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with('purpose')),
              ~case_when(. == 'Strongly disagree' ~ 1,
                         . == 'Somewhat disagree' ~ 2,
                         . == 'Neither agree nor disagree' ~ 3,
                         . == 'Somewhat agree' ~ 4,
                         . == 'Strongly agree' ~ 5)) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with('purpose')),
              ~paste0(., '_num'))


  purpose_reversed <- purpose_num

  purpose_reversed$purpose_3r_num <- purpose_num$purpose_3_num*(-1)+6
  purpose_reversed$purpose_12r_num <- purpose_num$purpose_12_num*(-1)+6


  # Score Subscale from reveresed DF

  ## 8 Item
  purpose_scored <- purpose_reversed %>%
    dplyr::mutate(purpose_index = rowSums(dplyr::select(., purpose_1_num, purpose_2_num,
                                          purpose_4_num:purpose_7_num,
                                          purpose_11_num, purpose_12r_num), na.rm = T),
           purpose_mean = rowMeans(dplyr::select(., purpose_1_num, purpose_2_num,
                                          purpose_4_num:purpose_7_num,
                                          purpose_11_num, purpose_12r_num), na.rm = T))

  # purpose_scored <- purpose_reversed %>%
  #   dplyr::mutate(purpose_index = rowSums(dplyr::select(., purpose_1_num, purpose_3r_num, purpose_4_num:purpose_6_num, purpose_12r_num)),
  #          purpose_mean = rowMeans(dplyr::select(., purpose_1_num, purpose_3r_num,
  #                                         purpose_4_num:purpose_6_num, purpose_12r_num), na.rm = T))


  purpose_clean <- merge(purpose_factored, purpose_scored, by = idVar)


  Purpose_single_scoring <- function(val, table) {
    # print(val)
    val <- round(val, digits = 4)
    p <- with(table, setNames(percents, raw))[toString(val)]
    t <- with(table, setNames(tScores, raw))[toString(val)]

    return(c(p, t))
  }

  Purpose_per_t_scoring <- function(df){

    Purpose <- data.frame(Percent = NA, TScore = NA)


    for (row in seq(nrow(df))) {
      Purpose[row, c("Percent", "TScore")] <- Purpose_single_scoring(df$purpose_mean[row], purpose)
    }
    return(Purpose)
  }

  test <- Purpose_per_t_scoring(purpose_clean)

  purpose_clean2 <- cbind(purpose_clean, test) %>%
    dplyr::select(idVar, purpose_mean, purpose_index, Percent, TScore, everything())

  return(purpose_clean2)
}
