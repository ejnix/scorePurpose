#' Score Purpose in Life Questionnaire
#'
#' Calculate mean scores, tScores, and percentiles from raw Purpose in Life data. Input raw data from survey as a data frame, and receive a scored data frame as output
#'
#' @param input_df data frame
#' @param form string
#' @param missing_threshold double
#' @param tscore logical
#'
#' @return data frame
#' @export
#' @examples
#' score_purpose(test_data)
score_purpose <- function(input_df, form = 'full', missing_threshold = .5, tscore = F){

  # # Add unique identifier for merging
  input_df <- dplyr::mutate(input_df, unique_id_for_merging = dplyr::row_number())


  # Housekeeping
  upper_prefix = F
  verbose_prefix = F
  abrv_prefix = F
  standard_prefix = F

  # converts Purpose_prefix to lowercase if needed
  if(any(grepl('Purpose', names(input_df)))){
    df <- dplyr::rename_at(input_df, dplyr::vars(dplyr::starts_with('Purpose')),
                           ~tolower(.))
    upper_prefix = T

  }
  if (any(grepl('purpose_in_life', names(input_df)))){ # abbreviates purpose_in_life prefix if necessary
    df <- dplyr::rename_at(input_df, dplyr::vars(dplyr::starts_with('purpose_in_life')),
                           ~gsub('_in_life', '', .))
    verbose_prefix = T

  }
  if (any(grepl('pil', names(input_df)))){   # extends pil prefix if necessary
    df <- dplyr::rename_at(input_df, dplyr::vars(dplyr::starts_with('pil')),
                           ~gsub('pil', 'purpose', .))
    abrv_prefix = T

  }
  if (any(grepl('purpose', names(input_df)))){ # checks if standard prefix is used
    df <- input_df
    standard_prefix = T
  }

  if(sum(upper_prefix, verbose_prefix, abrv_prefix, standard_prefix) > 1){
    stop("Prefix of Purpose in Life data does not match an expected format: purpose, Purpose, purpose_in_life, pil. Please check data and be sure column names match an expected format")
  }


  purpose_count <- sum(grepl('^purpose', names(df)))

  if(!purpose_count > 1){
    stop("Could not find any Purpose in Life variables. Check your data to make sure the prefix purpose is present on all columns with Purpose in Life data")
  }
  if(purpose_count > 12){
    stop('There are greater than 12 Purpose in Life Variables. Please check your data to make sure there are 8 to 12 variables with the prefix \'purpose\'')
  }



  if(suppressWarnings(all(lapply(dplyr::select(df, dplyr::starts_with('purpose')), is.numeric)))){
    purpose_range <- range(dplyr::select(df, dplyr::starts_with('purpose')), na.rm = T)

    if(!(purpose_range[1] >= 1 & purpose_range[2] <= 5)){
      stop('Range of Purpose in Life data (numeric) is not between 1 and 5, please check your data')
    }


    df <- dplyr::mutate_at(df, dplyr::vars(dplyr::starts_with('purpose')),
                           ~dplyr::case_when(. == 1 ~ 'Strongly Disagree' ,
                                             . == 2 ~ 'Disagree',
                                             . == 3 ~ 'Neither Agree Nor Disagree',
                                             . == 4 ~ 'Agree',
                                             . == 5 ~ 'Strongly Agree'))
    numeric_tf = T

  }else{
    numeric_tf = F
  }





  # Isolate and factor variables
  purpose_raw <- dplyr::mutate_at(df, dplyr::vars(dplyr::starts_with('purpose')),
                                  ~stringr::str_to_title(.))

  purpose_raw <- dplyr::mutate_at(purpose_raw, dplyr::vars(dplyr::starts_with('purpose')),
                                  ~factor(., levels = c("Strongly Disagree",
                                                        "Somewhat Disagree",
                                                        "Neither Agree Nor Disagree",
                                                        "Somewhat Agree",
                                                        "Strongly Agree")))

  s <- dplyr::select(purpose_raw, dplyr::starts_with('purpose'))

  if(!all(!grepl("\\s+[a-z]", s))){ # Test whether first character in each word is capitalized (Title Case Check)
    warning('Strings are not in Title Case, please check that each word starts with a capital in your data')
  }


  # Determine presence of underscore in col_names
  if(sum(grepl('purpose_\\d+', names(s))) == 6|sum(grepl('purpose_\\d+', names(s))) == 12){
    underscore = T
  }else if(sum(grepl('purpose\\d+', names(s))) == 6|sum(grepl('purpose\\d+', names(s))) == 12){
    underscore = F
  }else{
    warning('Column Names are in an unrecognized format. Format should be purpose_#')
  }


  # Convert missing threshold to decimal
  if (missing_threshold > 1){
    missing_threshold = missing_threshold / 100
  }


  # Scoring

  # purpose_factored <- input_df |>
  #   dplyr::select(dplyr::starts_with('purpose')) |>
  #   dplyr::mutate_at(dplyr::vars(dplyr::starts_with('purpose')),
  #             ~factor(., levels = c("Strongly Disagree", "Somewhat Disagree", "Neither Agree nor Disagree",
  #                                   "Somewhat Agree", "Strongly Agree")))


  purpose_num <- dplyr::mutate_at(purpose_raw, dplyr::vars(dplyr::starts_with('purpose')),
                                  ~dplyr::case_when(. == 'Strongly Disagree' ~ 1,
                                                    . == 'Somewhat Disagree' ~ 2,
                                                    . == 'Neither Agree Nor Disagree' ~ 3,
                                                    . == 'Somewhat Agree' ~ 4,
                                                    . == 'Strongly Agree' ~ 5))

  purpose_num <- dplyr::rename_at(purpose_num, dplyr::vars(dplyr::starts_with('purpose')),
                                  ~paste0(., '_num'))


  # purpose_reversed <- purpose_num


  # Score Subscale from reveresed DF

  ## Full Scale
  if (form == 'full'){

    purpose_num$purpose_3r_num <- purpose_num$purpose_3_num*(-1)+6
    purpose_num$purpose_12r_num <- purpose_num$purpose_12_num*(-1)+6

    purpose_num <- dplyr::select(purpose_num, -c(purpose_3_num, purpose_12_num))


    # purpose_scored <- purpose_num |>
    #   dplyr::mutate(purpose_index = rowSums(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
    #                                                       purpose_4_num:purpose_11_num, purpose_12r_num), na.rm = T),
    #          purpose_mean = rowMeans(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
    #                                                purpose_4_num:purpose_11_num, purpose_12r_num), na.rm = T))


    purpose_num <-
      dplyr::mutate(purpose_num,
                    purpose_index = ifelse(rowSums(is.na(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                                       purpose_4_num:purpose_11_num, purpose_12r_num)))  # checks if missing values exceed missing threshold for each row
                                           < ncol(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                                purpose_4_num:purpose_11_num, purpose_12r_num)) * missing_threshold,
                                           rowSums(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                                 purpose_4_num:purpose_11_num, purpose_12r_num), na.rm = T),
                                           NA),
                    purpose_mean = ifelse(rowSums(is.na(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                                      purpose_4_num:purpose_11_num, purpose_12r_num)))  # checks if missing values exceed missing threshold for each row
                                          < ncol(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                               purpose_4_num:purpose_11_num, purpose_12r_num)) * missing_threshold,
                                          rowMeans(dplyr::select(purpose_num, purpose_1_num, purpose_2_num, purpose_3r_num,
                                                                 purpose_4_num:purpose_11_num, purpose_12r_num), na.rm = T),
                                          NA))






  }else if (form == 'short'){

    purpose_num$purpose_6r_num <- purpose_num$purpose_6_num*(-1)+6

    purpose_num <- dplyr::select(purpose_num, -purpose_6_num)



    # Short Form
    # purpose_scored <- purpose_num |>
    #   dplyr::mutate(purpose_index = rowSums(dplyr::select(., purpose_1_num:purpose_5_num, purpose_6r_num)),
    #          purpose_mean = rowMeans(dplyr::select(., purpose_1_num:purpose_5_num, purpose_6r_num), na.rm = T))


    purpose_num <-
      dplyr::mutate(purpose_num,
                    purpose_index = ifelse(rowSums(is.na(dplyr::select(purpose_num, purpose_1_num:purpose_5_num, purpose_6r_num)))  # checks if missing values exceed missing threshold for each row
                                           < ncol(dplyr::select(purpose_num, purpose_1_num:purpose_5_num, purpose_6r_num)) * missing_threshold,
                                           rowSums(dplyr::select(purpose_num, purpose_1_num:purpose_5_num, purpose_6r_num), na.rm = T),
                                           NA),
                    purpose_mean = ifelse(rowSums(is.na(dplyr::select(purpose_num, purpose_1_num:purpose_5_num, purpose_6r_num)))  # checks if missing values exceed missing threshold for each row
                                          < ncol(dplyr::select(purpose_1_num:purpose_5_num, purpose_6r_num)) * missing_threshold,
                                          rowMeans(dplyr::select(purpose_num, purpose_1_num:purpose_5_num, purpose_6r_num), na.rm = T),
                                          NA))
  }


  # Percent Missingness

  purpose_num <- dplyr::mutate(purpose_num,
                               purpose_NApct = rowSums(is.na(dplyr::select(purpose_num, dplyr::matches('purpose(.*)_num'))))
                               / ncol(dplyr::select(purpose_num, dplyr::matches('purpose(.*)_num'))))



  purpose_clean <- dplyr::select(purpose_num, unique_id_for_merging, purpose_index, purpose_mean, purpose_NApct)

  # purpose_clean <- merge(purpose_factored, purpose_scored, by = idVar)





  if (tscore == T){
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

    purpose_clean2 <- cbind(purpose_clean, test) |>
      dplyr::select(idVar, purpose_mean, purpose_index, Percent, TScore, everything())

    return(purpose_clean2)

  }



  # Reintegrate Scored Data with Original Dataframe

  purpose_scored <- merge(input_df, purpose_clean, by = 'unique_id_for_merging')



  # if (upper_prefix == F){
  #   if (underscore == T){
  #     purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before='purpose_1')
  #   }else if (underscore == F){
  #     purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before='purpose1')
  #   }
  #
  # }
  #
  # if (upper_prefix == T){
  #   if (underscore == T){
  #     purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before='Purpose_1')
  #   } else if (underscore == F){
  #     purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before='Purpose1')
  #   }
  #
  # }



  if (verbose_prefix == T){
    first_var = grep('purpose', names(purpose_scored), value = T)[1]

    if (underscore == T){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    } else if (underscore == F){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    }

  }


  if (abrv_prefix == T){
    first_var = grep('pil', names(purpose_scored), value = T)[1]

    if (underscore == T){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    } else if (underscore == F){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    }

  }



  if (upper_prefix == T){
    first_var = grep('Purpose', names(purpose_scored), value = T)[1]

    if (underscore == T){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    } else if (underscore == F){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    }

  }

  if (standard_prefix == T){
    first_var = grep('purpose', names(purpose_scored), value = T)[1]

    if (underscore == T){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    } else if (underscore == F){
      purpose_scored <- dplyr::relocate(purpose_scored, names(purpose_clean), .before=first_var)
    }

  }


  purpose_scored <- dplyr::select(purpose_scored, -unique_id_for_merging)


  return(purpose_scored) # Returns only input_df with added subscales. All factoring, etc. does not remain

}


load("data/purpose.rda")

utils::globalVariables(c("purpose_12r_num", "purpose_1_num", "purpose_2_num", "purpose_3_num",
                         "purpose_3r_num", "purpose_4_num", "purpose_5_num", "purpose_6_num",
                         "purpose_6r_num", "purpose_NApct", "purpose_index", "purpose_mean",
                         "unique_id_for_merging", "Percent", "TScore", "everything", "purpose_11_num", "purpose_12_num"))
