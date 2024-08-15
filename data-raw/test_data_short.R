## code to prepare `test_data_short` dataset goes here

response_list <- c('Strongly Disagree', 'Somewhat Disagree', 'Neither Agree Nor Disagree', 'Somewhat Agree', 'Strongly Agree')

test_data_short <- data.frame(purpose_1 = sample(response_list, 10, replace=TRUE),
                        purpose_2 = sample(response_list, 10, replace=TRUE),
                        purpose_3 = sample(response_list, 10, replace=TRUE),
                        purpose_4 = sample(response_list, 10, replace=TRUE),
                        purpose_5 = sample(response_list, 10, replace=TRUE),
                        purpose_6 = sample(response_list, 10, replace=TRUE))


test_data_short[8,sample(ncol(test_data_short),2)] <- NA
test_data_short[9,sample(ncol(test_data_short),3)] <- NA
test_data_short[10,sample(ncol(test_data_short),5)] <- NA

# Add Subject ID
test_data_short <- data.frame(SubjectID = 1:10, test_data_short)

usethis::use_data(test_data_short, overwrite = TRUE)
