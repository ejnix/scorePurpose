## code to prepare `test_data` dataset goes here

response_list <- c('Strongly Disagree', 'Somewhat Disagree', 'Neither Agree Nor Disagree', 'Somewhat Agree', 'Strongly Agree')

test_data <- data.frame(purpose_1 = sample(response_list, 10, replace=TRUE),
                        purpose_2 = sample(response_list, 10, replace=TRUE),
                        purpose_3 = sample(response_list, 10, replace=TRUE),
                        purpose_4 = sample(response_list, 10, replace=TRUE),
                        purpose_5 = sample(response_list, 10, replace=TRUE),
                        purpose_6 = sample(response_list, 10, replace=TRUE),
                        purpose_7 = sample(response_list, 10, replace=TRUE),
                        purpose_8 = sample(response_list, 10, replace=TRUE),
                        purpose_9 = sample(response_list, 10, replace=TRUE),
                        purpose_10 = sample(response_list, 10, replace=TRUE),
                        purpose_11 = sample(response_list, 10, replace=TRUE),
                        purpose_12 = sample(response_list, 10, replace=TRUE))


test_data[8,sample(ncol(test_data),3)] <- NA
test_data[9,sample(ncol(test_data),6)] <- NA
test_data[10,sample(ncol(test_data),9)] <- NA

# Add Subject ID
test_data <- data.frame(SubjectID = 1:10, test_data)


usethis::use_data(test_data, overwrite = TRUE)
