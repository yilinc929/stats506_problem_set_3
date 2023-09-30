email_data <- read.csv("/Users/cathy/Desktop/us-500.csv", header = TRUE)
#a)
email_net <- sum(grep("@.*\\.net", email_data))
email_count <- nrow(email_data)
proportion_email_w_net <- email_net/email_count
proportion_email_w_net

#b)
email_non_alpha <- sum(grep("[:alnum:]+", email_data))
proportion_email_w_non_alpha <- email_non_alpha/email_count
proportion_email_w_non_alpha 

#c)
area_codes1 <- substr(email_data$phone1, 1, 3)
area_codes2 <- substr(email_data$phone2, 1, 3)
area_codes <- c(area_codes1,area_codes2)
area_codes <- table(area_codes)
most_common_area_code <- names(which.max(area_codes))

#d)
address <- email_data$address
apt_num <- as.numeric(sub('.*#', '', address))
apt_num <- na.omit(apt_num)
log_apt_num <- log(apt_num)
hist(log_apt_num, main="Histogram of Log Apartment Numbers",
     xlab="Log of Apartment Numbers")

#e)
leading_digits <- as.numeric(substr(apt_num,1,1))
benford_dist <- log10(1 + 1/seq(1, 9))
observed_counts <- table(leading_digits)
expected_counts <- benford_dist * length(leading_digits)
expected_prob <- expected_counts / sum(expected_counts)
print(observed_counts)
print(expected_counts)
chi_squared_test <- chisq.test(observed_counts, p = expected_prob)
print(chi_squared_test)


#f)
address <- email_data$address
street_num <- sub(" .*$", " ", address)
last_digits <- as.numeric(substr(street_num,nchar(street_num)-1,nchar(street_num)-1))
last_digits[last_digits==0] <- NA
last_digits <- na.omit(last_digits)
observed_1 <- table(last_digits)

benford_dist <- log10(1 + 1/seq(1, 9))
expected_counts <- benford_dist * length(leading_digits)
expected_prob <- expected_counts / sum(expected_counts)

print(observed_1)
print(expected_counts)
chi_squared_test_1 <- chisq.test(observed_1, p = expected_prob)
print(chi_squared_test_1)
