#ex. D1

data = read.csv("probabilitati.csv")
sample_mean = mean(data$probabilitati)
sample_sd = sd(data$probabilitati)
n = length(data$probabilitati)
sigma_squared = 92.16
sigma = sqrt(sigma_squared)
alpha_95 = 0.05
z_95 = qnorm(1 - alpha_95 / 2)
margin_error_95 = z_95 * sigma / sqrt(n)
ci_95_lower = sample_mean - margin_error_95
ci_95_upper = sample_mean + margin_error_95
alpha_99 = 0.01
z_99 = qnorm(1 - alpha_99 / 2)
margin_error_99 = z_99 * sigma / sqrt(n)
ci_99_lower = sample_mean - margin_error_99
ci_99_upper = sample_mean + margin_error_99
print(paste("Interval de încredere de 95% pentru media punctajului: [", round(ci_95_lower, 2), ", ", round(ci_95_upper, 2), "]", sep=""))
print(paste("Interval de încredere de 99% pentru media punctajului: [", round(ci_99_lower, 2), ", ", round(ci_99_upper, 2), "]", sep=""))






#ex. D2

data = read.csv("statistica.csv")
sample_mean = mean(data$statistica)
sample_sd = sd(data$statistica)
n = length(data$statistica)
alpha_95 = 0.05
z_95 = qnorm(1 - alpha_95 / 2)
margin_error_95 = z_95 * sample_sd / sqrt(n)
ci_95_lower = sample_mean - margin_error_95
ci_95_upper = sample_mean + margin_error_95
alpha_99 = 0.01
z_99 = qnorm(1 - alpha_99 / 2)
margin_error_99 = z_99 * sample_sd / sqrt(n)
ci_99_lower = sample_mean - margin_error_99
ci_99_upper = sample_mean + margin_error_99
print(paste("Interval de încredere de 95% pentru media punctajului: [", round(ci_95_lower, 2), ", ", round(ci_95_upper, 2), "]", sep=""))
print(paste("Interval de încredere de 99% pentru media punctajului: [", round(ci_99_lower, 2), ", ", round(ci_99_upper, 2), "]", sep=""))






#ex. D3

alfa_1 = 0.01
alfa_5 = 0.05
n = 100
succese = 86
p_prim = succese / n
p0 = 0.15
z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
critical_z_1 = qnorm(1 - alfa_1 / 2)
critical_z_5 = qnorm(1 - alfa_5 / 2)
z_score
critical_z_1
critical_z_5
if (abs(z_score) > critical_z_1) {
  cat("1% ---> semnificativ\n")
} else {
  cat("1% ---> nu semnificativ\n")
}

if (abs(z_score) > critical_z_5) {
  cat("5% ---> semnificativ\n")
} else {
  cat("5% ---> nu semnificativ\n")
}
