#ex. A1
#a
poisson= function(lambda, k, m) {
  x = k:m
  prob = dpois(x, lambda)
  return(prob)
}

geometric= function(p, k, m) {
  x = k:m
  prob = dgeom(x, p)
  return(prob)
}

binomial= function(n, p, k, m) {
  x = k:m
  prob = dbinom(x, n, p)
  return(prob)
}

lambda = 2.5
p = 0.3
n = 20
k = 5
m = 10
poisson_prob=poisson(lambda, k, m);
geometric_prob=geometric(p, k, m)
binomial_prob=binomial(n, p, k, m)
print(poisson_prob)
print(geometric_prob)
print(binomial_prob)




#b

binomial= function(n,p){
  x=0:n;
  y=dbinom(x,n,p);
  barplot(y,space=0);
}
#binomial(20,0.3)

geometric = function(n,p){
  x=0:(n-1);
  y=dgeom(x,p);
  barplot(x,space=0);
}
#geometric(20,0.3)

poisson = function(n,l){
  x=0:(n-1);
  y=dpois(x,l);
  barplot(y,space=0);
}
#poisson(20,2.5)

plot_probabilities = function(k, m, lambda, p, n) {
  x = k:m
  poisson_prob = poisson(n,lambda)
  geometric_prob = geometric(n,p)
  binomial_prob = binomial(n, p)
}

lambda = 2.5
p = 0.3
n = 20
k = 5
m = 10
plot_probabilities(k, m, lambda, p, n)
#plot_probabilities(4,25,1,0.6,50)




#c

find_k0_poisson = function(lambda) 
{
  k0 = 0
  cumulative_prob = ppois(k0, lambda)
  while (cumulative_prob <= 1 - 10^-6) 
  {
    k0 = k0 + 1
    cumulative_prob = ppois(k0, lambda)
  }
  return(k0)
}

lambda=2.5
find_k0_poisson(lambda)





#ex. A2
#a

read_and_analyze_grades = function(file_path) 
{
  data = read.table(file_path, header = TRUE)
  P = data$P
  S = data$S
  #frecvențe absolute
  freq_abs_P = table(P)
  freq_abs_S = table(S)
  #frecvențe relative
  freq_rel_P = as.vector(freq_abs_P) / length(P)
  freq_rel_S = as.vector(freq_abs_S) / length(S)
  #medii
  mean_P = mean(P)
  mean_S = mean(S)
  result = list(
    freq_abs_P = freq_abs_P,
    freq_abs_S = freq_abs_S,
    freq_rel_P = freq_rel_P,
    freq_rel_S = freq_rel_S,
    mean_P = mean_P,
    mean_S = mean_S)
  return(result)
}

file_path = "note_PS.csv"
result = read_and_analyze_grades(file_path)
print(result)




#b

remove_outliers_and_plot = function(file_path, sample_name) {
  data = read.table(file_path, header = TRUE, sep = ",")
  sample = as.numeric(data[[sample_name]])
  #val aberante 
  Q1 = quantile(sample, 0.25)
  Q3 = quantile(sample, 0.75)
  IQR = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  cleaned_sample = sample[sample >= lower_bound & sample <= upper_bound]
  cleaned_sample = as.numeric(cleaned_sample)
  #frecv pe intervalele (1,2], (2,3],..., (9,10]
  intervals = cut(cleaned_sample, breaks = seq(1, 10, by = 1), right = TRUE)
  freq_table = table(intervals)
  barplot(freq_table, col = "blue")
  return(cleaned_sample)
}

file_path = "note_PS.csv"
cleaned_P = remove_outliers_and_plot(file_path, "P")
cleaned_S = remove_outliers_and_plot(file_path, "S")
