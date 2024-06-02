#ex. C1

#a.
generate_random_permutation = function(n) {
  random_values = runif(n,0,1)
  permutation = order(random_values)
  print(random_values)
  return(permutation)
}

n = 10
generate_random_permutation(n)




#b.

generate_random_word = function(k) {
  word = sample(0:1, k, replace = TRUE)
  return(word)
}

lexicographic_compare = function(word1, word2) {
  min_length = min(length(word1), length(word2))
  for (i in 1:min_length) {
    if (word1[i] < word2[i]) {
      return(TRUE)
    } 
    else {
      return(FALSE)  # word2 este lexicografic mai mic decât word1
    }
  }
  
  if (length(word1) < length(word2)) {
    return(TRUE)  
  } 
  else {
    return(FALSE)  
  }
  
  while (TRUE) {
    new_bit1 = sample(0:1, 1) 
    new_bit2 = sample(0:1, 1)  
    
    if (new_bit1 < new_bit2) {
      return(TRUE)  
    } 
    else {
      return(FALSE) 
    }
  }
}

k = 5  
word1 = generate_random_word(k)
word2 = generate_random_word(k)  
result = lexicographic_compare(word1, word2)
word1
word2
result




#c.

quick_sort = function(words) {
  n = length(words)
  sorted = FALSE
  iterations = 0  
  max_iterations = n^2  
  
  while (!sorted && iterations < max_iterations) {
    iterations = iterations + 1 
    print(paste("Iterația:", iterations))  
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (!lexicographic_compare(words[[i]], words[[j]])) {
          temp = words[[i]]
          words[[i]] = words[[j]]
          words[[j]] = temp
        }
      }
    }
    sorted = TRUE
    for (i in 1:(n - 1)) {
      if (!lexicographic_compare(words[[i]], words[[i + 1]])) {
        sorted = FALSE
        break
      }
    }
  }
  
  return(words)
}

k = 5  
n = 3 
word_list = lapply(1:n, function(x) generate_random_word(k))
print("Lista de cuvinte înainte de sortare:")
print(word_list)
sorted_words = quick_sort(word_list)
print("Lista de cuvinte după sortare:")
print(sorted_words)




#d.

quick_sort = function(words) {
  n = length(words)
  sorted = FALSE
  iterations = 0  
  max_iterations = n^2  
  
  aux = c(1,2,3)
  
  while (!sorted && iterations < max_iterations) {
    iterations = iterations + 1 
    print(paste("Iterația:", iterations))  
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        if (!lexicographic_compare(words[[i]], words[[j]])) {
          temp = words[[i]]
          words[[i]] = words[[j]]
          words[[j]] = temp
          temp1=aux[[i]]
          aux[[i]]=aux[[j]]
          aux[[j]]=temp1
        }
      }
    }
    sorted = TRUE
    for (i in 1:(n - 1)) {
      if (!lexicographic_compare(words[[i]], words[[i + 1]])) {
        sorted = FALSE
        break
      }
    }
  }
  return(aux)
}

k = 5  
n = 3 
word_list = lapply(1:n, function(x) generate_random_word(k))
print("Lista de cuvinte înainte de sortare:")
print(word_list)
orders = quick_sort(word_list)
orders





#ex. C2

#a.
max_cut_random = function(V, E, iterations = 1000) {
  best_cut_size = 0
  for (i in 1:iterations) {
    A_indices = sample(1:length(V), length(V) %/% 2)
    A = V[A_indices]
    B = setdiff(V, A)
    cut_size = sum(sapply(E, function(edge) (edge[1] %in% A & edge[2] %in% B) | (edge[2] %in% A & edge[1] %in% B)))
    if (cut_size > best_cut_size) {
      best_cut_size = cut_size
    }
  }
  return(best_cut_size)
}

V = 1:6  
E = list(c(1, 2), c(1, 3), c(2, 4), c(3, 5), c(4, 6))  
max_cut_size = max_cut_random(V, E)
print("Cardinalul maxim al tăieturii găsite:")
print(max_cut_size)




#b.

#Pentru a crește șansele de a găsi o tăietură de cardinal maxim, 
#putem rula algoritmul aleator de mai multe ori și să păstrăm tăietura 
#cu cel mai mare cardinal găsit.
