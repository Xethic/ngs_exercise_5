library("gtools")

sig_values = c(0.01,0.02,0.09,0.81,0.54,0.04,0.05,0.11,0.44,0.08,0.03,0.06,0.07,0.22,0.21,0.34,0.77,0.89,0.45,0.13,0.32,0.42,0.21,0.73,0.66,0.88)
pathway = c(0,1,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
perm_count = 1000

sig_values_sorted = sort(sig_values, partial = NULL, na.last = NA, decreasing = TRUE, index.return = TRUE)
pathway_sorted = c()
sort_indizes = sig_values_sorted[2]
#print(sig_values_sorted)

index = 1
for(sort_index in sort_indizes){
  pathway_sorted <- append(pathway_sorted, pathway[sort_index], after=index)
  index = index +1
}
#print(pathway_sorted)

first_sum = 0
higher_sum_counter = 0
for(i in 1:perm_count){
  running_sum = 0
  if(i!=1){
    pathway_sorted = permute(pathway_sorted);
  }
  max = 0
  index = 1
  for(value in sig_values_sorted[1]){
    if(pathway_sorted[index] == 1){
      running_sum = running_sum + length(sig_values) - sum(pathway)
    }
    else{
      running_sum = running_sum - sum(pathway)
    }
    if(running_sum > max){
      max = running_sum
    }
    index = index +1
  }
  if(i == 1){
    first_sum = max
  }
  else{
    if(max > first_sum){
      higher_sum_counter = higher_sum_counter + 1
    }
  }
}

p_value = 0
if(perm_count > 0){
  p_value = higher_sum_counter/perm_count
}

print("Evaluted p-value:")
print(p_value)
