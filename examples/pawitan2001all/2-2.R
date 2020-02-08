x_i <- c(218, 88, 254, 33, 368, 235, 294, 115, 9)
l <- function(seq_num, x_i){
  L <- seq_num
  for(i in 1:length(N)){
    L[i] <- prod(rep(1, length(x_i)) / seq_num[i]) * (max(x_i) <= seq_num[i])
  }
  L
}

seq_num <- seq(350,600)
ll <- l(seq_num, x_i)

plot(seq_num, ll / max(ll), type = "l")