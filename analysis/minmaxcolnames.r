# tm_cors <- matrix(runif(20), nrow=4)
# rownames(tm_cors) <- paste0("foo", seq(nrow(tm_cors)))
# colnames(tm_cors) <- paste0("bar", seq(ncol(tm_cors)))

result <- t(sapply(seq(nrow(tm_cors)), function(i) {
  j <- which.min(tm_cors[i,])
  c(paste(rownames(tm_cors)[i], colnames(tm_cors)[j], sep='/'), tm_cors[i,j])
}))

print(tm_cors)
print(result)
