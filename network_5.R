library(stats)

DQ<-as.matrix(Q_vector)-q_star
aj<-adj_matrix

objective_function <- function(qij_mat_flat, DQ, aj) {
  qij_mat <- matrix(0, nrow = N, ncol = N)
  qij_mat[upper.tri(qij_mat)] <- -qij_mat_flat
  qij_mat[lower.tri(qij_mat)] <- t(qij_mat)[lower.tri(qij_mat)]
  qij_mat[lower.tri(qij_mat)] <- -qij_mat[lower.tri(qij_mat)]
  predicted_DQ <- rowSums(aj * qij_mat)
  # objective function
  sum((DQ - predicted_DQ)^2)
}

qij_mat_initial <- rep(0, N*(N-1)/2)

#optim function
opt_result <- optim(qij_mat_initial, objective_function, DQ = DQ, aj = aj, method = "L-BFGS-B")

qij_mat_nash <- matrix(0, nrow = N, ncol = N)
qij_mat_nash[upper.tri(qij_mat_nash)] <- opt_result$par
qij_mat_nash[lower.tri(qij_mat_nash)] <- t(qij_mat_nash)[lower.tri(qij_mat_nash)]
qij_mat_nash[lower.tri(qij_mat_nash)] <- -qij_mat_nash[lower.tri(qij_mat_nash)]

qij_mat_nash<-qij_mat_nash*aj

rowSums(qij_mat_nash) #= -DQ









