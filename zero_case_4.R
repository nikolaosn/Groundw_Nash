#zero case: everyone pumps Qi
q_zero <- as.matrix(Q_vector)

si_zero<-coef_s %*% q_zero
Ucost_i_zero<-c*si_zero*q_zero
sum(Ucost_i_zero)
