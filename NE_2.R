r_matrix<-drop_units(st_distance(points))
diag(r_matrix) <- r0

b<-(log(R)-log(r_matrix))/(log(R)-log(r0))
B_prime<-matrix(b, nrow=N)
diag(B_prime) <- 2  # Set diagonal elements to 2
a <- rep(1, N)  # Vector a with all elements 1
a <- a*2*pi*Tr/(c*(log(R)-log(r0)))

# Compute (B')^{-1}
B_prime_inv <- solve(B_prime)

# p using relationship [16]
p <- Q_tot / sum(B_prime_inv %*% a)

# Compute q* using relationship [15]
q_star <- B_prime_inv %*% (p * a)


#Testing relationship: si+sii=p/c
coef_s=log(R/r_matrix)/(2*pi*Tr)
si_nash=coef_s %*% q_star
sii_nash=q_star*log(R/r0)/(2*pi*Tr)

si_sii=si_nash+sii_nash
#testing errors: the errors between si+sii and p/c need to be minor
errors=si_sii-p/c

#calculating overall pumping cost:
U_ben<-(q_star-Q_vector)*p
Ucost_i_nash<-c*si_nash*q_star
sum(Ucost_i_nash)
print(q_star)





