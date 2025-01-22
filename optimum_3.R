
#we consider the last element as player N
lamda<-matrix(nrow=N,ncol=N)

i=0
while(i<N){
  i=i+1
  j=0
  while(j<N){
    j=j+1
    lamda[i,j]<-log(r_matrix[N,j]/r_matrix[i,j])/(2*pi*Tr)
  }
}

lamda[N,]<-1
Z_matrix<-(1:N)*0 ; Z_matrix[N]<-Q_tot
lamda_inv <- solve(lamda)

q_m <- lamda_inv %*% Z_matrix
si_m<-coef_s %*% q_m
Ucost_i_m<-c*si_m*q_m
sum(Ucost_i_m)



