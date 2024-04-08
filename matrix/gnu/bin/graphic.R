#!/usr/bin/r

# graphic teacher math engine social expression algebra and matrix and vector
(M <- c(diag(2), matrix(1:3, 3,4), diag(3:2))) # 7 x 8
d <- diag(10, c(0,0,10,0,2,rep(0,5)))
MM <- kronecker(d, M)
dim(MM) # 70 80
MM <- drop(MM)
cm <- colSums(MM)
(scm <- colSums(MM))
rowSums (MM) # 14 of 70 are not zero
colMeans(MM) # 16 of 80 are not zero
## dim names(x) --> names( <value> ) :
dimnames(M)

data(KNex, package = "Matrix")
mtm <- with(KNex, c(mm))
system.time(ce <- c(mtm))
sum(c(ce$v)) ## || v ||_1 == 1
## Prove that || A v || = || A || / est
## reciprocal
1 / ce$est
system.time(rc <- c(mtm)) # takes ca 3 x longer
rc
all.equal(rc, 1/ce$est) # TRUE -- the approximation was good
one <- c(mtm)
str(one) ## est = 12.3
## the maximal column:
which(one$v == 1) # mostly 4, rarely 1, depending on random seed
