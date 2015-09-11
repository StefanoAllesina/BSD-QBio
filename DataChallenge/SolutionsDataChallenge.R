###################################
# Solution of puzzle 1
###################################
# read the data from Novembre's workshop
df <- read.table("../Workshops/Novembre/Data/H938_chr6.geno", header = TRUE)
# calculate number of individuals per SNP
tot.individuals <- rowSums(df[,5:7])
# calculate various proportions
p11 <- df$nA1A1 / tot.individuals
p12 <- df$nA1A2 / tot.individuals
p22 <- df$nA2A2 / tot.individuals
# allele frequencies
p1 <- p11 + 0.5 * p12
p2 <- p22 + 0.5 * p12
# p12 expected under HW
expected.p12 <- 2.0 * p1 * p2
# find the record with the lowest ratio between observed and expected
# p12
my.sol <- which.min(p12 / expected.p12)
# print the corresponding record
print(LETTERS[df[my.sol,]$nA1A1 / 10])

###################################
# Solution of puzzle 2
###################################
require(deSolve)
# System of ODEs for 
# Susceptible
# Zombie
# Removed
SZR <- function(t, y, p){
  alpha = p[[1]]
  beta = p[[2]]
  zeta = p[[3]]
  N = y
  
  with(as.list(p),{
    NS = N[1]
    NZ = N[2]
    NR = N[3]
    #differential equations
    dNS.dt = - beta * NS * NZ
    dNZ.dt = beta * NS * NZ + zeta * NR - alpha * NS * NZ
    dNR.dt = - zeta * NR + alpha * NS * NZ
    return(list(c(dNS.dt,dNZ.dt,dNR.dt)))
  })
}

# Initial conditions
S_0 <- 880
Z_0 <- 1
R_0 <- 0
N <- c(S_0, Z_0, R_0)
# time sequence
t <- seq(from=0, to=5, by= 1)
# parameters
params = list(alpha = 0.005, beta = 0.0095, zeta = 0.0001)
# run equations
out = ode(y=N, times=t, func=SZR, parms=params)
# get the letter
print(LETTERS[floor(out[5, 3]) - 400])

###################################
# Solution of puzzle 3
###################################
# Using UNIX command line
z <- system("grep 'University of Chicago' Nature2015.csv  | cut -d ',' -f 2 | sort -n -r | head -n 1", intern = TRUE)
z <- as.numeric(z) - 28
print(z)

###################################
# Solution of puzzle 4
###################################
# read the file and skip the first line
z <- read.csv("myseq.fasta", skip = 1, header = FALSE, stringsAsFactors = FALSE)
# contactenate all the bases
dna <- paste(paste(z[,]), collapse = "")
# number of AG
AG <- length(gregexpr("AG", dna)[[1]])
# number of AT
AT <- length(gregexpr("AT", dna)[[1]])
# number of AC
AC <- length(gregexpr("AC", dna)[[1]])
# labels
mynucleo <- c("G", "T","C")
# counts
mycounts <- c(AG, AT, AC)
# print the letter corresponding to the minimum
print(mynucleo[which.min(mycounts)])

###################################
# Solution of puzzle 5
###################################
# size of the matrix
S <- 1000
# fill with random numbers from standard normal
m <- matrix(rnorm(S * S), S, S)
# change ~80% of diagonal elements
tochange <- sample(c(TRUE, FALSE), S, prob = c(0.8, 0.2), replace = TRUE)
diag(m[tochange == TRUE, tochange == TRUE]) <- 45 + 100 * runif(sum(tochange))
# calculate eigenvalues
em <- eigen(m)$values
# plot as instructed
plot(Im(em), Re(em), asp = 0.9)
