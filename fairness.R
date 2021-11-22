# Global parameters: number of qualified and unqualified applicants from groups
# A and B.

Q_A <- 4
N_A <- 8
Q_B <- 4
N_B <- 12

# demographic parity: satisfied as long as proportion of applicants we admit
# from each group is equal.

checkDemParity <- function(a_qa, a_na, a_qb, a_nb){
  return((a_qa + a_na)/(Q_A + N_A) == (a_qb + a_nb)/(Q_B + N_B))
}

checkDemParity(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 6)

# equalized odds: the proportion of qualified applicants from group A who are
# admitted should be equal to the proportion of qualified applicants from group
# B who are admitted (and likewise for unqualified applicants)

checkEO <- function(a_qa, a_na, a_qb, a_nb){
  qualifed <- a_qa/Q_A == a_qb/Q_B
  unqualified <- a_na/N_A == a_nb/N_B
  return(qualifed && unqualified)
}

checkEO(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 6)

# calibration: the proportion of admitted applicants from group A who are
# qualified should be equal to the proportion of admitted applicants from group
# B who are qualified (and likewise for rejected applicants)

checkCal <- function(a_qa, a_na, a_qb, a_nb){
  admitted <- a_qa/(a_qa + a_na) == a_qb/(a_qb + a_nb)
  # Get total rejected numbers for A and B
  r_a <- Q_A + N_A - (a_qa + a_na)
  r_b <- Q_B + N_B - (a_qb + a_nb)
  # Compare proportions of rejected candidates who were qualified
  rejected <- (Q_A - a_qa)/r_a == (Q_B - a_qb)/r_b
  return(admitted & rejected)
}

checkCal(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 4)

checkAssortment <- function(a_qa, a_na, a_qb, a_nb){
  # Check inputs
  stopifnot(a_qa <= Q_A & a_na <= N_A & a_qb <= Q_B & a_nb <= N_B)
  ret <- c(checkDemParity(a_qa, a_na, a_qb, a_nb),
           checkEO(a_qa, a_na, a_qb, a_nb),
           checkCal(a_qa, a_na, a_qb, a_nb))
  names(ret) <- c("Demographic Parity", "Equalized Odds", "Calibration")
  return(ret)
}

checkAssortment(a_qa = 3, a_na = 0, a_qb = 3, a_nb = 1)
checkAssortment(a_qa = 2, a_na = 2, a_qb = 2, a_nb = 3)
checkAssortment(a_qa = 0, a_na = 4, a_qb = 0, a_nb = 8)

checkAssortment(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 6)
checkAssortment(a_qa = 3, a_na = 0, a_qb = 4, a_nb = 0)
checkAssortment(a_qa = 4, a_na = 0, a_qb = 4, a_nb = 0)


