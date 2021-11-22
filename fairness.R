

q_a <- 4
n_a <- 8
q_b <- 4
n_b <- 12

# demographic parity: fine as long as proportion of students we admit
# from each group is equal. for example...


checkDemParity <- function(a_qa, a_na, a_qb, a_nb){
  return((a_qa + a_na)/(q_a + n_a) == (a_qb + a_nb)/(q_b + n_b))
}

checkDemParity(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 6)

# equalized odds

checkEO <- function(a_qa, a_na, a_qb, a_nb){
  qualifed <- a_qa/q_a == a_qb/q_b
  unqualified <- a_na/n_a == a_nb/n_b
  return(qualifed && unqualified)
}

checkEO(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 6)

# calibration

checkCal <- function(a_qa, a_na, a_qb, a_nb){
  admitted <- a_qa/(a_qa + a_na) == a_qb/(a_qb + a_nb)
  # Get total rejected numbers for A and B
  r_a <- q_a + n_a - (a_qa + a_na)
  r_b <- q_b + n_b - (a_qb + a_nb)
  # Compare proportions of rejected candidates who were qualified
  rejected <- (q_a - a_qa)/r_a == (q_b - a_qb)/r_b
  return(admitted & rejected)
}

checkCal(a_qa = 2, a_na = 4, a_qb = 2, a_nb = 4)

checkAssortment <- function(a_qa, a_na, a_qb, a_nb){
  if(a_qa > q_a | a_na > n_a | a_qb > q_b | a_nb > n_b){
    stop("a_qa > q_a | a_na > n_a | a_qb > q_b | a_nb > n_b")
  }
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


