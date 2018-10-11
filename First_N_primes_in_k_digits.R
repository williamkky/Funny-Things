library("primes")
library("Rmpfr")

#Muiltple Precision Number

n1 <- mpfr(1, precBits = 1024)
sn1=exp(n1)
n2 <- mpfr(2, precBits = 1024)
sn2=sqrt(n2)

n2=mpfr(-1, precBits= 66439)


Number_to_Character=function(precBits,sn){
  digi=floor(precBits*log(2,10))
  sn=formatMpfr(x = sn,digits = digi,trim = T)
  sn=strsplit(sn,split = "")
  sn=as.character(sn[[1]])
  sn=sn[which(sn!=".")]
  return(sn)
}
First_N_primes_in_k_digits=function(N=1,k){
  
  i=0
  Prime_List=NULL
  
  while (length(Prime_List)<N) {
    test_Prime_List=generate_primes(10^(k-1)+i,10^(k-1)+1+i)
    Prime_List=unique(append(Prime_List,test_Prime_List))
    i = i+1
    if (isTRUE(tail(Prime_List,1)>10^k)){
      Prime_List=Prime_List[-length(Prime_List)]
      break
    }
  }
  
  return(Prime_List)
}
First_N_k_digits_primes_in_specific_number=function(N=1,k,precBits,sn){
  
  sn=Number_to_Character(precBits,sn)
    
  i=0
  Prime_Number=NULL
  Position=NULL
  while(T){
    
    if (is.na(sn[(1+i)]=="0")){
      break
    }
    if (sn[(1+i)]=="0"){
      i += 1
    }
    
    test_sn=paste(sn[(1+i):(k+i)], collapse = "")

    if(isprime(test_sn)){
      Prime_Number=unique(append(Prime_Number,test_sn))
      Position=append(Position,i+1)
    }
    i =i + 1
    if (length(Prime_Number)==N){
      break
    }
  }
  
  Summary_list=data.frame(Prime_Number,Position)
  return(Summary_list)
}

#First 100 prime in 10 digits number with precision 2048 bits (2^2048)
First_N_primes_in_k_digits(N = 500,k = 10)

#First 20 10-digits prime in exp(1) with precision 2048 bits (2^2048)
First_N_k_digits_primes_in_specific_number(N = 1000,k = 10,precBits = 33220,sqrt(mpfr(2, precBits = 33220)))

e1=Number_to_Character(precBits = 33220,exp(mpfr(1, precBits = 33220)))
sq2=Number_to_Character(precBits = 33220,sqrt(mpfr(2, precBits = 33220)))
sq5=Number_to_Character(precBits = 33220,sqrt(mpfr(5, precBits = 33220)))

Ten_digits_prime_10000_digits_long_e1=First_N_k_digits_primes_in_specific_number(N = 1000,k = 10,precBits = 33220,exp(mpfr(1, precBits = 33220)))
hist(as.numeric(Ten_digits_prime_10000_digits_long_e1[[2]]),breaks = 10)
