## Black-Scholes Function
BS <-function(S, K, T, r, sig, type="C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}



## Function to find BS Implied Vol using Bisection Method
implied.vol <-function(S, K, T, r, market, type){
  sig <- 1.5
  sig.up <- 2.3
  sig.down <- 1
  count <- 0
  err <- BS(S, K, T, r, sig, type) - market 
 
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.01 && count<1000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- BS(S, K, T, r, sig, type) - market
    count <- count + 1
  }
 
  ## return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}

BS(42,55,.012,29/252,2.08, "C")
BS(39.50,39.50,.012,(15/252),1.2, "C")
implied.vol(39.50,39.50,.012,(15/252),2.2, "C")

