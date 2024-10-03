#'Some auxiliary functions for the Heidke skill score
#'@param p success probability
#'@param n the number of trials
prFun = function(p, n){
    pbinom(floor(n*.5), size = n, prob = p)
}
prFunDeriv = function(p, n){
    seqK = 0:floor(.5*n)
    sum(choose(n, seqK)*(seqK*p^(seqK-1) *(1-p)^(n-seqK) - p^seqK*(n-seqK) * (1-p)^(n-seqK-1)))
}
prFunDerivFull = function(p, n){
    seqK = 0:floor(.5*n)
    -2*prFun(p, n) + (1-2*p)*prFunDeriv(p,n)
}