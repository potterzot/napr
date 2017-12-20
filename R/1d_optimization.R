loglikelihood = function(x) {
  # parameters:
  #   x is a vector of iid observations
  # returns:
  #   log likelihood 
  y = log(x)
  
  return(y)
}

f.grid = function(f,lo,hi) {
  l = lo
  h = hi
  while(h-l > 0.0000001) {
    x = (0:1000)/1000*(h-l)+l
    y = f(x) #evaluate the function at x
    m = max(y)
    i = (1:length(x))[y==m]
    l = x[i-1]
    h = x[i+1]
  }
  plot(x,y,type="l")
  c(x[y==m][1],m)
}

f.gold = function(f, lo, hi, tol=0.0000001) {
  g = 1-(sqrt(5)-1)/2 # 1 - golden mean
  l = lo
  h = hi
  a = l + g*(h-l) #alpha adjustment size
  fa = f(a)
  b = a + g*(h-a)
  fb = f(b)
  
  while (h-l > tol) {
    if(fa > fb) {
      h = b
      b = a
      fb = fa
      a = b - g*(b-1)
      fa = f(a)
    }
    else {
      l = a
      a = b
      fa = fb
      b = a + g*(h-a)
      fb = f(b)
    }
  }
  c(a,fa)
}
