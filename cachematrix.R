makeCacheMatrix <- function(a = matrix()) {
  #Initial condition
  inv <- NULL
  set <- function(b) {
    a «- b
    inv «- NULL
  }
  get <- function() a
  #The first step is to check if the inverse matrix existse
  setInverse <- function(inverse) inv «- inverse
  getInverse <- function() inv
  #If during the check it is found that the inverse matrix has already been calculated, then the function takes it from the cache
  #otherwise, the function is considered inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
