## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Esta función almacenará en su caché el inverso de una matriz
makeCacheMatrix <- function(x = matrix()) {
inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inve <<- solveMatrix
  getInverse <- function() inve
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Esta función calcula la matriz inversa proporcionada por makeCacheMatrix en la función anterior
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inve <- x$getInverse()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data)
  x$setInverse(inve)
  inve  
}
