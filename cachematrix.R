## Two functions:  makeCacheMatrix takes a square invertible matrix and prepares
##it for use in the next function cacheSolve, which either solves the inversion
##of the matrix, or if it has already been solved, returns the solution from cache.



#Sets up data/function structure to allow invertible matrix to be stored, and for
#cacheSolve function to retrieve inversion from cache if available
makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    
    get <- function() {x}
    
    setinverse <- function(inv) {mi <<- inv}
    
    getinverse <- function() {mi}
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


# Solve matrix inversion, or retrieve previously calculated inversion from cache,
#using structure established in makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  mi <- x$getinverse()
  
  if(!is.null(mi)) {
        message("getting cached inversion")
        return(mi)
  }
  
  data <- x$get()
  
  mi <- solve(data, ...)
  
  x$setinverse(mi)
  
  mi
  
}