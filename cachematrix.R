## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_matrix <- function(inverse) inv <<- inverse
  get_matrix <- function() inv
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_matrix()
    #checks to see if inv has a value already cached and returns the value if it does
    if(!is.null(inv)) {
      message("accessing cached data.")
      return(inv)

    }
    data <- x$get()
    
    #If inverse isnt cached, calculates the inverse with the solve function
    inv <- solve(data, ...)
    
    #sets the matrix to the inverse value
    x$set_matrix(inv)
    inv
  }
