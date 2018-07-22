
## This function creates a special "matrix",which is a list containing a function to:

#1- set the value of the vector
#2- get the value of the vector
#3- set the value of the inverse matrix
#4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


#The following function calculates the inverse of the special "matrix" created with the above function.
#it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache
#Otherwise, it calculates the mean of the data

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv  <- solve(data, ...)
  x$setinv(inv)
  inv
}
