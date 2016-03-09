

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){#Set the Matrix
    x <<- y 
    inv <<- NULL 
  } 
  get <- function() x #Get the Matrix
  setInverse <- function(inverse) inv <<- inverse  #Set the Inverse of a Matrix
  getInverse <- function() inv #Get the Inverse of a Matrix
  list(set = set, get = get, 
    setInverse = setInverse, 
    getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() 
  if(!is.null(inv)){#Check if the Inverse has already been calculated
    message("getting cached data") #If the Inverse has been calculated,it gets the inverse from the cache 
    return(inv) 
    } 
  mat <- x$get() #If the Inverse has not been calculated, it gets the Inverse
  inv <- solve(mat, ...) 
  x$setInverse(inv)#sets the value of the Inverse in the cache
  inv
}

