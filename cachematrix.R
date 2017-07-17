#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x - a matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y       # Set the value of matrix
    inv <<- NULL  # Clear cache
  }
  
  get <- function() x  # get the value of matrix
  
  setinverse <- function(inverse) inv <<- inverse  # set the inverse in cache
  
  getinverse <- function() inv  #get inverse from cache
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#' This function computes the inverse of a special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the inverse will be retrieved 
#' from the cache. Otherwise, it will be calculated and stored in the cache.
#' 
#' @param x - a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()  # Get inverted matrix from cache
  
  # If the cache is not empty, return it
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()      # Get value of matrix
  inv <- solve(data)   # Calculate the inverse
  x$setinverse(inv)    # Cache the inverse
  inv                  # Return the inverse
}
