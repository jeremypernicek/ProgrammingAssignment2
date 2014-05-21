## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #store matrix and set inversematrix to NULL
  datamatrix <<- x
  inversematrix <- NULL
  
  #get function gets the original matrix
  get <-function() datamatrix
  
  #setinverse sets the inverse of the original matrix and stores it
  setinverse <- function(inverse) inversematrix <<- inverse
  
  #Gets thinverse of the original matrix if none exists returns NULL
  getinverse <- function() inversematrix
  
  #Creates a list of internal functions get, setinverse, getinverse which is returned
  list(get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # retrieves the inverse of the matrix
  inverse<-x$getinverse()
  # Check if the inverse has been calculated
  if(!is.null(inverse)) {
    message("Matrix has already been calculated. Retrieving solved matrix")
    # returns the inverse that has been calculated and cached
    return(inverse)
  }
  # If the inverse has not been calculated before
  # the inverse matrix is calculated
  inverse<-solve(x$get(), ...)
  # caches the calculated inverse matrix
  x$setinverse(inverse)
  # returns the inverse matrix
  inverse
}
