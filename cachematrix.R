## Programming Assignment #2 - Frank Seidel
## This script has two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" that it capable of storing
##                 the inverse for future use.
## cacheSolve calculates the inverse of a matrix and stores it for future use
##            without the need for recalculation unless the data changes.

## makeCacheMatrix - This is a function to create a special "matrix" that
##        allows retrieval of the inverse of a matrix once it
##        has been computed.  The set function allows the "matrix"
##        to assign new data and resets the stored inverse to a NULL
##        object.  The get function returns the matrix stored in x.
##        The setinverse function is used by the cacheSolve function
##        to store the inverse calcuated by the solve function in the i
##        variable in the global environment.  The getinverse returns the
##        inverse value stored in the cached variable i.  The 
##        makeCacheMatrix function returns a list of the available functions
##        (i.e., set, get, setinverse, getinverse) created in this function.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function (y) {
    x <<- y
    i <<- NULL   # Reset the cached inverse (i) to null if it exists
  }
  get <- function() x
  setinverse <- function(inverse) i<<-inverse #Store the inverse in the variable i in the (parent) global environment
  getinverse <- function() i
  list (set=set, get=get, 
        setinverse=setinverse,
        getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cached inverse.")
    return(i)  # Not needed with else clause variant I created.
    # NOTE: I decided to change to an else to the structure because it would 
    # remove the necessity of the return(i) used here since the i immediately
    # after the else clause would return the cached i.  Since many  
    # consider it poor structured programming style to have more than one exit
    # point (return) in a function, I wanted to demonstrate the apporiate
    # use of else too.
  }
  else {
    matrixdata <- x$get()
    i <- solve(matrixdata, ...)  # solve for the inverse using the stored matrix data
    x$setinverse(i)
  }
  ## Return a matrix that is the inverse of 'x'
  i
}
