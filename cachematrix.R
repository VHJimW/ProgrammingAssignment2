## These functions cache and return the inverse
## of a matrix to save processing time in a loop scenario


## makeCacheMatrix -- returns a list of functions:
##  setmatx: sets the value of the matrix
##  getmatx: gets the value of the matrix
##  setminv: set the inverse value of the matrix
##  getminv: gets the stored inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize i to null
      i <- NULL
      
      ## define setmatx funciton
      ## assigns x & i to an enviroment outside the current environment
      setmatx <- function(y){
            x <<- y
            i <<- NULL
      }
      
      ## define functions for getmatx, setminv, & getminv
      getmatx <- function() x
      setminv <- function(matinv) i <<- matinv
      getminv <- function() i
      
      ## Create list of functions
      list(setmatx = setmatx, getmatx = getmatx, setminv = setminv, getminv = getminv)
}


## cacheSolve -- Calculates, or retireves previously calculated, the matrix inverse

cacheSolve <- function(x, ...) {
       
      ## assigns i to the inverse matrix value 
      i <- x$getminv()
      
      ## if the inverse is already calculated then return it
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      
      ## if value is not calculated then do so
      i <- solve(x$getmatx(),...)
      
      ## set new inverse to cache
      x$setminv(i)
      
      return(i)
      
}
