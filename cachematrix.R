## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #creates an object within the makeCacheMatrix
  set <- function(y) {     #resets what was already cached if a new argument was set
    x <<- y      # changes x in the parent environment for a value y
    m <<- NULL    # sets m to null in the parent environment
  }
  get <- function() x # gets an argument x from the makeCacheMatrix environment
  setinv <- function(solve) m <<- solve # assigns the input value to m in the parent environment
  getinv <- function() m # gets the correct value of m
  list(set = set, get = get,      #gives the list of functions defined within the function
       setinv = setinv,
       getinv = getinv)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()   # calling the getinv function on the x object       
  if(!is.null(m)) { #checks if anything was cached in m
    message("getting cached data")
    return(m) #if there was a value already calculated it returns cached data
  }
  data <- x$get()     # if there wasn't it takes the vector from the object... 
  m <- solve(data, ...) # and then it calculates the inversion
  x$setinv(m)           # and sets the calculated value into the object
  m
        ## Return a matrix that is the inverse of 'x'
}
