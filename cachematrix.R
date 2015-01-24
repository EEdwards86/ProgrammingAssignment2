##Programming Assignment 2

#Function makeCacheMAtrix
makeCacheMatrix <- function(x = matrix()){
  #create empty/null cache for inverted matrix
  m <- NULL
  #to use in future, where y is the alternative matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse
  setSolve <- function(mean) m <<- inverse
  #get the value of the inverse
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}
  
# Second Function cacheSolve  
cacheSolve <- function(x, ...) {
  #if matrix hasn't changed, retrieving inverse from cache
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #If it doesn't find a cached value, it finds the inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
}  
