## input argument is a matrix of 3 rows and columns which includes 9  random numbers
##from 1 to 100
#then set solved value "s" as NULL
#then I changed mean function to solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##as earlier here I have changed "mean" to solve and "m" to "s"

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  mat <- x$get()
  s <- solve(mat, ...)
  x$setInverse(s)
  s

}
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
