## My functions allow you to set a matrix, get its value, and take the inverse of that matrix. 

## makecachematrix takes a matrix and puts it in a variable x. it sets the matrix through the 'set' command and gets the matrix in the variavle x 
## through the 'get' command. It also allows you to set your own matrix through the 'setinverse' function. It puts the set matrix into a variable m
## and get its value back through the 'getinverse' command

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y){
    x <<-y
    m <<- NULL
  }
  get = function() x
  setinverse = function(inv) m <<- inv
  getinverse = function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve  puts the inverse of the matrix in variable x into another variable called m. Then it checks if m is NOT empty, in which case it retrives the
## variable m and throws out its value. If m is empty, then it gets the matrix in x, puts the inverse value of x in m, sets the inverse value in variable x
## to be m, and returns m.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
