## These pair of functions work as a whole.
## The first, makeCacheMatrix() creates a object of class = list of 4.
## This object contains getters and setters of the values inside the list.
## These values are not stored inside the function.
## Instead they are stored in a parent enviroment (<<-).
## These values are a matrix and the inverse of such matrix + getter and setter.
## The second function, cacheSolve(), use as argument the object created by 
##      function makeCacheMatrix().
## Then check if there is already a computed inverse.
## If it is, print the inverse. If not, calulate the inverse, set it, and display.


## This function is in cahrge of creating a new list, a new kind of object.
## This object contains a matrix, the inverse of the matrix(if already calculated)
##      and the setter and getter function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the inverse is already calculated. 
## If not, it calculates the inverse, set it using the setter.
## If yes, it uses the getter function to print the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
