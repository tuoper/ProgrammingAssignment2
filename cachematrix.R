#These functions serve the purpose to save time, so that there is no need
#to calculate new inverse matrix every time if it remains the same anyway.

makeCacheMatrix <- function(x = matrix()) { #Function takes matrix as an argument
        m <- NULL       #variable m is set null
        set <- function(y) {    #function to set matrix
                x <<- y         #sets the x from higher environment to y
                m <<- NULL      #sets the m from higher environment to null
        }
        get <- function() x     #returns the matrix
        setInverse <- function(inverse) m <<- inverse #sets the inverse matrix
        getInverse <- function() m      #returns the inversed matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        m <- x$getInverse()     #sets m to inverse matrix of x
        if(!is.null(m)) {       #if m is not null returns the cached inverse matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()         #if m is null, creates new variable data which is the x matrix
        m <- solve(data, ...)   #solves matrix x's inverse and sets it to m
        x$setInverse(m)         #caches the inverse matrix m to special matrix x
        m                       #returns the inverse matrix m
}