## The role of the two functions described below is to cache the inverse of a
## matrix thereby avoiding repeated time-consuming matrix inverse computations 
## for cases where value of the inverse of the matrix already exists

## makeCacheMatrix function will create a special "matrix" object that can cache 
## its inverse. The function returns a list of functions to set the value of the 
## matrix, get the value of the matrix, set the value of the inverse, and get the
## value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix function described above. If the inverse has already been
## calculated, the function will get the inverse from the cache and skips the
## computation otherwise it will compute the inverse of the data and sets the 
## value of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
