## Since calcualting the inverse of amatrix is costly,
## These functions create an object that caches the inverse
## af a given matrix

## returns a list of 4 fuctions
## get teh matrix
## set teh matrix
## get the inverse, initialized to NULL
## set the inverse

makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## retrieves teh inverse of the stored matrix
## if the cahed inverse is null, calculates and
## atores the inverse of the stored matrix

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
