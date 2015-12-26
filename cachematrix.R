## cacheSolve uses makeCacheMatrix in order to check if the inverse of a matrix
## has been calculated and cached, if so it retrieves and returns it. otherwise
## it calculates, caches and returns it 

## makeCacheMatrix is a function that takes a matrix and has the ability to 
## access the cached inverse of that matrixnd  the abilty to calculate, cache it
## and return it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y){
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setInverse <- function (mInverse) m <<- mInverse
    getInverse <- function () m
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}

## cacheSolve is a function that takes a matrix, checks to see if the inverse 
## of that matrix has been cached, if so it gets and returns it, otherwise it 
## calculates, caches and returns it

cacheSolve <- function(x, ...) {
        
    m <- x$getInverse()
    if (!is.null(m)){
        message ("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve (data)
    x$setInverse(m)
    m
}

