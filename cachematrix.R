## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(a, n) {
    b <- length(a)
    c <- sqrt(b)
    if(n == c){
        x <- matrix(a, n, n)
    }else{
        remove(x)
        stop("The imput has to be square matrix, please enter another variables")
    }
    det_x <- det(x)
    if(det_x == 0){
        remove(x)
        stop("Determinant of matrix cannot be equal to zero, please enter another variables")
    }
    s <- NULL
    set <- function(y) {
        x <<- matrix(y, n, n)
        s <- NULL
    }
    get <- function() x
    set_minv <- function(minv) s <<- minv
    get_minv <- function() s
    list(set = set, get = get, set_minv = set_minv, get_minv = get_minv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    s <- x$get_minv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    input_matrix <- x$get()
    s <- solve(input_matrix)
    x$set_minv(s)
    s
        ## Return a matrix that is the inverse of 'x'
}

x <- makeCacheMatrix(c(2,3,4,6,7,8,9,4,2), 3)
x$get()
x$set(1:10)
cacheSolve(x)