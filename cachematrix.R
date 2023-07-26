## These functions is used to calculate inverse matrix and store data to cache
## from where it can be called

## This function creates a matrix which can be inverted (squared and det > 0)
## It also creates a list of fuctions get, set_minv and get_minv, which are
## used in a fuction "cacheSolve"

makeCacheMatrix <- function(a, n) {
    ## Checks if matrix is squared and create matrix x
    b <- length(a)
    c <- sqrt(b)
    if(n == c){
        x <- matrix(a, n, n)
    }else{
        stop("The imput has to be square matrix, please enter another variables")
    }
    ## Checks if det > 0
    det_x <- det(x)
    if(det_x == 0){
        stop("Determinant of matrix cannot be equal to zero, please enter another variables")
    }
    ## Declaration of variable s and fuctions used in "cacheSolve"
    s <- NULL
    get <- function() x
    set_minv <- function(minv) s <<- minv
    get_minv <- function() s
    list(get = get, set_minv = set_minv, get_minv = get_minv)
}

## This function checks if there is inverted matrix, which is already stored 
## in cache. If TRUE, it returns stored matrix. If FALSE it calculates
## matrix inversion


cacheSolve <- function(x, ...) {
    ## Checks if inverted matrix is already stored in cache and returns the value
    s <- x$get_minv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## Calculation of matrix inversion
    input_matrix <- x$get()
    s <- solve(input_matrix)
    x$set_minv(s)
    s
        ## Return a matrix that is the inverse of 'x'
}