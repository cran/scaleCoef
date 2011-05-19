rankM.default <-
function(x, ...) {
    length(x)+1-rank(abs(x), ...)
}

