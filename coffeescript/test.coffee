console.log "Hello, World!"

fibo_list = (x) ->
        results = []
        a = 0
        b = 1
        for _ in [0..x]
                results.push(a)
                [a, b] = [b, a + b]
        results

# This is stupid, you should never use it, but it's an experiment
lisp_like_reverse = (list) ->
        inner = (source, dest) ->
                if source.length == 0
                        dest
                else
                        [head..., last] = source
                        dest.push last
                        inner head, dest
        inner list, []

console.log fibo_list 10
console.log lisp_like_reverse [1..10]
