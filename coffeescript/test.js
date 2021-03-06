// Generated by CoffeeScript 1.8.0
(function() {
  var fibo_list, lisp_like_reverse,
    __slice = [].slice;

  console.log("Hello, World!");

  fibo_list = function(x) {
    var a, b, results, _, _i, _ref;
    results = [];
    a = 0;
    b = 1;
    for (_ = _i = 0; 0 <= x ? _i <= x : _i >= x; _ = 0 <= x ? ++_i : --_i) {
      results.push(a);
      _ref = [b, a + b], a = _ref[0], b = _ref[1];
    }
    return results;
  };

  lisp_like_reverse = function(list) {
    var inner;
    inner = function(source, dest) {
      var head, last, _i;
      if (source.length === 0) {
        return dest;
      } else {
        head = 2 <= source.length ? __slice.call(source, 0, _i = source.length - 1) : (_i = 0, []), last = source[_i++];
        dest.push(last);
        return inner(head, dest);
      }
    };
    return inner(list, []);
  };

  console.log(fibo_list(10));

  console.log(lisp_like_reverse([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]));

}).call(this);
