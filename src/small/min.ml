qual LT(x): x < 3;;

? let min = fun n -> fun m -> if n < m then n else m in min 2 3;;

# We should think pretty hard about _why_ this one works.  (I mean,
# it probably only works exactly this way for only a single callsite.
# If there were more, we'd probably need to juice up the qualifiers
# a fair bit to make it continue to work.)
