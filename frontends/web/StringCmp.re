let minimum = (a, b, c) => min(a, min(b, c));
/* Calculates Levenshtein distance: number of changes to turn
 * string s into string t. Lower score = more similar strings. */
let distance = (s, t) => {
  let m = String.length(s);
  let n = String.length(t);
  /* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t */
  let d = Array.make_matrix(m + 1, n + 1, 0);
  for (i in 0 to m) {
    /* the distance of any first string to an empty second string */
    d[i][0] = i;
  };
  for (j in 0 to n) {
    /* the distance of any second string to an empty first string */
    d[0][j] = j;
  };
  for (j in 1 to n) {
    for (i in 1 to m) {
      if (s.[i - 1] == t.[j - 1]) {
        d[i][j] = d[i - 1][j - 1];
      } else {
        /* no operation required */
        d[i][j] =
          minimum(
            d[i - 1][j] + 1, /* a deletion */
            d[i][j - 1] + 1, /* an insertion */
            d[i - 1][j - 1] + 1,
          ); /* a substitution */
      };
    };
  };
  d[m][n];
};
