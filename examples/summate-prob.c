#include <stdlib.h>
#include <stdio.h>
#include <math.h>

struct array_prob {
  int size; double * data;
};

double logSumExp2(double _a, double _b)
{
  return (_a > _b) ? (_a + log1p((expm1((_b - _a)) + 1))) : (_b + log1p((expm1((_a - _b)) + 1)));
}

double fn_a(struct array_prob a_b, unsigned int b_c)
{
  printf ("a_b: [%f, %f], b_c: %u\n", (a_b.data)[0], (a_b.data)[1], b_c);
  double out_d;
  unsigned int _e;
  unsigned int _f;
  struct array_prob arr_g;
  struct array_prob summate_arr_h;
  double maxV_i;
  double sum_k;
  unsigned int maxI_j;
  unsigned int i_l;
  _e = 0;
  arr_g = a_b;
  _f = arr_g.size;
  /* ---------- Begin Summate ---------- */
  summate_arr_h.size = (_f - _e);
  summate_arr_h.data = ((double *)malloc((summate_arr_h.size * sizeof(double))));
  for (i_l = 0; i_l < summate_arr_h.size; i_l++)
  {
    double _m;
    double _n;
    struct array_prob arr_o;
    unsigned int _p;
    double _q;
    unsigned int _r;
    double p_s;
    arr_o = a_b;
    _p = i_l;
    _n = arr_o.data[_p];
    _r = b_c;
    p_s = log1p((_r - 1));
    _q = p_s;
    _m = logSumExp2(_n,_q);
    summate_arr_h.data[i_l] = _m;
    if (((maxV_i < _m) || (i_l == 0)))
    {
      maxV_i = _m;
      maxI_j = i_l;
    }
  }
  sum_k = 0.0;
  for (i_l = 0; i_l < summate_arr_h.size; i_l++)
  {
    if ((i_l != maxI_j))
     sum_k += exp((summate_arr_h.data[i_l] - maxV_i));
  }
  out_d = (maxV_i + log(sum_k));
  free(summate_arr_h.data);
  /* ----------- End Summate ----------- */
  return out_d;
}


