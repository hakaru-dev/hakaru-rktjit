#include <stdlib.h>
#include <stdio.h>
#include <math.h>

struct array_nat {
  int size; unsigned int * data;
};

unsigned int fn_a(struct array_nat a_b, unsigned int b_c)
{
  unsigned int out_d;
  unsigned int _e;
  unsigned int _f;
  struct array_nat arr_g;
  unsigned int i_h;
  unsigned int acc_i;
  _e = 0;
  arr_g = a_b;
  _f = arr_g.size;
  /* ---------- Begin Summate ---------- */
  acc_i = 0;
  for (i_h = _e; i_h < _f; i_h++)
  {
    unsigned int _j;
    unsigned int _k;
    struct array_nat arr_l;
    unsigned int _m;
    unsigned int _n;
    arr_l = a_b;
    _m = i_h;
    _k = arr_l.data[_m];
    _n = b_c;
    _j = (_n + _k);
    acc_i += _j;
    out_d = acc_i;
  }
  /* ----------- End Summate ----------- */
  return out_d;
}


