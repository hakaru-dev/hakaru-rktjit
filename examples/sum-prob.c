#include <stdlib.h>
#include <stdio.h>
#include <math.h>

double logSumExp2(double _a, double _b)
{
  return (_a > _b) ? (_a + log1p((expm1((_b - _a)) + 1))) : (_b + log1p((expm1((_a - _b)) + 1)));
}

double fn_a(double a_b, unsigned int b_c)
{
  printf("a_b: %f, b_c %u\n", a_b, b_c);
  double out_d;
  double _e;
  double _f;
  unsigned int _g;
  double p_h;
  _e = a_b;
  _g = b_c;
  p_h = log1p((_g - 1));
  printf("p_h: %f\n", p_h);
  _f = p_h;
  out_d = logSumExp2(_e,_f);
  printf("out_d: %f\n", out_d);
  fflush(stdout);
  return out_d;
}


