#include <stdlib.h>
#include <stdio.h>
#include <math.h>

struct array_prob {
  int size; double * data;
};

struct array_nat {
  int size; unsigned int * data;
};

struct dat_DDV {
  int index;
};

double logSumExp2(double _a, double _b)
{
  return (_a > _b) ? (_a + log1p((expm1((_b - _a)) + 1))) : (_b + log1p((expm1((_a - _b)) + 1)));
}

double logSumExp3(double _a, double _b, double _c)
{
  return (_a > _b) ? (_a > _c) ? (_a + log1p(((expm1((_c - _a)) + expm1((_b - _a))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2))) : (_b > _c) ? (_b + log1p(((expm1((_c - _b)) + expm1((_a - _b))) + 2))) : (_c + log1p(((expm1((_b - _c)) + expm1((_a - _c))) + 2)));
}

struct array_prob fn_a(struct array_prob topic_prior_b, struct array_prob word_prior_c,
		       struct array_nat z_d,
		       struct array_nat w_e,
		       struct array_nat doc_f,
		       unsigned int docUpdate_g)
{
  struct array_prob out_h;
  unsigned int topic_prior_size_i;
  struct array_prob arr_j;
  double topic_prior_sum_k;
  unsigned int _l;
  unsigned int _m;
  struct array_prob summate_arr_n;
  double maxV_o;
  double sum_q;
  unsigned int maxI_p;
  unsigned int i13_r;
  unsigned int word_prior_size_v;
  struct array_prob arr_w;
  double word_prior_sum_x;
  unsigned int _y;
  unsigned int _z;
  struct array_prob summate_arr_a0;
  double maxV_a1;
  double sum_a3;
  unsigned int maxI_a2;
  unsigned int i13_a4;
  double count_z_a8;
  unsigned int _a9;
  unsigned int _aa;
  unsigned int _ab;
  struct array_nat arr_ac;
  unsigned int i13_ad;
  unsigned int acc_ae;
  double p_ao;
  unsigned int zNew8_ap;
  arr_j = topic_prior_b;
  topic_prior_size_i = arr_j.size;
  _l = 0;
  _m = topic_prior_size_i;
  /* ---------- Begin Summate ---------- */
  summate_arr_n.size = (_m - _l);
  summate_arr_n.data = ((double *)malloc((summate_arr_n.size * sizeof(double))));
  for (i13_r = 0; i13_r < summate_arr_n.size; i13_r++)
  {
    double _s;
    struct array_prob arr_t;
    unsigned int _u;
    arr_t = topic_prior_b;
    _u = i13_r;
    _s = arr_t.data[_u];
    summate_arr_n.data[i13_r] = _s;
    if (((maxV_o < _s) || (i13_r == 0)))
    {
      maxV_o = _s;
      maxI_p = i13_r;
    }
  }
  sum_q = 0.0;
  for (i13_r = 0; i13_r < summate_arr_n.size; i13_r++)
  {
    if ((i13_r != maxI_p))
     sum_q += exp((summate_arr_n.data[i13_r] - maxV_o));
  }
  topic_prior_sum_k = (maxV_o + log(sum_q));
  free(summate_arr_n.data);
  /* ----------- End Summate ----------- */
  arr_w = word_prior_c;
  word_prior_size_v = arr_w.size;
  _y = 0;
  _z = word_prior_size_v;
  /* ---------- Begin Summate ---------- */
  summate_arr_a0.size = (_z - _y);
  summate_arr_a0.data = ((double *)malloc((summate_arr_a0.size * sizeof(double))));
  for (i13_a4 = 0; i13_a4 < summate_arr_a0.size; i13_a4++)
  {
    double _a5;
    struct array_prob arr_a6;
    unsigned int _a7;
    arr_a6 = word_prior_c;
    _a7 = i13_a4;
    _a5 = arr_a6.data[_a7];
    summate_arr_a0.data[i13_a4] = _a5;
    if (((maxV_a1 < _a5) || (i13_a4 == 0)))
    {
      maxV_a1 = _a5;
      maxI_a2 = i13_a4;
    }
  }
  sum_a3 = 0.0;
  for (i13_a4 = 0; i13_a4 < summate_arr_a0.size; i13_a4++)
  {
    if ((i13_a4 != maxI_a2))
     sum_a3 += exp((summate_arr_a0.data[i13_a4] - maxV_a1));
  }
  word_prior_sum_x = (maxV_a1 + log(sum_a3));
  free(summate_arr_a0.data);
  /* ----------- End Summate ----------- */
  _aa = 0;
  arr_ac = z_d;
  _ab = arr_ac.size;
  /* ---------- Begin Summate ---------- */
  acc_ae = 0;
  for (i13_ad = _aa; i13_ad < _ab; i13_ad++)
  {
    unsigned int _af;
    struct dat_DDV _ag;
    unsigned int _ah;
    unsigned int _ai;
    struct dat_DDV _aj;
    unsigned int _ak;
    unsigned int _al;
    struct array_nat arr_am;
    unsigned int _an;
    _ah = i13_ad;
    _ai = docUpdate_g;
    _ag.index = (_ah == _ai) ? 0 : 1;
     if ((_ag.index == 0))
     {
       _af = 0;
     }
     else
      if ((_ag.index == 1))
      {
        arr_am = z_d;
        _an = i13_ad;
        _ak = arr_am.data[_an];
        _al = 0;
        _aj.index = (_ak < _al) ? 0 : 1;
         if ((_aj.index == 0))
         {
           _af = 0;
         }
         else
          if ((_aj.index == 1))
          {
            _af = 1;
          }
      }
    acc_ae += _af;
    _a9 = acc_ae;
  }
  /* ----------- End Summate ----------- */
  p_ao = log1p((_a9 - 1));
  count_z_a8 = p_ao;
  out_h.size = topic_prior_size_i;
  out_h.data = ((double *)malloc((out_h.size * sizeof(double))));
  /* ----------- Create Array ----------- */
  for (zNew8_ap = 0; zNew8_ap < out_h.size; zNew8_ap++)
  {
    unsigned int count_z_znew_aq;
    unsigned int _ar;
    unsigned int _as;
    struct array_nat arr_at;
    unsigned int i13_au;
    unsigned int acc_av;
    double inner_product_b5;
    unsigned int _b6;
    unsigned int _b7;
    unsigned int i_ba;
    double t_b8;
    double c_b9;
    unsigned int _d7;
    unsigned int _d8;
    unsigned int i_db;
    double t_d9;
    double c_da;
    _ar = 0;
    arr_at = z_d;
    _as = arr_at.size;
    /* ---------- Begin Summate ---------- */
    acc_av = 0;
    for (i13_au = _ar; i13_au < _as; i13_au++)
    {
      unsigned int _aw;
      struct dat_DDV _ax;
      unsigned int _ay;
      unsigned int _az;
      struct dat_DDV _b0;
      unsigned int _b1;
      unsigned int _b2;
      struct array_nat arr_b3;
      unsigned int _b4;
      _ay = i13_au;
      _az = docUpdate_g;
      _ax.index = (_ay == _az) ? 0 : 1;
       if ((_ax.index == 0))
       {
         _aw = 0;
       }
       else
        if ((_ax.index == 1))
        {
          _b1 = zNew8_ap;
          arr_b3 = z_d;
          _b4 = i13_au;
          _b2 = arr_b3.data[_b4];
          _b0.index = (_b1 == _b2) ? 0 : 1;
           if ((_b0.index == 0))
           {
             _aw = 1;
           }
           else
            if ((_b0.index == 1))
            {
              _aw = 0;
            }
        }
      acc_av += _aw;
      count_z_znew_aq = acc_av;
    }
    /* ----------- End Summate ----------- */
    _b6 = 0;
    _b7 = topic_prior_size_i;
    /* ---------- Begin Product ---------- */
    t_b8 = 0.0;
    c_b9 = 0.0;
    for (i_ba = _b6; i_ba < _b7; i_ba++)
    {
      double x_bb;
      double y_bc;
      double z_bd;
      unsigned int upper_bound_be;
      unsigned int _bf;
      unsigned int _bg;
      struct array_nat arr_bh;
      unsigned int i13_bi;
      unsigned int acc_bj;
      double count_doc_entries_c0;
      unsigned int _c1;
      unsigned int _c2;
      unsigned int _c3;
      struct array_nat arr_c4;
      unsigned int i13_c5;
      unsigned int acc_c6;
      double p_cr;
      double partial_sum_cs;
      double _ct;
      double _cu;
      unsigned int _cv;
      unsigned int _cw;
      unsigned int i18_cz;
      double t_cx;
      double c_cy;
      _bf = 0;
      arr_bh = w_e;
      _bg = arr_bh.size;
      /* ---------- Begin Summate ---------- */
      acc_bj = 0;
      for (i13_bi = _bf; i13_bi < _bg; i13_bi++)
      {
        unsigned int _bk;
        struct dat_DDV _bl;
        unsigned int _bm;
        unsigned int _bn;
        struct array_nat arr_bo;
        unsigned int _bp;
        struct dat_DDV _bq;
        struct dat_DDV _br;
        struct dat_DDV not_bs;
        unsigned int _bt;
        unsigned int _bu;
        struct array_nat arr_bv;
        unsigned int _bw;
        struct dat_DDV _bx;
        unsigned int _by;
        unsigned int _bz;
        _bm = docUpdate_g;
        arr_bo = doc_f;
        _bp = i13_bi;
        _bn = arr_bo.data[_bp];
        _bl.index = (_bm == _bn) ? 0 : 1;
         if ((_bl.index == 0))
         {
           arr_bv = w_e;
           _bw = i13_bi;
           _bt = arr_bv.data[_bw];
           _bu = 0;
           not_bs.index = (_bt < _bu) ? 0 : 1;
           not_bs.index = (not_bs.index == 1) ? 0 : 1;
           _by = i_ba;
           _bz = zNew8_ap;
           _bx.index = (_by == _bz) ? 0 : 1;
           _bq.index = (!(_bx.index == _br.index));
            if ((_bq.index == 0))
            {
              _bk = 1;
            }
            else
             if ((_bq.index == 1))
             {
               _bk = 0;
             }
         }
         else
          if ((_bl.index == 1))
          {
            _bk = 0;
          }
        acc_bj += _bk;
        upper_bound_be = acc_bj;
      }
      /* ----------- End Summate ----------- */
      _c2 = 0;
      arr_c4 = w_e;
      _c3 = arr_c4.size;
      /* ---------- Begin Summate ---------- */
      acc_c6 = 0;
      for (i13_c5 = _c2; i13_c5 < _c3; i13_c5++)
      {
        unsigned int _c7;
        struct dat_DDV _c8;
        unsigned int _c9;
        unsigned int _ca;
        struct array_nat arr_cb;
        unsigned int _cc;
        struct dat_DDV _cd;
        struct dat_DDV _ce;
        struct dat_DDV not_cf;
        unsigned int _cg;
        unsigned int _ch;
        struct array_nat arr_ci;
        unsigned int _cj;
        struct dat_DDV _ck;
        unsigned int _cl;
        unsigned int _cm;
        struct array_nat arr_cn;
        unsigned int _co;
        struct array_nat arr_cp;
        unsigned int _cq;
        arr_cb = doc_f;
        _cc = i13_c5;
        _c9 = arr_cb.data[_cc];
        _ca = docUpdate_g;
        _c8.index = (_c9 == _ca) ? 0 : 1;
         if ((_c8.index == 0))
         {
           _c7 = 0;
         }
         else
          if ((_c8.index == 1))
          {
            arr_ci = w_e;
            _cj = i13_c5;
            _cg = arr_ci.data[_cj];
            _ch = 0;
            not_cf.index = (_cg < _ch) ? 0 : 1;
            not_cf.index = (not_cf.index == 1) ? 0 : 1;
            _cl = i_ba;
            arr_cn = z_d;
            arr_cp = doc_f;
            _cq = i13_c5;
            _co = arr_cp.data[_cq];
            _cm = arr_cn.data[_co];
            _ck.index = (_cl == _cm) ? 0 : 1;
            _cd.index = (!(_ck.index == _ce.index));
             if ((_cd.index == 0))
             {
               _c7 = 1;
             }
             else
              if ((_cd.index == 1))
              {
                _c7 = 0;
              }
          }
        acc_c6 += _c7;
        _c1 = acc_c6;
      }
      /* ----------- End Summate ----------- */
      p_cr = log1p((_c1 - 1));
      count_doc_entries_c0 = p_cr;
      _ct = count_doc_entries_c0;
      _cu = word_prior_sum_x;
      partial_sum_cs = logSumExp2(_ct,_cu);
      _cv = 0;
      _cw = upper_bound_be;
      /* ---------- Begin Product ---------- */
      t_cx = 0.0;
      c_cy = 0.0;
      for (i18_cz = _cv; i18_cz < _cw; i18_cz++)
      {
        double x_d0;
        double y_d1;
        double z_d2;
        double _d3;
        double _d4;
        unsigned int _d5;
        double p_d6;
        _d3 = partial_sum_cs;
        _d5 = i18_cz;
        p_d6 = log1p((_d5 - 1));
        _d4 = p_d6;
        x_d0 = logSumExp2(_d3,_d4);
        y_d1 = (x_d0 - c_cy);
        z_d2 = (t_cx + y_d1);
        c_cy = ((z_d2 - t_cx) - y_d1);
        t_cx = z_d2;
      }
      x_bb = t_cx;
      /* ----------- End Product ----------- */
      y_bc = (x_bb - c_b9);
      z_bd = (t_b8 + y_bc);
      c_b9 = ((z_bd - t_b8) - y_bc);
      t_b8 = z_bd;
    }
    inner_product_b5 = t_b8;
    /* ----------- End Product ----------- */
    _d7 = 0;
    _d8 = topic_prior_size_i;
    /* ---------- Begin Product ---------- */
    t_d9 = 0.0;
    c_da = 0.0;
    for (i_db = _d7; i_db < _d8; i_db++)
    {
      double x_dc;
      double y_dd;
      double z_de;
      unsigned int _df;
      unsigned int _dg;
      unsigned int i18_dj;
      double t_dh;
      double c_di;
      _df = 0;
      _dg = word_prior_size_v;
      /* ---------- Begin Product ---------- */
      t_dh = 0.0;
      c_di = 0.0;
      for (i18_dj = _df; i18_dj < _dg; i18_dj++)
      {
        double x_dk;
        double y_dl;
        double z_dm;
        unsigned int inner_sum_dn;
        unsigned int _do;
        unsigned int _dp;
        struct array_nat arr_dq;
        unsigned int i13_dr;
        unsigned int acc_ds;
        unsigned int _ec;
        unsigned int _ed;
        unsigned int _ee;
        unsigned int _ef;
        struct array_nat arr_eg;
        unsigned int i13_eh;
        unsigned int acc_ei;
        unsigned int j_f0;
        double t_ey;
        double c_ez;
        _do = 0;
        arr_dq = w_e;
        _dp = arr_dq.size;
        /* ---------- Begin Summate ---------- */
        acc_ds = 0;
        for (i13_dr = _do; i13_dr < _dp; i13_dr++)
        {
          unsigned int _dt;
          struct dat_DDV _du;
          unsigned int _dv;
          unsigned int _dw;
          struct array_nat arr_dx;
          unsigned int _dy;
          struct dat_DDV _dz;
          struct dat_DDV _e0;
          unsigned int _e1;
          unsigned int _e2;
          struct array_nat arr_e3;
          unsigned int _e4;
          struct array_nat arr_e5;
          unsigned int _e6;
          struct dat_DDV _e7;
          unsigned int _e8;
          unsigned int _e9;
          struct array_nat arr_ea;
          unsigned int _eb;
          arr_dx = doc_f;
          _dy = i13_dr;
          _dv = arr_dx.data[_dy];
          _dw = docUpdate_g;
          _du.index = (_dv == _dw) ? 0 : 1;
           if ((_du.index == 0))
           {
             _dt = 0;
           }
           else
            if ((_du.index == 1))
            {
              _e1 = i_db;
              arr_e3 = z_d;
              arr_e5 = doc_f;
              _e6 = i13_dr;
              _e4 = arr_e5.data[_e6];
              _e2 = arr_e3.data[_e4];
              _e0.index = (_e1 == _e2) ? 0 : 1;
              _e8 = i18_dj;
              arr_ea = w_e;
              _eb = i13_dr;
              _e9 = arr_ea.data[_eb];
              _e7.index = (_e8 == _e9) ? 0 : 1;
              _dz.index = (!(_e7.index == _e0.index));
               if ((_dz.index == 0))
               {
                 _dt = 1;
               }
               else
                if ((_dz.index == 1))
                {
                  _dt = 0;
                }
            }
          acc_ds += _dt;
          inner_sum_dn = acc_ds;
        }
        /* ----------- End Summate ----------- */
        _ec = 0;
        _ee = 0;
        arr_eg = w_e;
        _ef = arr_eg.size;
        /* ---------- Begin Summate ---------- */
        acc_ei = 0;
        for (i13_eh = _ee; i13_eh < _ef; i13_eh++)
        {
          unsigned int _ej;
          struct dat_DDV _ek;
          unsigned int _el;
          unsigned int _em;
          struct array_nat arr_en;
          unsigned int _eo;
          struct dat_DDV _ep;
          struct dat_DDV _eq;
          unsigned int _er;
          unsigned int _es;
          struct dat_DDV _et;
          unsigned int _eu;
          unsigned int _ev;
          struct array_nat arr_ew;
          unsigned int _ex;
          _el = docUpdate_g;
          arr_en = doc_f;
          _eo = i13_eh;
          _em = arr_en.data[_eo];
          _ek.index = (_el == _em) ? 0 : 1;
           if ((_ek.index == 0))
           {
             _er = i_db;
             _es = zNew8_ap;
             _eq.index = (_er == _es) ? 0 : 1;
             _eu = i18_dj;
             arr_ew = w_e;
             _ex = i13_eh;
             _ev = arr_ew.data[_ex];
             _et.index = (_eu == _ev) ? 0 : 1;
             _ep.index = (!(_et.index == _eq.index));
              if ((_ep.index == 0))
              {
                _ej = 1;
              }
              else
               if ((_ep.index == 1))
               {
                 _ej = 0;
               }
           }
           else
            if ((_ek.index == 1))
            {
              _ej = 0;
            }
          acc_ei += _ej;
          _ed = acc_ei;
        }
        /* ----------- End Summate ----------- */
        /* ---------- Begin Product ---------- */
        t_ey = 0.0;
        c_ez = 0.0;
        for (j_f0 = _ec; j_f0 < _ed; j_f0++)
        {
          double x_f1;
          double y_f2;
          double z_f3;
          double _f4;
          double _f5;
          unsigned int _f6;
          double p_f7;
          double _f8;
          unsigned int _f9;
          double p_fa;
          double _fb;
          struct array_prob arr_fc;
          unsigned int _fd;
          double _fe;
          double _ff;
          unsigned int _fg;
          double p_fh;
          double _fi;
          struct array_prob arr_fj;
          unsigned int _fk;
          double _fl;
          double _fm;
          double _fn;
          double _fo;
          double _fp;
          double _fq;
          _f6 = inner_sum_dn;
          p_f7 = log1p((_f6 - 1));
          _f5 = p_f7;
          _f9 = j_f0;
          p_fa = log1p((_f9 - 1));
          _f8 = p_fa;
          arr_fc = word_prior_c;
          _fd = i18_dj;
          _fb = arr_fc.data[_fd];
          _f4 = logSumExp3(_f5,_f8,_fb);
          _fg = count_z_znew_aq;
          p_fh = log1p((_fg - 1));
          _ff = p_fh;
          arr_fj = topic_prior_b;
          _fk = zNew8_ap;
          _fi = arr_fj.data[_fk];
          _fe = logSumExp2(_ff,_fi);
          _fn = count_z_a8;
          _fo = topic_prior_sum_k;
          _fm = logSumExp2(_fn,_fo);
          _fl = (-_fm);
          _fq = inner_product_b5;
          _fp = (-_fq);
          x_f1 = (_fe + (_fl + (_fp + _f4)));
          y_f2 = (x_f1 - c_ez);
          z_f3 = (t_ey + y_f2);
          c_ez = ((z_f3 - t_ey) - y_f2);
          t_ey = z_f3;
        }
        x_dk = t_ey;
        /* ----------- End Product ----------- */
        y_dl = (x_dk - c_di);
        z_dm = (t_dh + y_dl);
        c_di = ((z_dm - t_dh) - y_dl);
        t_dh = z_dm;
      }
      x_dc = t_dh;
      /* ----------- End Product ----------- */
      y_dd = (x_dc - c_da);
      z_de = (t_d9 + y_dd);
      c_da = ((z_de - t_d9) - y_dd);
      t_d9 = z_de;
    }
    out_h.data[zNew8_ap] = t_d9;
    /* ----------- End Product ----------- */
  }
  return out_h;
}


