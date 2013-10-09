#define __USE_FIXED_PROTOTYPES__

/*********************************************************************

			       外部関数

*********************************************************************/

#ifdef __tsu__

#include "libtsu/libtsu.h"

#define NULL 0

#define pi 3.14159265358979323846

#define fabs __builtin_fabs

extern "C" {
  extern double fhalf (double) __attribute__ ((const)); // 半分
  extern double inv (double) __attribute__ ((const)); // 逆数
  extern double sqrt (double) __attribute__ ((const)); // 平方根
  extern double sqrtinv (double) __attribute__ ((const)); // 平方根の逆数
  extern double cos (double) __attribute__ ((const)); // 余弦
  extern double atan (double) __attribute__ ((const)); // 正接の逆関数
  extern double fmod20 (double) __attribute__ ((const)); // 剰余
  extern double frac (double) __attribute__ ((const)); // 小数部分
}

#else // __tsu__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define pi M_PI

static double sio_sync_read_double (void) {
  double d;
  scanf ("%lf", & d);
  return d;
}

static int sio_writable (void) {
  return random () % 2;
}

static void sio_write (int x) {
  putchar (x);
}

static double fhalf (double x) {
  return x / 2.0;
}

static double inv (double x) {
  return 1.0 / x;
}

static double sqrtinv (double x) {
  return 1.0 / sqrt (x);
}

static double fmod20 (double x) {
  return (x - 20.0 * (floor (x / 20.0)));
}

static double frac (double x) {
  return (x - floor (x));
}

#endif // __tsu__

/*********************************************************************

			   基本的な読み書き

*********************************************************************/

static double read_double (void) {
  return sio_sync_read_double ();
}

static int read_int (void) {
  return (int) read_double ();
}

#define write_queue_size 256 // サイズ
static unsigned write_queue_num = 0; // 現在の数
static int write_queue [write_queue_size]; // キュー
static int * write_queue_head = NULL; // 先頭
static int * write_queue_tail = NULL; // 末尾

static void write_init (void) {
  write_queue_head = write_queue;
  write_queue_tail = write_queue;
}

// 書き出しを試みる

static void write_flush (void) {
  if (write_queue_num != 0 && sio_writable ()) {
    sio_write (* write_queue_head);
    write_queue_num --;
    write_queue_head ++;
    
    if (write_queue_head == write_queue + write_queue_size) {
      write_queue_head = write_queue;
    }
  }
}

// 書き込みキューをすべてフラッシュする

static void write_flush_all (void) {
  while (write_queue_num != 0) {
    write_flush ();
  }
}

// 1オクテットを書き込みキューに入れる

static void write_octet (int x) {
  while (write_queue_num >= write_queue_size) {
    write_flush ();
  }

  * write_queue_tail = x;
  write_queue_num ++;
  write_queue_tail ++;

  if (write_queue_tail == write_queue + write_queue_size) {
    write_queue_tail = write_queue;
  }
}

// 整数を文字列にして書き出す

static void write_int (int x) {
  {
    int tmp = 0;
    
    while (x >= 10000) {
      tmp ++;
      x -= 10000;
    }

    if (tmp != 0) {
      write_octet (tmp + '0');
    }
  }

  {
    int tmp = 0;
    
    while (x >= 1000) {
      tmp ++;
      x -= 1000;
    }
    
    if (tmp != 0) {
      write_octet (tmp + '0');
    }
  }

  {
    int tmp = 0;
    
    while (x >= 100) {
      tmp ++;
      x -= 100;
    }
    
    if (tmp != 0) {
      write_octet (tmp + '0');
    }
  }

  {
    int tmp = 0;
    
    while (x >= 10) {
      tmp ++;
      x -= 10;
    }
    
    if (tmp != 0) {
      write_octet (tmp + '0');
    }
  }

  {
    int tmp = 0;
    
    while (x >= 1) {
      tmp ++;
      x -= 1;
    }
    
    if (tmp != 0) {
      write_octet (tmp + '0');
    }
  }
}

/*********************************************************************

			       型の定義

*********************************************************************/

typedef unsigned bool_t; // 真偽値（0なら偽、それ以外は真）
typedef unsigned card_t; // 物の個数

typedef double deg_t; // 角度（度）
typedef double rad_t; // 角度（ラジアン）

// 距離

typedef double dist_t;

#define dist_max 1e+15 // 最大の長さとして用いる初期値
#define dist_far 1e+14 // 無視できるほどの遠さ
#define dist_back -0.1 // 後方かどうかの判定基準
#define dist_delta 0.01 // 誤差を回避するための微調整

// 画素の明るさ

typedef double hil_t;

#define hil_min 0.0
#define hil_max 256.0

// 三角関数の値で表現された角度

typedef struct {
  double cos;
  double sin;
} trig_t;

// ピクセル値

typedef struct {
  hil_t r;
  hil_t g;
  hil_t b;
} pixel_t;

static const pixel_t black_pixel = { hil_min, hil_min, hil_min };
static const pixel_t white_pixel = { hil_max, hil_max, hil_max };

// 3次元ベクトル

typedef struct {
  dist_t x;
  dist_t y;
  dist_t z;
} vec_t;

// 3次元空間における回転

// 元のプログラムでは人間にわかりやすいよう、パラメータが
// 3つあるところもあるが、数学的には2つで十分

typedef struct {
  trig_t tx; // x軸を回転軸として回転
  trig_t ty; // y軸を回転軸として回転
} rot2_t;

typedef struct {
  trig_t tx; // x軸を回転軸として回転
  trig_t ty; // y軸を回転軸として回転
  trig_t tz; // z軸を回転軸として回転
} rot3_t;

// 出力画像に関する情報

// スクリーンが正方形なので、出力画像も正方形に限る
// 走査を開始する行は0に固定

typedef struct {
  card_t size; // 一辺の長さ
  card_t half; // 一辺の長さの半分
} output_t;

// スクリーンに関する情報

#define screen_size 128.0 // 一辺の長さ
#define screen_orig_view_z -200.0 // 変換前の視点（z軸上に固定）

typedef struct {
  vec_t pos; // 位置
  rot2_t dir; // 向き
  vec_t rot_view; // 回転後の視点
  vec_t trans_view; // 回転・平行移動後の視点
} screen_t;

// 光源に関する情報

typedef struct {
  rot2_t dir; // 角度であらわした方向
  vec_t vec; // 単位ベクトルであらわした方向
  double str; // 強さ
} light_t;

// テクスチュア番号

typedef enum {
  tex_plain = 0, // 無地
  tex_checker = 1, // チェッカ
  tex_stripe = 2, // ストライプ
  tex_circle = 3, // 同心円
  tex_spot = 4 // 斑点
} tex_t;

// プリミティブの種類

typedef enum {
  type_rect = 1, // 直方体
  type_plane = 2, // 平面
  type_quad = 3, // 2次曲面
  type_cone = 4 // 錘
} type_t;

// 表面の特性

typedef enum {
  surf_rand = 1, // 乱反射
  surf_mirror = 2 // 鏡面
} surf_t;

// 極性

// 内部か外部かの2値なので、内部なら真、外部なら偽として
// 真偽値であらわす

typedef bool_t pol_t;

// プリミティブ

#define max_prim 64 // プリミティブの数の最大値

typedef struct {
  type_t type; // 種類
  pol_t pol; // 極性

  vec_t param; // パラメータ
  vec_t offset; // オフセット

  tex_t tex; // テクスチュア
  pixel_t color; // 色

  surf_t surf; // 表面の特性
  double ref; // 乱反射率（surfが乱反射のときは1）
  hil_t hil; // ハイライト強度

  bool_t rot; // 回転の有無（2次曲面のみサポート）
  rot3_t rot3; // 回転角（rotが偽のときは無意味）
  vec_t cross; // クロスタームの係数（rotが偽のときは無意味）
} prim_t;

static prim_t * const noprim = NULL;

// ANDプリミティブ

#define max_andprim 32 // ANDプリミティブの数の最大値

typedef struct {
  prim_t * prims [max_prim];
} andprim_t;

static andprim_t * const noandprim = NULL;

// ORプリミティブ

#define max_orprim 32 // ORプリミティブの数の最大値

typedef struct {
  prim_t * range;
  andprim_t * andprims [max_andprim];
} orprim_t;

static orprim_t * const noorprim = NULL;

// 直方体の面を指定

typedef enum {
  rectsurf_x = 1, // yz平面に平行な面
  rectsurf_y = 2, // zx平面に平行な面
  rectsurf_z = 3 // xy平面に平行な面
} rectsurf_t;

/*********************************************************************

			   種々の細かな関数

*********************************************************************/

// 排他的論理和

inline bool_t my_xor (bool_t x, bool_t y) {
  return (x && ! y) || (! x && y);
}

// 浮動小数を2倍

inline static double fdbl (double x) {
  return x + x;
}

// 浮動小数を2乗

inline static double fsq (double x) {
  return x * x;
}

// 浮動小数を3乗

inline static double fcube (double x) {
  return fsq (x) * x;
}

// 浮動小数を4乗

inline static double fquad (double x) {
  return fsq (fsq (x));
}

// cosとsinを計算

inline static void cossin (trig_t * trig, rad_t rad) {
  trig -> cos = cos (rad);
  trig -> sin = sqrt (1 - fsq (trig -> cos));

  if (rad < 0.0) {
    trig -> sin = - trig -> sin;
  }
}

// 浮動小数の絶対値を逆数の2乗に変換（符号はそのまま）

static double inv2 (double x) {
  double tmp;

  tmp = fsq (x);

  if (tmp != 0.0) {
    tmp = inv (tmp);

    if (x < 0.0) {
      tmp = - tmp;
    }
  }

  return tmp;
}

// 度をラジアンに変換

inline static rad_t deg_to_rad (deg_t deg) {
  return deg * (pi / 180.0);
}

// 入力データにおいて終了をあらわす整数を判定

// -1が終了をあらわすので、1を加えて0だったら終了、
// 0でなかったら継続

inline static bool_t cont (unsigned num) {
  return (++ num);
}

// ベクトルを交換

static void swap_vec (vec_t * x, vec_t * y) {
  vec_t tmp;

  tmp = * x;
  * x = * y;
  * y = tmp;
}

// ベクトルを反転

inline static void neg_vec (vec_t * vec) {
  vec -> x = - vec -> x;
  vec -> y = - vec -> y;
  vec -> z = - vec -> z;
}

inline static void neg_vec_2 (vec_t * dest, const vec_t * src) {
  dest -> x = - src -> x;
  dest -> y = - src -> y;
  dest -> z = - src -> z;
}

// ベクトルを半分にする

inline static void half_vec (vec_t * vec) {
  vec -> x = fhalf (vec -> x);
  vec -> y = fhalf (vec -> y);
  vec -> z = fhalf (vec -> z);
}

// ベクトルをスカラー倍

inline static void scale_vec_3 (vec_t * dest,
				const vec_t * src,
				double scalar) {
  dest -> x = scalar * src -> x;
  dest -> y = scalar * src -> y;
  dest -> z = scalar * src -> z;
}

// ベクトルの和を計算

inline static void add_vec (vec_t * dest, const vec_t * src) {
  dest -> x += src -> x;
  dest -> y += src -> y;
  dest -> z += src -> z;
}

// ベクトルの差を計算

inline static void sub_vec (vec_t * dest,
			    const vec_t * src) {
  dest -> x -= src -> x;
  dest -> y -= src -> y;
  dest -> z -= src -> z;
}

inline static void sub_vec_3 (vec_t * dest,
			      const vec_t * src1,
			      const vec_t * src2) {
  dest -> x = src1 -> x - src2 -> x;
  dest -> y = src1 -> y - src2 -> y;
  dest -> z = src1 -> z - src2 -> z;
}

// ベクトルの要素をかける

inline static void mul_vec_3 (vec_t * dest,
			      const vec_t * src1,
			      const vec_t * src2) {
  dest -> x = src1 -> x * src2 -> x;
  dest -> y = src1 -> y * src2 -> y;
  dest -> z = src1 -> z * src2 -> z;
}

// ベクトルの要素の平方根をとってかける

inline static void mul_sqrt_vec (vec_t * dest,
				 const vec_t * src) {
  dest -> x *= sqrt (src -> x);
  dest -> y *= sqrt (src -> y);
  dest -> z *= sqrt (src -> z);
}

// ベクトルの内積

static double inprod_vec (const vec_t * vec1, const vec_t * vec2) {
  double tmp;

  tmp = vec1 -> x * vec2 -> x;
  tmp += vec1 -> y * vec2 -> y;
  tmp += vec1 -> z * vec2 -> z;

  return tmp;
}

// ベクトルを正規化

static void norm_vec (vec_t * vec) {
  double tmp;

  tmp = sqrtinv (fsq (vec -> x) + fsq (vec -> y) + fsq (vec -> z));

  vec -> x *= tmp;
  vec -> y *= tmp;
  vec -> z *= tmp;
}

// ベクトルを巡回しながらかける

static double rotprod_vec (const vec_t * a,
			   const vec_t * b) {
  double tmp;

  tmp = a -> x * b -> y * b -> z;
  tmp += a -> y * b -> z * b -> x;
  tmp += a -> z * b -> x * b -> y;

  return tmp;
}

// ベクトルを2重にかける

static double dblprod_vec (const vec_t * a, const vec_t * b) {
  double tmp;

  tmp = a -> x * fsq (b -> x);
  tmp += a -> y * fsq (b -> y);
  tmp += a -> z * fsq (b -> z);

  return tmp;
}

// ベクトルをねじってかける

static void twistprod_vec (vec_t * dest,
			   const vec_t * src1,
			   const vec_t * src2) {
  dest -> x = src1 -> y * src2 -> z + src1 -> z * src2 -> y;
  dest -> y = src1 -> z * src2 -> x + src1 -> x * src2 -> z;
  dest -> z = src1 -> x * src2 -> y + src1 -> y * src2 -> x;
}

/*********************************************************************

			     補助的な関数

*********************************************************************/

// 2次曲面の回転において、回転行列を計算

static void rotate_prim_1 (double r [3] [3],
			   const rot3_t * rot3) {
  double cc, cs, sc, ss;

  cc = rot3 -> tx . cos * rot3 -> ty . cos;
  cs = rot3 -> tx . cos * rot3 -> ty . sin;
  sc = rot3 -> tx . sin * rot3 -> ty . cos;
  ss = rot3 -> tx . sin * rot3 -> ty . sin;

  r [0] [0] = rot3 -> ty . cos * rot3 -> tz . cos;
  r [0] [1] = ss * rot3 -> tz . cos;
  r [0] [1] -= rot3 -> tx . cos * rot3 -> tz . sin;
  r [0] [2] = cs * rot3 -> tz . cos;
  r [0] [2] += rot3 -> tx . sin * rot3 -> tz . sin;
  r [1] [0] = rot3 -> ty . cos * rot3 -> tz . sin;
  r [1] [1] = ss * rot3 -> tz . sin;
  r [1] [1] += rot3 -> tx . cos * rot3 -> tz . cos;
  r [1] [2] = cs * rot3 -> tz . sin;
  r [1] [2] -= rot3 -> tx . sin * rot3 -> tz . cos;
  r [2] [0] = - rot3 -> ty . sin;
  r [2] [1] = sc;
  r [2] [2] = cc;
}

// 2次曲面の回転において、クロスタームを計算

static void rotate_prim_2 (double r [3] [3],
			   const vec_t * param,
			   vec_t * cross) {
  cross -> x = param -> x * r [0] [1] * r [0] [2];
  cross -> x += param -> y * r [1] [1] * r [1] [2];
  cross -> x += param -> z * r [2] [1] * r [2] [2];
  cross -> x = fdbl (cross -> x);
  cross -> y = param -> x * r [0] [2] * r [0] [0];
  cross -> y += param -> y * r [1] [2] * r [1] [0];
  cross -> y += param -> z * r [2] [2] * r [2] [0];
  cross -> y = fdbl (cross -> y);
  cross -> z = param -> x * r [0] [0] * r [0] [1];
  cross -> z += param -> y * r [1] [0] * r [1] [1];
  cross -> z += param -> z * r [2] [0] * r [2] [1];
  cross -> z = fdbl (cross -> z);
}

// 2次曲面の回転において、2乗の項の係数を計算

static void rotate_prim_3 (double r [3] [3],
			   vec_t * param) {
  double x, y, z;

  x = fsq (r [0] [0]) * param -> x;
  x += fsq (r [1] [0]) * param -> y;
  x += fsq (r [2] [0]) * param -> z;

  y = fsq (r [0] [1]) * param -> x;
  y += fsq (r [1] [1]) * param -> y;
  y += fsq (r [2] [1]) * param -> z;

  z = fsq (r [0] [2]) * param -> x;
  z += fsq (r [1] [2]) * param -> y;
  z += fsq (r [2] [2]) * param -> z;

  param -> x = x;
  param -> y = y;
  param -> z = z;
}

// 2次曲面の回転を処理

static void rotate_prim (prim_t * prim) {
  double r [3] [3];

  rotate_prim_1 (r, & prim -> rot3);
  rotate_prim_2 (r, & prim -> param, & prim -> cross);
  rotate_prim_3 (r, & prim -> param);
}

/*********************************************************************

			    グローバル変数

*********************************************************************/

static output_t output; // 出力画像の情報
static screen_t screen; // スクリーンの情報
static light_t light; // 光源の情報

static dist_t dot; // スクリーン上で出力画像1ドットに当たる正方形の一辺の長さ

static prim_t prims [max_prim]; // プリミティブの配列
static card_t num_prims = 0; // 読み込んだプリミティブの数

static andprim_t andprims [max_andprim]; // ANDプリミティブの配列
static card_t num_andprims = 0; // 読み込んだANDプリミティブの数

static orprim_t orprims [max_orprim]; // ORプリミティブの配列
static card_t num_orprims = 0; // 読み込んだORプリミティブの数

static double energy; // 現在の光線のエネルギー
static double bright; // 現在の明るさ
static pixel_t pixel; // 現在計算中のピクセル値

static vec_t sightline; // 現在の視線方向の単位ベクトル
static vec_t viewpoint; // 現在の視点

static vec_t intsec_cand; // intsec_pointの候補
static vec_t intsec_point; // 現在の交点
static const prim_t * intsec_prim; // 交点のあるプリミティブ
static rectsurf_t intsec_rectsurf; // どの面で交わったか（直方体の場合）
static vec_t intsec_normal; // 交点における法線

/*********************************************************************

		      データの読み書きを行う関数

*********************************************************************/

// ppmのヘッダを書く

static void write_header (void) {
  write_octet ('P');
  write_octet ('6');
  write_octet ('\n');

  write_int (output . size);
  write_octet (' ');
  write_int (output . size);
  write_octet ('\n');

  write_octet ('2');
  write_octet ('5');
  write_octet ('5');
  write_octet ('\n');
}

// 画素を書き出す

static void write_hil (hil_t hil) {
  int x;

  x = (int) hil;

  if (x < 0) {
    x = 0;
  } else if (x > 255) {
    x = 255;
  }

  write_octet (x);
}

// ピクセル値を書き出す

static void write_pixel (pixel_t * p) {
  write_hil (p -> r);
  write_hil (p -> g);
  write_hil (p -> b);
}

// ピクセル値を読み込む

static void read_pixel (pixel_t * p) {
  p -> r = read_double ();
  p -> g = read_double ();
  p -> b = read_double ();
}

// 3次元ベクトルを読み込む

static void read_vec (vec_t * vec) {
  vec -> x = read_double ();
  vec -> y = read_double ();
  vec -> z = read_double ();
}

// 3次元空間における回転を読み込む

static void read_rot2 (rot2_t * rot2) {
  cossin (& rot2 -> tx, deg_to_rad (read_double ()));
  cossin (& rot2 -> ty, deg_to_rad (read_double ()));
}

static void read_rot3 (rot3_t * rot3) {
  cossin (& rot3 -> tx, deg_to_rad (read_double ()));
  cossin (& rot3 -> ty, deg_to_rad (read_double ()));
  cossin (& rot3 -> tz, deg_to_rad (read_double ()));
}

// 出力画像に関する情報を読み込む

static void read_output (output_t * o) {
  o -> size = read_int ();
  o -> half = o -> size / 2;
}

// スクリーンに関する情報を読み込む

static void read_screen (screen_t * s) {
  read_vec (& s -> pos);
  read_rot2 (& s -> dir);

  s -> rot_view . x = screen_orig_view_z
                    * s -> dir . tx . cos
                    * s -> dir . ty . sin;
  s -> trans_view . x = s -> rot_view . x
                      + s -> pos . x;
  s -> rot_view . y = - screen_orig_view_z
                    * s -> dir . tx . sin;
  s -> trans_view . y = s -> rot_view . y
                      + s -> pos . y;
  s -> rot_view . z = screen_orig_view_z
                    * s -> dir . tx . cos
                    * s -> dir . ty . cos;
  s -> trans_view . z = s -> rot_view . z
                      + s -> pos . z;
}

// 光源に関する情報を読み込む

static void read_light (light_t * l) {
  (void) read_double (); // 光源の数は1に固定
  read_rot2 (& l -> dir);
  l -> str = read_double ();

  l -> vec . x = l -> dir . tx . cos * l -> dir . ty . sin;
  l -> vec . y = - l -> dir . tx . sin;
  l -> vec . z = l -> dir . tx . cos * l -> dir . ty . cos;
}

// プリミティブのパラメータを読み込む

static void read_param (vec_t * param, type_t type) {
  read_vec (param);

  switch (type) {
  case type_plane: // 平面は法線ベクトルを正規化
    norm_vec (param);
    return;
  case type_quad: // 2次曲面はパラメータを変換
    param -> x = inv2 (param -> x);
    param -> y = inv2 (param -> y);
    param -> z = inv2 (param -> z);
    return;
  default:
    return;
  }
}

// プリミティブを読み込む

static bool_t read_prim (prim_t * prim) {
  unsigned tmp;

  tmp = read_int ();
  if (! cont (tmp)) return 0;
  prim -> tex = (tex_t) tmp;

  prim -> type = (type_t) read_int ();
  prim -> surf = (surf_t) read_int ();
  prim -> rot = read_int ();

  read_param (& prim -> param, prim -> type);
  read_vec (& prim -> offset);

  prim -> pol = (read_int () > 0);
  prim -> ref = read_double ();
  prim -> hil = read_double ();

  read_pixel (& prim -> color);

  // 平面は極性が負になるようにする

  if (prim -> type == type_plane) {
    if (prim -> pol) {
      neg_vec (& prim -> param);
      prim -> pol = 0;
    }
  }

  // 2次曲面の回転

  if (prim -> rot) {
    read_rot3 (& prim -> rot3);
    rotate_prim (prim);
  }

  return 1;
}

// ANDプリミティブを読み込む

static bool_t read_andprim (andprim_t * andprim) {
  card_t i = 0;
  unsigned tmp;

  tmp = read_int ();
  if (! cont (tmp)) return 0;
  andprim -> prims [i ++] = & prims [tmp];

  while (1) {
    tmp = read_int ();
    if (! cont (tmp)) {
      andprim -> prims [i] = noprim;
      return 1;
    }
    andprim -> prims [i ++] = & prims [tmp];
  }
}

// ORプリミティブを読み込む

static bool_t read_orprim (orprim_t * orprim) {
  card_t i = 0;
  unsigned tmp;

  tmp = read_int ();
  if (! cont (tmp)) return 0;
  orprim -> range = (tmp == 99) ? noprim : (& prims [tmp]);

  while (1) {
    tmp = read_int ();
    if (! cont (tmp)) {
      orprim -> andprims [i] = noandprim;
      return 1;
    }
    orprim -> andprims [i ++] = & andprims [tmp];
  }
}

// データ全体を読み込む

static void read_data (void) {
  // 基本的な情報を読み込む

  read_output (& output);
  read_screen (& screen);
  read_light (& light);

  // プリミティブを読み込む

  while (1) {
    bool_t tmp;
    tmp = read_prim (& prims [num_prims]);
    if (! tmp) break;
    num_prims ++;
  }

  // ANDプリミティブを読み込む

  while (1) {
    bool_t tmp;
    tmp = read_andprim (& andprims [num_andprims]);
    if (! tmp) break;
    num_andprims ++;
  }

  // ORプリミティブを読み込む

  while (1) {
    bool_t tmp;
    tmp = read_orprim (& orprims [num_orprims]);
    if (! tmp) break;
    num_orprims ++;
  }

  dot = screen_size / (dist_t) (int) output . size;
}

/*********************************************************************

			      主要な関数

*********************************************************************/

// intsec_pointにおけるintsec_primの法線ベクトルを求める

static void normal_rect (void) {
  switch (intsec_rectsurf) {
  case rectsurf_x:
    {
      intsec_normal . x = (sightline . x > 0.0 ? -1.0 : 1.0);
      intsec_normal . y = 0.0;
      intsec_normal . z = 0.0;

      return;
    }
  case rectsurf_y:
    {
      intsec_normal . x = 0.0;
      intsec_normal . y = (sightline . y > 0.0 ? -1.0 : 1.0);
      intsec_normal . z = 0.0;

      return;
    }
  case rectsurf_z:
    {
      intsec_normal . x = 0.0;
      intsec_normal . y = 0.0;
      intsec_normal . z = (sightline . z > 0.0 ? -1.0 : 1.0);

      return;
    }
  default:
    {
      return; // コンパイラの警告を回避
    }
  }
}

static void normal_plane (void) {
  neg_vec_2 (& intsec_normal, & intsec_prim -> param); // 平面は極性が負
}

static void normal_quad_cone (void) {
  vec_t ip;

  sub_vec_3 (& ip, & intsec_point, & intsec_prim -> offset); // 平行移動
  mul_vec_3 (& intsec_normal, & ip, & intsec_prim -> param);

  if (intsec_prim -> rot) {
    vec_t tmp;

    twistprod_vec (& tmp, & ip, & intsec_prim -> cross);
    half_vec (& tmp);
    add_vec (& intsec_normal, & tmp);
  }

  if (! intsec_prim -> pol) {
    neg_vec (& intsec_normal);
  }

  norm_vec (& intsec_normal);
}

static void normal (void) {
  switch (intsec_prim -> type) {
  case type_rect:
    normal_rect ();
    return;
  case type_plane:
    normal_plane ();
    return;
  case type_quad:
  case type_cone:
    normal_quad_cone ();
    return;
  default:
    return; // コンパイラの警告を回避
  }
}

// intsec_candがprimに含まれるかどうかを判定

static bool_t interior (const prim_t * prim) {
  vec_t is;
  sub_vec_3 (& is, & intsec_cand, & prim -> offset); // 平行移動

  switch (prim -> type) {
  case type_rect:
    {
      return my_xor (fabs (is . x) > prim -> param . x ||
		  fabs (is . y) > prim -> param . y ||
		  fabs (is . z) > prim -> param . z,
		  prim -> pol);
    }
  case type_plane:
    {
      return inprod_vec (& prim -> param, & is) > 0.0; // 平面は極性が負
    }
  case type_quad:
  case type_cone:
    {
      double tmp;

      tmp = dblprod_vec (& prim -> param, & is);

      if (prim -> rot) {
	tmp += rotprod_vec (& prim -> cross, & is);
      }

      if (prim -> type == type_quad) {
	tmp -= 1.0;
      }

      return my_xor (tmp > 0.0, prim -> pol);
    }
  default:
    {
      return 0; // コンパイラの警告を回避
    }
  }
}

// 視線とプリミティブの交わり方を調べる

static bool_t intersect_rect (const vec_t * vp,
			      const prim_t * prim,
			      dist_t * dist,
			      rectsurf_t * rectsurf) {
  dist_t d;

  // [BUG?] distが負のときに1が返る可能性

  if (sightline . x != 0.0) {
    // yz平面に平行な面に関して調べる

    d = my_xor (prim -> pol, sightline . x > 0.0)
        ? (prim -> param . x)
	: (- prim -> param . x);
    d -= vp -> x;
    d /= sightline . x;

    if (fabs (vp -> y + sightline . y * d) <= prim -> param . y &&
	fabs (vp -> z + sightline . z * d) <= prim -> param . z) {
      * rectsurf = rectsurf_x;
      * dist = d;
      return 1; // 交わった
    }
  }

  if (sightline . y != 0.0) {
    // zx平面に平行な面に関して調べる

    d = my_xor (prim -> pol, sightline . y > 0.0)
        ? (prim -> param . y)
	: (- prim -> param . y);
    d -= vp -> y;
    d /= sightline . y;

    if (fabs (vp -> z + sightline . z * d) <= prim -> param . z &&
	fabs (vp -> x + sightline . x * d) <= prim -> param . x) {
      * rectsurf = rectsurf_y;
      * dist = d;
      return 1; // 交わった
    }
  }

  if (sightline . z != 0.0) {
    // xy平面に平行な面に関して調べる

    d = my_xor (prim -> pol, sightline . z > 0.0)
        ? (prim -> param . z)
	: (- prim -> param . z);
    d -= vp -> z;
    d /= sightline . z;

    if (fabs (vp -> x + sightline . x * d) <= prim -> param . x &&
	fabs (vp -> y + sightline . y * d) <= prim -> param . y) {
      * rectsurf = rectsurf_z;
      * dist = d;
      return 1; // 交わった
    }
  }

  return 0; // 交わらなかった
}

static bool_t intersect_plane (const vec_t * vp,
			       const prim_t * prim,
			       dist_t * dist) {
  dist_t d; // 点と平面の符号つき距離

  d = inprod_vec (& sightline, & prim -> param); // 平面は極性が負

  if (d <= 0.0) {
    return 0; // 平行、あるいは向きが逆
  }

  * dist = - inprod_vec (vp, & prim -> param) / d;

  return 1; // 交わった
}

static bool_t intersect_quad (const vec_t * vp,
			      const prim_t * prim,
			      dist_t * dist) {
  double a, b, c; // 2次方程式の係数

  a = dblprod_vec (& prim -> param, & sightline);

  {
    vec_t tmp;
    mul_vec_3 (& tmp, vp, & prim -> param);
    b = inprod_vec (& tmp, & sightline);
    c = inprod_vec (& tmp, vp);
  }

  if (prim -> rot) {
    a += rotprod_vec (& prim -> cross, & sightline);
    c += rotprod_vec (& prim -> cross, vp);

    {
      vec_t tmp;
      twistprod_vec (& tmp, vp, & sightline);
      b += fhalf (inprod_vec (& prim -> cross, & tmp));
    }
  }

  if (prim -> type == type_quad) {
    c -= 1.0;
  }

  if (a == 0.0) {
    return 0;
  }

  {
    double d = fsq (b) - a * c; // 判別式

    if (d < 0.0) {
      return 0;
    }

    * dist = - b;

    d = sqrt (d);

    if (prim -> pol) {
      * dist -= d;
    } else {
      * dist += d;
    }

    * dist /= a;
  }

  return 1; // 交わった
}

static bool_t intersect (const prim_t * prim,
			 dist_t * dist,
			 rectsurf_t * rectsurf) {
  vec_t vp;

  sub_vec_3 (& vp, & viewpoint, & prim -> offset); // 平行移動

  switch (prim -> type) {
  case type_rect:
    return intersect_rect (& vp, prim, dist, rectsurf);
  case type_plane:
    return intersect_plane (& vp, prim, dist);
  case type_quad:
  case type_cone:
    return intersect_quad (& vp, prim, dist);
  default:
    return 0; // コンパイラの警告を回避
  }
}

// 視線をたどって物体との交点を求める

static bool_t trace (void) {
  // const prim_t * prev_intsec_prim = intsec_prim;
  dist_t dist = dist_max;

  // すべてのORプリミティブ定義を順に調べていく

  card_t id_orprim;
  for (id_orprim = 0;
       id_orprim < num_orprims;
       id_orprim ++) {
    const orprim_t * orprim = & orprims [id_orprim];

    if (orprim -> range != noprim) {
      // レンジプリミティブが指定されているので、
      // まずレンジプリミティブとの交わり方をチェック

      rectsurf_t dummy;
      dist_t d;

      if (! intersect (orprim -> range, & d, & dummy) ||
	  d >= dist) {
	continue; // まったく交わらない、あるいは既知のものより遠い
      }
    }

    // ORされているANDプリミティブを順に見ていく

    card_t id_andprim;
    for (id_andprim = 0;
	 orprim -> andprims [id_andprim] != noandprim;
	 id_andprim ++) {
      const andprim_t * andprim = orprim -> andprims [id_andprim];

      // ANDされているプリミティブを順に調べる

      rectsurf_t rectsurf;
      dist_t d;

      card_t id_prim;
      for (id_prim = 0;
	   andprim -> prims [id_prim] != noprim;
	   id_prim ++) {
	const prim_t * prim = andprim -> prims [id_prim];
/*
	if (prim == prev_intsec_prim) {
	  continue; // [BUG?] 前回に反射が起こったプリミティブなので計算は不要?
	}
*/
	if (! intersect (prim, & d, & rectsurf)) {
	  // プリミティブの表面と視線が交わらない

	  if (prim -> pol) {
	    break; // 内部が真なので、視線と交わらない
	  } else {
	    continue; // 外部が真なので、視線を完全に含む
	  }
	}

	if (d > dist || d < dist_back) {
	  continue; // 既知の交点より遠い、あるいは視点の後方にある
	}

	d += dist_delta; // 誤差を回避

	// 交点の候補を求める

	scale_vec_3 (& intsec_cand, & sightline, d);
	add_vec (& intsec_cand, & viewpoint);

	// 実際にすべてのプリミティブに含まれているかどうか
	// 交点の候補を審査

	{
	  card_t id_prim2;
	  const prim_t * prim2;

	  for (prim2 = andprim -> prims [id_prim2 = 0];
	       prim2 != noprim;
	       prim2 = andprim -> prims [++ id_prim2]) {
	    if (prim == prim2) {
	      continue; // このプリミティブに含まれるのは自明
	    }

	    if (! interior (prim2)) {
	      goto continue2; // 審査失格
	    }
	  }
	}

	// 審査合格

	dist = d;

	intsec_point = intsec_cand;
	intsec_prim = prim;
	intsec_rectsurf = rectsurf;

      continue2:
	;
      }
    }
  }

  // 遠すぎるときや、後方の場合は偽を返す

  return (dist > dist_back && dist < dist_far); 
}

// 光源の白色光を計算

static void lighten (void) {
  double tmp = - inprod_vec (& light . vec, & sightline);

  if (tmp >= 0.0) {
    hil_t hil = fcube (tmp) * energy * light . str;

    pixel . r += hil;
    pixel . g += hil;
    pixel . b += hil;
  }
}

// 新しい視点（現在の交点）から光線を逆にたどって影を求める

static bool_t shadow (void) {
  swap_vec (& sightline, & light . vec); // 視線と光線を入れかえる

  // すべてのORプリミティブ定義を順に調べていく

  card_t id_orprim;
  for (id_orprim = 0;
       id_orprim < num_orprims;
       id_orprim ++) {
    const orprim_t * orprim = & orprims [id_orprim];

    if (orprim -> range != noprim) {
      // レンジプリミティブが指定されているので、
      // まずレンジプリミティブとの交わり方をチェック

      rectsurf_t dummy;
      dist_t d;

      if (! intersect (orprim -> range, & d, & dummy) ||
	  d >= dist_back) {
	continue; // まったく交わらない、あるいは前方で交わる
      }
    }

    // ORされているANDプリミティブを順に見ていく

    card_t id_andprim;
    for (id_andprim = 0;
	 orprim -> andprims [id_andprim] != noandprim;
	 id_andprim ++) {
      const andprim_t * andprim = orprim -> andprims [id_andprim];

      // ANDされているプリミティブを順に調べる

      rectsurf_t rectsurf;
      dist_t d;

      card_t id_prim;
      for (id_prim = 0;
	   andprim -> prims [id_prim] != noprim;
	   id_prim ++) {
	const prim_t * prim = andprim -> prims [id_prim];
/*
        // 凸でないANDプリミティブも作れるので正しくない?
	if (prim == intsec_prim) {
	  continue; // [BUG?] 自分自身なので考慮しない? (すでに法線ベクトルで考慮?)
	}
*/
	if (! intersect (prim, & d, & rectsurf)) {
	  // プリミティブの表面と光線が交わらない

	  if (prim -> pol) {
	    break; // 内部が真なので、光線と交わらない
	  } else {
	    continue; // 外部が真なので、光線を完全に含む
	  }
	}

	if (d >= dist_back) {
	  continue; // 光線の前方で交わる
	}

	d += dist_delta; // 誤差を回避

	// 交点の候補を求める

	scale_vec_3 (& intsec_cand, & sightline, d);
	add_vec (& intsec_cand, & viewpoint);

	// 実際にすべてのプリミティブに含まれているかどうか
	// 交点の候補を審査

	{
	  card_t id_prim2;
	  const prim_t * prim2;

	  for (prim2 = andprim -> prims [id_prim2 = 0];
	       prim2 != noprim;
	       prim2 = andprim -> prims [++ id_prim2]) {
	    if (prim == prim2) {
	      continue; // このプリミティブに含まれるのは自明
	    }

	    if (! interior (prim2)) {
	      goto continue2; // 審査失格
	    }
	  }
	}

	// 審査合格

	swap_vec (& sightline, & light . vec);
	return 1; // 影になっている

      continue2:
	;
      }
    }
  }

  swap_vec (& sightline, & light . vec);
  return 0; // 影になっていない
}

// 物体表面の明るさを計算

static void brighten (void) {
  if (shadow()) {
    bright = 0.0; // 影になっている
    return;
  }

  bright = - inprod_vec (& light . vec, & intsec_normal);

  if (bright < 0.0) {
    bright = 0.0; // 光があたらない
  }

  bright += 0.2; // 底上げ
  bright *= energy; // 反射による弱まり
  bright *= intsec_prim -> ref; // プリミティブの乱反射率
}

// テクスチュアを処理

static void texture (void) {
  pixel_t p = intsec_prim -> color;

  switch (intsec_prim -> tex) {
  case tex_checker: // zx方向のチェッカ
    {
      dist_t mod_z, mod_x;
      bool_t tmp;

      mod_z = fmod20 (intsec_point . z - intsec_prim -> offset . z);
      mod_x = fmod20 (intsec_point . x - intsec_prim -> offset . x);
      tmp = my_xor (mod_z > 10.0, mod_x > 10.0);

      p . g = tmp ? hil_min : hil_max;

      break;
    }
  case tex_stripe: // y軸方向のストライプ
    {
      double tmp;

      tmp = fsq (sin (0.25 * intsec_point . y));

      p . r = hil_max * tmp;
      p . g = hil_max * (1.0 - tmp);

      break;
    }
  case tex_circle: // zx平面方向の同心円
    {
      double tmp;

      tmp = fsq (intsec_point . z - intsec_prim -> offset . z);
      tmp += fsq (intsec_point . x - intsec_prim -> offset . x);
      tmp = sqrt (tmp);
      tmp = frac (0.1 * tmp);
      tmp = cos (pi * tmp);
      tmp = fsq (tmp);

      p . g = hil_max * tmp;
      p . b = hil_max * (1.0 - tmp);

      break;
    }
  case tex_spot: // 球面上の斑点
    {
      double d, u, v;
      vec_t vec;

      sub_vec_3 (& vec, & intsec_point, & intsec_prim -> offset);
      mul_sqrt_vec (& vec, & intsec_prim -> param);

      d = sqrt (fsq (vec . z) + fsq (vec . x));

      u = (fabs (vec . x) > 0.0001)
	  ? (30.0 / pi) * atan (fabs (vec . z / vec . x))
	  : 15.0;
      v = (fabs (u) > 0.0001)
	  ? (30.0 / pi) * atan (fabs (vec . y / d))
	  : 15.0;

      d = 0.15 - (fsq (frac (u) - 0.5) + fsq (frac (v) - 0.5));

      p . b = (d <= 0.0) ? 0.0 : hil_max / 0.3 * d;

      break;
    }
  default:
    {
      break; // コンパイラの警告を回避
    }
  }

  pixel . r += bright * p . r;
  pixel . g += bright * p . g;
  pixel . b += bright * p . b;
}

// 反射の処理

static bool_t reflect (void) {
  double d;
  vec_t v;

  d = - fdbl (inprod_vec (& sightline, & intsec_normal));
  scale_vec_3 (& v, & intsec_normal, d);
  add_vec (& sightline, & v);

  if (intsec_prim -> surf == surf_rand) {
    double tmp;

    tmp = - inprod_vec (& light . vec, & sightline);

    if (tmp < 0.0) {
      tmp = 0.0;
    } else {
      tmp = fquad (tmp);
      tmp *= energy;
      tmp *= bright;
      tmp *= intsec_prim -> hil;

      pixel . r += tmp;
      pixel . g += tmp;
      pixel . b += tmp;
    }

    return 0;
  }

  energy *= 1.0 - intsec_prim -> ref;

  return 1;
}

// 出力画像の一点を生成

static void render (void) {
  card_t ref = 0;

  energy = 1.0;

  while (1) {
    write_flush ();

    // 視線が物体と交わるか調べる

    if (! trace ()) {
      // 光源から直接目に入ってくる光は無視

      if (ref != 0) {
	lighten ();
      }

      break;
    }

    write_flush ();

    normal (); // 法線ベクトルを計算

    viewpoint = intsec_point; // 反射の計算のため、交点を新たな視点とする

    write_flush ();

    brighten (); // 明るさを計算

    write_flush ();

    texture (); // テクスチュアを処理

    // エネルギーが小さくなったり、反射の回数が多くなったりしたらやめる

    if (energy < 0.1 || ref > 4) {
      break;
    }

    write_flush ();

    // 反射の処理

    if (! reflect ()) {
      break; // 乱反射したら終わり
    }

    ref ++;
  }
}

// 出力画像の一行を生成

static void scan_x (const vec_t * v0, dist_t len0) {
  card_t x = 0; // スキャンする列

  dist_t orig_x = - dot * (dist_t) (int) (output . half); // 変換前のx座標
  dist_t d_orig_x = dot; // xに関するorig_xの差分

  vec_t v = // 視線方向ベクトル
    { orig_x * screen . dir . ty . cos,
      0.0,
      - orig_x * screen . dir . ty . sin };
  add_vec (& v, v0);
  vec_t d_v = // xに関するv0の差分
    { d_orig_x * screen . dir . ty . cos,
      0.0,
      - d_orig_x * screen . dir . ty . sin };

  while (x < output . size) {
    dist_t len = len0 + fsq (orig_x); // vの長さの2乗

    // グローバル変数を初期化

    viewpoint = screen . trans_view;
    scale_vec_3 (& sightline, & v, sqrtinv (len));
    intsec_prim = noprim;
    pixel = black_pixel;

    // ピクセル値を計算して出力

    render ();
    write_pixel (& pixel);

    // ループ変数などを更新

    x ++;
    orig_x += d_orig_x;
    add_vec (& v, & d_v);
  }
}

// 出力画像の全体を生成

static void scan_y (void) {
  card_t y = 0; // スキャンする行

  dist_t orig_y = dot * (dist_t) (int) (output . half - 1); // 変換前のy座標
  dist_t d_orig_y = - dot; // yに関するorig_yの差分

  vec_t v0 = // orig_xが0のときの視線方向ベクトル
    { orig_y * screen . dir . tx . sin * screen . dir . ty . sin,
      orig_y * screen . dir . tx . cos,
      orig_y * screen . dir . tx . sin * screen . dir . ty . cos };
  sub_vec (& v0, & screen . rot_view);
  vec_t d_v0 = // yに関するv0の差分
    { d_orig_y * screen . dir . tx . sin * screen . dir . ty . sin,
      d_orig_y * screen . dir . tx . cos,
      d_orig_y * screen . dir . tx . sin * screen . dir . ty . cos };

  while (y < output . size) {
    dist_t len0 = fsq (screen_orig_view_z) + fsq (orig_y); // v0の長さの2乗

    scan_x (& v0, len0);

    // ループ変数などを更新

    y ++;
    orig_y += d_orig_y;
    add_vec (& v0, & d_v0);
  }
}

// メインルーチン

int main (void) {
  write_init (); // 書き込みキューを初期化
  read_data (); // データを読み込む
  write_header (); // PPMのヘッダを書く
  scan_y (); // 画像を生成
  write_flush_all (); // 出力をすべて吐き出す

  return 0;
}
