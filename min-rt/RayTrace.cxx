/*
 *
 * tiny ray tracing program
 *
 * Originally written by T.Yamamoto at Hokkaido Univ.
 * rewrited to Turbo PASCAL T.Matsumoto  1985 August
 * tranceplanted to MS-DOS 1987 Aug.5 by T.Matsumoto
 * ported to C (UNIX) on Feb.17 1988
 *
 * Dec.1996 ported to tsu-as by GANA
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <fcntl.h>
#include <time.h>
#include <string.h>

#if defined(WIN32)
#include <io.h>
#include <fcntl.h>
#define bool int
#define true  1
#define false 0
#endif

typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef float SIGN; // 負であるかどうかだけを表す型 (第 0bit 目が符号)


///////////////////////////////////////////////////////////////////////////////
// 浮動小数点ルーチン (マシン語で書き直しましょう)
//


#define f_sin(f)     ((float)sin ((double)(f)))
#define f_cos(f)     ((float)cos ((double)(f)))
#define f_abs(f)     ((float)fabs((double)(f)))
#define f_atan(f)    ((float)atan((double)(f)))
#define f_sqrt(f)    ((float)sqrt((double)(f)))
#define f_sq(f)      ((f) * (f)) // idea:hanawa
//#define f_round(f) ((float)(int)(f)) この define はうまくいかない (例:タイルテクスチャ)


// 1 / ��f
inline float f_sqrt_inverse(float f)
{
  f = (float)sqrt((double)f);
  if (f == 0)
    return 1.0f;
  return 1 / f;
}


// float 切り下げ。しかし C の整数から実数への変換方法はま
// ちまちであるのでこの if は C 言語としては意味を持たない。
inline float f_round(float f)
{
  if (0 <= f)
    return (float)(int)f;
  else
    return (float)((int)f - 1);
}
#define i_round(f) ((int)f_round(f))


// float の符号を返す
inline float f_sgn(float f)
{
  if (f == 0.0f)
    return 0.0f;
  else if (f < 0.0f)
    return -1.0f;
  else
    return 1.0f;
}
#define i_sgn(f) ((int)f_sgn(f))
#define i_sgn_mul(f0, f1) i_sgn((f0) * (f1)) // f0 * f1 の符号


// 余りを求める
inline float f_mod(float a, float b)
{
  return a - f_round(a / b) * b;
}


// 少数部分を取り出す
inline float f_fraction(float f)
{
  return f - f_round(f);
}


///////////////////////////////////////////////////////////////////////////////
// グローバル変数
//


#define pTX  0 // TX : ユーザ定義テクスチャ番号 0, 1, 2, 3, 4
#define pP   1 // P  : プリミティブ種類コード 1, 2, 3, 4
#define pSF  2 // SF : 表面特性 1, 2, 3 : 透明(謎)
#define pRT  3 // RT : 回転フラグ 0, 1
#define pa   0 // a  : プリミティブパラメータ
#define pb   1 // b  : プリミティブパラメータ
#define pc   2 // c  : プリミティブパラメータ
#define pX   3 // x  : オフセット
#define pY   4 // y  : オフセット
#define pZ   5 // z  : オフセット
#define pSG  6 // SG : 極性 1, -1
#define pREF 7 // REF: 表面乱反射率 0 〜 1
#define pHL  8 // HL : 乱反射面ハイライト強度 0 〜 255
#define pR   9 // R  : カラーコード
#define pG  10 // G  : カラーコード
#define pB  11 // B  : カラーコード
#define pRX 12 //θx : 回転角
#define pRY 13 //θy : 回転角
#define pRZ 14 //θz : 回転角

typedef int PrimitiveData_i[4];
typedef float PrimitiveData_f[15];
#define MAX_PRIMITIVE 60
#define MAX_AND 50
#define MAX_OR 10
PrimitiveData_i ds[MAX_PRIMITIVE + 1]; // プリミティブ定義 TX P SF RT
PrimitiveData_f d [MAX_PRIMITIVE + 1]; // プリミティブ定義 a b c x y z SG REF HL RGB θ
int dr[MAX_AND][32]; // AND 定義
//int dr[MAX_AND][9]; // AND 定義
int dm[MAX_OR][32]; // OR 定義
//int dm[MAX_OR][21]; // OR 定義

float ex,  ey,  ez;  // 視線の向きの単位ベクトル
float nx,  ny,  nz;  // 交点での法線単位ベクトル
float VPx, VPy, VPz; // 計算中の視^H始点 (鏡で反射すると位置が変わる)
float IPx, IPy, IPz; // tracer と in-out と shadow_evaluator 計算中の交点の座標
float ipx, ipy, ipz; // 交点 (intersection point) の座標

int ipi;  // 上記の交点に対するプリミティブの番号
int nidx; // 法線を求めるときの指標

const float pai = 3.141592f; // π
const float rad_deg = pai / 180; // ラジアン/度


///////////////////////////////////////////////////////////////////////////////
// equation solver : 視線とプリミティブの重なり具合を調べる
//


SIGN equation_solver(int i,           // プリミティブインデックス
		     int &ni,         // return : 後で法線を求めるときの指標
		     float &distance) // return : 「視点」から「交わった位置」までの距離 (distance が負だったら交点は背後に存在)
  // 0 <= return : 交わる
  // return < 0  : 交わらない
  // 符号だけ返せば十分である
{
  PrimitiveData_i &dsi = ds[i];
  PrimitiveData_f &di  = d [i];
  
  // このプリミティブが中心にくるように視点 (VPx, VPy, VPz) を平行移動
  const float wx = VPx - di[pX];
  const float wy = VPy - di[pY];
  const float wz = VPz - di[pZ];
  
  switch (dsi[pP])
  {
    case 1: // 直方体
    {
      // di[pSG] SG : 極性 1, -1
      
      // int selector = i_sgn_mul(e1, di[pSG]);
      // if (selector == -1)
      //   distance = di[p1] - w1;
      // else if (selector == 1)
      //   distance = -(w1 + di[p1]);
      // if (selector)
      // {
      //   distance /= e1;
      //   if ((f_abs(distance * e2 + w2) <= di[p2]) &&
      //       (f_abs(distance * e3 + w3) <= di[p3]))
      //   {
      //     ni = 1;
      //     return SIGN(1);
      //   }
      // }
      
      // yz 面
      int selector = i_sgn_mul(ex, di[pSG]);
      if (selector == -1)
	// 面 x = di[pa] について
      {
	distance = (di[pa] - wx) / ex; // 「視点」から「視線と面の交点」までの距離 (負のこともありうる)
	if ((f_abs(distance * ey + wy) <= di[pb]) && // y 方向が矩形範囲に入っているか
	    (f_abs(distance * ez + wz) <= di[pc]))   // z 方向が矩形範囲に入っているか
	  // この面と交わる
	{
	  ni = 1;
	  return SIGN(1);
	}
      }
      else if (selector == 1)
	// 面 x = -di[pa] について
      {
	distance = -(wx + di[pa]) / ex;
	if ((f_abs(distance * ey + wy) <= di[pb]) &&
	    (f_abs(distance * ez + wz) <= di[pc]))
	{
	  ni = 1;
	  return SIGN(1);
	}
      }
      
      selector = i_sgn_mul(ey, di[pSG]);
      if (selector == -1)
	// 面 y = di[pb] について
      {
	distance = (di[pb] - wy) / ey;
	if ((f_abs(distance * ex + wx) <= di[pa]) &&
	    (f_abs(distance * ez + wz) <= di[pc])) 
	{
	  ni = 2;
	  return SIGN(1);
	}
      }
      else if (selector == 1)
	// 面 y = -di[pb] について
      {
	distance = -(wy + di[pb]) / ey;
	if ((f_abs(distance * ex + wx) <= di[pa]) &&
	    (f_abs(distance * ez + wz) <= di[pc])) 
	{
	  ni = 2;
	  return SIGN(1);
	}
      }
      
      selector = i_sgn_mul(ez, di[pSG]);
      if (selector == -1)
	// 面 z = di[pc] について
      {
	distance = (di[pc] - wz) / ez;
	if ((f_abs(distance * ex + wx) <= di[pa]) &&
	    (f_abs(distance * ey + wy) <= di[pb]))
	{
	  ni = 3;
	  return SIGN(1);
	}
      }
      else if (selector == 1)
	// 面 z = -di[pc] について
      {
	distance = -(wz + di[pc]) / ez;
	if ((f_abs(distance * ex + wx) <= di[pa]) &&
	    (f_abs(distance * ey + wy) <= di[pb]))
	{
	  ni = 3;
	  return SIGN(1);
	}
      }
      
      // この直方体と交わらない
      return SIGN(-1);
    }
    
    case 2: // 平面
    {
      // (di[pa], di[pb], di[pc]) : 平面の法線ベクトル (大きさ 1) 法線のむいている方向が外側
      float tmp = float(-SIGN(di[pSG])) * (ex * di[pa] + ey * di[pb] + ez * di[pc]); // a^→・(-n^→) : 内積 : -cosθ
      if (tmp < 0)
	return SIGN(tmp); // 交わるが向きがちがう
      else if (tmp == 0)
	return SIGN(-1); // 平行で交わらない
      else
      {
	distance = float(SIGN(di[pSG])) * (wx * di[pa] + wy * di[pb] + wz * di[pc]) / tmp;
	// 「視点」から「視線と面の交点」までの距離 >= 0
	return SIGN(tmp);
      }
    }
    
    case 3: // 二次曲面
    case 4: // 錐
    {
      float A = f_sq(ex) * di[pa] + f_sq(ey) * di[pb] + f_sq(ez) * di[pc];
      if (dsi[pRT] == 1) // 回転あり(謎)
	A += ey * ez * di[pRX] + ez * ex * di[pRY] + ex * ey * di[pRZ];
      if (A == 0)
	return -1.0f;
      float tmpx = wx * di[pa];
      float tmpy = wy * di[pb];
      float tmpz = wz * di[pc];
      float B = tmpx * ex + tmpy * ey + tmpz * ez;
      float C = tmpx * wx + tmpy * wy + tmpz * wz;
      if (dsi[pRT] == 1) // 回転あり(謎)
      {
	B = B + ((wy * ez + wz * ey) * di[pRX] + (wx * ez + wz * ex) * di[pRY] + (wx * ey + wy * ex) * di[pRZ]) * 0.5f;
	C = C +   wy *      wz       * di[pRX] +  wx *      wz *       di[pRY] +  wx *      wy *       di[pRZ];
      }
      if (dsi[pP] == 3) // 楕円体
	C = C - 1;
      // 0 = A * distance^2 + 2 * B * distance + C という二次方程式を解く
      float tmp = f_sq(B) - A * C;
      if (tmp < 0)
	return SIGN(tmp); // 視線と交わらない
      int selector = i_sgn(SIGN(di[pSG]));
      if (selector == 1)
	distance = (-B - f_sqrt(tmp)) / A;
      else
	distance = (-B + f_sqrt(tmp)) / A;
      return SIGN(tmp);
    }
  }
  return SIGN(-1);
}


///////////////////////////////////////////////////////////////////////////////
// in-out function
//


SIGN in_out(int i)
  // 0 <= return : i 番めのプリミティブに (IPx, IPy, IPz) は含まれる
  // return <  0 :                              〃              ない
{
  PrimitiveData_i &dsi = ds[i];
  PrimitiveData_f &di  = d [i];

  // このプリミティブが中心にくるように交点 (IPx, IPy, IPz) を平行移動
  const float wx = IPx - di[pX];
  const float wy = IPy - di[pY];
  const float wz = IPz - di[pZ];
  
  float tmp;
  switch (dsi[pP])
  {
    case 1: // 直方体
      tmp = -f_sgn(SIGN(di[pSG]));
      if (di[pa] < f_abs(wx)) break;
      if (di[pb] < f_abs(wy)) break;
      if (di[pc] < f_abs(wz)) break;
      tmp = -tmp;
      break;
    case 2: // 平面
      tmp = -SIGN(di[pSG]) * (di[pa] * wx + di[pb] * wy + di[pc] * wz);
      break;
      // case 3:
      // case 4:
      //   tmp = f_sq(wx) * di[pa] + f_sq(wy) * di[pb] + f_sq(wz) * di[pc];
      //   if (dsi[pRT] != 0) // 回転あり
      //     tmp+= wy * wz * di[pRX] + wx * wz * di[pRY] + wx * wy * di[pRZ];
      //   tmp *= SIGN(di[pSG]);
      //   if (dsi[pP] == 3)
      //      tmp = SIGN(di[pSG]) - tmp;
      //   else
      //      tmp = -tmp;
      //   break;
    case 3: // 楕円体
      tmp = 1 - (f_sq(wx) * di[pa] + f_sq(wy) * di[pb] + f_sq(wz) * di[pc]);
      if (dsi[pRT] != 0) // 回転あり
	tmp = tmp - wy * wz * di[pRX] - wx * wz * di[pRY] - wx * wy * di[pRZ];
      tmp = SIGN(di[pSG]) * tmp;
      break;
    case 4: // 錐
      tmp = f_sq(wx) * di[pa] + f_sq(wy) * di[pb] + f_sq(wz) * di[pc];
      if (dsi[pRT] != 0) // 回転あり
	tmp = tmp + wy * wz * di[pRX] + wx * wz * di[pRY] + wx * wy * di[pRZ];
      tmp = -SIGN(di[pSG]) * tmp;
      break;
  }
  return SIGN(tmp);
}


///////////////////////////////////////////////////////////////////////////////
// 交点 (ipx, ipy, ipz) の部分での面の法線 (normal vector) を求める
//


void normal_vector(void)
  // (nx, ny, nz) : return 法線
{
  switch (ds[ipi][pP])
  {
    case 1: // 直方体
      if (nidx == 1) // 指標
      {
	nx = -f_sgn(ex);
	ny = 0.0f;
	nz = 0.0f;
      }
      else
      {
	nx = 0.0f;
	if (nidx == 2)
	{
	  ny = -f_sgn(ey);
	  nz = 0.0f;
	}
	else // nidx == 3
	{
	  ny = 0.0f;
	  nz = -f_sgn(ez);
	}
      }
      break;
    case 2: // 平面
    {
      PrimitiveData_f &di = d[ipi];
      nx = di[pa];
      ny = di[pb];
      nz = di[pc];
      if (SIGN(di[pSG]) < 0)
      {
	nx = -nx;
	ny = -ny;
	nz = -nz;
      }
      break;
    }
    case 3: // 楕円体
    case 4: // 錐
    {
      PrimitiveData_f &di = d[ipi];
      // (wx, wy, wz) : 楕円体が中心にある時の交点の座標
      float wx = ipx - di[pX];
      float wy = ipy - di[pY];
      float wz = ipz - di[pZ];
      nx = wx * di[pa];
      ny = wy * di[pb];
      nz = wz * di[pc];
      if (ds[ipi][pRT] == 1) // 回転あり
      {
	nx += 0.5f * (wy * di[pRZ] + wz * di[pRY]);
	ny += 0.5f * (wx * di[pRZ] + wz * di[pRX]);
	nz += 0.5f * (wx * di[pRY] + wy * di[pRX]);
      }
      // 法線ベクトルの大きさを 1 に正規化する
      float tmp = f_sqrt_inverse(f_sq(nx) + f_sq(ny) + f_sq(nz)) * SIGN(d[ipi][pSG]);
      nx *= tmp;
      ny *= tmp;
      nz *= tmp;
      break;
    }
  }
}


///////////////////////////////////////////////////////////////////////////////
// user defined texture
//


void user_defined_texture(void)
  // return : d[ipi][pR] d[ipi][pG] d[ipi][pB] : 色
{
  PrimitiveData_f &di = d[ipi];
  switch (ds[ipi][pTX]) // ユーザー定義テクスチャ
  {
    case 0:
      break;
    case 1: // x-z 平面のチェッカー模様
    {
      // このプリミティブが中心にくるように交点 (ipx, ipy, ipz) を平行移動
      float wx = ipx - di[pX];
      float wz = ipz - di[pZ];
      
      // 10x10 のチェッカー模様
      bool tmp = true;
      if (10 < f_mod(wx, 20.0f))
	tmp = !tmp;
      if (10 < f_mod(wz, 20.0f))
	tmp = !tmp;
      if (tmp)
	di[pG] = 255.0f;
      else
	di[pG] = 0.0f;
      break;
    }
    case 2: // y 軸方向のストライプ
    {
      // 赤と緑の周期は 2πずれている
      // CHANGED
      float tmp = f_sq(f_sin(ipy * 0.25));
      di[pR] = 255 *  tmp     ; // 周期 4π で赤が増減
      di[pG] = 255 * (1 - tmp); // 周期 4π で緑が増減
//    di[pR] = 255 * f_sq(f_sin(ipy * 0.25)); // 周期 4π で赤が増減
//    di[pG] = 255 * f_sq(f_cos(ipy * 0.25)); // 周期 4π で緑が増減
      break;
    }
    case 3: // x-z 平面の同心円
    {
      // このプリミティブが中心にくるように交点 (ipx, ipy, ipz) を平行移動
      float wx = ipx - di[pX];
      float wz = ipz - di[pZ];
      
      float tmp = f_sqrt(f_sq(wx) + f_sq(wz)); // 原点からの距離
      // 緑と青の周期は 5 ずれている
      tmp = pai * f_fraction(tmp * 0.1f); // 周期 10
//    di[pG] = f_sq(f_cos(tmp)) * 255; // 周期 10 で緑が増減
//    di[pB] = f_sq(f_sin(tmp)) * 255; // 周期 10 で青が増減
      // CHANGED
      tmp = f_sq(f_cos(tmp));
      di[pG] =  tmp      * 255; // 周期 10 で緑が増減
      di[pB] = (1 - tmp) * 255; // 周期 10 で青が増減
      break;
    }
    case 4: // 球面上の斑点模様
    {
      // このプリミティブが中心にくるように交点 (ipx, ipy, ipz) を平行移動し、
      // 楕円体が単位球である時の座標に交点を変換
      float wx = (ipx - di[pX]) * f_sqrt(di[pa]);
      float wy = (ipy - di[pY]) * f_sqrt(di[pb]);
      float wz = (ipz - di[pZ]) * f_sqrt(di[pc]);
      
      float tmp1 = f_sqrt(f_sq(wx) + f_sq(wz)); // y = wy 平面で単位球を切った時の断面の半径
      float tmp2;
      float tmp3;
      if (0.0001 < f_abs(wx))
	tmp2 = f_atan(f_abs(wz / wx)) // x-z 平面上での x 軸から計った角度 0 〜 π/2
	  // /(pai * 0.5f) * 15; // 15 等分 (π/2 の中に 15 個の斑点ができる)
	  * 9.549296585514f;
      else
	tmp2 = 15.0f;
      if (0.0001 < f_abs(tmp1))
	tmp3 = f_atan(f_abs(wy / tmp1)) // 中心と交点を結ぶ直線の x-z 平面上からの角度 0 〜 π/2
	  // / (pai * 0.5f) * 15; // 15 等分
	  * 9.549296585514f;
      else
	tmp3 = 15.0f;
      tmp1 = 0.15f -
	(f_sq(0.5f - f_fraction(tmp2)) + f_sq(0.5f - f_fraction(tmp3))); // 一つの斑点の中心からの距離の二乗
      if (tmp1 <= 0)
	di[pB] = 0.0f; // 中心から遠いところは青が 0
      else
	di[pB] = (256 / 0.3f) * tmp1; // 一番近い所では青が 256 * 5
      break;
    }
    case 5: // scheme バージョンでついているのでこれにも追加
    {
      // このプリミティブが中心にくるように交点 (ipx, ipy, ipz) を平行移動
      float wx = ipx - di[pX];
      float wy = ipy - di[pY];
      float wz = ipz - di[pZ];
      
      // 10x10x10 のチェッカー模様
      bool tmp = true;
      if (10 < f_mod(wx, 20.0f))
	tmp = !tmp;
      if (10 < f_mod(wy, 20.0f))
	tmp = !tmp;
      if (10 < f_mod(wz, 20.0f))
	tmp = !tmp;
      if (tmp)
	di[pG] = 255.0f;
      else
	di[pG] = 0.0f;
      break;
    }
  }
  return;
}


///////////////////////////////////////////////////////////////////////////////
// tracer : 視点から視線ベクトルを辿(trace)ります
//


SIGN tracer(void)
  // 0 < return : 交わった
  // return < 0 : 交わらなかった
  // (ipx, ipy, ipz) : 交点の座標
  // ipi : 上記の交点に対するプリミティブの番号
  // nidx : 後で法線を求めるときの指標
{
  // int iy = ipi; // 鏡面で反射後は ipi は反射した物体を表している
  float minimum_distance = 1e+15f; // 一番近いプリミティブの交点までの距離
  // ipi : minimum_distance に対するプリミティブの番号
  
  int im = 0;
  do
    // for (int im = 0; dm[im][0] != -1; im++) と同じ意味。
    // 必ず dm[im][0] != -1 であるはず (OR 定義は必ず一つは存在) なので for より比較が一回少ない。
    // ある OR 定義について。
  {
    {
      int i = dm[im][0];
      if (i < 99)
	// i < 99 だったら、視線ベクトルがプリミティブ i の中を通るかどうかテスト
      {
	int dummy;
	float distance;
	SIGN result = equation_solver(i, dummy, distance); // テスト
	if (result < 0 || // 通らないか
	    minimum_distance <= distance) // このプリミティブ i よりも近い位置で既に交わっている
	  continue;
      }
    }
    
    int imi = 1;
    do
      // for (int il = 1; dm[im][imi] != -1; il++) と同じ意味。
      // 必ず dm[im][1] != -1 であるはず (空の OR 定義はない) なので for より比較が一回少ない。
      // ある OR 定義の中の各要素について。
    {
      int ir = dm[im][imi]; // これから OR として 考慮する対象の AND 定義
      
      int iri = 0;
      do
	// for (int iri = 0; dr[ir][iri] != -1; iri++) と同じ意味。
	// 必ず dr[ir][0] != -1 であるはず (AND 定義は必ず一つは存在) なので for より比較が一回少ない。
	// ある AND 定義について。
      {
	int ni;
	float distance;
	{
	  int i = dr[ir][iri];
/*
	  if (i == iy)
	    // 鏡面で反射後は iy 番目のプリミティブは、反射した物体な
	    // ので考慮しない???(謎)
	    continue;
*/
	  SIGN result = equation_solver(i, ni, distance);
	  if (result < 0)
	    if (0 < SIGN(d[i][pSG]))
	      // 視線ベクトルがプリミティブ i の中を通らなくて、かつ極
	      // 性が正 (つまり内側) ならば、 AND 定義なので、この AND 
	      // 定義とは交わらないことが決定。
	      break;
	    else
	      continue;
	  if (minimum_distance <= distance || // このプリミティブ i よりも近い位置で既に交わっている
	      distance < -0.1)  // 視点の背後で交わった。なぜ 0.1 なのかは謎
	    continue;
	}

	// 交点までの距離のさば読み。(謎)
	distance = distance + 0.01f;
	// (IPx, IPy, IPz) : 交点の座標
	IPx = ex * distance + VPx;
	IPy = ey * distance + VPy;
	IPz = ez * distance + VPz;
	
	// (VPx, VPy, VPz) から distance の距離にある点 (交点) がこの 
	// AND 定義のプリミティブ全てに含まれていることを確かめる。一
	// つでも入ってないものがあれば AND 定義ゆえこの交点では交わら
	// ないことになる。
	{
	  int iri2 = 0;
	  do
	  {
	    if (iri2 != iri)
	      if (in_out(dr[ir][iri2]) < 0)
		goto next_iri; // 入ってなかった
	  } while (dr[ir][++iri2] != -1);
	}

	// ここまで無事にたどり着いていれば、この AND 定義 ir の中で一
	// 番近くの交点が求まっている。
	minimum_distance = distance; // 一番近くのプリミティブとの交点までの距離を更新
	ipx = IPx; // 交点の座標を更新
	ipy = IPy; // 交点の座標を更新
	ipz = IPz; // 交点の座標を更新
	ipi = dr[ir][iri]; // minimum_distance に対するプリミティブの番号を更新
	nidx = ni; // 後で法線を求めるときの指標
	next_iri:;
      } while (dr[ir][++iri] != -1);
      
    } while (dm[im][++imi] != -1);
    
  } while (dm[++im][0] != -1);
  
  if (1e+14 < minimum_distance || minimum_distance < -0.1)
    // 遠すぎるか、背後の時は失敗
    return SIGN(-1);
  else
    return SIGN(1);
}


///////////////////////////////////////////////////////////////////////////////
// 影の計算 : shadow evaluator
//


float shadow_evaluator(void)
  // return : 明るさ
{
  float bright = 1.0f; // とりあえず影はないと仮定
  
  // tracer とほぼ同じ構造であるが、始点が視線と物体の交点になった所と、
  // 視線の向きではなく光源の向きで trace するところが違う
  
  int im = 0;
  do
  {
    {
      int i = dm[im][0];
      if (i < 99)
	// i < 99 だったら、視線ベクトルがプリミティブ i の中を通るかどうかテスト
      {
	int dummy;
	float distance;
	SIGN result = equation_solver(i, dummy, distance); // テスト
	if (result < 0 || // 通らないか
	    -0.1 <= distance) // 始点より前方で交わるので、影には関係ない。なぜ 0.1 なのかは謎
	  continue;
      }
    }
    int imi = 1;
    do
    {
      int ir = dm[im][imi]; // これから OR として考慮する対象の AND 定義
      
      int iri = 0;
      do
      {
	int i = dr[ir][iri];
/*
	if (i == ipi) // 自分自身(については法線ベクトルによって既に考慮済み)
	  continue;
*/
	{
	  int dummy;
	  float distance;
	  SIGN result  = equation_solver(i, dummy, distance);
	  if (result < 0)
	    if (0 < SIGN(d[i][pSG]))
	      // 視線ベクトルがプリミティブ i の中を通らなくて、かつ極性
	      // が正 (つまり内側) ならば、 AND 定義なので、この AND 定
	      // 義とは交わらないことが決定。
	      break;
	    else
	      continue;
	  if (-0.1 <= distance) // 始点より前方で交わるので、影には関係ない。なぜ 0.1 なのかは謎
	    continue;
	  
	  // 交点までの距離のさば読み。(謎)
	  distance = distance + 0.01f;
	  // (IPx, IPy, IPz) : 計算中の交点の座標
	  IPx = ex * distance + VPx;
	  IPy = ey * distance + VPy;
	  IPz = ez * distance + VPz;
	}

	// (VPx, VPy, VPz) から distance の距離にある点 (交点) がこの AND 定
	// 義のプリミティブ全てに含まれていることを確かめる。一つでも
	// 入ってないものがあれば AND 定義ゆえこの交点では交わらないこ
	// とになる。
	{
	  int iri2 = 0;
	  do
	  {
	    if (iri2 != iri)
	      if (in_out(dr[ir][iri2]) < 0)
		goto next_iri; // 入ってなかった
	  } while (dr[ir][++iri2] != -1);
	}
	
	// ここまで無事にたどり着いていれば、この AND 定義の中での交点が求まっている
	if (ds[i][pSF] != 3) // 透明(謎)でない
	  return 0.0f;
	// 透明(謎)
	bright = bright - 0.25f;
	if (bright <= 0)
	  return 0.0f; // 明るさがなくなっていたら 終わり
	next_iri:;
      } while (dr[ir][++iri] != -1);
    } while (dm[im][++imi] != -1);
  } while (dm[++im][0] != -1);
  return bright;
}


///////////////////////////////////////////////////////////////////////////////
// メイン
//   ・RS232C からデータを読み込む
//   ・レイトレ実行時には変化しない定数の計算
//   ・スクリーンの各点についてレイトレを実行
//   ・RS232C へデータを書き込む


bool  is_writing(void) { return rand() % 2; }
void  write_value(BYTE value);
float read_float(void);
int   read_int(void);
int   read_int(int i);

// キュー付き書き込み (idea:sumii)

#define WRITE_QUEUE_SIZE 3
int  write_queue[WRITE_QUEUE_SIZE]; // キュー
int *write_queue_enq; // データを追加していく端
int *write_queue_deq; // データを取り出していく端
int  write_queue_num; // 現在キューに入っているデータの数

void write_queue_init(void)
{
  write_queue_enq = write_queue;
  write_queue_deq = write_queue;
  write_queue_num = 0;
}

void write_flush(void)
{
  if (is_writing())
    return;
  if (0 < write_queue_num)
  {
    write_value((BYTE)*write_queue_deq);
    write_queue_deq++;
    write_queue_num--;
    if (write_queue_deq == write_queue + WRITE_QUEUE_SIZE)
      write_queue_deq = write_queue;
  }
}

void write_flush_all(void)
{
  while (0 < write_queue_num)
    write_flush();
}

void write_enq(int value)
{
  while (write_queue_num == WRITE_QUEUE_SIZE)
    write_flush();
  *write_queue_enq = value;
  write_queue_enq++;
  write_queue_num++;
  if (write_queue_enq == write_queue + WRITE_QUEUE_SIZE)
    write_queue_enq = write_queue;
}


void word2ascii(int num)
{
  char buf[20];
  sprintf(buf, "%d", num);
  char *b = buf;
  while (*b)
    write_enq(*(b++));
}


// 入れ替え〜
void swap(float *ex, float *lsx, float *ey, float *lsy, float *ez, float *lsz)
{
  float work;
  work = *ex; *ex = *lsx; *lsx = work;
  work = *ey; *ey = *lsy; *lsy = work;
  work = *ez; *ez = *lsz; *lsz = work;
  return;
}


// メイン
char *cpu_main(void)
{
  write_queue_init();
  
  const float z0    = -200.0f; // 回転／移動する前のスクリーンからの相対視点
  const float sq_z0 = 40000.0f; // z0^2

  ///////////////////////////////////////////////////////////////////////////////
  // データ読み込みと定数の計算 (RS232C が遅いため平行計算)
  //
  
  // (CPU : 10MHz) / (RS232C : 38400bps = 4800bytes/s) = 2000instractions/s = 50fmul/s
  
  // 以下コマンドラインオプション

  int   screen_width   = read_int(0);
  int   screen_height  = read_int(1);
  
  write_enq('P');
  write_enq('6');
  write_enq('\n');
  word2ascii(screen_width);
  write_enq(' ');
  word2ascii(screen_height);
  write_enq('\n');
  write_enq('2');
  write_enq('5');
  write_enq('5');
  write_enq('\n');
  
  const float dx_or_dy = 128.0f / (float)screen_width; // スクリーンの 1 ドットの大きさ
  const int offset     = screen_width / 2; // スクリーンの端から中心へのオフセット
  
  int   y_offset       = read_int(2);
  int   ikasama_distance = 0;
  if (y_offset < 0)
  {
    ikasama_distance = -y_offset;
    y_offset = 0;
  }
  bool  with_shadow    = read_int(3);

  // 以下定義ファイルから読み込む
  
  float viewplane_x    = read_float(); // スクリーン x
  float viewplane_y    = read_float(); // スクリーン y
  float viewplane_z    = read_float(); // スクリーン z

  float viewangle_x                = read_float() * rad_deg; // 視線の角度 x (度→ラジアン)
  const float cos_viewangle_x      = f_cos(viewangle_x);
  const float sin_viewangle_x      = f_sin(viewangle_x);
  float viewangle_y                = read_float() * rad_deg; // 視線の角度 y (度→ラジアン)
  const float cos_viewangle_y      = f_cos(viewangle_y);
  const float sin_viewangle_y      = f_sin(viewangle_y);
  const float relative_viewpoint_x =  cos_viewangle_x * sin_viewangle_y * z0; // スクリーンからの相対視点 x
  const float relative_viewpoint_y = -sin_viewangle_x                   * z0; // スクリーンからの相対視点 y
  const float relative_viewpoint_z =  cos_viewangle_x * cos_viewangle_y * z0; // スクリーンからの相対視点 z
  const float viewpoint_x          = relative_viewpoint_x + viewplane_x; // 視点 x
  const float viewpoint_y          = relative_viewpoint_y + viewplane_y; // 視点 y
  const float viewpoint_z          = relative_viewpoint_z + viewplane_z; // 視点 z

  float lsn = read_float(); // 光源の個数 (not used)

  // (lsx, lsy, lsz) 光源 (light source) の向きの単位ベクトル
  float lsx;
  float lsy = read_float() * rad_deg; // 光源の向き 1 (ラジアン)
  float tmp = f_cos(lsy);
  float lsz = read_float() * rad_deg; // 光源の向き 2 (ラジアン)
  lsx       =  f_sin(lsz) * tmp;
  lsy       = -f_sin(lsy);
  lsz       =  f_cos(lsz) * tmp;
  
  float beam = read_float(); // 鏡面反射で光源の方向を向いた時のハイライト強度パラメータ
  
  // プリミティブ定義
  int primitive_no;
  {
    int primitive_i;
    for (primitive_i = 0; ; primitive_i++)
    {
      PrimitiveData_i &dsi = ds[primitive_i];
      
      dsi[pTX] = read_int();  // TX : ユーザ定義テクスチャ番号 0, 1, 2, 3, 4
      if (dsi[pTX] < 0)
	break; // プリミティブ定義終了
      if (primitive_i == MAX_PRIMITIVE)
	return "%s: There are too many PRIMITIVE definitions!\n";

      dsi[pP ] = read_int(); // P   : プリミティブ種類コード 1, 2, 3, 4
      dsi[pSF] = read_int(); // SF  : 表面特性 1, 2, 3 : 透明(謎)
      dsi[pRT] = read_int(); // RT  : 回転フラグ 0, 1
      
      PrimitiveData_f &di = d[primitive_i];
      di[pa] = read_float(); // a   : プリミティブパラメータ
      di[pb] = read_float(); // b   : プリミティブパラメータ
      di[pc] = read_float(); // c   : プリミティブパラメータ

      if (dsi[pP] == 3) // 2 次曲面ならば 1 / a^2 へ補正
      {
	float tmp = di[pa];
	if (tmp != 0)
	  di[pa] = f_sgn(tmp) / f_sq(tmp);
	tmp = di[pb];
	if (tmp != 0)
	  di[pb] = f_sgn(tmp) / f_sq(tmp);
	tmp = di[pc];
	if (tmp != 0)
	  di[pc] = f_sgn(tmp) / f_sq(tmp);
      }

      di[pX] = read_float(); // x   : オフセット
      di[pY] = read_float(); // y   : オフセット
      di[pZ] = read_float(); // z   : オフセット

      di[pSG ] = read_float(); // SG  : 極性 1, -1
      di[pREF] = read_float(); // REF : 表面乱反射率 0 〜 1
      di[pHL ] = read_float(); // HL  : 乱反射面ハイライト強度 0 〜 255
      
      if (dsi[pP] == 2) // 平面ならば法線ベクトルの長さを 1 に正規化
      {
	float tmp = f_sqrt_inverse(f_sq(di[pa]) + f_sq(di[pb]) + f_sq(di[pc]));
	if (0 < SIGN(di[pSG])) // SG : 極性 1, -1
	{
	  di[pSG] = SIGN(-1);
	  tmp = -tmp;
	}
	di[pa] *= tmp;
	di[pb] *= tmp;
	di[pc] *= tmp;
      }
      
      di[pR] = read_float(); // r   : カラーコード
      di[pG] = read_float(); // g   : カラーコード
      di[pB] = read_float(); // b   : カラーコード

      if (ds[primitive_i][pRT] == 1)
      {
	// 3 次元回転行列で回転させているらしい
	// 2 時曲面プリミティブ以外には意味がない
	
	float tmp;
	tmp = read_float() * rad_deg; // θx : 回転角
	float cx   = f_cos(tmp);
	float sx   = f_sin(tmp);
	tmp = read_float() * rad_deg; // θy : 回転角
	float cy   = f_cos(tmp);
	float sy   = f_sin(tmp);
	float sxsy = sx * sy;
	float sxcy = sx * cy;
	float cxsy = cx * sy;
	float cxcy = cx * cy;
	tmp = read_float() * rad_deg; // θz : 回転角
	float cz   = f_cos(tmp);
	float sz   = f_sin(tmp);
	float cysz = cy * sz;
	float cycz = cy * cz;
	float tmp1 = sxsy * cz - cx * sz;
	float tmp2 = sxsy * sz + cx * cz;
	float tmp3 = cxsy * cz + sx * sz;
	float tmp4 = cxsy * sz - sx * cz;
	float di0  = di[pa];
	float di1  = di[pb];
	float di2  = di[pc];

	float di0tmp1 = di0 * tmp1;
	float di0tmp3 = di0 * tmp3;
	float di1tmp2 = di1 * tmp2;
	float di1tmp4 = di1 * tmp4;
	float di2sxcy = di2 * sxcy;
	float di2cxcy = di2 * cxcy;
	di[pa ] =      di0 * f_sq(cycz) + di1 * f_sq(cysz) + di2 * f_sq(sy);
	di[pb ] =      di0tmp1 * tmp1   + di1tmp2 * tmp2   + di2sxcy * sxcy;
	di[pc ] =      di0tmp3 * tmp3   + di1tmp4 * tmp4   + di2cxcy * cxcy;
	di[pRX] = 2 * (di0tmp1 * tmp3   + di1tmp2 * tmp4   + di2sxcy * cxcy);
	di[pRY] = 2 * (di0tmp3 * cycz   + di1tmp4 * cysz   - di2cxcy * sy);
	di[pRZ] = 2 * (di0tmp1 * cycz   + di1tmp2 * cysz   - di2sxcy * sy);
      }
    }
    primitive_no = primitive_i;
  }
  
  // AND 定義
  {
    for (int and_i = 0; ; and_i++)
    {
      if ((dr[and_i][0] = read_int()) < 0)
	break; // AND 定義終了
      if (and_i == MAX_AND)
	return "%s: There are too many AND definitions!\n";
      for (int i = 1; ; i++)
	if ((dr[and_i][i] = read_int()) < 0)
	  break;
    }
  }
  
  // OR 定義
  {
    for (int or_i = 0; ; or_i++)
    {
      if ((dm[or_i][0] = read_int()) < 0)
	break; // OR 定義終了
      if (or_i == MAX_OR)
	return "%s: There are too many OR definitions!\n";
      for (int i = 1; ; i++)
	if ((dm[or_i][i] = read_int()) < 0)
	  break;
    }
  }
  
  ///////////////////////////////////////////////////////////////////////////////
  // メインループ
  //

  int ikasama_dots = 0;

  for (int scany0 = y_offset; scany0 < screen_height; scany0++)
  {
    const float yo1   = dx_or_dy * (offset - 1 - scany0); // 回転／移動する前のスクリーン上での scany0 の y 座標
    
    // 視線の向きの単位ベクトルを作る準備
    const float met1  = f_sq(yo1) + sq_z0; // (��met : 直線[y=yo1, z=0] と 直線[y=0, z=z0] の間の距離)^2
    const float yss12 = yo1 * sin_viewangle_x * sin_viewangle_y - relative_viewpoint_x; // x (未完成)
    const float yc_12 = yo1 * cos_viewangle_x                   - relative_viewpoint_y; // y (完成)
    const float ysc12 = yo1 * sin_viewangle_x * cos_viewangle_y - relative_viewpoint_z; // z (未完成)
    
    // 前々回の色
    int pre_red;
    int pre_green;
    int pre_blue;
    
    for (int scanx0 = 0; scanx0 < screen_width; ) // scanx0++)
    {
      float energy = 1.0f; // エナジー (反射するたびに弱まっていく。 REF によって弱まりかたが変わる)
      
      ipx = dx_or_dy * (scanx0 - offset); // 回転／移動する前のスクリーン上での scanx0 の x 座標
      ipy = yo1;
      
      // (ex, ey, ez) : 視線の向きの単位ベクトル
      float metric = f_sqrt_inverse(f_sq(ipx) + met1); // 視線の向きのベクトルの大きさの逆数
      ex = metric * (yss12 + ipx * cos_viewangle_y);
      ey = metric *  yc_12;
      ez = metric * (ysc12 - ipx * sin_viewangle_y);
      
      // (VPx, VPy, VPz) : 視^H始点 (鏡で反射をすると、位置が変わる)
      VPx = viewpoint_x;
      VPy = viewpoint_y;
      VPz = viewpoint_z;
      
      ipi = -1;
      
      // スクリーン上の色
      int red   = 0;
      int green = 0;
      int blue  = 0;
      
      write_flush();
      
      for (int ref = 0; ; ref++) // 反射回数
      {
	SIGN result = tracer(); // (ipx, ipy, ipz) と ipi と nidx が更新される
	
	write_flush();
	
	if (result < 0) // 交わらなかった 
	{
	  if (ref != 0)
	  {
	    // test highlight
	    float hl_f = -ex * lsx - ey * lsy - ez * lsz; // -cosθ : -光源単位ベクトルと視線単位ベクトルの内積
	    if (hl_f < 0)
	      hl_f = 0.0f;
	    int hl = (int)(f_sq(hl_f) * hl_f * energy * beam); // 視線ベクトルが光源の方向をむいているほど明るい(^3)
	    red   += hl;
	    green += hl;
	    blue  += hl;
	  }
	  break;
	}
	
	// 法線ベクトルを生成
	normal_vector();
	
	write_flush();
	
	// 視^H始点の位置を更新
	VPx = ipx;
	VPy = ipy;
	VPz = ipz;
	
	// brightness control & shadowing
	float br1 = nx * lsx + ny * lsy + nz * lsz; // cosθ : 光源単位ベクトルと法線の内積
	if (0 < br1)
	  br1 = 0.0f; // 光が当たっていない

	PrimitiveData_f &di = d[ipi];
	
	float bright = (0.2f - br1) // 明るさを 0.2 だけ底上げする
	  * energy // 鏡面反射で光がだんだん弱まると energy も 1 から減ってゆく
	  * di[pREF]; // REF : 表面乱反射率 0 〜 1
	if (with_shadow) // 影を計算
	{
	  // 視線の向きの単位ベクトルと、
	  // 光源の向きの単位ベクトルを入れ替え
	  swap(&ex, &lsx, &ey, &lsy, &ez, &lsz);
	  bright *= shadow_evaluator();
	  swap(&ex, &lsx, &ey, &lsy, &ez, &lsz);
	  
	  write_flush();
	}
	// brightness determined
	
	if (bright != 0.0) // CHANGED (ADDED)
	  user_defined_texture(); // ユーザー定義テクスチャ
	
	red   += i_round(bright * di[pR]);
	green += i_round(bright * di[pG]);
	blue  += i_round(bright * di[pB]);
	
	write_flush();
	
	if (energy < 0.1 || // 反射エネルギーが小さいか
	    4 < ref) // 反射回数が 4 回以上の時
	  break; // 終わり
	
	PrimitiveData_i &dsi = ds[ipi];
	if (dsi[pSF] == 1) // SF == 1 乱反射
	{
	  if (0 < di[pHL]) // simple high light
	  {
	    // 反射 : 視線ベクトルを反射方向へと向きを変える
	    float tmp = -2 * (ex * nx + ey * ny + ez * nz); // -2cosθ : -2 * 法線単位ベクトルと視線単位ベクトルの内積
	    ex += nx * tmp;
	    ey += ny * tmp;
	    ez += nz * tmp;
	    
	    float hl_f = -ex * lsx - ey * lsy - ez * lsz;
	    // -cosθ : -光源単位ベクトルと視線単位ベクトルの内積
	    if (hl_f < 0)
	      hl_f = 0.0f;
	    int hl = (int)(f_sq(f_sq(hl_f)) * energy * bright * di[pHL]); // 視線ベクトルが光源の方向をむいているほど明るい(^4)
	    red   += hl;
	    green += hl;
	    blue  += hl;
	  }
	  break;
	}
	else if (dsi[pSF] == 2) // SF == 2 鏡面 reflection support
	{
	  energy *= (1 - di[pREF]); // エナジーを弱める
	  
	  // 反射
	  float tmp = -2 * (ex * nx + ey * ny + ez * nz);
	  ex += nx * tmp;
	  ey += ny * tmp;
	  ez += nz * tmp;
	}
	else
	  break;
      }
      
      if (255 < red  ) red   = 255;
      if (255 < green) green = 255;
      if (255 < blue ) blue  = 255;

      if (ikasama_distance)
      {
	ikasama_dots++;
	
	if (scanx0 == 0)
	{
	  pre_red   = red;
	  pre_green = green;
	  pre_blue  = blue;
	  scanx0 += 2;
	}
	else if (scanx0 % 2 == 0)
	{
	  write_enq(pre_red);
	  write_enq(pre_green);
	  write_enq(pre_blue);
	  if ((pre_red   - red  ) * (pre_red   - red  ) +
	      (pre_green - green) * (pre_green - green) +
	      (pre_blue  - blue ) * (pre_blue  - blue )
	      <= ikasama_distance)
	  {
	    write_enq((pre_red   + red  ) / 2);
	    write_enq((pre_green + green) / 2);
	    write_enq((pre_blue  + blue ) / 2);
	    scanx0 += 2;
	  }
	  else
	    scanx0 --;
	  pre_red   = red;
	  pre_green = green;
	  pre_blue  = blue;
	}
	else
	{
	  write_enq(red);
	  write_enq(green);
	  write_enq(blue);
	  scanx0 += 3;
	}
	
	if (screen_width + 2 == scanx0)
	  ;
	else if (screen_width + 1 == scanx0)
	{
	  write_enq(pre_red);
	  write_enq(pre_green);
	  write_enq(pre_blue);
	}
	else if (screen_width == scanx0)
	{
	  write_enq(pre_red);
	  write_enq(pre_green);
	  write_enq(pre_blue);
	  scanx0 --;
	}
      }
      else
      {
	write_enq(red);
	write_enq(green);
	write_enq(blue);
	scanx0++;
      }
    }
  }
  
  write_flush_all();
  
  if (ikasama_distance)
    fprintf(stderr, "IKASAMA calculated %d pixels. (%dx%d=%d)\n",
	    ikasama_dots, screen_width, screen_height, screen_width * screen_height);
  
  return NULL;
}


///////////////////////////////////////////////////////////////////////////////
// SUN 側サポート
//


char *appname; // RayTrace の名前
FILE *input_fp  = stdin ; // 入力ファイル
FILE *output_fp = stdout; // 出力ファイル


// CPU 側の実装では RS232C へ書く関数
void write_value(BYTE value)
{
  fwrite(&value, 1, 1, output_fp);
}


// CPU 側の実装では RS232C から読む関数
float read_float(void)
{
#define DELIMITER "x(){}[], \t\r\n"
  static char buf[1024];
  static bool is_this_first_call = true;
  char *num = NULL;
  if (is_this_first_call)
    is_this_first_call = false;
  else
    num = strtok(NULL, DELIMITER);
  while (1)
  {
    if (num && *num != '#' && *num != ';')
      return (float)atof(num);
    if (!fgets(buf, sizeof(buf), input_fp))
    {
      fprintf(stderr, "%s: data format error.\n", appname);
      exit(1);
    }
    num = strtok(buf, DELIMITER);
  }
#undef DELIMITER
}


// CPU 側の実装では RS232C から読む関数
int read_int(void)
{
  return (int)read_float();
}


int  _screen_width  = 256;
int  _screen_height = 256;
int  _y_offset      = 0;
int  _with_shadow   = false;


int read_int(int i)
{
  if (i == 0) return _screen_width;
  if (i == 1) return _screen_height;
  if (i == 2) return _y_offset;
  return _with_shadow;
}


char *output_filename = NULL;


// オプション解析
bool option(int &argc, char *argv[])
{
  bool r = true;
  while (1 < argc)
  {
    int remove = 2;
    if (!strcmp(argv[1], "--help") ||
	!strcmp(argv[1], "-help"))
    {
      fprintf(stderr,
	      "usage: %s [-w <width>] [-h <height>] [-s] [-O <offset>] [-o <outfile.ppm>] [<infile.sld>]\n"
	      "<infile.sld>    : source filename        (default:stdin)\n"
	      "<outfile.ppm>   : output filename        (default:stdout)\n"
	      "-w <width>      : image width            (default:256)\n"
	      "-h <height>     : image height           (default:256)\n"
	      "-O <offset>     : y-direction INT offset (default:0)\n"
	      "-s              : shadowing on \n"
	      "-I <distance^2> : ikasama distance^2 (on color space)\n"
	      "-i              : equals -I 16\n"
	      "                  if -I and -O are specified, the last of them is effective.\n"
	      "\n"
	      "example: %s -w 256 -h 222 -O 30 -s -o tron.ppm tron.sld\n",
	      appname, appname);
      remove = 1;
      r = false;
    }
    else if (!strcmp(argv[1], "-w") && 3 <= argc) _screen_width   = atoi(argv[2]);
    else if (!strcmp(argv[1], "-h") && 3 <= argc) _screen_height  = atoi(argv[2]);
    else if (!strcmp(argv[1], "-O") && 3 <= argc) _y_offset       = atoi(argv[2]);
    else if (!strcmp(argv[1], "-s")             ) _with_shadow = true, remove = 1;
    else if (!strcmp(argv[1], "-I") && 3 <= argc) _y_offset = -atoi(argv[2]);
    else if (!strcmp(argv[1], "-i")             ) _y_offset = -16, remove = 1;
    else if (!strcmp(argv[1], "-o") && 3 <= argc) output_filename = argv[2];
    else
      break;
    for (int i = 1 + remove; i < argc; i++)
      argv[i - remove] = argv[i];
    argc -= remove;
  }
  return r;
}


// メイン
int main(int argc, char *argv[])
{
  appname = argv[0];
  if (!option(argc, argv))
    return 1;
  
  if (1 < argc)
    if ((input_fp = fopen(argv[1], "rt")) == NULL)
    {
      fprintf(stderr,  "%s: %s: no such file\n",  appname, argv[1]);
      return 1;
    }
  
  if (output_filename)
    if ((output_fp = fopen(output_filename, "wb")) == NULL)
    {
      fprintf(stderr,  "%s: %s: cannot create file.\n",  appname, output_filename);
      return 1;
    }
  
#ifdef WIN32
  _setmode(fileno(output_fp), _O_BINARY);
#elif __CYGWIN32__
  if (fileno(output_fp) == 1)
  {
    fprintf(stderr, "stdout: not supported on gnu-win32(CYGWIN32). use -o <outfile.ppm>\n");
    return 1;
  }
#endif
  char *errstr = cpu_main();
  if (errstr)
    fprintf(stderr, errstr, appname);
  
  fclose(output_fp);
  fclose(input_fp);
  return !!errstr;
}
