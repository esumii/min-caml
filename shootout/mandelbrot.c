#include<stdio.h>
#include<stdlib.h>

double dbl(double f) {
  return f + f;
}

int main ()
{
  int w, h, x, y;
  int i, iter = 1000;
  double limit = 2.0;
  double zr, zi, zr2, zi2, cr, ci, tr, ti;

  w = 400;
  h = w;

  for(y=0;y<h;y++)
    {
      for(x=0;x<w;x++)
        {
	  zr = 0.0; zi = 0.0; zr2 = 0.0; zi2 = 0.0;
	  cr = (dbl((double)x)/w - 1.5); ci=(dbl((double)y)/h - 1.0);

	  for (i=0;i<iter;i++)
            {
	      tr = zr2 - zi2 + cr;
	      ti = dbl(zr)*zi + ci;
	      zr = tr; zi = ti;
	      zr2 = zr*zr;
	      zi2 = zi*zi;
	      if (zr2+zi2 > limit*limit) {
		putchar('0');
		goto cont;
	      }
            }

	  putchar('1');

	cont:
	  ;
        }
    }

  return(0);
}
