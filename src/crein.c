#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define NMAX 10
#define EPS1 0.00000000000000001
#define EPS2 0.000000000000001
#define EPS3 0.0000001
#define EPS4 0.0001

double probl[NMAX] = {0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

int n = 6;
double matr[NMAX * NMAX * NMAX] = {
    1, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 1,

/*                               0,  0, 30,  0,  0,  0,
                               0,  0,  5,  0, 25,  0,
                               1,  8,  1,  0,  8, 12,
                               0,  0,  0,  5, 10, 15,
                               0,  5,  1,  4,  8, 12,
                               0,  0,  1,  4,  8, 17,

                               0, 48,  0,  0,  0,  0,
                               1, 23,  0,  4,  5, 15,
                               0,  0,  8,  0, 40,  0,
                               0,  2,  0,  6, 10, 30,
                               0,  1,  5,  4, 23, 15,
                               0,  2,  0,  8, 10, 28,

                               0,  0,  0, 96,  0,  0,
                               0,  4,  0, 12, 20, 60,
                               0,  0,  0, 16, 32, 48,
                               1,  6,  5, 24, 30, 30,
                               0,  4,  4, 12, 28, 48,
                               0,  8,  4,  8, 32, 44,

                               0,  0,  0,  0,240,  0,
                               0,  5, 25, 20,115, 75,
                               0, 40,  8, 32, 64, 96,
                               0, 10, 10, 30, 70,120,
                               1, 23,  8, 28, 69,111,
                               0, 10,  8, 32, 74,116,

                               0,  0,  0,  0,  0,360,
                               0, 15,  0, 60, 75,210,
                               0,  0, 12, 48, 96,204,
                               0, 30, 15, 30,120,165,
                               0, 15, 12, 48,111,174,
                               1, 28, 17, 44,116,154

                               0,  0, 42,  0,  0,  0,
                               0,  0,  6,  0, 36,  0,
                               1, 10,  1,  0, 10, 20,
                               0,  0,  0,  6, 12, 24,
                               0,  6,  1,  5, 10, 20,
                               0,  0,  1,  5, 10, 26,

                               0, 70,  0,  0,  0,  0,
                               1, 34,  0,  5,  6, 24,
                               0,  0, 10,  0, 60,  0,
                               0,  2,  0,  8, 12, 48,
                               0,  1,  6,  5, 34, 24,
                               0,  2,  0, 10, 12, 46,

                               0,  0,  0,175,  0,  0,
                               0,  5,  0, 20, 30,120,
                               0,  0,  0, 25, 50,100,
                               1,  8,  6, 40, 48, 72,
                               0,  5,  5, 20, 45,100,
                               0, 10,  5, 15, 50, 95,

                               0,  0,  0,  0,420,  0,
                               0,  6, 36, 30,204,144,
                               0, 60, 10, 50,100,200,
                               0, 12, 12, 48,108,240,
                               1, 34, 10, 45,106,224,
                               0, 12, 10, 50,112,236,

                               0,  0,  0,  0,  0,840,
                               0, 24,  0,120,144,552,
                               0,  0, 20,100,200,520,
                               0, 48, 24, 72,240,456,
                               0, 24, 20,100,224,472,
                               1, 46, 26, 95,236,436 */

    0, 0, 110, 0, 0, 0,
    0, 0, 10, 0, 100, 0,
    1, 18, 1, 0, 18, 72,
    0, 0, 0, 10, 20, 80,
    0, 10, 1, 9, 18, 72,
    0, 0, 1, 9, 18, 82,

    0, 198, 0, 0, 0, 0,
    1, 98, 0, 9, 10, 80,
    0, 0, 18, 0, 180, 0,
    0, 2, 0, 16, 20, 160,
    0, 1, 10, 9, 98, 80,
    0, 2, 0, 18, 20, 158,

    0, 0, 0, 891, 0, 0,
    0, 9, 0, 72, 90, 720,
    0, 0, 0, 81, 162, 648,
    1, 16, 10, 144, 160, 560,
    0, 9, 9, 72, 153, 648,
    0, 18, 9, 63, 162, 639,

    0, 0, 0, 0, 1980, 0,
    0, 10, 100, 90, 980, 800,
    0, 180, 18, 162, 324, 1296,
    0, 20, 20, 160, 340, 1440,
    1, 98, 18, 153, 334, 1376,
    0, 20, 18, 162, 344, 1436,

    0, 0, 0, 0, 0, 7920,
    0, 80, 0, 720, 800, 6320,
    0, 0, 72, 648, 1296, 5904,
    0, 160, 80, 560, 1440, 5680,
    0, 80, 72, 648, 1376, 5744,
    1, 158, 82, 639, 1436, 5604
};
double xmax = 110;

/*int n=4;
double matr[NMAX*NMAX]={1,2,3,4,2,1,2,3,3,2,1,2,4,3,2,1};
double xmax=10;*/

static void xscanf(char *p) {
	if (scanf("%c", p) != 1) {
		fprintf(stderr, "scanf error\n");
		exit(1);
	}
}

static void lincomb(n, m, p, m1)
    int n;
    double m[], p[], m1[];
{
    int n2, i, j, k;
    double x;

    for (n2 = n * n, i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	    for (x = k = 0; k < n; k++, m1[n * i + j] = x)
		x += p[k] * m[n2 * k + n * i + j];
}

static void cpol(n, a, c, q)
    int n;
    double a[], c[], q[];
{
    int i, j, k, li, lj, l;
    double b[NMAX], x, y;
    long d;

    for (i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	    q[n * i + j] = (i == j) ? 1 : 0;
    for (k = n - 1; k > 0; k--) {
	for (x = i = 0; i <= k; i++)
	    for (j = 0; j < k; j++)
		if ((y = fabs(a[n * i + j])) > x) {
		    x = y;
		    li = i;
		    lj = j;
		}
/*  printf("x=%8.2f,li=%d,lj=%d\n\n",x,li,lj);*/
	for (i = 0; i < n; i++) {
	    x = a[n * i + k];
	    a[n * i + k] = a[n * i + li];
	    a[n * i + li] = x;
	    x = q[n * i + k];
	    q[n * i + k] = q[n * i + li];
	    q[n * i + li] = x;
	}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (j = 0; j < n; j++) {
	    x = a[n * k + j];
	    a[n * k + j] = a[n * li + j];
	    a[n * li + j] = x;
	}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (i = 0; i < n; i++) {
	    x = a[n * i + k - 1];
	    a[n * i + k - 1] = a[n * i + lj];
	    a[n * i + lj] = x;
	    x = q[n * i + k - 1];
	    q[n * i + k - 1] = q[n * i + lj];
	    q[n * i + lj] = x;
	}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (j = 0; j < n; j++) {
	    x = a[n * (k - 1) + j];
	    a[n * (k - 1) + j] = a[n * lj + j];
	    a[n * lj + j] = x;
	}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
  printf("\n");
  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",q[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (j = 0; j < n; j++)
	    b[j] = a[n * k + j];
	for (x = a[n * k + k - 1], i = 0; i < n; i++) {
	    a[n * i + k - 1] /= x;
	    q[n * i + k - 1] /= x;
	}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
  printf("\n");
  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",q[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (j = 0; j < n; j++)
	    if (j != k - 1)
		for (i = 0; i < n; i++) {
		    a[n * i + j] -= a[n * i + k - 1] * b[j];
		    q[n * i + j] -= q[n * i + k - 1] * b[j];
		}
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
  printf("\n");
  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",q[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
	for (j = 0; j < n; j++)
	    for (c[j] = l = 0; l < n; l++)
		c[j] += a[n * l + j] * b[l];
	for (j = 0; j < n; j++)
	    a[n * (k - 1) + j] = c[j];
/*  for (i=0; i<n; i++)
   {
   for (j=0; j<n; j++)
    printf(" %8.2f",a[n*i+j]);
   printf("\n");
   }
   xscanf(&i);*/
    }
    for (j = 0; j < n; j++) {
	c[j] = -((long) (a[j] + ((a[j] > 0) ? 0.5 : -0.5)));
/*   printf("%8.20f %8.20f\n",a[j],c[j]);*/
    }
}

static void roots(n, a, xmax, x)
    int n;
    double a[], xmax, x[];
{
    int k, i;
    double y, u, p, p1, d, b[NMAX];

    for (y = xmax, k = n; k > 0; x[n - (k--)] = y) {
/*  printf("a:"); for (i=0; i<k; i++) printf(" %0.2f",a[i]); printf("\n");*/
	do {
	    for (p = 1, i = 0; i < k; i++)
		p = p * y + a[i];
	    for (p1 = k, i = 1; i < k; i++)
		p1 = p1 * y + a[i - 1] * (k - i);
/*  printf("y=%0.20f,p=%0.20f,p1=%0.20f,d=%0.20f\n",y,p,p1,p/p1);
    xscanf(&i);*/
	    y -= (d = p / p1);
	}
	while (fabs(p) > EPS1 && fabs((fabs(y) < 1) ? d : d / y) > EPS2);
	u = ((long) (y + ((y > 0) ? 0.5 : -0.5)));
	if (fabs(y - u) < EPS3)
	    y = u;
/*  printf("x[%d]=%0.8f\n",n-k,y);*/
	if (k > 1)
	    for (a[0] += y, i = 1; i < k - 1; i++)
		a[i] += a[i - 1] * y;
    }
}

static void egvect(n, r, q, e)
    int n;
    double r[], q[], e[];
{
    int i, j, k;
    double x, y;

    for (i = 0; i < n; i++) {
	for (y = j = 0; j < n; e[n * i + (j++)] = x, y += x * x)
	    for (x = k = 0; k < n; k++)
		x = x * r[i] + q[n * j + k];
	for (y = sqrt(y), j = 0; j < n; j++)
	    e[n * i + j] /= y;
    }
}

static void egmatr(n, matr, egv, egm, s)
    int n;
    double matr[], egv[], egm[], s[];
{
    int n2, i, j, k, l, a;
    double x, y, z, u, max, min;

    for (n2 = n * n, i = 0; i < n; i++)
	for (j = 0; j < n; egm[n * i + j] = y * sqrt(x), s[n * i + (j++)] = max - min) {
	    for (min = max = x = a = k = 0; k < n; k++, x += z * z) {
		for (z = l = 0; l < n; l++)
		    z += matr[n2 * i + n * k + l] * egv[n * j + l];
		if (!k)
		    y = (z * egv[n * j] > 0) ? 1.0 : -1.0;
		if (fabs(u = egv[n * j + k]) > EPS4) {
		    u = z / u;
		    if (!a || u > max)
			max = u;
		    if (!a || u < min)
			min = u;
		    a = 1;
		}
	    }
	    z = ((long) (x + ((x > 0) ? 0.5 : -0.5)));
	    if (fabs(x - z) < EPS3)
		x = z;
	}
}

static void egmult(n, p, c, m)
    int n;
    double p[], m[], *c;
{
    int i, j;
    double x, u;

    for (*c = i = 0; i < n; i++)
	(*c) += p[n * i];
    u = ((long) (*c + 0.5));
    if (fabs(*c - u) < EPS3)
	*c = u;
    for (i = 0; i < n; i++) {
	for (x = j = 0; j < n; j++)
	    x += p[n * j + i] * p[n * j + i] / p[n * j];
	u = ((long) ((x = *c / x) + 0.5));
	m[i] = (fabs(x - u) < EPS3) ? u : x;
    }
}

static void creinpar(n, c, m, p, q)
    int n;
    double c, m[], p[], q[];
{
    int n2, i, j, k, l;
    double x;

    for (n2 = n * n, i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	    for (k = 0; k < n; q[n2 * i + n * j + (k++)] = x * m[i] * m[j] / c)
		for (x = l = 0; l < n2; l += n)
		    x += p[l + i] * p[l + j] * p[l + k] / (p[l] * p[l]);
}

static int absbound(n, m, c, i, j, s1, s2)
    int n, *i, *j;
    double m[], c[];
    long *s1, *s2;
{
    int n2, k;
    char ch;

    for (n2 = n * n, *i = 0; *i < n; (*i)++)
	for (*j = 0; *j < n; (*j)++) {
	    for (*s2 = ((i == j) ? m[*i] * (m[*i] + 1) / 2 : m[*i] * m[*j]),
			 *s1 = k = 0; k < n; k++)
		if (c[n2 * (*i) + n * (*j) + k] > EPS3)
		    (*s1) += m[k];
	    printf("i=%d,j=%d,s1=%ld,s2=%ld\n", *i, *j, *s1, *s2);
	    xscanf(&ch);
	    if ((*s1) > (*s2))
		return 0;
	}
    return 1;
}

int main()
{
    double mt[NMAX * NMAX];
    double coef[NMAX], rts[NMAX], q[NMAX * NMAX], egv[NMAX * NMAX], egm[NMAX * NMAX];
    double sigma[NMAX * NMAX], mult[NMAX], card, crpar[NMAX * NMAX * NMAX];
    double min, max, u, x;
    int n2, i, j, k;
    long m1, m2;
    char c;

/* for (i=0; i<n; i++)
  for (j=0; j<n; j++)
   mt[i*n+j]=matr[i*n+j];*/

    lincomb(n, matr, probl, mt);
    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++)
	    printf(" %8.0f", mt[n * i + j]);
	printf("\n");
    }
    xscanf(&c);
    cpol(n, mt, coef, q);
    printf("pol:");
    for (i = 0; i < n; i++)
	printf(" %8.0f", coef[i]);
    printf("\nq:\n");
    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++)
	    printf(" %10.6f", q[n * i + j]);
	printf("\n");
    }
    xscanf(&c);
    roots(n, coef, xmax, rts);
    printf("rts:");
    for (i = 0; i < n; i++)
	printf(" %6.20f", rts[i]);
    printf("\n");
    xscanf(&c);
    egvect(n, rts, q, egv);
    printf("egv:\n");
    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++)
	    printf(" %10.6f", egv[n * i + j]);
	printf("\n");
    }
    xscanf(&c);
    egmatr(n, matr, egv, egm, sigma);
    printf("egm:\n");
    for (i = 0; i < n; i++) {
	for (j = 0; j < n; j++)
	    printf(" %11.6f", egm[n * i + j]);
	printf("\n");
    }
    printf("\nsigma:\n");
    for (max = i = 0; i < n; i++) {
	for (j = 0; j < n; j++) {
	    printf(" %8.6f", (u = sigma[n * i + j]));
	    if (max < u)
		max = u;
	}
	printf("\n");
    }
    printf("max=%8.20f\n", max);
    xscanf(&c);
    egmult(n, egm, &card, mult);
    printf("card=%10.20f\nmult:\n", card);
    for (i = 0; i < n; i++)
	printf(" %10.20f", mult[i]);
    xscanf(&c);
    creinpar(n, card, mult, egm, crpar);
    printf("crpar:\n");
    for (n2 = n * n, min = i = 0; i < n; i++) {
	for (j = 0; j < n; j++) {
	    for (k = 0; k < n; k++) {
		printf(" %12.7f", (u = crpar[n2 * i + n * j + k]));
		if (u < min)
		    min = u;
	    }
	    printf("\n");
	}
	printf("\n");
    }
    printf("min=%8.20f\n", min);
    xscanf(&c);
    if (!absbound(n, mult, crpar, &i, &j, &m1, &m2))
	printf("i=%d,j=%d,m1=%ld,m2=%ld\n", i, j, m1, m2);
    return 0;
}
