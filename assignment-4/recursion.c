// Nandadeep Davuluru
//-------------------------------------------------------------------------

// Regular vs tail recursive functions
//
#include "stdio.h"

// factorial function

int fac(int n) {
	if (n <= 1) 
		return 1;
	return n * fac(n-1);
}

int facTR(int n) {
	int helper(int k, int acc) {
		if (k <= 1)
			return acc;
		return helper(k-1, k*acc);
	}
	return helper(n, 1);
}

// unknown function g

int g(int n) {
	if (n <= 3) 
		return 1;
	else return g(n-1) + g(n-2) + g(n-3);
}

int gTR(int n) {

	int helper(int n, int a, int b, int c) {
		if (n <= 3) return a;
		return helper(n-1, a+b+c, a, b);

	}
	return helper(n, 1, 1, 1);


}

// unknown function h

int h(int n) {
	if (n <= 1)
		return 1;
	if (n%2 == 0) 
		return h(n/2) + n;
	else
		return h(n-1) + 1;
}

int hTR(int n) {
	int helper(int m, int acc) {

		if (m <= 1) 
			return acc;
		if (m % 2 == 0) 
			return helper(m / 2 , m + acc );
		else 
			return helper(m - 1, acc + 1);

	}
	return helper(n, 1);

}

int main() {
	printf("fac(1-10)   = ");
	for (int i=1; i<10; i++) printf("%d, ", fac(i));
	printf("%d\n", fac(10));

	printf("facTR(1-10) = ");
	for (int i=1; i<10; i++) printf("%d, ", facTR(i));
	printf("%d\n", facTR(10));

	printf("g(1-10)   = ");
	for (int i=1; i<10; i++) printf("%d, ", g(i));
	printf("%d\n", g(10));

	printf("gTR(1-10) = ");
	for (int i=1; i<10; i++) printf("%d, ", gTR(i));
	printf("%d\n", gTR(10));

	printf("h(1-10)   = ");
	for (int i=1; i<10; i++) printf("%d, ", h(i));
	printf("%d\n", h(10));

	printf("hTR(1-10) = ");
	for (int i=1; i<10; i++) printf("%d, ", hTR(i));
	printf("%d\n", hTR(10));
}
