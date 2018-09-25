//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: J. Li (8/2018)
//-------------------------------------------------------------------------

// Quiksort in C
//
// Usage: linux> ./qsort N
// 
// 
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#define MINSIZE   10 

// Swap two array elements, a[i] <-> a[j]
void swap(int *a, int i, int j) {
  if (i == j) return;
  int tmp = a[i];
  a[i] = a[j];
  a[j] = tmp;
}

// Create an array to contain a random permutation of 1..N
int *createArray(int N) {
  int *a = (int *) malloc(sizeof(int) * N);
  int i, j;
  for (i=0; i<N; i++)
    a[i] = i + 1;
  for (i=0; i<N; i++) {
    j = (rand()*1./RAND_MAX)*(N-1); // a random val in [0,N-1]
    swap(a, i, j);
  }
  return a;
}

// Bubble sort for the base cases
void bubbleSort(int *a, int low, int high) {
  if (low >= high) return;
  int i, j;
  for (i = low; i <= high; i++)
    for (j = i+1; j <= high; j++) 
      if (a[i] > a[j])
	swap(a, i, j);
}

// Partition an array range a[low, high] into two sub-ranges
int partition(int *a, int low, int high) {
  int pivot, middle, i;
  pivot = a[high]; 	// use highest element as pivot 
  middle = low;
  for (i=low; i<high; i++)
    if (a[i] < pivot) {
      swap(a, i, middle);
      middle++;
    }
  swap(a, high, middle);
  return middle;
}

// QuickSort an array range a[low, high]
void quickSort(int *a, int low, int high) {
  if (high - low < MINSIZE) {
    bubbleSort(a, low, high);
  } else {
    int middle = partition(a, low, high);
    if (low < middle)
      quickSort(a, low, middle-1);
    if (middle < high)
      quickSort(a, middle+1, high);
  }
}
 
// Print an array
void printArray(int *a, int N) {
  int i;
  for (i=0; i<N; i++)
    printf("%d, ", a[i]);
  printf("\n");
}

// Verify a sorted array
void verifyArray(int *a, int N) {
  int i;
  for (i=0; i<N-2; i++)
    if (a[i]>a[i+1]) {
      printf("FAILED: a[%d]=%d, a[%d]=%d\n", 
	     i, a[i], i+1, a[i+1]);
      return;
    }
  printf("Result verified!\n");
}

// Main routine for testing quickSort
// 
int main(int argc, char **argv)
{
  int *a, N, i, j;
  
  assert(argc > 1);
  N = atoi(argv[1]);
  assert(N > 1);

  a = createArray(N);
  printf("Initial array:\n");
  printArray(a, N);

  quickSort(a, 0, N-1);
  printf("After quickSort:\n");
  printArray(a, N);
  verifyArray(a, N);

}
