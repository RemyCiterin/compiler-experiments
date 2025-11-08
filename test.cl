
typedef struct {
  char a;
  char b;
  int x;
  unsigned long int y;
  int foo[42];
  float4 z;
} foo;

constant int test_x = 5;
global int x = 'a';
global int test_y = test_x;

global int test_array[100];

__kernel void square(
   __global float* input,
   __global float* output,
   const unsigned int count,
   global foo *f) {


  int i = get_global_id(0);
  if(i < count)
     output[i] = input[i] * input[i];
}
