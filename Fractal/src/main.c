#include <stdio.h>
#include "image.h"
#include "fractal.h"

int main()
{
  image_p img = create_image(500, 500);

  fill_white(img);
  fractal(img, SIERPINSKI);
  save_pgm(img, "fractal1.pgm");

  fill_white(img);
  fractal(img, MANDELBROT);
  save_pgm(img, "fractal2.pgm");

  return 0;
}
